#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
library(sf)
library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(ggplot2)
library(ggridges)
library(here)
#
#
#
#
#
#
#
out_dir <- "/proj/mhinolab/users/rbless/data/Obstacles_Output"

cfg <- list(
  master_path      = file.path(out_dir, "parcels_master.gpkg"),
  repeat_threshold = 2L
)
#
#
#
#
#
#
#
needed <- c("parcel_index", "cntyfips", "SITUS.CITY",
            "flooded_any", "eligible", "fr_1_4", "interpolate",
            "value", "cent_lat", "cent_lng", "eif_lat", "eif_lon",
            "applied", "funded", "pct_white_2020", "pct_black_2020")

master_cols <- names(st_read(cfg$master_path, quiet = TRUE,
  query = "SELECT * FROM \"parcels_master\" LIMIT 0"))

for (w in needed) {
  cat(if (w %in% master_cols) "[ok]  " else "[MISS] ", w, "\n", sep = "")
}
#
#
#
#
#
#
#
parcels <- st_read(cfg$master_path, quiet = TRUE) |>
  st_drop_geometry() |>
  transmute(
    parcel_index   = as.integer(parcel_index),
    county         = as.character(cntyfips),
    city           = as.character(SITUS.CITY),
    flood_ct       = suppressWarnings(as.numeric(flooded_any)),
    elig_ct        = suppressWarnings(as.numeric(eligible)),
    res_flag       = suppressWarnings(as.integer(fr_1_4)),
    interpolate    = suppressWarnings(as.integer(interpolate)),
    value          = suppressWarnings(as.numeric(value)),
    lat            = suppressWarnings(as.numeric(cent_lat)),
    lng            = suppressWarnings(as.numeric(cent_lng)),
    eif_lat        = suppressWarnings(as.numeric(eif_lat)),
    eif_lon        = suppressWarnings(as.numeric(eif_lon)),
    applied        = as.logical(applied),
    funded         = as.logical(funded),
    pct_white_2020 = suppressWarnings(as.numeric(pct_white_2020)),
    pct_black_2020 = suppressWarnings(as.numeric(pct_black_2020))
  ) |>
  mutate(
    community = if_else(is.na(city) | str_trim(city) == "",
                        paste0("COUNTY_", county),
                        str_to_upper(str_trim(city)))
  )

cat("parcels loaded:", format(nrow(parcels), big.mark = ","), "\n")
#
#
#
if ("bg_pct_white_2013" %in% names(parcels)) {
  cat("BG columns already on parcels — skipping load-bg\n")
} else {
  master_bg <- sf::st_read(
    file.path(out_dir, "parcels_master.gpkg"),
    quiet = TRUE,
    query = paste0(
      "SELECT parcel_index, bg_geoid, ",
      "bg_pct_white_2013, bg_pct_black_2013, ",
      "bg_median_income_2013, bg_median_home_value_2013 ",
      "FROM parcels_master"
    )
  ) |>
    sf::st_drop_geometry() |>
    mutate(parcel_index = as.integer(parcel_index))
  
  parcels <- parcels |>
    left_join(master_bg, by = "parcel_index")
  
  cat("Parcels with BG match:",
      format(sum(!is.na(parcels$bg_geoid)), big.mark = ","), "of",
      format(nrow(parcels), big.mark = ","), "\n")
}
#
#
#
#
#
#
#
# Set of communities where the local government assisted with >=1 HMA
# application (draft Fig. 2 stage-3 definition).
app_communities <- parcels |>
  filter(applied) |>
  distinct(community) |>
  pull(community)

cat("communities with >=1 application:", length(app_communities), "\n")

pipeline <- parcels |>
  # restrict to residential properties (Fig. 2 universe)
  filter(res_flag == 1L | interpolate == 1L) |>
  mutate(
    s1_flooded   = !is.na(flood_ct) & flood_ct >= 1,
    s2_eligible  = !is.na(elig_ct)  & elig_ct  >= 1,
    s3_community = community %in% app_communities,
    s4_applied   = applied,
    s5_funded    = funded,
    repeat_exp   = !is.na(flood_ct) & flood_ct >= cfg$repeat_threshold
  ) |>
  # cumulative "reached stage k" — enforces nesting
  mutate(
    reach1 = s1_flooded,
    reach2 = reach1 & s2_eligible,
    reach3 = reach2 & s3_community,
    reach4 = reach3 & s4_applied,
    reach5 = reach4 & s5_funded
  )
#
#
#
#
#
#
#
funnel_for <- function(df, label) {
  r <- c(sum(df$reach1), sum(df$reach2), sum(df$reach3),
         sum(df$reach4), sum(df$reach5))
  tibble(
    group        = label,
    stage_num    = 1:5,
    stage        = factor(
      c("Flooded", "Eligible", "Community Applied",
        "Applied", "Funded"),
      levels = c("Flooded", "Eligible", "Community Applied",
                 "Applied", "Funded")),
    n            = r,
    pct_of_prev  = c(NA, r[-1] / r[-5]),
    pct_of_flood = r / r[1]
  )
}

funnel_all    <- funnel_for(pipeline,                     "All flood-exposed")
funnel_repeat <- funnel_for(filter(pipeline, repeat_exp), "Repeat flood-exposed")

funnel <- bind_rows(funnel_all, funnel_repeat)

fig_dir <- here::here("figures")
dir.create(fig_dir, showWarnings = FALSE)
readr::write_csv(funnel, file.path(fig_dir, "fig2_funnel_table.csv"))

funnel |>
  mutate(
    n            = format(n, big.mark = ","),
    `% of prev`  = if_else(is.na(pct_of_prev), "—",
                           paste0(round(pct_of_prev * 100, 1), "%")),
    `% of flood` = paste0(round(pct_of_flood * 100, 1), "%")
  ) |>
  select(group, stage, n, `% of prev`, `% of flood`) |>
  knitr::kable()
#
#
#
#
#
stage_order_5 <- c("Flooded", "Eligible", "Community Applied",
                   "Applied", "Funded")

panel_labels_2 <- c(
  "All flood-exposed"    = "A. All flooded properties",
  "Repeat flood-exposed" = "B. Repeat-flooded properties"
)

fig2_data <- funnel |>
  filter(stage != "Eligible") |>             # drop Eligible from displayed bars
  mutate(
    stage = recode(as.character(stage),
                   "In a community" = "Community Applied"),
    stage = factor(stage, levels = c("Flooded", "Community Applied",
                                      "Applied", "Funded")),
    panel = factor(recode(group, !!!panel_labels_2),
                   levels = unname(panel_labels_2)),
    label_text = paste0(
      format(n, big.mark = ","),
      "  (", round(pct_of_flood * 100, 1), "%)"
    )
  )

fig2 <- ggplot(fig2_data, aes(x = n, y = stage)) +
  geom_col(fill = "grey60", width = 0.65) +
  geom_text(aes(label = label_text),
            hjust = -0.06, size = 3.3, colour = "grey20") +
  facet_wrap(~ panel, ncol = 2, scales = "free_x") +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.18)),
    labels = scales::label_dollar(scale = 1e-3, suffix = "k")
  ) +
  scale_y_discrete(limits = rev) +
  labs(
    #title = "Fig. 2  Flooded-to-funded pipeline",
    x = "Number of parcels",
    y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.y        = element_text(face = "bold", colour = "grey15"),
    axis.text.x        = element_text(colour = "grey30"),
    axis.title.x       = element_text(colour = "grey30", margin = margin(t = 6)),
    strip.text         = element_text(face = "bold", hjust = 0,
                                      colour = "grey10", size = 11),
    strip.background   = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.spacing.x    = unit(1.5, "cm"),
    plot.title         = element_text(face = "bold"),
    plot.margin        = margin(10, 14, 10, 10)
  )

fig2
#
#
#
dir.create(here::here("figures"), showWarnings = FALSE)
ggsave(here::here("figures", "fig2.png"),
       fig2, width = 7.48, height = 4, dpi = 300)
#
#
#
#
#
#
#
#
#
#
#
#
#
# Compute Panel B as percentile WITHIN block group instead of EIF cell.
# Filter to BGs with >= 5 parcels for meaningful within-neighborhood rank.
value_df <- pipeline |>
  filter(!is.na(value), value > 0, !is.na(bg_geoid)) |>
  mutate(
    pct_A = dplyr::percent_rank(value) * 100
  ) |>
  group_by(bg_geoid) |>
  mutate(
    bg_n  = dplyr::n(),
    pct_B = if_else(bg_n >= 5, dplyr::percent_rank(value) * 100, NA_real_)
  ) |>
  ungroup()

cat("sample parcels (fr_1_4 OR interp, w/ value + bg):",
    format(nrow(value_df), big.mark = ","), "\n")
cat("distinct block groups:",
    format(dplyr::n_distinct(value_df$bg_geoid), big.mark = ","), "\n")
cat("parcels with valid Panel B percentile (bg_n >= 5):",
    format(sum(!is.na(value_df$pct_B)), big.mark = ","), "\n")
```
#
#
#
#
#
#
stage_levels_4 <- c("Flooded", "Community Applied", "Applied", "Funded")

panel_labels_3 <- c(
  pct_A = "A. Within Study Area",
  pct_B = "B. Within Block Group"
)

fig3_hist_data <- bind_rows(
  value_df |> filter(reach1) |> mutate(stage = "Flooded"),
  value_df |> filter(reach3) |> mutate(stage = "Community Applied"),
  value_df |> filter(reach4) |> mutate(stage = "Applied"),
  value_df |> filter(reach5) |> mutate(stage = "Funded")
) |>
  select(stage, pct_A, pct_B) |>
  tidyr::pivot_longer(c(pct_A, pct_B),
                      names_to = "panel", values_to = "pct") |>
  filter(!is.na(pct)) |>
  mutate(
    stage = factor(stage, levels = stage_levels_4),
    panel = factor(recode(panel, !!!panel_labels_3),
                   levels = unname(panel_labels_3))
  )

fig3_medians <- fig3_hist_data |>
  group_by(panel, stage) |>
  summarise(median_pct = median(pct, na.rm = TRUE), .groups = "drop")

fig3 <- ggplot(fig3_hist_data, aes(x = pct)) +
  geom_histogram(binwidth = 5, boundary = 0,
                 fill = "grey60", colour = "grey30", linewidth = 0.2) +
  geom_vline(data = fig3_medians,
             aes(xintercept = median_pct),
             colour = "firebrick", linewidth = 0.6) +
  facet_grid(stage ~ panel,
             scales = "free_y",
             axes = "all",
             switch = "y") +
  scale_y_continuous(position = "right",
                     labels = scales::label_comma()) +
  scale_x_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 20),
    labels = function(x) paste0(x, "%"),
    expand = c(0.005, 0)
  ) +
  labs(
    x = "Property value percentile",
    y = "Number of parcels"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    strip.text.x       = element_text(face = "bold", size = 10),
    strip.text.y.left  = element_text(face = "bold", size = 9, angle = 90,
                                      hjust = 0.5, vjust = 0.5, colour = "grey15"),
    strip.background   = element_blank(),
    strip.placement    = "outside",
    panel.grid.minor   = element_blank(),
    panel.spacing      = unit(0.5, "lines"),
    plot.title         = element_text(face = "bold"),
    axis.text          = element_text(size = 8, colour = "grey30"),
    axis.title         = element_text(size = 9, colour = "grey20")
  )

fig3
#
#
#
#
readr::write_csv(fig3_medians,
                 here::here("figures", "fig3_medians.csv"))

dir.create(here::here("figures"), showWarnings = FALSE)
ggsave(file.path(here::here("figures"), "fig3.png"),
       fig3, width = 7.5, height = 9, dpi = 300)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
# Per-block-group race shares — one row per BG, with stage flags.
# bg_pct_white_2013 / bg_pct_black_2013 are already on a 0-100 scale.
bgs_at_stage <- value_df |>
  filter(!is.na(bg_geoid)) |>
  group_by(bg_geoid) |>
  summarise(
    pct_white = dplyr::first(bg_pct_white_2013),
    pct_black = dplyr::first(bg_pct_black_2013),
    s1 = any(reach1, na.rm = TRUE),
    s3 = any(reach3, na.rm = TRUE),
    s4 = any(reach4, na.rm = TRUE),
    s5 = any(reach5, na.rm = TRUE),
    .groups = "drop"
  )

fig4_hist_data <- bgs_at_stage |>
  tidyr::pivot_longer(c(s1, s3, s4, s5),
                      names_to = "stage_key", values_to = "in_stage") |>
  filter(in_stage) |>
  mutate(stage = recode(stage_key,
    s1 = "Flooded", s3 = "Community Applied",
    s4 = "Applied", s5 = "Funded"
  )) |>
  select(stage, pct_white, pct_black) |>
  tidyr::pivot_longer(c(pct_white, pct_black),
                      names_to = "race", values_to = "pct") |>
  filter(!is.na(pct)) |>
  mutate(
    stage = factor(stage, levels = stage_levels_4),
    race  = factor(recode(race,
      pct_white = "A. % Non-Hispanic White",
      pct_black = "B. % Black"
    ), levels = c("A. % Non-Hispanic White", "B. % Black"))
  )

fig4_medians <- fig4_hist_data |>
  group_by(stage, race) |>
  summarise(median_pct = median(pct, na.rm = TRUE), .groups = "drop")

fig4 <- ggplot(fig4_hist_data, aes(x = pct)) +
  geom_histogram(binwidth = 5, boundary = 0,
                 fill = "grey60", colour = "grey30", linewidth = 0.2) +
  geom_vline(data = fig4_medians,
             aes(xintercept = median_pct),
             colour = "firebrick", linewidth = 0.6) +
  facet_grid(stage ~ race, scales = "free_y", switch = "y") +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 20),
                     labels = function(x) paste0(x, "%"),
                     expand = c(0.005, 0)) +
  scale_y_continuous(position = "right",
                     labels = scales::label_comma()) +
  labs(
    #title    = "Fig. 4  Neighborhood racial composition by HMA stage",
    #subtitle = "Census block groups, 2013 ACS",
    x        = "Percent of residents (block group)",
    y        = "Number of block groups"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    strip.text.x       = element_text(face = "bold", size = 10),
    strip.text.y.left  = element_text(face = "bold", size = 9, angle = 90,
                                      hjust = 0.5, colour = "grey15"),
    strip.background   = element_blank(),
    strip.placement    = "outside",
    panel.grid.minor   = element_blank(),
    panel.spacing      = unit(0.5, "lines"),
    plot.title         = element_text(face = "bold")
  )

fig4
#
#
#
# KS tests for racial composition shifts: flooded baseline vs funded stage.
# One observation per block group containing >= 1 parcel reaching each stage.

get_bg_dist <- function(pipeline, stage_flag, race_col) {
  pipeline |>
    dplyr::filter(.data[[stage_flag]],
                  !is.na(.data[[race_col]]),
                  !is.na(bg_geoid)) |>
    dplyr::distinct(bg_geoid, .keep_all = TRUE) |>
    dplyr::pull(.data[[race_col]])
}

stage_flooded <- "reach1"   # cumulative "reached Flooded" boolean
stage_funded  <- "reach5"   # cumulative "reached Funded"  boolean
col_white     <- "bg_pct_white_2013"
col_black     <- "bg_pct_black_2013"

# Distributions (one value per block group at each stage)
white_flood  <- get_bg_dist(pipeline, stage_flooded, col_white)
white_funded <- get_bg_dist(pipeline, stage_funded,  col_white)
black_flood  <- get_bg_dist(pipeline, stage_flooded, col_black)
black_funded <- get_bg_dist(pipeline, stage_funded,  col_black)

# Two-sample KS tests (suppress ties warning for large samples)
ks_white <- suppressWarnings(ks.test(white_funded, white_flood))
ks_black <- suppressWarnings(ks.test(black_funded, black_flood))

# Bundle for downstream use
ks_tests <- list(
  white = list(
    D         = unname(ks_white$statistic),
    p         = ks_white$p.value,
    n_flooded = length(white_flood),
    n_funded  = length(white_funded)
  ),
  black = list(
    D         = unname(ks_black$statistic),
    p         = ks_black$p.value,
    n_flooded = length(black_flood),
    n_funded  = length(black_funded)
  )
)

# Console output for sanity-checking on render
cat("KS tests: flooded baseline vs funded stage (block-group level)\n")
cat("-----------------------------------------------------------\n")
cat(sprintf("NH White:  D = %.3f   p = %s   n_flooded = %d   n_funded = %d\n",
            ks_tests$white$D,
            format.pval(ks_tests$white$p, eps = 1e-4),
            ks_tests$white$n_flooded,
            ks_tests$white$n_funded))
cat(sprintf("Black:     D = %.3f   p = %s   n_flooded = %d   n_funded = %d\n",
            ks_tests$black$D,
            format.pval(ks_tests$black$p, eps = 1e-4),
            ks_tests$black$n_flooded,
            ks_tests$black$n_funded))
#
#
#
#
#
#
fig4_medians |>
  mutate(
    n          = NA_integer_,   # counts live in fig4_hist_data if needed
    median_pct = round(median_pct, 1)
  ) |>
  select(race, stage, median_pct) |>
  arrange(race, stage) |>
  knitr::kable(col.names = c("Panel", "Stage", "Median pct"))
#
#
#
dir.create(here::here("figures"), showWarnings = FALSE)
ggsave(here::here("figures", "fig4.png"),
       fig4, width = 8, height = 9, dpi = 300)
write_csv(fig4_medians, here::here("figures", "fig4_medians.csv"))
#
#
#
#
#
#
#
#
#
#
#
#
#
# Per-block-group economic medians, with stage flags. Same aggregation as Fig 4.
bgs_econ_at_stage <- value_df |>
  filter(!is.na(bg_geoid)) |>
  group_by(bg_geoid) |>
  summarise(
    income = dplyr::first(bg_median_income_2013),
    hvalue = dplyr::first(bg_median_home_value_2013),
    s1 = any(reach1, na.rm = TRUE),
    s3 = any(reach3, na.rm = TRUE),
    s4 = any(reach4, na.rm = TRUE),
    s5 = any(reach5, na.rm = TRUE),
    .groups = "drop"
  )

fig5_long <- bgs_econ_at_stage |>
  tidyr::pivot_longer(c(s1, s3, s4, s5),
                      names_to = "stage_key", values_to = "in_stage") |>
  filter(in_stage) |>
  mutate(stage = recode(stage_key,
    s1 = "Flooded", s3 = "Community Applied",
    s4 = "Applied", s5 = "Funded"
  )) |>
  mutate(stage = factor(stage, levels = stage_levels_4))

fig5_income <- fig5_long |>
  select(stage, val = income) |>
  filter(!is.na(val))
fig5_hvalue <- fig5_long |>
  select(stage, val = hvalue) |>
  filter(!is.na(val))

fig5_medians <- bind_rows(
  fig5_income |> group_by(stage) |>
    summarise(median_val = median(val, na.rm = TRUE), .groups = "drop") |>
    mutate(metric = "income"),
  fig5_hvalue |> group_by(stage) |>
    summarise(median_val = median(val, na.rm = TRUE), .groups = "drop") |>
    mutate(metric = "hvalue")
)

knitr::kable(
  fig5_medians |>
    mutate(median_val = round(median_val, 0)) |>
    arrange(metric, stage),
  caption = "Fig 5 supplementary medians by stage"
)
#
#
#
#
#
inc_medians <- fig5_medians |> filter(metric == "income")

fig5a <- ggplot(fig5_income, aes(x = val)) +
  geom_histogram(binwidth = 10000, boundary = 0,
                 fill = "grey60", colour = "grey30", linewidth = 0.2) +
  geom_vline(data = inc_medians,
             aes(xintercept = median_val),
             colour = "firebrick", linewidth = 0.6) +
  facet_wrap(~ stage, ncol = 1, scales = "free_y", strip.position = "left") +
  coord_cartesian(xlim = c(0, 175000)) +
  scale_x_continuous(
    breaks = seq(0, 175000, 25000),
    labels = scales::label_dollar(scale = 1e-3, suffix = "k"),
    expand = c(0.005, 0)
  ) +
  scale_y_continuous(position = "right",
                     labels = scales::label_comma()) +
  labs(
    #title    = "Fig. 5a  Block-group median household income by HMA stage",
    subtitle = "2013 ACS 5-year estimates",
    x        = "Median household income ($)",
    y        = "Number of block groups"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    strip.text.y.left = element_text(face = "bold", size = 9, angle = 0,
                                      hjust = 1, colour = "grey15"),
    strip.background  = element_blank(),
    strip.placement   = "outside",
    panel.grid.minor  = element_blank(),
    panel.spacing.y   = unit(0.5, "lines"),
    plot.title        = element_text(face = "bold")
  )

fig5a
#
#
#
#
#
hv_medians <- fig5_medians |> filter(metric == "hvalue")

fig5b <- ggplot(fig5_hvalue, aes(x = val)) +
  geom_histogram(binwidth = 25000, boundary = 0,
                 fill = "grey60", colour = "grey30", linewidth = 0.2) +
  geom_vline(data = hv_medians,
             aes(xintercept = median_val),
             colour = "firebrick", linewidth = 0.6) +
  facet_wrap(~ stage, ncol = 1, scales = "free_y", strip.position = "left") +
  coord_cartesian(xlim = c(0, 500000)) +
  scale_x_continuous(
    breaks = seq(0, 500000, 100000),
    labels = scales::label_dollar(scale = 1e-3, suffix = "k"),
    expand = c(0.005, 0)
  ) +
  scale_y_continuous(position = "right",
                     labels = scales::label_comma()) +
  labs(
    #title    = "Fig. 5b  Block-group median home value by HMA stage",
    subtitle = "2013 ACS 5-year estimates (owner-occupied housing units)",
    x        = "Median home value ($)",
    y        = "Number of block groups"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    strip.text.y.left = element_text(face = "bold", size = 9, angle = 0,
                                      hjust = 1, colour = "grey15"),
    strip.background  = element_blank(),
    strip.placement   = "outside",
    panel.grid.minor  = element_blank(),
    panel.spacing.y   = unit(0.5, "lines"),
    plot.title        = element_text(face = "bold")
  )

fig5b
#
#
#
#
#
dir.create(here::here("figures"), showWarnings = FALSE)
ggsave(here::here("figures", "fig5a_income.png"),
       fig5a, width = 6, height = 7, dpi = 300)
ggsave(here::here("figures", "fig5b_home_value.png"),
       fig5b, width = 6, height = 7, dpi = 300)
write_csv(fig5_medians, here::here("figures", "fig5_medians.csv"))
#
#
#
#
#
# Load the parcel-level classification produced in step 05
parcel_mit_type <- readr::read_csv(
  file.path(out_dir, "parcel_mit_type.csv"),
  show_col_types = FALSE
) |>
  dplyr::mutate(parcel_index = as.integer(parcel_index))

# --- Build funded subset with mit_type ------------------------------------
# Universe = flooded funded parcels (matches the funnel's reach5 = reach4 & funded)
funded_typed <- pipeline |>
  dplyr::filter(reach5) |>
  dplyr::left_join(parcel_mit_type |> dplyr::select(parcel_index, mit_type),
                   by = "parcel_index") |>
  dplyr::mutate(mit_type = dplyr::if_else(is.na(mit_type),
                                          "Other/Unknown", mit_type))

# --- Counts table ---------------------------------------------------------
type_counts <- funded_typed |>
  dplyr::count(mit_type, name = "n") |>
  dplyr::mutate(
    pct_of_all        = round(n / sum(n) * 100, 1),
    pct_of_classified = dplyr::if_else(
      mit_type %in% c("Buyout", "Elevation"),
      round(n / sum(n[mit_type %in% c("Buyout", "Elevation")]) * 100, 1),
      NA_real_
    )
  ) |>
  dplyr::arrange(dplyr::desc(n))

cat("Funded parcels by mitigation type (universe = funnel reach5):\n")
print(type_counts)

# --- Buyout vs Elevation comparison set -----------------------------------
funded_pair <- funded_typed |>
  dplyr::filter(mit_type %in% c("Buyout", "Elevation")) |>
  dplyr::left_join(value_df |> dplyr::select(parcel_index, pct_A),
                   by = "parcel_index")

cat("\nBuyout + Elevation subset: ",
    format(nrow(funded_pair), big.mark = ","),
    " parcels (", format(sum(funded_pair$repeat_exp, na.rm = TRUE),
                          big.mark = ","),
    " repeat-flooded)\n", sep = "")

# --- Helper: stack data into Panel A (all) + Panel B (repeat-flooded) -----
build_panels <- function(df, value_col) {
  dplyr::bind_rows(
    df |> dplyr::mutate(panel = "A. All funded"),
    df |> dplyr::filter(repeat_exp) |>
      dplyr::mutate(panel = "B. Repeat-flooded funded")
  ) |>
    dplyr::mutate(panel = factor(panel,
                                  levels = c("A. All funded",
                                             "B. Repeat-flooded funded"))) |>
    dplyr::rename(x = {{ value_col }}) |>
    dplyr::filter(!is.na(x))
}

# --- Figure: property value percentile ------------------------------------
val_data    <- build_panels(funded_pair, pct_A)
val_medians <- val_data |>
  dplyr::group_by(panel, mit_type) |>
  dplyr::summarise(median_x = round(median(x, na.rm = TRUE), 1),
                   .groups = "drop")

fig_si_mit_type_value <- ggplot(val_data, aes(x = x, fill = mit_type)) +
  geom_histogram(binwidth = 5, boundary = 0,
                 position = "identity", alpha = 0.55,
                 colour = "grey30", linewidth = 0.15) +
  geom_vline(data = val_medians,
             aes(xintercept = median_x, colour = mit_type),
             linetype = "dashed", linewidth = 0.6, show.legend = FALSE) +
  facet_wrap(~ panel, nrow = 1, scales = "fixed") +
  scale_fill_manual(values = c("Buyout" = "#1f78b4", "Elevation" = "#e31a1c"),
                    name = "Mitigation type") +
  scale_colour_manual(values = c("Buyout" = "#1f78b4", "Elevation" = "#e31a1c")) +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 20),
                     labels = function(x) paste0(x, "%"),
                     expand = c(0.005, 0)) +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(x = "Property value (study-area percentile)",
       y = "Number of funded parcels") +
  theme_minimal(base_size = 11) +
  theme(
    legend.position  = "top",
    strip.text       = element_text(face = "bold", size = 10),
    panel.grid.minor = element_blank(),
    panel.spacing    = unit(2, "lines")
  )

ggsave(file.path(fig_dir, "figS2_mit_type_value.png"),
       plot = fig_si_mit_type_value,
       width = 8, height = 4, dpi = 300, bg = "white")

# --- Figure: % Black share ------------------------------------------------
black_data    <- build_panels(funded_pair, bg_pct_black_2013)
black_medians <- black_data |>
  dplyr::group_by(panel, mit_type) |>
  dplyr::summarise(median_x = round(median(x, na.rm = TRUE), 1),
                   .groups = "drop")

fig_si_mit_type_black <- ggplot(black_data, aes(x = x, fill = mit_type)) +
  geom_histogram(binwidth = 5, boundary = 0,
                 position = "identity", alpha = 0.55,
                 colour = "grey30", linewidth = 0.15) +
  geom_vline(data = black_medians,
             aes(xintercept = median_x, colour = mit_type),
             linetype = "dashed", linewidth = 0.6, show.legend = FALSE) +
  facet_wrap(~ panel, nrow = 1, scales = "fixed") +
  scale_fill_manual(values = c("Buyout" = "#1f78b4", "Elevation" = "#e31a1c"),
                    name = "Mitigation type") +
  scale_colour_manual(values = c("Buyout" = "#1f78b4", "Elevation" = "#e31a1c")) +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 20),
                     labels = function(x) paste0(x, "%"),
                     expand = c(0.005, 0)) +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(x = "Block-group share of Black residents",
       y = "Number of funded parcels") +
  theme_minimal(base_size = 11) +
  theme(
    legend.position  = "top",
    strip.text       = element_text(face = "bold", size = 10),
    panel.grid.minor = element_blank(),
    panel.spacing    = unit(2, "lines")
  )

ggsave(file.path(fig_dir, "figS3_mit_type_black.png"),
       plot = fig_si_mit_type_black,
       width = 8, height = 4, dpi = 300, bg = "white")

# --- KS tests (Buyout vs Elevation) per panel -----------------------------
ks_by_panel <- function(panel_data, panel_name) {
  d  <- panel_data |> dplyr::filter(panel == panel_name)
  x1 <- d$x[d$mit_type == "Buyout"]
  x2 <- d$x[d$mit_type == "Elevation"]
  if (length(x1) < 2 || length(x2) < 2) return(NULL)
  k <- suppressWarnings(ks.test(x1, x2))
  list(D = unname(k$statistic), p = k$p.value,
       n_buyout = length(x1), n_elevation = length(x2))
}

ks_val_a <- ks_by_panel(val_data,   "A. All funded")
ks_val_b <- ks_by_panel(val_data,   "B. Repeat-flooded funded")
ks_blk_a <- ks_by_panel(black_data, "A. All funded")
ks_blk_b <- ks_by_panel(black_data, "B. Repeat-flooded funded")

# --- Bundle for index_inputs.rds ------------------------------------------
mit_type_analysis <- list(
  counts        = type_counts,
  value_medians = val_medians,
  black_medians = black_medians,
  ks_value_all       = ks_val_a,
  ks_value_repeat    = ks_val_b,
  ks_black_all       = ks_blk_a,
  ks_black_repeat    = ks_blk_b
)

cat("\n=== KS tests (Buyout vs Elevation) ===\n")
cat("Property value:\n")
cat(sprintf("  Panel A (all funded):       D = %.3f  p = %s  (n_buy = %d, n_elev = %d)\n",
            ks_val_a$D, format.pval(ks_val_a$p, eps = 1e-4),
            ks_val_a$n_buyout, ks_val_a$n_elevation))
cat(sprintf("  Panel B (repeat-flooded):   D = %.3f  p = %s  (n_buy = %d, n_elev = %d)\n",
            ks_val_b$D, format.pval(ks_val_b$p, eps = 1e-4),
            ks_val_b$n_buyout, ks_val_b$n_elevation))
cat("% Black:\n")
cat(sprintf("  Panel A (all funded):       D = %.3f  p = %s  (n_buy = %d, n_elev = %d)\n",
            ks_blk_a$D, format.pval(ks_blk_a$p, eps = 1e-4),
            ks_blk_a$n_buyout, ks_blk_a$n_elevation))
cat(sprintf("  Panel B (repeat-flooded):   D = %.3f  p = %s  (n_buy = %d, n_elev = %d)\n",
            ks_blk_b$D, format.pval(ks_blk_b$p, eps = 1e-4),
            ks_blk_b$n_buyout, ks_blk_b$n_elevation))

cat("\nWrote figS2_mit_type_value.png and figS3_mit_type_black.png\n")
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
sens_counts <- readr::read_csv(
  file.path(out_dir, "parcels_events_R_sens.csv"),
  show_col_types = FALSE
) |>
  dplyr::mutate(parcel_index = as.integer(parcel_index))

cat("Loaded sensitivity counts for",
    format(nrow(sens_counts), big.mark = ","), "parcels\n")
#
#
#
# Build a parallel pipeline_sens using sensitivity flood/eligibility flags
pipeline_sens <- pipeline |>
  dplyr::left_join(sens_counts, by = "parcel_index") |>
  dplyr::mutate(
    flooded_any_sens = dplyr::coalesce(flooded_any_sens, 0L),
    eligible_sens    = dplyr::coalesce(eligible_sens,    0L),
    
    s1_flooded_sens  = flooded_any_sens >= 1,
    s2_eligible_sens = eligible_sens    >= 1,
    repeat_exp_sens  = flooded_any_sens >= 2,
    
    reach1_sens = s1_flooded_sens,
    reach2_sens = reach1_sens & s2_eligible_sens,
    reach3_sens = reach2_sens & s3_community,
    reach4_sens = reach3_sens & s4_applied,
    reach5_sens = reach4_sens & s5_funded
  )

# Build sensitivity funnel
funnel_for_sens <- function(df, label) {
  r <- c(sum(df$reach1_sens), sum(df$reach2_sens), sum(df$reach3_sens),
         sum(df$reach4_sens), sum(df$reach5_sens))
  tibble::tibble(
    group        = label,
    stage_num    = 1:5,
    stage        = factor(
      c("Flooded", "Eligible", "Community Applied", "Applied", "Funded"),
      levels = c("Flooded", "Eligible", "Community Applied",
                 "Applied", "Funded")),
    n            = r,
    pct_of_prev  = c(NA, r[-1] / r[-5]),
    pct_of_flood = r / r[1]
  )
}

funnel_sens_all    <- funnel_for_sens(pipeline_sens,
                                      "Sensitivity all (71 events)")
funnel_sens_repeat <- funnel_for_sens(
  pipeline_sens |> dplyr::filter(repeat_exp_sens),
  "Sensitivity repeat (71 events)"
)
#
#
#
#
#
funnel_compare <- dplyr::bind_rows(
  funnel_all      |> dplyr::mutate(scenario = "Baseline (78)"),
  funnel_sens_all |> dplyr::mutate(scenario = "Sensitivity (71)")
) |>
  dplyr::select(scenario, stage, n, pct_of_prev, pct_of_flood)

funnel_compare |>
  dplyr::mutate(
    n            = format(n, big.mark = ","),
    `% of prev`  = dplyr::if_else(is.na(pct_of_prev), "—",
                                  paste0(round(pct_of_prev * 100, 1), "%")),
    `% of flood` = paste0(round(pct_of_flood * 100, 1), "%")
  ) |>
  dplyr::select(scenario, stage, n, `% of prev`, `% of flood`) |>
  knitr::kable(
    caption = "Baseline (78 events) vs Sensitivity (71 events) funnel"
  )
#
#
#
#
#
# Property value percentile (Panel A) at sensitivity-funded stage
# Use the same study-area-wide percentile rank computed in value_df
sens_med_value_A <- value_df |>
  dplyr::filter(parcel_index %in%
                  (pipeline_sens |> dplyr::filter(reach5_sens) |>
                     dplyr::pull(parcel_index))) |>
  dplyr::summarise(med_pct_A = round(median(pct_A, na.rm = TRUE), 1)) |>
  dplyr::pull(med_pct_A)

# Block-group race medians at sensitivity-funded stage
sens_med_race <- pipeline_sens |>
  dplyr::filter(reach5_sens, !is.na(bg_geoid)) |>
  dplyr::distinct(bg_geoid, .keep_all = TRUE) |>
  dplyr::summarise(
    med_white = round(median(bg_pct_white_2013, na.rm = TRUE), 1),
    med_black = round(median(bg_pct_black_2013, na.rm = TRUE), 1),
    n_bg      = dplyr::n_distinct(bg_geoid)
  )

# Pull baseline medians from fig3 / fig4 medians tables
baseline_med_value_A <- fig3_medians$median_pct[
  fig3_medians$panel == "A. Within Study Area" &
  fig3_medians$stage == "Funded"
]
baseline_med_white <- fig4_medians$median_pct[
  fig4_medians$race == "A. % Non-Hispanic White" &
  fig4_medians$stage == "Funded"
]
baseline_med_black <- fig4_medians$median_pct[
  fig4_medians$race == "B. % Black" &
  fig4_medians$stage == "Funded"
]

sens_med_table <- tibble::tibble(
  Metric = c("Property value (Panel A pctile)",
             "NH White share (BG, %)",
             "Black share (BG, %)"),
  `Baseline (78)` = c(baseline_med_value_A,
                      baseline_med_white,
                      baseline_med_black),
  `Sensitivity (71)` = c(sens_med_value_A,
                         sens_med_race$med_white,
                         sens_med_race$med_black)
)

knitr::kable(sens_med_table,
             caption = "Funded-stage medians: baseline vs sensitivity")
#
#
#
#
#
get_bg_dist_sens <- function(df, stage_flag, race_col) {
  df |>
    dplyr::filter(.data[[stage_flag]],
                  !is.na(.data[[race_col]]),
                  !is.na(bg_geoid)) |>
    dplyr::distinct(bg_geoid, .keep_all = TRUE) |>
    dplyr::pull(.data[[race_col]])
}

white_flood_sens  <- get_bg_dist_sens(pipeline_sens, "reach1_sens", "bg_pct_white_2013")
white_funded_sens <- get_bg_dist_sens(pipeline_sens, "reach5_sens", "bg_pct_white_2013")
black_flood_sens  <- get_bg_dist_sens(pipeline_sens, "reach1_sens", "bg_pct_black_2013")
black_funded_sens <- get_bg_dist_sens(pipeline_sens, "reach5_sens", "bg_pct_black_2013")

ks_white_sens <- suppressWarnings(ks.test(white_funded_sens, white_flood_sens))
ks_black_sens <- suppressWarnings(ks.test(black_funded_sens, black_flood_sens))

ks_compare <- tibble::tibble(
  Distribution = c("NH White", "Black"),
  `Baseline D` = c(round(ks_tests$white$D, 3),
                   round(ks_tests$black$D, 3)),
  `Sensitivity D` = c(round(unname(ks_white_sens$statistic), 3),
                      round(unname(ks_black_sens$statistic), 3)),
  `Sensitivity p` = c(format.pval(ks_white_sens$p.value, eps = 1e-4),
                      format.pval(ks_black_sens$p.value, eps = 1e-4))
)

knitr::kable(ks_compare,
             caption = "KS tests (flooded vs funded BG distributions) — baseline vs sensitivity")
#
#
#
#
#
sensitivity_analysis <- list(
  excluded_events = c(
    "Unnamed Piedmont flooding b (2003)", "TS Andrea (2013)",
    "TS Allison (2001)", "Unnamed western flooding (2000)",
    "Unnamed western flooding (2006)", "Unnamed western flooding (2003)",
    "Unnamed eastern flooding (2004)"
  ),
  funnel_baseline    = funnel_all,
  funnel_sens        = funnel_sens_all,
  funnel_sens_repeat = funnel_sens_repeat,
  funnel_compare     = funnel_compare,
  med_value_A_sens   = sens_med_value_A,
  med_white_sens     = sens_med_race$med_white,
  med_black_sens     = sens_med_race$med_black,
  n_bg_sens          = sens_med_race$n_bg,
  ks_white_sens      = list(D = unname(ks_white_sens$statistic),
                            p = ks_white_sens$p.value),
  ks_black_sens      = list(D = unname(ks_black_sens$statistic),
                            p = ks_black_sens$p.value)
)
#
#
#
#
#
# Block-group level correlation between race and property value
# Supports the Discussion claim that less-white neighborhoods tend to be lower-value
# in eastern North Carolina (2013 ACS)

# One observation per block group with complete demographic and value data
bg_demo <- parcels |>
  filter(!is.na(bg_geoid)) |>
  group_by(bg_geoid) |>
  summarise(
    pct_white         = dplyr::first(bg_pct_white_2013),
    pct_black         = dplyr::first(bg_pct_black_2013),
    median_income     = dplyr::first(bg_median_income_2013),
    median_home_value = dplyr::first(bg_median_home_value_2013),
    .groups = "drop"
  ) |>
  filter(!is.na(pct_white),
         !is.na(median_home_value),
         median_home_value > 0)

cat("Block groups with complete race + home value data:",
    format(nrow(bg_demo), big.mark = ","), "\n")

# Spearman correlations (rank-based, no distributional assumptions)
race_value_cor <- list(
  white_home_value = cor(bg_demo$pct_white, bg_demo$median_home_value,
                         method = "spearman"),
  black_home_value = cor(bg_demo$pct_black, bg_demo$median_home_value,
                         method = "spearman"),
  white_income     = cor(bg_demo$pct_white, bg_demo$median_income,
                         method = "spearman", use = "complete.obs"),
  black_income     = cor(bg_demo$pct_black, bg_demo$median_income,
                         method = "spearman", use = "complete.obs"),
  n_bgs            = nrow(bg_demo)
)

cat("\nBlock-group correlations (2013 ACS, n =",
    format(race_value_cor$n_bgs, big.mark = ","), "):\n")
cat(sprintf("NH White x Home value:  Spearman rho = %+.3f\n",
            race_value_cor$white_home_value))
cat(sprintf("Black x Home value:     Spearman rho = %+.3f\n",
            race_value_cor$black_home_value))
cat(sprintf("NH White x Income:      Spearman rho = %+.3f\n",
            race_value_cor$white_income))
cat(sprintf("Black x Income:         Spearman rho = %+.3f\n",
            race_value_cor$black_income))

# Reshape for the two-panel figure (race x economic measure)
bg_demo_long <- bg_demo |>
  tidyr::pivot_longer(c(median_home_value, median_income),
                      names_to = "metric", values_to = "value") |>
  mutate(
    metric = factor(recode(metric,
      median_home_value = "Median home value",
      median_income     = "Median household income"
    ), levels = c("Median home value", "Median household income"))
  )

# Compute per-facet correlation labels
cor_labels <- bg_demo_long |>
  group_by(metric) |>
  summarise(
    rho = cor(pct_white, value, method = "spearman", use = "complete.obs"),
    .groups = "drop"
  ) |>
  mutate(label = sprintf("Spearman ρ = %+.2f", rho))

# Two-panel supplementary figure
fig_si_race_value <- ggplot(bg_demo_long,
                            aes(x = pct_white, y = value)) +
  geom_point(alpha = 0.25, size = 0.5, color = "grey40") +
  geom_smooth(method = "lm", color = "firebrick",
              se = FALSE, linewidth = 0.7) +
  geom_label(data = cor_labels,
             aes(x = 3, y = Inf, label = label),
             hjust = 0, vjust = 1.4,
             inherit.aes = FALSE,
             size = 3.5,
             label.size = 0.3,
             fill = "white",
             label.padding = unit(0.3, "lines")) +
  facet_wrap(~ metric, scales = "free_y", nrow = 1) +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 20),
                     labels = function(x) paste0(x, "%")) +
  scale_y_continuous(labels = scales::label_dollar(scale = 0.001,
                                                    suffix = "k")) +
  labs(
    x = "Block-group share of non-Hispanic White residents",
    y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    strip.text       = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.spacing    = unit(1, "lines")
  )

fig_si_race_value

ggsave(file.path(fig_dir, "figS1_race_value_correlation.png"),
       plot = fig_si_race_value,
       width = 8, height = 4, dpi = 300, bg = "white")

cat("\nWrote figS1_race_value_correlation.png\n")
#
#
#
#
# Pre-compute everything the manuscript prose references inline.
# Saved as a single small RDS so index.qmd can read it without touching
# parcels_master.gpkg — making the manuscript renderable from any machine.

index_inputs <- list(
  # Sample sizes
  n_sample        = sum(parcels$res_flag == 1L | parcels$interpolate == 1L,
                        na.rm = TRUE),
  n_counties      = length(unique(parcels$county)),
  n_bg            = length(unique(parcels$bg_geoid[!is.na(parcels$bg_geoid)])),

  # Pipeline funnel — both groups in long format
  funnel = funnel,

  # Figure medians
  fig3_medians = fig3_medians,
  fig4_medians = fig4_medians,
  fig5_medians = fig5_medians

  ks_tests = ks_tests                               
  mit_type_analysis = mit_type_analysis,            # ← buyout/elevation
  race_value_cor  = race_value_cor,                 # ← race-value correlation
  sensitivity_analysis = sensitivity_analysis       # ← new sensitivity
)

# Also pull the CoreLogic-vs-ACS agreement summary from 07b's output, if it exists.
# Wrapped in tryCatch so Step 8 still runs cleanly if the file isn't there yet.
acs_cl_summary <- tryCatch({
  cmp <- readr::read_csv(file.path(out_dir, "acs_corelogic_value_comparison.csv"),
                         show_col_types = FALSE)
  list(
    n_bgs            = nrow(cmp),
    spearman_rho     = cor(cmp$cl_median_tvc, cmp$bg_median_home_value_2013,
                           method = "spearman"),
    median_ratio     = median(cmp$cl_median_tvc / cmp$bg_median_home_value_2013,
                              na.rm = TRUE)
  )
}, error = function(e) NULL)

index_inputs$acs_cl_summary <- acs_cl_summary

saveRDS(index_inputs, here::here("figures", "index_inputs.rds"))
cat("Wrote index_inputs.rds with", length(index_inputs), "elements\n")
#
#
#
#
#
#
