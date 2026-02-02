# render_qmd.R

if (requireNamespace("renv", quietly = TRUE)) renv::activate()

# Diagnostics
cat("R working directory:", getwd(), "\n")
qmd <- normalizePath("notebooks/01_parcel_ETL.qmd", mustWork = TRUE)
cat("Rendering:", qmd, "\n")
cat("QUARTO_PATH:", Sys.getenv("QUARTO_PATH"), "\n")

if (!requireNamespace("quarto", quietly = TRUE)) {
  install.packages("quarto", repos = "https://cloud.r-project.org")
}

# Render using an absolute path
quarto::quarto_render(input = qmd)

