# SLURM Job 46489428 — Failure Diagnosis

**Date:** 2026-04-29  
**Failed chunk:** `[one-to-many-by-type]` in `04_spatial_join.qmd` (~lines 337–362)

---

## Error Summary

`dplyr::distinct()` / `count()` could not find columns:
- `PROPERTY INDICATOR CODE`
- `LAND USE CODE`
- `COUNTY USE DESCRIPTION`

in `cl_dist_nosort`.

---

## Root Cause (Confirmed)

GDAL replaces spaces with dots when writing to GeoPackage format. When `cl_wID.gpkg` was written and re-read, column names with spaces were mangled. These renamed columns then propagated through the spatial join into `parcel_cl_dist_joins_nosort.csv`.

**Confirmed via diagnostic (2026-04-29):** The three expected columns are absent (`FALSE FALSE FALSE`). The actual column names in `cl_dist_nosort` use dot-separated names:

| Expected | Actual in CSV |
|---|---|
| `PROPERTY INDICATOR CODE` | `PROPERTY.INDICATOR.CODE` |
| `LAND USE CODE` | `LAND.USE.CODE` |
| `COUNTY USE DESCRIPTION` | `COUNTY.USE.DESCRIPTION` |

---

## Fix Options

### Option 1 (Quickest): Update column references in `04_spatial_join.qmd`

In the `[one-to-many-by-type]` chunk, replace space-separated column names with dot-separated names:

```r
# Before
count(`PROPERTY INDICATOR CODE`, `LAND USE CODE`, `COUNTY USE DESCRIPTION`)

# After
count(`PROPERTY.INDICATOR.CODE`, `LAND.USE.CODE`, `COUNTY.USE.DESCRIPTION`)
```

Search the entire `04_spatial_join.qmd` for any other references to the space-separated names and update them accordingly.

### Option 2 (Upstream fix): Rename columns after `st_read()`

After reading `cl_wID.gpkg`, rename the mangled CL columns back to their original space-separated names before any joins:

```r
cl_unmatched <- st_read(
  "/proj/mhinolab/users/rbless/data/Obstacles_Output/cl_wID.gpkg",
  quiet = TRUE
)

# Rename dots back to spaces for known CL columns only
cl_unmatched <- cl_unmatched |>
  rename_with(~ str_replace_all(.x, "\\.", " "), 
              matches("^(PROPERTY|LAND|COUNTY|STATE|ZONING|TOTAL|LAND|IMPROVEMENT|ASSESSED|MARKET|YEAR|EFFECTIVE|OWNER|APN|SITUS|MAILING|ALTERNATE|ONLINE|ORIGINAL)"))
```

> ⚠️ Apply only to known CL columns — not geometry, `parcel_index`, `cl_index`, `distance`, etc.

### Option 3 (Long-term): Avoid routing CL attributes through GeoPackage

Save CL attribute data as `.csv` or `.parquet` separately and join back by ID after spatial operations, avoiding GDAL column name mangling entirely.

---

## Notes

- `cl_dist_nosort` has **1,145,843 rows × 58 columns**
- Notebooks 01–03 completed successfully; only `04_spatial_join.qmd` failed
- Job runtime before failure: ~1h 18m
