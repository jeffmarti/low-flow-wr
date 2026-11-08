# ==============================================================================
# prepare_app_data.R
# ------------------------------------------------------------------------------
# Turns the geocoded + failed CSVs into the files app.R reads, and downloads
# the WRIA and county boundary layers (once) so the published app never has
# to fetch anything at startup.
#
# The two CSVs keep the SAME columns the app has always read (the old
# WaterRightReportGeocodeAPP.R output), so app.R's loading code is unchanged.
#
# Input : newest pipeline/work/lowflow_geocoded_*.csv
#         newest pipeline/work/lowflow_geocode_failed_*.csv
# Output: data/lowflow_rights.csv   geocoded records (has lon / lat)
#         data/geocode_failed.csv   records that could not be placed
#         data/data_date.txt        "Data retrieved" date shown in the About tab
#         data/boundaries.rds       list(wria, county) boundary layers
#
# SAFETY CHECK: if the new mapped count is less than MIN_SHARE_OF_PREVIOUS of
# the current data/lowflow_rights.csv, the script stops WITHOUT overwriting
# anything (a half-broken download should never reach the live app).
#
# Run from the project root:  source("pipeline/prepare_app_data.R")
# REQUIRED PACKAGES: dplyr, readr, stringr, sf
# ==============================================================================

library(dplyr)
library(readr)
library(stringr)
library(sf)

# ==============================================================================
# SETTINGS
# ==============================================================================

WORK_DIR <- "pipeline/work"
OUT_DIR  <- "data"

MIN_SHARE_OF_PREVIOUS <- 0.8   # stop if mapped records drop below 80% of last run

# Boundary layers rarely change. FALSE = reuse data/boundaries.rds if present.
REFRESH_BOUNDARIES <- FALSE

WRIA_GEOJSON_URL <- paste0(
  "https://services.arcgis.com/6lCKYNJLvwTXqrmp/arcgis/rest/services/",
  "ECY/FeatureServer/11/query?outFields=*&where=1%3D1&f=geojson"
)
COUNTY_GEOJSON_URL <- paste0(
  "https://tigerweb.geo.census.gov/arcgis/rest/services/TIGERweb/",
  "State_County/MapServer/11/query?where=STATE%3D'53'",
  "&outFields=NAME&f=geojson"
)

# Columns app.R expects, in the order the old geocoder wrote them
APP_COLS <- c("Record Document Number", "WR Doc ID", "Certificate Number",
              "Person or Organization", "Status", "Phase", "Stage",
              "Priority Date Claim First Use", "Purpose of Use", "Qi", "UOM", "Qa",
              "WRIA", "County", "TRS", "lon", "lat", "QQ", "Q",
              "First Source Name Listed")

# ==============================================================================
# HELPERS
# ==============================================================================

newest <- function(pattern) {
  f <- list.files(WORK_DIR, pattern = pattern, full.names = TRUE)
  if (length(f) == 0) return(NULL)
  f[which.max(file.mtime(f))]
}

read_chr <- function(f) read_csv(f, col_types = cols(.default = "c"),
                                 na = c("", "NA"), show_col_types = FALSE)

# Keep the app's columns (adding any that are missing as blank) + extras last
to_app_cols <- function(df, extra = character()) {
  for (cl in setdiff(APP_COLS, names(df))) df[[cl]] <- NA_character_
  df[, c(APP_COLS, intersect(extra, names(df)))]
}

fetch_boundary <- function(url, label) {
  message("Downloading ", label, " ...")
  tryCatch({
    lyr <- st_read(url, quiet = TRUE) |> st_make_valid()
    lyr <- lyr |>
      st_transform(32610) |>
      st_simplify(dTolerance = 50, preserveTopology = TRUE) |>
      st_transform(4326)
    message("  ", label, ": ", nrow(lyr), " features")
    lyr
  }, error = function(e) {
    message("  WARNING - could not download ", label, ": ", e$message)
    NULL
  })
}

# ==============================================================================
# STEP 1 — Read the geocoded + failed CSVs
# ==============================================================================

geo_file  <- newest("^lowflow_geocoded_.*\\.csv$")
fail_file <- newest("^lowflow_geocode_failed_.*\\.csv$")
if (is.null(geo_file)) stop("No lowflow_geocoded_*.csv in ", WORK_DIR,
                            ". Run geocode_records.R first.")
message("Geocoded file: ", basename(geo_file))
message("Failed file  : ", if (is.null(fail_file)) "(none)" else basename(fail_file))

geo  <- to_app_cols(read_chr(geo_file))
fail <- if (is.null(fail_file)) geo[0, ] else
  to_app_cols(read_chr(fail_file), extra = "fail_reason")

# Records with coordinates outside Washington are a sign of a bad TRS — report them
lon <- as.numeric(geo$lon); lat <- as.numeric(geo$lat)
outside <- sum(lon < -125 | lon > -116.5 | lat < 45.4 | lat > 49.1, na.rm = TRUE)

# ==============================================================================
# STEP 2 — Safety check against the data the app has now
# ==============================================================================

out_geo  <- file.path(OUT_DIR, "lowflow_rights.csv")
out_fail <- file.path(OUT_DIR, "geocode_failed.csv")
n_prev   <- if (file.exists(out_geo)) nrow(read_chr(out_geo)) else NA_integer_

if (!is.na(n_prev) && nrow(geo) < MIN_SHARE_OF_PREVIOUS * n_prev) {
  stop("Only ", nrow(geo), " mapped records this run vs ", n_prev,
       " in the current app data (below ", 100 * MIN_SHARE_OF_PREVIOUS,
       "%). Nothing was overwritten. Check the WRTS download before trusting it.")
}

# ==============================================================================
# STEP 3 — Write the app's data files
# ==============================================================================

stamp     <- str_extract(basename(geo_file), "20\\d{6}")
data_date <- if (!is.na(stamp)) as.Date(stamp, "%Y%m%d") else as.Date(file.mtime(geo_file))

dir.create(OUT_DIR, showWarnings = FALSE)
write_csv(geo  |> arrange(`Record Document Number`), out_geo,  na = "")
write_csv(fail |> arrange(as.integer(WRIA), `Record Document Number`), out_fail, na = "")
writeLines(format(data_date, "%Y-%m-%d"), file.path(OUT_DIR, "data_date.txt"))

# ==============================================================================
# STEP 4 — Boundary layers (WRIA + county), downloaded once
# ==============================================================================

bnd_file <- file.path(OUT_DIR, "boundaries.rds")

# Also re-download if an earlier run saved a file with a layer missing
bnd_incomplete <- file.exists(bnd_file) &&
  with(readRDS(bnd_file), is.null(wria) || is.null(county))

if (REFRESH_BOUNDARIES || !file.exists(bnd_file) || bnd_incomplete) {
  wria_sf   <- fetch_boundary(WRIA_GEOJSON_URL,   "WRIA boundaries")
  county_sf <- fetch_boundary(COUNTY_GEOJSON_URL, "WA county boundaries")

  if (!is.null(wria_sf)) {
    nr_col <- names(wria_sf)[str_detect(names(wria_sf),
                                        regex("NR$|NUM$|NUMBER$", ignore_case = TRUE))][1]
    nm_col <- names(wria_sf)[str_detect(names(wria_sf),
                                        regex("NM$|NAME$", ignore_case = TRUE))][1]
    if (is.na(nr_col)) stop("Could not find the WRIA number column in the WRIA layer.")
    wria_sf <- wria_sf |>
      mutate(WRIA = as.character(as.integer(.data[[nr_col]])),
             wria_display = paste0("WRIA ", WRIA, " — ",
                                   if (!is.na(nm_col)) .data[[nm_col]] else "")) |>
      select(WRIA, wria_display)
  }
  if (!is.null(county_sf)) county_sf <- county_sf |> select(NAME)

  # Never replace a good layer with a failed download
  old <- if (file.exists(bnd_file)) readRDS(bnd_file) else list(wria = NULL, county = NULL)
  saveRDS(list(wria   = if (is.null(wria_sf))   old$wria   else wria_sf,
               county = if (is.null(county_sf)) old$county else county_sf), bnd_file)
} else {
  message("Reusing existing ", bnd_file, " (set REFRESH_BOUNDARIES <- TRUE to re-download)")
}

bnd <- readRDS(bnd_file)

# ==============================================================================
# STEP 5 — Validation summary (check these each refresh)
# ==============================================================================

src_type <- if_else(str_to_lower(str_trim(geo$`First Source Name Listed`)) == "groundwater",
                    "Groundwater", "Surface Water", missing = "Surface Water")

cat("\n================ APP DATA SUMMARY ================\n")
cat("Data retrieved              :", format(data_date, "%m/%d/%Y"), "\n")
cat("Records in export           :", nrow(geo) + nrow(fail), "\n")
cat("Mapped                      :", nrow(geo),
    if (!is.na(n_prev)) paste0("(previous run: ", n_prev, ")") else "", "\n")
cat("Not mapped                  :", nrow(fail), "\n")
if (nrow(fail) > 0 && "fail_reason" %in% names(fail)) print(table(fail$fail_reason, useNA = "ifany"))
cat("Source type (mapped):\n"); print(table(src_type, useNA = "ifany"))
cat("Points outside WA bounds    :", outside, if (outside > 0) " <-- CHECK" else "", "\n")
cat("WRIA layer                  :", if (is.null(bnd$wria)) "MISSING" else nrow(bnd$wria), "\n")
cat("County layer                :", if (is.null(bnd$county)) "MISSING" else nrow(bnd$county), "\n")
cat("Wrote                       :", out_geo, "|", out_fail, "| data/data_date.txt\n")
cat("==================================================\n")
