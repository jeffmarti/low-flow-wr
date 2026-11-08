# ==============================================================================
# app.R — WA Low Flow Water Rights Viewer (Shiny)
# ==============================================================================
# PURPOSE : Interactive Leaflet map of WA water rights with low flow
#           restrictions. Supports filtering, WRIA/county overlays, and
#           CSV + KMZ download.
#
# DATA: refreshed weekly by pipeline/run_pipeline.R (GitHub Actions) — see README.
#   low-flow-wr/
#     app.R                        ← this file
#     data/
#       lowflow_rights.csv         ← geocoded WRTS low flow records (pipeline)
#       geocode_failed.csv         ← records that could not be geocoded (pipeline)
#       data_date.txt              ← "Data retrieved" date (pipeline)
#       boundaries.rds             ← WRIA + county layers (pipeline)
#       purpose_codes.csv          ← purpose of use code definitions
#
# REQUIRED PACKAGES:
#   shiny, leaflet, leaflet.extras, tidyverse, sf, DT, zip, htmltools, glue
#
# INSTALL (run once in RStudio Console):
#   install.packages(c("shiny", "leaflet", "leaflet.extras", "tidyverse",
#                      "sf", "DT", "zip", "htmltools", "glue"))
# ==============================================================================

library(shiny)
library(leaflet)
library(leaflet.extras)   # addFullscreenControl()
library(tidyverse)
library(sf)
library(DT)
library(zip)
library(htmltools)
library(glue)


# ==============================================================================
# SECTION 1: CONSTANTS
# ==============================================================================

# Base URL for WA Ecology water right records.
# Full URL = ECOLOGY_WR_URL + WR Doc ID  (e.g., "6800476")
ECOLOGY_WR_URL <- paste0(
  "https://appswr.ecology.wa.gov/waterrighttrackingsystem/",
  "WaterRights/WaterRightRecord.aspx?waRecId="
)

# GeoJSON endpoint for WRIA boundaries (from WA Ecology ArcGIS service)
WRIA_GEOJSON_URL <- paste0(
  "https://services.arcgis.com/6lCKYNJLvwTXqrmp/arcgis/rest/services/",
  "ECY/FeatureServer/11/query?outFields=*&where=1%3D1&f=geojson"
)

# GeoJSON endpoint for WA County boundaries (US Census TIGERweb; FIPS 53 = WA)
COUNTY_GEOJSON_URL <- paste0(
  "https://tigerweb.geo.census.gov/arcgis/rest/services/TIGERweb/",
  "State_County/MapServer/11/query?where=STATE%3D'53'",
  "&outFields=NAME&f=geojson"
)

# Marker colors for Leaflet map (hex RGB)
COL_GW <- "#1565C0"   # Groundwater — deep blue
COL_SW <- "#E65100"   # Surface Water — deep orange

# KML icon colors — ABGR format (alpha, blue, green, red as 2-digit hex pairs)
# Deep blue  #1565C0: R=15, G=65, B=C0  →  ABGR = ff C0 65 15
KML_COL_GW <- "ffC06515"
# Deep orange #E65100: R=E6, G=51, B=00  →  ABGR = ff 00 51 E6
KML_COL_SW <- "ff0051E6"

# Data currency date — displayed in the About tab as "Data retrieved".
# ------------------------------------------------------------------------------
# Read from data/data_date.txt, which pipeline/prepare_app_data.R writes on
# every refresh (the date the WRTS export was downloaded). Stored as file
# content because the hosting service overwrites file timestamps on deploy.
#
# Falls back to today's date if data_date.txt is missing (e.g., first run).
# ------------------------------------------------------------------------------
DATA_DATE <- tryCatch(
  as.Date(readLines("data/data_date.txt", n = 1)),
  error = function(e) Sys.Date()
)


# ==============================================================================
# SECTION 2: WRIA NAME LOOKUP TABLE
# (Reused directly from WA Water Rights PLSS Geocoder project)
# ==============================================================================

wria_names <- tribble(
  ~wria_num, ~wria_name,
  1L,  "Nooksack",               2L,  "San Juan",
  3L,  "Lower Skagit-Samish",    4L,  "Upper Skagit",
  5L,  "Stillaguamish",          6L,  "Island",
  7L,  "Snohomish",              8L,  "Cedar-Sammamish",
  9L,  "Duwamish-Green",        10L,  "Puyallup-White",
  11L, "Nisqually",             12L,  "Chambers-Clover",
  13L, "Deschutes",             14L,  "Kennedy-Goldsborough",
  15L, "Kitsap",                16L,  "Skokomish-Dosewallips",
  17L, "Quilcene-Snow",         18L,  "Elwha-Dungeness",
  19L, "Lyre-Hoko",             20L,  "Soleduc",
  21L, "Queets-Quinault",       22L,  "Lower Chehalis",
  23L, "Upper Chehalis",        24L,  "Willapa",
  25L, "Grays-Elochoman",       26L,  "Cowlitz",
  27L, "Lewis",                 28L,  "Salmon-Washougal",
  29L, "Wind-White Salmon",     30L,  "Klickitat",
  31L, "Rock-Glade",            32L,  "Walla Walla",
  33L, "Lower Snake",           34L,  "Palouse",
  35L, "Middle Snake",          36L,  "Esquatzel Coulee",
  37L, "Lower Yakima",          38L,  "Naches",
  39L, "Upper Yakima",          40L,  "Alkali-Squilchuck",
  41L, "Lower Crab",            42L,  "Grand Coulee",
  43L, "Upper Crab-Wilson",     44L,  "Moses Coulee",
  45L, "Wenatchee",             46L,  "Entiat",
  47L, "Chelan",                48L,  "Methow",
  49L, "Okanogan",              50L,  "Foster",
  51L, "Nespelem",              52L,  "Sanpoil",
  53L, "Lower Lake Roosevelt",  54L,  "Lower Spokane",
  55L, "Little Spokane",        56L,  "Hangman",
  57L, "Middle Spokane",        58L,  "Middle Lake Roosevelt",
  59L, "Colville",              60L,  "Kettle",
  61L, "Upper Lake Roosevelt",  62L,  "Pend Oreille"
)

# Purpose of use code definitions — loaded from data/purpose_codes.csv.
# ------------------------------------------------------------------------------
# The CSV has two columns: "Purpose of Use Codes" and "Purpose".
# We rename them to `code` and `definition`, trim whitespace from both
# (several codes in the source file have trailing spaces), and convert
# codes to uppercase so matching is case-insensitive. The data field
# may contain "IFlow" while the CSV spells it "Iflow" — toupper() on
# both sides at match time handles that transparently.
# ------------------------------------------------------------------------------
purpose_defs <- tryCatch({
  read_csv(
    "data/purpose_codes.csv",
    col_types      = cols(.default = "c"),
    show_col_types = FALSE
  ) %>%
    rename(code = 1, definition = 2) %>%   # use position — header has a BOM
    mutate(
      code       = str_trim(code),
      definition = str_trim(definition),
      code_upper = toupper(code)             # key used for case-insensitive lookup
    ) %>%
    filter(!is.na(code), code != "")
}, error = function(e) {
  warning("Could not load data/purpose_codes.csv: ", e$message,
          " — falling back to built-in table.")
  # Minimal fallback so the app still runs if the file is missing
  tribble(
    ~code,   ~definition,           ~code_upper,
    "IR",    "Irrigation",          "IR",
    "MU",    "Municipal",           "MU",
    "CI",    "Commercial/Indust.",  "CI",
    "Iflow", "Instream Flow",       "IFLOW",
    "MT",    "Mitigation",          "MT"
  )
})


# ==============================================================================
# SECTION 3: DATA LOADING FUNCTION
# Runs once at startup. Reads CSV, converts dates, classifies source type,
# joins WRIA names, and builds the popup HTML stored as a column.
# ==============================================================================

load_wr_data <- function(path = "data/lowflow_rights.csv") {
  
  df <- read_csv(path, col_types = cols(.default = "c"), show_col_types = FALSE)
  
  # ---- Step 3a: Rename columns to remove spaces and backtick requirements ---
  # This makes all downstream code much cleaner.
  df <- df %>%
    rename(
      doc_number    = `Record Document Number`,
      wr_doc_id     = `WR Doc ID`,
      cert_number   = `Certificate Number`,
      holder        = `Person or Organization`,
      purpose       = `Purpose of Use`,
      priority_raw  = `Priority Date Claim First Use`,
      inst_flow     = Qi,
      unit          = UOM,
      annual_vol    = Qa,
      trs           = TRS,
      source_name   = `First Source Name Listed`
    )
  
  # ---- Step 3b: Clean up "NA" strings in optional fields --------------------
  df <- df %>%
    mutate(across(c(cert_number, QQ, Q), ~ na_if(., "NA")))
  
  # ---- Step 3c: Compute derived columns -------------------------------------
  df <- df %>%
    mutate(
      
      # Coordinates as numeric
      lat = as.numeric(lat),
      lon = as.numeric(lon),
      
      # Priority date: stored as "mm/dd/yyyy" character strings in geocoded output.
      # Some older records (Claims, pre-application) have no priority date —
      # as.Date() returns NA for those gracefully.
      # NOTE: If you ever encounter a file where dates came through as Excel serial
      # integers instead (e.g., 42923), swap this line for:
      #   as.Date(as.numeric(priority_raw), origin = "1899-12-30")
      priority_date = suppressWarnings(
        as.Date(priority_raw, format = "%m/%d/%Y")
      ),
      priority_display = if_else(
        is.na(priority_date),
        "Not recorded",
        format(priority_date, "%m-%d-%Y")
      ),
      
      # WRIA as integer for the lookup join below
      wria_num = suppressWarnings(as.integer(as.numeric(WRIA))),
      
      # Classify source type: groundwater vs. all other sources (surface water)
      source_type = case_when(
        str_to_lower(str_trim(source_name)) == "groundwater" ~ "Groundwater",
        TRUE ~ "Surface Water"
      ),
      
      # ---- Diversion rate conversion to CFS for marker sizing --------------
      # inst_flow (Qi) is stored as a character string; parse to numeric first.
      # Surface water rights are in CFS; groundwater rights are in GPM.
      # Conversion: 1 CFS = 448.831 GPM  →  GPM / 448.831 = CFS
      # cfs_for_size is used ONLY for visual scaling — popups and the data
      # table always display the original inst_flow and unit values unchanged.
      inst_flow_num = suppressWarnings(as.numeric(inst_flow)),
      cfs_for_size  = case_when(
        is.na(inst_flow_num)                  ~ NA_real_,
        str_to_upper(str_trim(unit)) == "GPM" ~ inst_flow_num / 448.831,
        TRUE                                  ~ inst_flow_num  # already CFS
      ),
      
      # ---- Marker radius: log scale, clamped 4–18px -----------------------
      # Log scaling handles the 4 orders of magnitude in this dataset
      # (0.001–1,500 CFS) far better than sqrt, which either floors most
      # small records or caps large ones too early.
      # Formula: 2 + 4 * log10(cfs * 10 + 1)
      # Key breakpoints against actual data distribution:
      #   < 0.1 CFS  →  4px (floor)
      #     1 CFS    →  6px
      #    10 CFS    → 10px
      #   100 CFS    → 14px
      #   ≥ 500 CFS  → 18px (cap)
      # Records with no diversion rate get a neutral 5px radius.
      marker_radius = case_when(
        is.na(cfs_for_size) ~ 5,
        TRUE ~ pmin(18, pmax(4, 2 + 4 * log10(cfs_for_size * 10 + 1)))
      ),
      
      # Assign marker color and KML style based on source type
      marker_color = if_else(source_type == "Groundwater", COL_GW, COL_SW),
      kml_style    = if_else(source_type == "Groundwater",
                             "#style_groundwater", "#style_surface_water")
    )
  
  # ---- Step 3d: Join WRIA names and build display label ---------------------
  df <- df %>%
    left_join(wria_names, by = "wria_num") %>%
    mutate(
      # e.g., "1 — Nooksack" or "99 — WRIA 99" for any unknown number
      wria_label = paste0(wria_num, " \u2014 ",
                          coalesce(wria_name, paste0("WRIA ", WRIA)))
    )
  
  # ---- Step 3e: Build Ecology hyperlink and popup HTML per row --------------
  # The popup HTML is pre-computed here so the server doesn't have to rebuild
  # it on every filter change — it's just stored as a column and passed directly
  # to addCircleMarkers().
  
  df <- df %>%
    mutate(
      
      # Ecology record hyperlink — only include if wr_doc_id is available
      ecology_link = if_else(
        !is.na(wr_doc_id) & wr_doc_id != "",
        paste0('<a href="', ECOLOGY_WR_URL, wr_doc_id,
               '" target="_blank" style="color:#1565C0;font-weight:600;">',
               '&#128203; View WA Ecology Record</a>'),
        '<span style="color:#999;">No Ecology link available</span>'
      ),
      
      # Full popup HTML table (safe values passed through htmlEscape)
      popup_html = paste0(
        '<div style="font-size:12px;font-family:Arial,sans-serif;',
        'min-width:230px;max-width:320px;">',
        '<p style="margin:0 0 8px;">', ecology_link, '</p>',
        '<table style="border-collapse:collapse;width:100%;">',
        
        '<tr>',
        '<td style="padding:3px 6px;border-bottom:1px solid #eee;',
        'color:#555;white-space:nowrap;"><b>Water Right No.</b></td>',
        '<td style="padding:3px 6px;border-bottom:1px solid #eee;">',
        htmlEscape(doc_number), '</td></tr>',
        
        '<tr><td style="padding:3px 6px;border-bottom:1px solid #eee;',
        'color:#555;"><b>Holder</b></td>',
        '<td style="padding:3px 6px;border-bottom:1px solid #eee;">',
        htmlEscape(holder), '</td></tr>',
        
        '<tr><td style="padding:3px 6px;border-bottom:1px solid #eee;',
        'color:#555;"><b>Purpose</b></td>',
        '<td style="padding:3px 6px;border-bottom:1px solid #eee;">',
        htmlEscape(purpose), '</td></tr>',
        
        '<tr><td style="padding:3px 6px;border-bottom:1px solid #eee;',
        'color:#555;"><b>Priority Date</b></td>',
        '<td style="padding:3px 6px;border-bottom:1px solid #eee;">',
        coalesce(priority_display, "Unknown"), '</td></tr>',
        
        '<tr><td style="padding:3px 6px;border-bottom:1px solid #eee;',
        'color:#555;"><b>Diversion Rate</b></td>',
        '<td style="padding:3px 6px;border-bottom:1px solid #eee;">',
        coalesce(inst_flow, "—"), ' ', coalesce(unit, ""), '</td></tr>',
        
        '<tr><td style="padding:3px 6px;border-bottom:1px solid #eee;',
        'color:#555;"><b>Annual Vol.</b></td>',
        '<td style="padding:3px 6px;border-bottom:1px solid #eee;">',
        coalesce(annual_vol, "—"), ' ac-ft</td></tr>',
        
        '<tr><td style="padding:3px 6px;border-bottom:1px solid #eee;',
        'color:#555;"><b>Water Source</b></td>',
        '<td style="padding:3px 6px;border-bottom:1px solid #eee;">',
        htmlEscape(coalesce(source_name, "—")), '</td></tr>',
        
        '<tr><td style="padding:3px 6px;border-bottom:1px solid #eee;',
        'color:#555;"><b>Source Type</b></td>',
        '<td style="padding:3px 6px;border-bottom:1px solid #eee;">',
        source_type, '</td></tr>',
        
        '<tr><td style="padding:3px 6px;border-bottom:1px solid #eee;',
        'color:#555;"><b>WRIA</b></td>',
        '<td style="padding:3px 6px;border-bottom:1px solid #eee;">',
        htmlEscape(wria_label), '</td></tr>',
        
        '<tr><td style="padding:3px 6px;border-bottom:1px solid #eee;',
        'color:#555;"><b>County</b></td>',
        '<td style="padding:3px 6px;border-bottom:1px solid #eee;">',
        htmlEscape(coalesce(County, "—")), '</td></tr>',
        
        '<tr><td style="padding:3px 6px;color:#555;"><b>Phase / Stage</b></td>',
        '<td style="padding:3px 6px;">',
        htmlEscape(coalesce(Phase, "—")), ' / ',
        htmlEscape(coalesce(Stage, "—")), '</td></tr>',
        
        '</table></div>'
      )
    )
  
  # ---- Step 3f: Drop rows that have no usable coordinates ------------------
  df <- df %>% filter(!is.na(lat), !is.na(lon))
  
  df
}

# Execute the load function at startup
# If the file is missing or malformed, wr_data will be NULL and the app
# will display an informative message rather than crashing.
wr_data <- tryCatch(
  load_wr_data(),
  error = function(e) {
    warning("Could not load data/lowflow_rights.csv — ", e$message)
    NULL
  }
)

# ------------------------------------------------------------------------------
# Load failed geocode records (data/geocode_failed.csv).
# These are records from the original WRTS export that could not be matched
# to a BLM PLSS section polygon. Common causes:
#   • BLM CadNSDI database gaps for certain township/range/section combinations
#     (notably T33.0N/R20.0E sections in WRIA 48 Okanogan)
#   • Western-range townships (R##W) with limited BLM coverage in WA
#   • Older "CWRIS" / "GWRIS" suffix records with non-standard TRS formats
# The file is displayed in the About tab with live Ecology links.
# A plain-text URL column is added so the CSV download is also useful.
# ------------------------------------------------------------------------------
failed_geocode <- tryCatch({
  df <- read_csv(
    "data/geocode_failed.csv",
    col_types      = cols(.default = "c"),
    show_col_types = FALSE
  ) %>%
    rename(
      doc_number   = `Record Document Number`,
      wr_doc_id    = `WR Doc ID`,
      cert_number  = `Certificate Number`,
      holder       = `Person or Organization`,
      purpose      = `Purpose of Use`,
      priority_raw = `Priority Date Claim First Use`,
      source_name  = `First Source Name Listed`
    ) %>%
    mutate(
      wria_num = suppressWarnings(as.integer(as.numeric(WRIA))),
      # Build plain-text Ecology URL (works in CSV; not a hyperlink)
      ecology_url = if_else(
        !is.na(wr_doc_id) & wr_doc_id != "",
        paste0(ECOLOGY_WR_URL, wr_doc_id),
        NA_character_
      )
    ) %>%
    left_join(wria_names, by = "wria_num") %>%
    mutate(
      wria_label = paste0(wria_num, " \u2014 ",
                          coalesce(wria_name, paste0("WRIA ", WRIA)))
    )
  message("Failed geocode records loaded: ", nrow(df))
  df
}, error = function(e) {
  warning("Could not load data/geocode_failed.csv — ", e$message)
  NULL
})


# ==============================================================================
# SECTION 4: FETCH BOUNDARY LAYERS AT STARTUP
# ==============================================================================

# Helper: fetch a GeoJSON layer, print status to console, return NULL on failure
fetch_geojson <- function(url, layer_name) {
  tryCatch({
    message("Fetching ", layer_name, " ...")
    layer <- st_read(url, quiet = TRUE)
    message(layer_name, ": loaded ", nrow(layer), " features | ",
            "columns: ", paste(names(layer), collapse = ", "))
    layer
  }, error = function(e) {
    message("WARNING — Could not load ", layer_name, ": ", e$message)
    NULL
  })
}

# Use the layers saved by pipeline/prepare_app_data.R (data/boundaries.rds) so
# the published app doesn't depend on the ArcGIS / TIGERweb services being up.
# Falls back to downloading them live if the file is missing or a layer is NULL.
bnd <- tryCatch(readRDS("data/boundaries.rds"),
                error = function(e) list(wria = NULL, county = NULL))
wria_sf   <- bnd$wria
county_sf <- bnd$county
if (is.null(wria_sf))   wria_sf   <- fetch_geojson(WRIA_GEOJSON_URL,   "WRIA boundaries")
if (is.null(county_sf)) county_sf <- fetch_geojson(COUNTY_GEOJSON_URL, "WA County boundaries")

# Build a display label column on the WRIA layer so we control what appears
# in tooltips. The exact column names vary by ECY service version; this block
# auto-detects them by looking for common naming patterns.
# (The saved layer already has wria_display, so this is skipped for it.)
if (!is.null(wria_sf) && !"wria_display" %in% names(wria_sf)) {
  nr_col <- names(wria_sf)[str_detect(names(wria_sf),
                                      regex("NR$|NUM$|NUMBER$", ignore_case = TRUE))][1]
  nm_col <- names(wria_sf)[str_detect(names(wria_sf),
                                      regex("NM$|NAME$", ignore_case = TRUE))][1]
  
  if (!is.na(nr_col) && !is.na(nm_col)) {
    wria_sf$wria_display <- paste0("WRIA ", wria_sf[[nr_col]],
                                   " \u2014 ", wria_sf[[nm_col]])
  } else {
    # Fallback: use our wria_names lookup joined on whatever number column exists
    wria_sf$wria_display <- paste0("WRIA ", seq_len(nrow(wria_sf)))
    message("NOTE: Could not auto-detect WRIA number/name columns.",
            " Update the nr_col / nm_col detection above if needed.")
  }
}


# ==============================================================================
# SECTION 5: KMZ HELPER FUNCTIONS
# Adapted from WA Water Rights PLSS Geocoder (WaterRightReportGeocodeAPP.R).
# Key changes vs. the Geocoder KMZ:
#   • Two styles: groundwater (blue) vs. surface water (orange)
#   • Placemarks labeled by water right number (doc_number), not phase
#   • WRIA folder structure retained; no phase sub-folders
#   • Hyperlink uses wr_doc_id same as Geocoder pattern
# ==============================================================================

# ---- KML <Style> block: two icon styles ------------------------------------
kml_styles <- paste0(
  '  <Style id="style_groundwater">\n',
  '    <IconStyle>\n',
  '      <color>', KML_COL_GW, '</color>\n',
  '      <scale>0.9</scale>\n',
  '      <Icon>',
  '<href>http://maps.google.com/mapfiles/kml/shapes/placemark_circle.png</href>',
  '</Icon>\n',
  '    </IconStyle>\n',
  '    <LabelStyle><scale>0.7</scale></LabelStyle>\n',
  '  </Style>\n',
  '  <Style id="style_surface_water">\n',
  '    <IconStyle>\n',
  '      <color>', KML_COL_SW, '</color>\n',
  '      <scale>0.9</scale>\n',
  '      <Icon>',
  '<href>http://maps.google.com/mapfiles/kml/shapes/placemark_circle.png</href>',
  '</Icon>\n',
  '    </IconStyle>\n',
  '    <LabelStyle><scale>0.7</scale></LabelStyle>\n',
  '  </Style>'
)

# ---- Build KML description (HTML table inside CDATA) for one row -----------
build_kml_description <- function(row) {
  
  link_html <- if (!is.na(row$wr_doc_id) && row$wr_doc_id != "") {
    url <- paste0(ECOLOGY_WR_URL, row$wr_doc_id)
    paste0("<a href='", url, "'>View WA Ecology Water Right Record</a><br/><br/>")
  } else ""
  
  attrs <- list(
    "Water Right No." = row$doc_number,
    "WR Doc ID"       = row$wr_doc_id,
    "Certificate No." = row$cert_number,
    "Holder"          = row$holder,
    "Status"          = row$Status,
    "Phase"           = row$Phase,
    "Stage"           = row$Stage,
    "Priority Date"   = row$priority_display,
    "Purpose"         = row$purpose,
    "Diversion Rate"  = paste0(row$inst_flow, " ", row$unit),
    "Annual Volume"   = paste0(row$annual_vol, " ac-ft"),
    "Water Source"    = row$source_name,
    "Source Type"     = row$source_type,
    "WRIA"            = row$wria_label,
    "County"          = row$County,
    "TRS"             = row$trs
  )
  
  table_rows <- paste0(
    "<tr><td><b>", names(attrs), "</b></td><td>",
    sapply(attrs, function(v) {
      if (is.null(v) || is.na(v) || as.character(v) %in% c("", "NA", "NA ac-ft", "NA NA"))
        "&nbsp;"
      else
        htmlEscape(as.character(v))
    }),
    "</td></tr>",
    collapse = "\n"
  )
  
  paste0(
    "<![CDATA[", link_html,
    "<table border='1' cellpadding='4' cellspacing='0' ",
    "style='font-size:12px;font-family:Arial;'>",
    table_rows,
    "</table>]]>"
  )
}

# ---- Build a single KML <Placemark> for one water right --------------------
build_kml_placemark <- function(row, indent = "      ") {
  
  # Build optional <TimeStamp> block for Google Earth's timeline slider.
  # Format: yyyy-mm-dd (ISO 8601). Only included if priority_date is not NA.
  # When present, Google Earth shows a time scrubber at the top of the screen
  # letting users animate through water right history chronologically.
  timestamp_xml <- if (!is.null(row$priority_date) &&
                       !is.na(row$priority_date) &&
                       as.character(row$priority_date) != "") {
    paste0(indent, "  <TimeStamp><when>",
           format(as.Date(row$priority_date), "%Y-%m-%d"),
           "</when></TimeStamp>\n")
  } else ""
  
  paste0(
    indent, "<Placemark>\n",
    indent, "  <name>", htmlEscape(as.character(row$doc_number)), "</name>\n",
    indent, "  <styleUrl>", row$kml_style, "</styleUrl>\n",
    timestamp_xml,
    indent, "  <description>", build_kml_description(row), "</description>\n",
    indent, "  <Point><coordinates>",
    row$lon, ",", row$lat, ",0",
    "</coordinates></Point>\n",
    indent, "</Placemark>"
  )
}

# ---- Assemble full KMZ from a filtered data frame --------------------------
build_lowflow_kmz <- function(df) {
  
  # Sort by WRIA number so Google Earth opens in order
  df_sorted <- df %>% arrange(wria_num)
  
  # One <Folder> per WRIA
  wria_groups <- df_sorted %>%
    group_by(wria_num, wria_label) %>%
    group_split()
  
  folder_xml <- map_chr(wria_groups, function(grp) {
    placemarks <- map_chr(seq_len(nrow(grp)), function(i) {
      build_kml_placemark(as.list(grp[i, ]))
    })
    paste0(
      "  <Folder>\n",
      "    <name>", grp$wria_label[1], "</name>\n",
      paste(placemarks, collapse = "\n"), "\n",
      "  </Folder>"
    )
  })
  
  kml_doc <- paste0(
    '<?xml version="1.0" encoding="UTF-8"?>\n',
    '<kml xmlns="http://www.opengis.net/kml/2.2">\n',
    '<Document>\n',
    '  <name>Instream flow limited water rights\u2014 ', Sys.Date(), '</name>\n',
    '  <description><![CDATA[Washington State water rights conditioned with ',
    'low flow restrictions. Generated ', format(Sys.Date(), "%B %d, %Y"),
    '. Blue = Groundwater | Orange = Surface Water.]]></description>\n',
    '  <LookAt>\n',
    '    <longitude>-120.5</longitude>\n',
    '    <latitude>47.4</latitude>\n',
    '    <altitude>0</altitude>\n',
    '    <range>800000</range>\n',
    '  </LookAt>\n',
    kml_styles, "\n",
    paste(folder_xml, collapse = "\n"), "\n",
    '</Document>\n</kml>'
  )
  
  # Write KML to a temp file, then zip it into a KMZ
  kml_tmp <- tempfile(fileext = ".kml")
  kmz_tmp <- tempfile(fileext = ".kmz")
  writeLines(kml_doc, kml_tmp, useBytes = FALSE)
  zip::zip(zipfile = kmz_tmp, files = kml_tmp, mode = "cherry-pick")
  
  kmz_tmp   # returned to the downloadHandler
}


# ==============================================================================
# SECTION 6: UI
# ==============================================================================

ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("

      /* ===== Banner ===== */
      .app-banner {
        background: linear-gradient(135deg, #1a3a5c, #2e6da4);
        color: white;
        padding: 14px 20px;
        margin: -15px -15px 18px -15px;
        display: flex;
        align-items: center;
        gap: 16px;
      }
      .banner-title {
        margin: 0;
        font-size: 20px;
        font-weight: 700;
        line-height: 1.2;
      }
      .banner-subtitle {
        margin: 3px 0 0;
        font-size: 12px;
        opacity: 0.85;
        font-style: italic;
      }

      /* ===== Sidebar ===== */
      .section-divider {
        border-top: 1px solid #dee2e6;
        margin: 12px 0 10px;
      }
      .section-label {
        font-size: 10.5px;
        font-weight: 700;
        color: #6c757d;
        text-transform: uppercase;
        letter-spacing: 0.06em;
        margin-bottom: 7px;
      }

      /* ===== Record count badge ===== */
      .record-badge {
        background: #f0f4f8;
        border-left: 3px solid #2e6da4;
        border-radius: 4px;
        padding: 5px 10px;
        font-size: 12px;
        margin-bottom: 2px;
      }

      /* ===== Map ===== */
      #map {
        border-radius: 6px;
        border: 1px solid #dee2e6;
      }

      /* ===== Download buttons ===== */
      .dl-btn {
        width: 100%;
        margin-bottom: 6px;
        text-align: left;
      }
      .dl-hint {
        font-size: 10.5px;
        color: #888;
        line-height: 1.45;
        margin-top: 4px;
      }

      /* ===== Purpose legend ===== */
      .purpose-row {
        font-size: 11px;
        line-height: 1.75;
        color: #444;
      }
      .purpose-code {
        font-weight: 700;
        color: #1a3a5c;
        display: inline-block;
        min-width: 48px;
      }

      /* ===== Collapsible table toggle ===== */
      details summary {
        cursor: pointer;
        font-weight: 600;
        color: #1a3a5c;
        padding: 6px 0;
        font-size: 13px;
      }
      details summary:hover { color: #2e6da4; }

    "))
  ),
  
  # ---- Banner ---------------------------------------------------------------
  tags$div(
    class = "app-banner",
    # Water-drop SVG icon (matches style used in other apps in this project)
    tags$svg(
      width = "46", height = "46", viewBox = "0 0 46 46",
      xmlns = "http://www.w3.org/2000/svg",
      tags$circle(cx = "23", cy = "23", r = "22",
                  fill = "rgba(255,255,255,0.12)",
                  stroke = "rgba(255,255,255,0.3)", `stroke-width` = "1.2"),
      # Water drop shape
      tags$path(
        d = "M23 7 C18 14 9 21 9 28 a14 14 0 0 0 28 0 C37 21 28 14 23 7 Z",
        fill = "rgba(255,255,255,0.88)"
      ),
      # Low flow wavy line inside drop
      tags$path(
        d = "M15 30 Q18 26 23 28 Q28 30 31 26",
        stroke = "#1a3a5c", `stroke-width` = "2.2",
        fill = "none", `stroke-linecap` = "round"
      )
    ),
    tags$div(
      tags$p(class = "banner-title",    "Instream Flow-limited Water Rights in Washington State"),
      tags$p(class = "banner-subtitle",
             "Washington State water rights conditioned with low flow restrictions")
    )
  ),
  
  sidebarLayout(
    
    # ---- Sidebar -------------------------------------------------------------
    sidebarPanel(
      width = 3,
      
      # Filter section
      tags$div(class = "section-label", "Filter Records"),
      
      # WRIA filter — multi-select with remove buttons
      selectizeInput(
        inputId  = "filter_wria",
        label    = "WRIA",
        choices  = NULL,
        multiple = TRUE,
        options  = list(
          placeholder = "All WRIAs",
          plugins     = list("remove_button")
        )
      ),
      
      # County filter
      selectizeInput(
        inputId  = "filter_county",
        label    = "County",
        choices  = NULL,
        multiple = TRUE,
        options  = list(
          placeholder = "All counties",
          plugins     = list("remove_button")
        )
      ),
      
      # Source type checkboxes
      checkboxGroupInput(
        inputId  = "filter_source",
        label    = "Source Type",
        choices  = c("Groundwater", "Surface Water"),
        selected = c("Groundwater", "Surface Water"),
        inline   = TRUE
      ),
      
      # Purpose of use filter
      selectizeInput(
        inputId  = "filter_purpose",
        label    = "Purpose of Use",
        choices  = NULL,
        multiple = TRUE,
        options  = list(
          placeholder = "All purposes",
          plugins     = list("remove_button")
        )
      ),
      
      # Priority date range slider — rendered dynamically so min/max are
      # set from the actual data range rather than hard-coded values.
      # Records with no priority date ("Not recorded") are always included
      # regardless of slider position — see filtered_data() in the server.
      tags$div(class = "section-divider"),
      tags$div(class = "section-label", "Priority Date Range"),
      uiOutput("priority_slider_ui"),
      tags$p(
        class = "dl-hint",
        "Records with no priority date recorded are always shown.",
        "Drag either handle to narrow the date range."
      ),
      
      # Map overlay toggles
      tags$div(class = "section-divider"),
      tags$div(class = "section-label", "Basemap"),
      
      radioButtons(
        inputId  = "basemap",
        label    = NULL,
        choices  = c(
          "USGS Hydro"     = "hydro",
          "USGS Topo"      = "topo",
          "Light (Gray Canvas)" = "gray"
        ),
        selected = "hydro"
      ),
      
      tags$div(class = "section-label", "Map Overlays"),
      
      checkboxInput("show_wria",   "WRIA Boundaries",       value = TRUE),
      checkboxInput("show_county", "County Boundaries",     value = FALSE),
      checkboxInput("show_plss",   "PLSS / TRS Grid (BLM)", value = FALSE),
      tags$p(
        class = "dl-hint",
        "BLM CadNSDI Township, Range & Section grid.",
        "Zoom to level 10+ for section lines to render.",
        "Useful for verifying TRS centroids."
      ),
      
      # Record count
      tags$div(class = "section-divider"),
      uiOutput("record_count_ui"),
      
      # Downloads
      tags$div(class = "section-divider"),
      tags$div(class = "section-label", "Download Filtered Data"),
      
      downloadButton(
        outputId = "dl_csv",
        label    = tagList(icon("file-csv"), " Download CSV"),
        class    = "btn btn-default dl-btn"
      ),
      
      downloadButton(
        outputId = "dl_kmz",
        label    = tagList(icon("earth-americas"), " Download KMZ"),
        class    = "btn btn-default dl-btn"
      ),
      
      tags$p(
        class = "dl-hint",
        "KMZ: placemarks labeled by water right number, ",
        "organized into WRIA folders, with a link to the ",
        "WA Ecology record. Blue = Groundwater | Orange = Surface Water."
      ),
      
      # Purpose code legend
      tags$div(class = "section-divider"),
      tags$div(class = "section-label", "Purpose of Use Codes"),
      uiOutput("purpose_legend_ui")
      
    ), # end sidebarPanel
    
    # ---- Main panel ----------------------------------------------------------
    mainPanel(
      width = 9,
      
      tabsetPanel(
        id = "main_tabs",
        
        # ================================================================
        # TAB 1: MAP
        # ================================================================
        tabPanel(
          title = tagList(icon("map"), " Map"),
          br(),
          
          # The Leaflet map
          leafletOutput("map", height = "630px"),
          
          br(),
          
          # Collapsible data table
          tags$details(
            tags$summary("View / Search Data Table"),
            tags$p(
              style = "font-size:11px; color:#666; margin: 6px 0 4px;",
              icon("hand-pointer"), " Click a row to fly the map to that record.",
              " The Water Right No. links directly to the WA Ecology record."
            ),
            br(),
            DTOutput("data_table")
          )
        ), # end Map tab
        
        # ================================================================
        # TAB 2: BACKGROUND & ABOUT
        # ================================================================
        tabPanel(
          title = tagList(icon("circle-info"), " Background & About"),
          br(),
          
          # ---- What this map shows ---------------------------------------
          tags$h4("About This Map"),
          tags$p(
            "This interactive map displays Washington State water rights that are
            conditioned with ", tags$b("low flow restrictions"), " — provisions
            that prohibit or limit a diversion or withdrawal when the flow of a
            source stream drops below a specified threshold. These conditions are
            an important tool for protecting instream flows and aquatic resources
            during periods of low water. Threshold levels may be imposed via an
            application-specific determination (usually in consultation with fishery managers)
            or as part of larger basin studies in support of an adopted instream flow rule.
            Instream flow water rights held by the State of Washington have priority over
            subsequently issued, junior water rights.(For more information, see the ",
            tags$a("Instream Flow Rules Explorer).",
                   href   = "https://waterwater.shinyapps.io/IFexplorer/",
                   target = "_blank"),
          ),
          
          
          tags$hr(),
          
          # ---- Data source and query criteria ----------------------------
          tags$h4("Data Source & Query Criteria"),
          tags$p(
            "Records were retrieved from the Washington State Department of Ecology's ",
            tags$a("Water Rights Tracking System (WRTS)",
                   href   = "https://appswr.ecology.wa.gov/waterrighttrackingsystem/WaterRights/default.aspx",
                   target = "_blank"),
            ". The following query criteria were applied:"
          ),
          tags$div(
            style = "background:#f0f4f8; border-left:3px solid #2e6da4;
                     border-radius:4px; padding:10px 16px; margin:10px 0;
                     font-size:12px; font-family:monospace; line-height:2;",
            
            tags$div(tags$b("Status: "), "Active"),
            tags$div(tags$b("Condition type: "), "Col. Rv Instream Flws OR Instream Resource Protection Program OR InStrm Flw/Min Flw w/bypass OR Low Flow OR Methow Rv Instream Flws OR Okanogan Rv Instream Flw OR Snake River Low Flow OR Wenatchee Rv Instream Flw"),
            tags$div(tags$b("Record count (geocoded): "),
                     if (!is.null(wr_data)) format(nrow(wr_data), big.mark = ",") else "—"),
            tags$div(tags$b("Record count (failed geocode): "),
                     if (!is.null(failed_geocode)) nrow(failed_geocode) else "—"),
            tags$div(tags$b("Data retrieved: "), format(DATA_DATE, "%B %d, %Y"))
          ),
          
          tags$hr(),
          
          # ---- Understanding the water right record ----------------------
          tags$h4("How to Read a Water Right Record"),
          tags$p(
            "Each mapped point links to the full water right record in WRTS via the
            ", tags$b("Water Right No."), " column in the data table, or through
            the popup that appears when you click a map marker. The WRTS pages 
            summmarize the attributes of the water right -- including any special provisions -- and also provide a link to scanned images from water right history. Understanding the document
            hierarchy is important:"
          ),
          tags$div(
            style = "margin: 0 0 12px 12px;",
            tags$p(tags$b("Certificate"), " — The final stage of a water right.
                Certificates often describe only the core elements of the right
                (source, quantity, purpose, place of use) and ", tags$em("may not"),
                   " explicitly restate all conditions and provisions, including
                low flow restrictions. Do not assume that the absence of a low
                flow condition in the certificate means none exists."
            ),
            tags$p(tags$b("Permit"), " — Issued after the application is approved.
                Permits typically carry the full set of conditions as originally
                written by the examiner. Low flow provisions are often found
                here, but if missing, it is still important to review the underlying
                report of examination."
            ),
            tags$p(tags$b("Report of Examination (ROE)"), " — The foundational
                document prepared by the Ecology examiner that establishes the
                basis for the water right and all its conditions. If you need to
                understand the origin and intent of a low flow provision, the ROE
                is the authoritative source."
            )
          ),
          tags$p(
            style = "background:#fff8e1; border-left:3px solid #f9a825;
                     border-radius:4px; padding:8px 12px; font-size:12px;",
            icon("triangle-exclamation", style = "color:#f9a825;"),
            " ", tags$b("Important:"), " Always review the full record — including
            the Permit and Report of Examination — to determine the complete terms
            of a low flow restriction. The certificate alone is not sufficient."
          ),
          
          tags$hr(),
          
          tags$h4("Subordination of 1997-1998 registered water right claims to instream flow rules"),
          tags$p(
            "There have been four water right claim registration periods in Washington State's history.
            The state Legislature made claims filed during the most recent (1997-98) registration opening
            subordinate to existing water rights (RCW 90.14.068). Thus there are some water right claims which are recorded
            as being subject to minimum instream flow rules.",
          ),
          
          tags$h4("Other forms of instream flow restrictions"),
          tags$p(
            "A water right holder may be
            required to comply with instream flow limitations imposed by other
            authorities, e.g., a federally-licensed hydroelectric project, or a major
            water purveyor who has entered into a Habitat Conservation
            Plan agreements under the Endangered Species Act. This information may not be reflected
            in the water right files for the project."
          ),
          
          # ---- Location accuracy -----------------------------------------
          tags$h4("Location Accuracy"),
          tags$p(
            "Point locations on this map represent ", tags$b("Township-Range-Section (TRS) centroids"),
            " geocoded from the legal land description recorded in WRTS. These are
            approximate center points of the legal section in which the diversion
            or withdrawal is located and should ", tags$b("not"), " be interpreted
            as the exact location of a diversion works, pump, or well."
          ),
          tags$p(
            "The actual point of diversion may be located anywhere within the
            indicated section — potentially up to approximately ", tags$b("±0.5 miles"),
            " from the displayed point. For precise location information, consult
            the water right record directly. The ",
            tags$b("PLSS / TRS Grid (BLM)"), " overlay in the sidebar can help
            orient points within their section boundaries."
          ),
          
          tags$hr(),
          
          # ---- Data disclaimer -------------------------------------------
          tags$h4("Data Disclaimer"),
          tags$p(
            "The information displayed in this application is sourced directly
            from the Washington State Department of Ecology's ",
            tags$a("Water Rights Tracking System (WRTS)",
                   href   = "https://appswr.ecology.wa.gov/waterrighttrackingsystem/WaterRights/default.aspx",
                   target = "_blank"),
            " and is presented without modification. The presence of a water
            right in this dataset reflects how Ecology has categorized that
            record within WRTS and does not constitute an independent
            determination that a low flow restriction exists or is
            enforceable."
          ),
          tags$p(
            "Users who identify apparent data errors, unexpected records, or
            questions about a specific water right should contact the
            Washington State Department of Ecology's Water Resources Program
            directly using Ecology's ",
            tags$a("Water Resources Web Map Customer Feedback Form.",
                   href   = "https://forms.office.com/Pages/ResponsePage.aspx?id=F-LQEU4mCkCLoFfcwSfXLTFKFFaAvxJPomsFWY6QxthUOURCN0lZNkY3SFlWUDc5Wjg2UEhZMk44RC4u",
                   target = "_blank"),
            tags$em(" I'm just the messenger.")
          ),
          
          tags$hr(),
          
          # ---- Contact / feedback ----------------------------------------
          tags$h4("Questions & Feedback"),
          tags$p(
            "jeffjmarti@gmail.com",
            tags$br(),
            tags$a("Jeff's Water Resources Data Portal",
                   href   = "https://jeffmarti.github.io/",
                   target = "_blank"),
          )
          
        ), # end About tab
        
        # ================================================================
        # TAB 3: UNMAPPED RECORDS
        # ================================================================
        tabPanel(
          title = tagList(icon("triangle-exclamation"), " Unmapped Records"),
          br(),
          
          tags$h4("Records Not Displayed on the Map"),
          tags$p(
            if (!is.null(failed_geocode)) nrow(failed_geocode) else "Some",
            " records from the WRTS export could not be geocoded and are therefore
            not shown on the map. Geocoding assigns a latitude/longitude to each
            record by querying the ",
            tags$a("BLM National PLSS CadNSDI service",
                   href   = "https://gis.blm.gov/arcgis/rest/services/Cadastral/BLM_Natl_PLSS_CadNSDI/MapServer",
                   target = "_blank"),
            " for the Township, Range, and Section recorded in WRTS."
          ),
          tags$p(
            "In some cases, Ecology's TRS description
            did not return a matching record in BLM's PLSS geocoding service. For example, in some cases, Ecology appears to have
            assigned a section number where a township was never formally
            sectionalized and BLM subdivided them into ",
            tags$b("Protracted Blocks"),
            "rather than standard sections. Geocoding can also fail where Ecology's section numbers do not match other form of legal subdivisions (e.g., Donation Land Claims). It's worth noting that Ecology's Water Right database does not provide entry fields for any other Township subdivision other than sections and quarter fractions."
          ),
          tags$p(
            "All 26 unmapped records remain part of the low flow water rights
            dataset. You can access the full record for each right using the
            links in the table below."
          ),
          
          tags$div(
            style = "margin-bottom:10px;",
            downloadButton(
              outputId = "dl_failed_csv",
              label    = tagList(icon("file-csv"), " Download Unmapped Records (CSV)"),
              class    = "btn btn-default"
            ),
            tags$span(
              style = "font-size:11px; color:#888; margin-left:10px;",
              "CSV includes a plain-text URL column for each Ecology record."
            )
          ),
          
          tags$p(
            style = "font-size:11px; color:#666; margin-bottom:6px;",
            icon("hand-pointer"),
            " Click the Water Right No. to open the full Ecology record."
          ),
          DTOutput("failed_table")
          
        ) # end Unmapped tab
        
      ) # end tabsetPanel
    ) # end mainPanel
  )
)


# ==============================================================================
# SECTION 7: SERVER
# ==============================================================================

server <- function(input, output, session) {
  
  # --------------------------------------------------------------------------
  # 7a: Populate filter dropdowns once wr_data is available
  # --------------------------------------------------------------------------
  
  observe({
    req(wr_data)
    
    # WRIA choices — integer keys, human-readable labels ("1 — Nooksack")
    wria_choices <- wr_data %>%
      distinct(wria_num, wria_label) %>%
      arrange(wria_num) %>%
      { setNames(.$wria_num, .$wria_label) }
    
    # County choices — alphabetical, NAs removed
    county_choices <- sort(na.omit(unique(wr_data$County)))
    
    # Purpose choices — split the comma-separated purpose field into unique codes,
    # then attach a human-readable definition where available
    purpose_raw <- wr_data$purpose %>%
      str_split(",") %>%
      unlist() %>%
      str_trim() %>%
      na.omit() %>%
      unique() %>%
      sort()
    
    purpose_choices <- setNames(
      purpose_raw,
      sapply(purpose_raw, function(code) {
        def <- purpose_defs$definition[purpose_defs$code_upper == toupper(code)]
        if (length(def) == 0 || is.na(def[1])) code
        else paste0(code, " \u2014 ", def[1])
      })
    )
    
    updateSelectizeInput(session, "filter_wria",    choices = wria_choices)
    updateSelectizeInput(session, "filter_county",  choices = county_choices)
    updateSelectizeInput(session, "filter_purpose", choices = purpose_choices)
  })
  
  # --------------------------------------------------------------------------
  # 7b: OUTPUT — priority date range slider
  # Rendered dynamically so min/max year values come from the actual data.
  # We use integer years (not full Date objects) for the slider — cleaner
  # display and easier to filter against. Records without a priority date
  # (priority_date = NA) are always included regardless of slider position.
  # --------------------------------------------------------------------------
  
  output$priority_slider_ui <- renderUI({
    req(wr_data)
    
    # Only consider rows that actually have a parsed priority date
    valid_dates <- wr_data$priority_date[!is.na(wr_data$priority_date)]
    
    min_yr <- as.integer(format(min(valid_dates), "%Y"))
    max_yr <- as.integer(format(max(valid_dates), "%Y"))
    
    sliderInput(
      inputId = "filter_year",
      label   = NULL,
      min     = min_yr,
      max     = max_yr,
      value   = c(min_yr, max_yr),   # default: full range selected
      step    = 1,
      sep     = "",                   # no thousands separator on years
      ticks   = FALSE
    )
  })
  # --------------------------------------------------------------------------
  
  filtered_data <- reactive({
    req(wr_data)
    df <- wr_data
    
    if (length(input$filter_wria) > 0) {
      df <- df %>% filter(wria_num %in% as.integer(input$filter_wria))
    }
    if (length(input$filter_county) > 0) {
      df <- df %>% filter(County %in% input$filter_county)
    }
    if (length(input$filter_source) > 0) {
      df <- df %>% filter(source_type %in% input$filter_source)
    } else {
      df <- df %>% slice(0)
    }
    if (length(input$filter_purpose) > 0) {
      pattern <- paste0(
        "(^|,\\s*)(", paste(input$filter_purpose, collapse = "|"), ")(\\s*,|$)"
      )
      df <- df %>% filter(str_detect(purpose, regex(pattern, ignore_case = TRUE)))
    }
    
    # Priority year filter — only apply once the slider has rendered
    # (input$filter_year is NULL until renderUI fires).
    # Records with no priority date (NA) are always kept — they shouldn't
    # be penalised for a data gap.
    if (!is.null(input$filter_year)) {
      yr_min <- input$filter_year[1]
      yr_max <- input$filter_year[2]
      df <- df %>%
        filter(
          is.na(priority_date) |
            (as.integer(format(priority_date, "%Y")) >= yr_min &
               as.integer(format(priority_date, "%Y")) <= yr_max)
        )
    }
    
    df
  })
  
  # --------------------------------------------------------------------------
  # 7c: OBSERVER — basemap switcher
  # --------------------------------------------------------------------------
  
  observe({
    proxy <- leafletProxy("map")
    group_to_show <- switch(input$basemap,
                            hydro = "USGS Hydro",
                            topo  = "USGS Topo",
                            gray  = "Light (Gray Canvas)"
    )
    proxy %>%
      hideGroup("USGS Hydro") %>%
      hideGroup("USGS Topo") %>%
      hideGroup("Light (Gray Canvas)") %>%
      showGroup(group_to_show)
  })
  
  # --------------------------------------------------------------------------
  # 7d: OUTPUT — record count badge
  # --------------------------------------------------------------------------
  
  output$record_count_ui <- renderUI({
    n   <- nrow(filtered_data())
    tot <- if (!is.null(wr_data)) nrow(wr_data) else 0L
    tags$div(
      class = "record-badge",
      tags$b(format(n, big.mark = ",")),
      " of ",
      format(tot, big.mark = ","),
      " records displayed"
    )
  })
  
  # --------------------------------------------------------------------------
  # 7e: OUTPUT — purpose code legend
  # --------------------------------------------------------------------------
  
  output$purpose_legend_ui <- renderUI({
    req(wr_data)
    present_codes <- wr_data$purpose %>%
      str_split(",") %>% unlist() %>% str_trim() %>%
      na.omit() %>% unique() %>% sort()
    rows <- lapply(present_codes, function(code) {
      def <- purpose_defs$definition[purpose_defs$code_upper == toupper(code)]
      def_text <- if (length(def) == 0 || is.na(def[1])) "(see Ecology WRTS help)"
      else def[1]
      tags$div(class = "purpose-row",
               tags$span(class = "purpose-code", code), def_text)
    })
    tagList(rows)
  })
  
  # --------------------------------------------------------------------------
  # 7f: OUTPUT — initial Leaflet map
  # --------------------------------------------------------------------------
  
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = TRUE)) %>%
      addTiles(
        urlTemplate = paste0("https://basemap.nationalmap.gov/arcgis/rest/services/",
                             "USGSHydroCached/MapServer/tile/{z}/{y}/{x}"),
        attribution = "USGS National Map | Hydro Cached",
        group       = "USGS Hydro"
      ) %>%
      addTiles(
        urlTemplate = paste0("https://basemap.nationalmap.gov/arcgis/rest/services/",
                             "USGSTopo/MapServer/tile/{z}/{y}/{x}"),
        attribution = "USGS National Map | Topo",
        group       = "USGS Topo"
      ) %>%
      # Esri Light Gray Canvas -- keyless replacement for CartoDB.Positron,
      # whose raster tiles now require an API key and are being retired.
      # Keyless swaps if Esri ever retires this service:
      #   providers$OpenStreetMap.Mapnik / providers$Esri.WorldTopoMap
      addProviderTiles(
        providers$Esri.WorldGrayCanvas,
        group = "Light (Gray Canvas)"
      ) %>%
      addWMSTiles(
        baseUrl = paste0("https://gis.blm.gov/arcgis/services/Cadastral/",
                         "BLM_Natl_PLSS_CadNSDI/MapServer/WMSServer"),
        layers  = "1,2",
        options = WMSTileOptions(format = "image/png", transparent = TRUE,
                                 opacity = 0.80, version = "1.3.0"),
        attribution = "PLSS grid: BLM National CadNSDI",
        group       = "PLSS / TRS Grid"
      ) %>%
      setView(lng = -120.5, lat = 47.4, zoom = 7) %>%
      addLegend(
        position = "bottomright",
        colors   = c(COL_GW, COL_SW),
        labels   = c("Groundwater", "Surface Water"),
        title    = "Water Source",
        opacity  = 0.9
      ) %>%
      # Size legend — SVG circles sized to match the log scale formula exactly.
      # Each SVG viewBox = (2r+4) x (2r+4) with cx=cy=r+2 so no circle
      # is clipped at the edges (clipping makes circles look like squares).
      addControl(
        position = "bottomright",
        html = paste0(
          '<div style="background:white;padding:8px 10px;border-radius:4px;',
          'border:1px solid #ccc;font-size:11px;font-family:Arial;line-height:2;">',
          '<b>Diversion Rate (CFS)</b><br/>',
          # No data → r=5, SVG 14x14, cx=cy=7
          '<svg width="40" height="14" style="vertical-align:middle;">',
          '<circle cx="7" cy="7" r="5" fill="#888" fill-opacity="0.7" stroke="#888" stroke-width="1"/></svg>',
          'No data<br/>',
          # ~1 CFS → r=6, SVG 16x16, cx=cy=8
          '<svg width="40" height="16" style="vertical-align:middle;">',
          '<circle cx="8" cy="8" r="6" fill="#888" fill-opacity="0.7" stroke="#888" stroke-width="1"/></svg>',
          '~1 CFS<br/>',
          # ~10 CFS → r=10, SVG 24x24, cx=cy=12
          '<svg width="40" height="24" style="vertical-align:middle;">',
          '<circle cx="12" cy="12" r="10" fill="#888" fill-opacity="0.7" stroke="#888" stroke-width="1"/></svg>',
          '~10 CFS<br/>',
          # ~100 CFS → r=14, SVG 32x32, cx=cy=16
          '<svg width="40" height="32" style="vertical-align:middle;">',
          '<circle cx="16" cy="16" r="14" fill="#888" fill-opacity="0.7" stroke="#888" stroke-width="1"/></svg>',
          '~100 CFS<br/>',
          # ≥500 CFS → r=18 (cap), SVG 40x40, cx=cy=20
          '<svg width="40" height="40" style="vertical-align:middle;">',
          '<circle cx="20" cy="20" r="18" fill="#888" fill-opacity="0.7" stroke="#888" stroke-width="1"/></svg>',
          '\u2265500 CFS',
          '</div>'
        )
      ) %>%
      showGroup("Water Rights") %>%
      addFullscreenControl()
  })
  
  # --------------------------------------------------------------------------
  # 7g: OBSERVER — water right markers
  # --------------------------------------------------------------------------
  
  observe({
    df    <- filtered_data()
    proxy <- leafletProxy("map")
    proxy %>% clearGroup("Water Rights")
    if (nrow(df) == 0) return(invisible(NULL))
    proxy %>%
      addCircleMarkers(
        data  = df, lng = ~lon, lat = ~lat, group = "Water Rights",
        color = ~marker_color, fillColor = ~marker_color,
        fillOpacity = 0.75, opacity = 1, radius = ~marker_radius, weight = 1.2,
        label = ~lapply(
          paste0("<b style='color:#1a3a5c;font-size:13px;'>",
                 htmlEscape(source_name), "</b>",
                 "<br/><span style='font-size:11px;color:#555;'>",
                 htmlEscape(doc_number), " &nbsp;|&nbsp; ",
                 htmlEscape(coalesce(holder, "")), "</span>"),
          HTML
        ),
        labelOptions = labelOptions(
          style     = list("font-size" = "12px", "padding" = "5px 10px",
                           "border-radius" = "4px"),
          direction = "auto", sticky = FALSE
        ),
        popup        = ~popup_html,
        popupOptions = popupOptions(maxWidth = 330, closeOnClick = TRUE),
        clusterOptions = markerClusterOptions(
          maxClusterRadius    = 60,
          spiderfyOnMaxZoom   = TRUE,
          showCoverageOnHover = TRUE,
          zoomToBoundsOnClick = TRUE
        )
      )
  })
  
  # --------------------------------------------------------------------------
  # 7h: OBSERVER — WRIA boundary overlay
  # --------------------------------------------------------------------------
  
  observe({
    proxy <- leafletProxy("map")
    if (input$show_wria) {
      if (is.null(wria_sf)) {
        showNotification("WRIA boundary layer failed to load at startup. Check the console.",
                         type = "warning", duration = 5)
        return(invisible(NULL))
      }
      proxy %>%
        clearGroup("WRIA Boundaries") %>%
        addPolygons(
          data = wria_sf, group = "WRIA Boundaries",
          color = "#1a3a5c", weight = 1.5, fillOpacity = 0.06, fillColor = "#1a3a5c",
          label = ~wria_display,
          labelOptions = labelOptions(
            style = list("font-size" = "11px", "font-weight" = "bold"),
            direction = "center", sticky = FALSE),
          highlightOptions = highlightOptions(
            color = "#2e6da4", weight = 2.5, fillOpacity = 0.14, bringToFront = FALSE)
        )
    } else {
      proxy %>% clearGroup("WRIA Boundaries")
    }
  })
  
  # --------------------------------------------------------------------------
  # 7i: OBSERVER — County boundary overlay
  # --------------------------------------------------------------------------
  
  observe({
    proxy <- leafletProxy("map")
    if (input$show_county) {
      if (is.null(county_sf)) {
        showNotification("County boundary layer failed to load at startup. Check the console.",
                         type = "warning", duration = 5)
        return(invisible(NULL))
      }
      proxy %>%
        clearGroup("County Boundaries") %>%
        addPolygons(
          data = county_sf, group = "County Boundaries",
          color = "#6a0dad", weight = 1.5, fillOpacity = 0.04, fillColor = "#6a0dad",
          label = ~NAME,
          labelOptions = labelOptions(style = list("font-size" = "11px"),
                                      direction = "center", sticky = FALSE),
          highlightOptions = highlightOptions(
            color = "#6a0dad", weight = 2.5, fillOpacity = 0.12, bringToFront = FALSE)
        )
    } else {
      proxy %>% clearGroup("County Boundaries")
    }
  })
  
  # --------------------------------------------------------------------------
  # 7j: OBSERVER — PLSS / TRS Grid overlay
  # --------------------------------------------------------------------------
  
  observe({
    proxy <- leafletProxy("map")
    if (input$show_plss) {
      proxy %>% showGroup("PLSS / TRS Grid")
    } else {
      proxy %>% hideGroup("PLSS / TRS Grid")
    }
  })
  
  # --------------------------------------------------------------------------
  # 7k: OUTPUT — data table
  # --------------------------------------------------------------------------
  
  output$data_table <- renderDT({
    df <- filtered_data()
    wr_link <- if_else(
      !is.na(df$wr_doc_id) & df$wr_doc_id != "",
      paste0('<a href="', ECOLOGY_WR_URL, df$wr_doc_id,
             '" target="_blank" style="color:#1565C0;font-weight:600;">',
             htmlEscape(df$doc_number), '</a>'),
      htmlEscape(df$doc_number)
    )
    df_display <- df %>%
      mutate(wr_link = wr_link) %>%
      select(
        "Water Right No."     = wr_link,
        "Holder"              = holder,
        "Purpose"             = purpose,
        "Priority Date"       = priority_display,
        "Diversion Rate"      = inst_flow,
        "Unit"                = unit,
        "Annual Vol. (ac-ft)" = annual_vol,
        "Source Type"         = source_type,
        "Water Source"        = source_name,
        "WRIA"                = wria_label,
        "County"              = County,
        "Phase"               = Phase,
        "Stage"               = Stage
      )
    datatable(
      df_display,
      escape    = FALSE,
      selection = "single",
      options   = list(scrollX = TRUE, pageLength = 15, dom = "lfrtip"),
      rownames  = FALSE,
      filter    = "top"
    )
  })
  
  # --------------------------------------------------------------------------
  # 7l: OBSERVER — zoom map to selected data table row
  # --------------------------------------------------------------------------
  
  observeEvent(input$data_table_rows_selected, {
    idx <- input$data_table_rows_selected
    req(length(idx) == 1)
    df  <- filtered_data()
    row <- df[idx, ]
    req(!is.na(row$lat), !is.na(row$lon))
    leafletProxy("map") %>%
      flyTo(lng = row$lon, lat = row$lat, zoom = 15) %>%
      clearGroup("row_highlight") %>%
      addCircleMarkers(
        lng = row$lon, lat = row$lat, group = "row_highlight",
        radius = 14, color = "#FFD700", fillColor = "transparent",
        fillOpacity = 0, opacity = 1, weight = 3,
        options = pathOptions(interactive = FALSE)
      )
  })
  
  # --------------------------------------------------------------------------
  # 7m: DOWNLOAD HANDLER — CSV
  # --------------------------------------------------------------------------
  
  output$dl_csv <- downloadHandler(
    filename = function() {
      paste0("WA_LowFlow_WaterRights_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      filtered_data() %>%
        select(-marker_color, -marker_radius, -kml_style, -popup_html, -ecology_link,
               -priority_date, -wria_num, -wria_name, -wria_label,
               -source_type, -priority_raw, -cfs_for_size, -inst_flow_num) %>%
        write_csv(file)
    }
  )
  
  # --------------------------------------------------------------------------
  # 7n: DOWNLOAD HANDLER — KMZ
  # --------------------------------------------------------------------------
  
  output$dl_kmz <- downloadHandler(
    filename = function() {
      paste0("WA_LowFlow_WaterRights_", format(Sys.Date(), "%Y%m%d"), ".kmz")
    },
    content = function(file) {
      df <- filtered_data()
      validate(need(nrow(df) > 0,
                    "No records to export. Adjust your filters and try again."))
      withProgress(message = "Building KMZ file...", value = 0.5, {
        kmz_path <- build_lowflow_kmz(df)
        file.copy(kmz_path, file)
      })
    }
  )
  
  # --------------------------------------------------------------------------
  # 7o: OUTPUT — failed geocode DT table
  # --------------------------------------------------------------------------
  
  output$failed_table <- renderDT({
    req(failed_geocode)
    wr_link <- if_else(
      !is.na(failed_geocode$wr_doc_id) & failed_geocode$wr_doc_id != "",
      paste0('<a href="', ECOLOGY_WR_URL, failed_geocode$wr_doc_id,
             '" target="_blank" style="color:#1565C0;font-weight:600;">',
             htmlEscape(failed_geocode$doc_number), '</a>'),
      htmlEscape(failed_geocode$doc_number)
    )
    df_display <- failed_geocode %>%
      mutate(wr_link = wr_link) %>%
      select(
        "Water Right No." = wr_link,
        "Holder"          = holder,
        "Purpose"         = purpose,
        "Priority Date"   = priority_raw,
        "Source"          = source_name,
        "WRIA"            = wria_label,
        "County"          = County,
        "TRS"             = TRS,
        "Phase"           = Phase,
        "Stage"           = Stage
      )
    datatable(
      df_display,
      escape    = FALSE,
      selection = "none",
      options   = list(scrollX = TRUE, pageLength = 30, dom = "tip"),
      rownames  = FALSE
    )
  })
  
  # --------------------------------------------------------------------------
  # 7p: DOWNLOAD HANDLER — failed geocode CSV
  # --------------------------------------------------------------------------
  
  output$dl_failed_csv <- downloadHandler(
    filename = function() {
      paste0("WA_LowFlow_UnmappedRecords_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      req(failed_geocode)
      failed_geocode %>%
        select(
          "Record Document Number" = doc_number,
          "WR Doc ID"              = wr_doc_id,
          "Ecology Record URL"     = ecology_url,
          "Certificate Number"     = cert_number,
          "Person or Organization" = holder,
          "Purpose of Use"         = purpose,
          "Priority Date"          = priority_raw,
          "First Source Name"      = source_name,
          "WRIA"                   = wria_label,
          County, TRS, Phase, Stage
        ) %>%
        write_csv(file)
    }
  )
  
} # end server


# ==============================================================================
# SECTION 8: LAUNCH
# ==============================================================================

shinyApp(ui = ui, server = server)