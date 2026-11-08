# low-flow-wr

Interactive Shiny/Leaflet map of Washington State water rights conditioned with
**low flow (instream flow) restrictions**. Data refreshes every Monday morning via
GitHub Actions; Posit Connect Cloud republishes the app from each new commit.

Live app: https://01a1bd4d-7986-bcca-5c42-73d443ccd94a.share.connect.posit.cloud
(published from this GitHub repo, republishes on push).

**Redirect chain — don't delete the old content:**
https://waterwater.shinyapps.io/low-flow-wr/ → old Connect Cloud content
`01a11dd4-8660-7ab6-5a08-c6ae18845070` → this app. The shinyapps.io redirect target
can't be changed, so the old content now runs a tiny forwarding app
(`../low-flow-wr-redirect/app.R`, deploy command in its header).

---

## How the data gets refreshed

No more manual download, geocoder app, or `write_data_date.R`. The pipeline does
all of it:

```
pipeline/run_pipeline.R
  1. fetch_wrts_export.R   query Ecology's WRTS search page + download the .xlsx -> pipeline/raw/
  2. geocode_records.R     PLSS TRS -> lon/lat via BLM CadNSDI (cached)        -> pipeline/work/
  3. prepare_app_data.R    write the files app.R reads                          -> data/
```

`.github/workflows/refresh-data.yml` runs it every **Monday 15:17 UTC** (8:17 am
Pacific daylight / 7:17 am standard), 30 minutes after wr-applications, and
commits the refreshed `data/` files back to the repo.

**Safety check:** if a run maps fewer than 80% of the records the app has now,
`prepare_app_data.R` stops without overwriting anything and the workflow fails
(GitHub emails you).

---

## Folder layout

```
low-flow-wr/                          <- RStudio project folder = GitHub repo
├── app.R                             the Shiny app                          (deploy)
├── manifest.json                     made by rsconnect::writeManifest()    (deploy)
├── data/
│   ├── lowflow_rights.csv            geocoded records       — pipeline    (deploy)
│   ├── geocode_failed.csv            unmapped records       — pipeline    (deploy)
│   ├── data_date.txt                 "Data retrieved" date  — pipeline    (deploy)
│   ├── boundaries.rds                WRIA + county layers   — pipeline    (deploy)
│   └── purpose_codes.csv             purpose of use code definitions       (deploy)
├── pipeline/
│   ├── run_pipeline.R                runs the three steps in order
│   ├── fetch_wrts_export.R
│   ├── geocode_records.R
│   ├── prepare_app_data.R
│   └── cache/section_bbox_cache.csv  PLSS section cache (from wr-applications)
├── .github/workflows/refresh-data.yml
├── .gitignore                        keeps raw/, work/, rsconnect/ out of GitHub
└── low-flow-wr.Rproj
```

---

## Weekly routine

Nothing, unless the Monday run fails (GitHub emails you).

**Before editing locally, always `git pull` first**, because the workflow commits
every week.

To refresh by hand: GitHub > Actions > "Refresh low flow data" > Run workflow.
Or locally in the RStudio console (project open):

```r
source("pipeline/run_pipeline.R")
```

Read the **APP DATA SUMMARY** it prints at the end.

---

## If you change app.R's packages

Re-write the manifest Connect Cloud uses, then commit and push:

```r
rsconnect::writeManifest(appFiles = c("app.R",
  "data/lowflow_rights.csv", "data/geocode_failed.csv", "data/data_date.txt",
  "data/boundaries.rds", "data/purpose_codes.csv"))
```

---

## WRTS Query Criteria (set in `pipeline/fetch_wrts_export.R`)

| Field | Value |
|---|---|
| Role | Primary |
| Doc Status | Active |
| Provision | Col. Rv Instream Flws OR Instream Resource Protection Program OR InStrm Flw/Min Flw w/bypass OR Low Flow OR Methow Rv Instream Flws OR Okanogan Rv Instream Flw OR Snake River Low Flow OR Wenatchee Rv Instream Flw |

If Ecology renames or drops one of these provisions, the fetch step stops with
an error naming it, rather than silently returning fewer records.

---

## Unmapped Records

About 26 records can't be geocoded. Most failures occur where Ecology's TRS
description did not match any record in BLM's PLSS CadNSDI service — typically
in townships that were never formally sectionalized and were instead divided
into **Protracted Blocks** by BLM. Ecology's WRTS uses conventional section
numbers in these areas that have no direct BLM equivalent.

The geocoder tries a **two-pass query** (Section first, then Protracted Block
fallback). Records that fail both land in `data/geocode_failed.csv` and on the
app's Unmapped Records tab. Sections BLM has no match for are re-checked every
90 days.

---

## Key App Features

| Feature | Notes |
|---|---|
| Clustered markers | Blue = Groundwater, Orange = Surface Water |
| Hover tooltip | Shows water source name + right number + holder |
| Click popup | Full attribute table + live Ecology WRTS link |
| Sidebar filters | WRIA, County, Source Type, Purpose of Use |
| Basemap switcher | USGS Hydro (default), USGS Topo, CartoDB |
| Map overlays | WRIA boundaries, County boundaries, PLSS/TRS grid |
| Data table | Linked water right numbers, click row to zoom map |
| CSV download | Exports current filtered view |
| KMZ download | WRIA folders, two-color icons, Ecology links |
| About tab | Background, query criteria, document hierarchy guide |
| Unmapped tab | Records with live links + CSV download |

---

## Related Apps (waterwater account)

| App | Repo / Script |
|---|---|
| WA Low Flow Water Rights (this app) | `jeffmarti/low-flow-wr` |
| WR Applications | `jeffmarti/wr-applications` |
| Permit Events | `jeffmarti/permit-events` |
| WA Water Rights PLSS Geocoder | `WaterRightReportGeocodeAPP.R` |
| Instream Flow Rules Explorer | `IFexplorer/` |
| SNODAS HUC8 Viewer | `snodas-huc8/` |
| Yakima Basin Dashboard | `yak-stor/` |
| RunoffForecast | `nwrfc-forecasts/` |
| SnowballsChance | `swe-probability/` |

---

*Last updated: November 2026 — moved to automated weekly refresh (GitHub Actions + Posit Connect Cloud).*
