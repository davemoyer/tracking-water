# Tracking Water: Portland

A personal pipeline that runs every morning on GitHub Actions. It pulls rivers, snowpack, precipitation and drought data for the Portland region, compares each to 1991-2020 normals, computes a **Local Drought Score**, renders a dashboard, and opens a GitHub issue when something crosses a threshold.

## What runs daily

1. **Ingest** (`R/ingest.R`): USGS Water Data APIs (via `dataRetrieval::read_waterdata_daily`), NRCS AWDB REST (SNOTEL), US Drought Monitor county DSCI, and an optional hand-entered Portland Water Bureau file. New stations backfill from Oct 1990. Existing ones re-pull the last 120 days to pick up revised provisional values.
2. **Silver** (`R/silver.R`): one long table, `station_key, date, value, unit, approval`.
3. **Gold** (`R/indicators.R`): day-of-water-year percentiles and normals, 28-day flow means, 182-day SPI-style precipitation percentiles.
4. **Drought score** (`R/drought.R`): weighted blend of precip (30%), flow (30%), Bull Run storage (25%) and snowpack (15%, Nov to Jun only), mapped to USDM D0 to D4 cutoffs. Missing components drop out and their weight is redistributed. At least 2 components are required.
5. **Alerts** (`R/alerts.R`): drought category D1 or worse, Mt. Hood SWE under 50% of median (Feb to Jun), a scored stream under its 10th percentile, or any station gone stale for 10 days. Only *new* alerts open an issue.

Data persists on a `data` branch that is force-pushed each run as a single commit, so the repo does not grow.

## Setup (about 15 minutes)

1. Create a GitHub repo and push this folder to `main`. Public is simplest; see the Pages note below for private repos.
2. Optional but recommended: get a free USGS API key at https://api.waterdata.usgs.gov/signup/ and add it as the repo secret `API_USGS_PAT` (Settings > Secrets and variables > Actions).
3. Actions tab > **Validate stations** > Run workflow. Read the log and:
   - fix any `NOT FOUND` IDs in `config/stations.csv`
   - copy Bull Run reservoir site IDs from the "Bull Run candidates" list into the `usgs_bullrun_res1/res2` rows and set `enabled` to `TRUE` (use 00054 if storage is listed, otherwise the elevation parameter)
4. Dashboard hosting:
   - **Public repo, or private on GitHub Pro:** Settings > Pages > Source = *GitHub Actions*, then add the repo variable `ENABLE_PAGES` = `true`.
   - **Private repo on a free plan:** leave `ENABLE_PAGES` unset; each run attaches the dashboard HTML as a downloadable artifact.
5. Actions tab > **Daily water update** > Run workflow. The first run backfills ~35 years and takes longer; later runs are quick.
6. Make sure you are *watching* the repo (at least Issues) so alert issues reach your email or the GitHub app.

## Maintenance

- **Manual Bull Run storage:** add rows to `data/manual/pwb_bull_run_storage.csv` (date, billions of gallons) from the Water Bureau's seasonal supply page whenever you like. It is shown on the dashboard; it only feeds the drought score once the USGS reservoir gages are enabled.
- **Tuning:** weights, reference period and thresholds live in `config/settings.R`; stations in `config/stations.csv`.
- **Failures:** if every USGS/NRCS pull fails the run fails, and GitHub emails you. Individual station errors are listed under *Data status* on the dashboard.
- **Inactivity:** GitHub may pause scheduled workflows in a repo with no activity for 60 days. If that happens, re-enable it from the Actions tab.

## Running locally

```r
# from the repo root
Rscript run_pipeline.R          # writes to ./data-store
quarto render dashboard/index.qmd
Rscript tests/synthetic_run.R   # offline test with fabricated data
```

Requires R >= 4.1 and the packages in `DESCRIPTION`.
