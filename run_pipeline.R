# Daily entry point: refresh silver, rebuild gold, score drought, evaluate alerts.
#   Rscript run_pipeline.R
# DATA_DIR (default "data-store") holds persistent outputs between runs.

for (f in c("config/settings.R", list.files("R", full.names = TRUE))) source(f)

data_dir <- Sys.getenv("DATA_DIR", "data-store")
dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)
today <- as.Date(Sys.getenv("RUN_DATE", as.character(Sys.Date())))

stations <- read.csv("config/stations.csv", colClasses = "character")
stations$enabled <- as.logical(stations$enabled)
stations <- stations[stations$enabled & stations$site_id != "", ]

# 1. Silver
silver_path <- file.path(data_dir, "silver.csv.gz")
res <- update_silver(stations, read_silver(silver_path), today)
readr::write_csv(res$silver, silver_path)
readr::write_csv(res$status, file.path(data_dir, "status.csv"))
log_msg("silver rows: ", nrow(res$silver))

# 2. Gold
gold <- build_gold(res$silver, stations)
readr::write_csv(gold$daily, file.path(data_dir, "gold_daily.csv.gz"))
readr::write_csv(gold$normals, file.path(data_dir, "normals.csv.gz"))

# 3. Drought score
drought <- build_drought_score(gold$daily, gold$normals, stations)
readr::write_csv(drought, file.path(data_dir, "drought_score.csv"))
last <- tail(drought[!is.na(drought$score), ], 1)
if (nrow(last)) log_msg("drought score ", round(last$score, 1), " (", last$category, ") on ", last$date)

# 4. Alerts: write new ones for the workflow to post as issues
active <- evaluate_alerts(gold$daily, drought, stations, res$status, today)
state_path <- file.path(data_dir, "alerts_state.csv")
d <- diff_alerts(active, state_path)
readr::write_csv(active, state_path)
if (nrow(d$new)) {
  body <- paste0("- ", d$new$message, collapse = "\n")
  writeLines(body, file.path(data_dir, "new_alerts.md"))
  log_msg(nrow(d$new), " new alert(s)")
} else if (file.exists(file.path(data_dir, "new_alerts.md"))) {
  file.remove(file.path(data_dir, "new_alerts.md"))
}

# Fail loudly if every automated source errored (e.g. an API change)
auto <- res$status[res$status$station_key %in% stations$station_key[stations$source %in% c("usgs", "nrcs")], ]
if (nrow(auto) && all(!is.na(auto$error))) stop("All USGS/NRCS pulls failed; see log above.")
