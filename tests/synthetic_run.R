# Offline test: fabricate plausible series (with dry 2015 and 2021) and run
# everything downstream of ingest. Writes to DATA_DIR (default tests/out).
#   Rscript tests/synthetic_run.R

for (f in c("config/settings.R", list.files("R", full.names = TRUE))) source(f)
set.seed(42)
data_dir <- Sys.getenv("DATA_DIR", "tests/out")
dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)
today <- as.Date("2026-11-10")

stations <- read.csv("config/stations.csv", colClasses = "character")
stations$enabled <- as.logical(stations$enabled)
stations <- stations[stations$enabled & stations$site_id != "", ]

dates <- seq(as.Date("1990-10-01"), today, by = "day")
wy <- water_year(dates); doy <- day_of_wy(dates)
dry <- ifelse(wy %in% c(2015, 2021), 0.55, 1) * exp(rnorm(length(unique(wy)), 0, 0.08))[wy - min(wy) + 1]
season <- cos(2 * pi * (doy - 110) / 365)          # wet peak ~ mid-January

fake <- function(key, src) {
  st <- stations[stations$station_key == key, ]
  v <- switch(st$role,
    score_precip = pmax(0, rgamma(length(dates), 0.6, 2) * (1.2 + season) * dry),
    score_swe    = pmax(0, 60 * dry * sin(pi * pmin(pmax(doy - 45, 0), 230) / 230)) + rnorm(length(dates), 0, 1),
    score_flow   = exp(log(800) + 1.2 * season + log(dry) + rnorm(length(dates), 0, 0.25)),
    overlay      = exp(log(20000) + 1.0 * season + 0.8 * log(dry) + rnorm(length(dates), 0, 0.2)),
    usdm         = NA)
  if (st$role == "score_swe") v <- pmax(v, 0)
  data.frame(station_key = key, date = dates, value = v, unit = st$unit, approval = "Approved")
}
silver <- do.call(rbind, lapply(stations$station_key[stations$source %in% c("usgs", "nrcs")], fake))

weeks <- seq(as.Date("2000-01-04"), today, by = "week")
for (k in stations$station_key[stations$source == "usdm"]) {
  dsci <- pmax(0, round(ifelse(water_year(weeks) %in% c(2015, 2021), 180, 20) + rnorm(length(weeks), 0, 25)))
  silver <- rbind(silver, data.frame(station_key = k, date = weeks, value = dsci, unit = "index", approval = "Published"))
}
silver <- silver[silver$date <= today - 2, ]        # simulate lag

status <- data.frame(station_key = stations$station_key, rows_pulled = 100L,
                     last_date = today - 2, error = NA_character_)
status$last_date[status$station_key == "usgs_fanno_durham"] <- today - 30   # force a stale alert

t0 <- Sys.time()
gold <- build_gold(silver, stations)
drought <- build_drought_score(gold$daily, gold$normals, stations)
active <- evaluate_alerts(gold$daily, drought, stations, status, today)
cat("gold built in", round(difftime(Sys.time(), t0, units = "secs"), 1), "s\n")

readr::write_csv(gold$daily, file.path(data_dir, "gold_daily.csv.gz"))
readr::write_csv(gold$normals, file.path(data_dir, "normals.csv.gz"))
readr::write_csv(drought, file.path(data_dir, "drought_score.csv"))
readr::write_csv(status, file.path(data_dir, "status.csv"))

# Checks
late_summer <- function(y) {
  x <- drought[drought$date >= as.Date(paste0(y, "-08-01")) & drought$date <= as.Date(paste0(y, "-09-30")), ]
  round(median(x$score, na.rm = TRUE), 1)
}
cat("median late-summer score: 2015 =", late_summer(2015), " 2021 =", late_summer(2021),
    " 2018 =", late_summer(2018), "\n")
stopifnot(late_summer(2015) < 10, late_summer(2021) < 10, late_summer(2018) > 20)
stopifnot(all(drought$score >= 0 & drought$score <= 100, na.rm = TRUE))
print(tail(drought, 3))
print(active)
stopifnot("stale_usgs_fanno_durham" %in% active$key)
cat("ALL CHECKS PASSED\n")
