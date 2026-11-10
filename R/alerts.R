# Threshold alerts. Only alerts that are newly active since the last run are
# reported, so the workflow opens one GitHub issue per new condition.

evaluate_alerts <- function(gold, drought, stations, status, today = Sys.Date()) {
  alerts <- list()
  add <- function(key, msg) alerts[[length(alerts) + 1]] <<- data.frame(key = key, message = msg)

  d <- drought[!is.na(drought$score), ]
  if (nrow(d)) {
    latest <- d[nrow(d), ]
    if (latest$category %in% c("D1", "D2", "D3", "D4")) {
      add(paste0("drought_", latest$category),
          sprintf("Local Drought Score is %.1f (%s) as of %s.", latest$score, latest$category, latest$date))
    }
  }

  month <- as.integer(format(today, "%m"))
  swe_keys <- stations$station_key[stations$role == "score_swe"]
  s <- gold[gold$station_key %in% swe_keys & !is.na(gold$pct_of_median), ]
  if (month %in% 2:6 && nrow(s)) {
    s <- s[s$date == max(s$date), ]
    pom <- mean(s$pct_of_median)
    if (pom < 50) add("swe_below_50", sprintf("Mt. Hood snowpack is %.0f%% of median (%s).", pom, max(s$date)))
  }

  for (k in stations$station_key[stations$role == "score_flow"]) {
    f <- gold[gold$station_key == k & !is.na(gold$pct), ]
    if (!nrow(f)) next
    f <- f[nrow(f), ]
    if (f$pct < 10) {
      lab <- stations$label[stations$station_key == k]
      add(paste0("flow_low_", k), sprintf("%s 28-day flow is at the %.0fth percentile (%s).", lab, f$pct, f$date))
    }
  }

  # pipeline health: automated sources that stopped updating
  auto <- stations$station_key[stations$source %in% c("usgs", "nrcs")]
  stale <- status[status$station_key %in% auto &
                  (is.na(status$last_date) | status$last_date < today - 10), ]
  for (k in stale$station_key) {
    add(paste0("stale_", k), sprintf("No data newer than 10 days for %s. Check the station or API.", k))
  }

  if (!length(alerts)) return(data.frame(key = character(), message = character()))
  do.call(rbind, alerts)
}

diff_alerts <- function(active, state_path) {
  prev <- if (file.exists(state_path)) readr::read_csv(state_path, show_col_types = FALSE)$key else character()
  list(new = active[!active$key %in% prev, ], cleared = setdiff(prev, active$key))
}
