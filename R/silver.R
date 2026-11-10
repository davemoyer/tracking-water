# Incrementally refresh the long-format silver table.
# New stations are backfilled; existing ones re-pull the last `repull_days`
# so provisional values that USGS/NRCS revise are picked up.

read_silver <- function(path) {
  if (!file.exists(path)) return(empty_silver())
  readr::read_csv(path, show_col_types = FALSE,
                  col_types = readr::cols(station_key = "c", date = "D", value = "d",
                                          unit = "c", approval = "c"))
}

update_silver <- function(stations, silver, today = Sys.Date()) {
  status <- list()
  for (i in seq_len(nrow(stations))) {
    st <- stations[i, ]
    have <- silver$date[silver$station_key == st$station_key]
    start <- if (st$source == "manual" || length(have) == 0) settings$backfill_start
             else max(have) - settings$repull_days
    log_msg(st$station_key, ": pulling from ", start)

    new <- tryCatch(fetchers[[st$source]](st, start, today), error = function(e) e)
    if (inherits(new, "error")) {
      log_msg("  ERROR ", conditionMessage(new), " (keeping existing rows)")
      status[[i]] <- data.frame(station_key = st$station_key, rows_pulled = NA_integer_,
                                last_date = if (length(have)) max(have) else as.Date(NA),
                                error = conditionMessage(new))
      next
    }
    new <- new[!is.na(new$date), ]
    keep <- !(silver$station_key == st$station_key & silver$date >= start)
    silver <- rbind(silver[keep, ], new)
    all_dates <- silver$date[silver$station_key == st$station_key]
    status[[i]] <- data.frame(station_key = st$station_key, rows_pulled = nrow(new),
                              last_date = if (length(all_dates)) max(all_dates) else as.Date(NA),
                              error = NA_character_)
  }
  silver <- silver[order(silver$station_key, silver$date), ]
  list(silver = silver, status = do.call(rbind, status))
}
