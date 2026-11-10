# Local Drought Score: weighted blend of component percentiles, mapped to D0-D4.

component_series <- function(gold, normals, stations, role, swe_floor = 1) {
  keys <- stations$station_key[stations$role == role & stations$enabled]
  g <- gold[gold$station_key %in% keys & !is.na(gold$pct), ]
  if (role == "score_swe") {
    # early/late season percentiles on near-zero snow are noise
    med <- merge(g[, c("station_key", "doy")], normals[, c("station_key", "doy", "p50")],
                 by = c("station_key", "doy"), all.x = TRUE, sort = FALSE)
    g <- g[!is.na(med$p50) & med$p50 >= swe_floor, ]
  }
  if (!nrow(g)) return(data.frame(date = as.Date(character()), pct = numeric()))
  stats::aggregate(pct ~ date, data = g, FUN = mean)
}

build_drought_score <- function(gold, normals, stations) {
  comps <- list(
    precip  = component_series(gold, normals, stations, "score_precip"),
    flow    = component_series(gold, normals, stations, "score_flow"),
    storage = component_series(gold, normals, stations, "score_storage"),
    swe     = component_series(gold, normals, stations, "score_swe")
  )
  dates <- sort(unique(do.call(c, lapply(comps, `[[`, "date"))))
  out <- data.frame(date = dates)
  for (k in names(comps)) {
    out[[k]] <- comps[[k]]$pct[match(dates, comps[[k]]$date)]
  }
  month <- as.integer(format(out$date, "%m"))
  out$swe[!month %in% settings$swe_months] <- NA

  w <- settings$weights[names(comps)]
  vals <- as.matrix(out[, names(comps)])
  wmat <- matrix(w, nrow(vals), length(w), byrow = TRUE) * !is.na(vals)
  out$n_components <- rowSums(!is.na(vals))
  out$score <- rowSums(vals * wmat, na.rm = TRUE) / rowSums(wmat)
  out$score[out$n_components < 2] <- NA
  out$category <- usdm_category(out$score, settings$usdm_breaks)

  # official benchmark: mean county DSCI, carried forward between weekly maps
  usdm_keys <- stations$station_key[stations$source == "usdm"]
  u <- gold[gold$station_key %in% usdm_keys, ]
  if (nrow(u)) {
    u <- stats::aggregate(value ~ date, data = u, FUN = mean)
    idx <- findInterval(out$date, u$date)
    out$usdm_dsci <- ifelse(idx > 0, u$value[pmax(idx, 1)], NA)
  } else out$usdm_dsci <- NA_real_
  out
}
