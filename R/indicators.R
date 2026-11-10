# Gold layer: per-station daily indicators and day-of-water-year normals.

# Stack reference values into a +/- window around each day of water year
ref_by_doy <- function(doy, value, wy, window) {
  offs <- -window:window
  d <- rep(doy, times = length(offs)) + rep(offs, each = length(doy))
  d <- ((d - 1) %% 365) + 1
  list(val = split(rep(value, length(offs)), factor(d, levels = 1:365)),
       yrs = vapply(split(rep(wy, length(offs)), factor(d, levels = 1:365)),
                    function(x) length(unique(x)), 0L))
}

# Empirical percentile of each value against its day-of-year reference pool
doy_percentile <- function(date, value) {
  doy <- day_of_wy(date)
  wy <- water_year(date)
  is_ref <- wy %in% settings$ref_years & !is.na(value)
  ref <- ref_by_doy(doy[is_ref], value[is_ref], wy[is_ref], settings$doy_window)
  out <- rep(NA_real_, length(value))
  for (d in 1:365) {
    idx <- which(doy == d & !is.na(value))
    r <- sort(ref$val[[d]])
    if (!length(idx) || ref$yrs[[d]] < settings$min_ref_years) next
    lo <- findInterval(value[idx], r, left.open = TRUE)
    hi <- findInterval(value[idx], r)
    out[idx] <- 100 * (lo + (hi - lo) / 2) / length(r)
  }
  out
}

doy_normals <- function(date, value) {
  doy <- day_of_wy(date)
  wy <- water_year(date)
  is_ref <- wy %in% settings$ref_years & !is.na(value)
  ref <- ref_by_doy(doy[is_ref], value[is_ref], wy[is_ref], settings$doy_window)
  q <- t(vapply(ref$val, function(r) {
    if (length(r) < 5) return(c(NA, NA, NA))
    stats::quantile(r, c(0.1, 0.5, 0.9), names = FALSE, na.rm = TRUE)
  }, numeric(3)))
  data.frame(doy = 1:365, p10 = q[, 1], p50 = q[, 2], p90 = q[, 3], n_years = ref$yrs)
}

# SPI-style percentile: trailing-window precip sums fit to a gamma distribution
# per day of year (Thom's estimator), with a point mass at zero.
spi_percentile <- function(date, sums) {
  doy <- day_of_wy(date)
  wy <- water_year(date)
  is_ref <- wy %in% settings$ref_years & !is.na(sums)
  ref <- ref_by_doy(doy[is_ref], sums[is_ref], wy[is_ref], settings$doy_window)
  out <- rep(NA_real_, length(sums))
  for (d in 1:365) {
    idx <- which(doy == d & !is.na(sums))
    r <- ref$val[[d]]
    if (!length(idx) || ref$yrs[[d]] < settings$min_ref_years) next
    q0 <- mean(r <= 0)
    rp <- r[r > 0]
    A <- log(mean(rp)) - mean(log(rp))
    shape <- (1 + sqrt(1 + 4 * A / 3)) / (4 * A)
    scale <- mean(rp) / shape
    out[idx] <- 100 * (q0 + (1 - q0) * stats::pgamma(sums[idx], shape = shape, scale = scale))
  }
  out
}

# Complete each station's daily series so rolling windows are calendar-true
complete_daily <- function(df) {
  full <- data.frame(date = seq(min(df$date), max(df$date), by = "day"))
  merge(full, df, by = "date", all.x = TRUE)
}

roll_sum <- function(x, n, min_frac = 0.9) {
  ok <- !is.na(x)
  s <- stats::filter(ifelse(ok, x, 0), rep(1, n), sides = 1)
  cnt <- stats::filter(as.numeric(ok), rep(1, n), sides = 1)
  out <- as.numeric(s) * n / as.numeric(cnt)      # scale up for small gaps
  out[as.numeric(cnt) < n * min_frac] <- NA
  out
}

build_gold <- function(silver, stations) {
  silver <- stats::aggregate(value ~ station_key + date, data = silver, FUN = mean)
  daily <- list(); normals <- list()

  for (i in seq_len(nrow(stations))) {
    st <- stations[i, ]
    df <- silver[silver$station_key == st$station_key, c("date", "value")]
    if (!nrow(df)) next
    if (st$source == "usdm" || st$source == "manual") {
      daily[[st$station_key]] <- data.frame(station_key = st$station_key, date = df$date,
        value = df$value, metric = df$value, pct = NA_real_, pct_of_median = NA_real_)
      next
    }
    if (nrow(df) < 30) next
    df <- complete_daily(df)

    metric <- switch(st$role,
      score_flow   = roll_sum(df$value, settings$flow_roll_days) / settings$flow_roll_days,
      score_precip = roll_sum(df$value, settings$spi_window_days),
      df$value)
    pct <- if (st$role == "score_precip") spi_percentile(df$date, metric)
           else doy_percentile(df$date, metric)

    nm <- doy_normals(df$date, metric)
    med <- nm$p50[day_of_wy(df$date)]
    pom <- ifelse(!is.na(med) & med > 0.5, 100 * metric / med, NA_real_)

    daily[[st$station_key]] <- data.frame(station_key = st$station_key, date = df$date,
      value = df$value, metric = metric, pct = pct, pct_of_median = pom)
    normals[[st$station_key]] <- cbind(station_key = st$station_key, nm)
  }

  gold <- do.call(rbind, daily)
  gold$wy <- water_year(gold$date)
  gold$doy <- day_of_wy(gold$date)
  rownames(gold) <- NULL
  list(daily = gold, normals = do.call(rbind, normals))
}
