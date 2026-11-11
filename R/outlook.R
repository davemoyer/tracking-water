# Recent rain and snow vs. normal, and how they moved the Local Drought Score.

# Reference values of x for a target day of water year, pooled +/- `pool` days
ref_values <- function(date, x, target_doy, pool = settings$doy_window) {
  doy <- day_of_wy(date)
  dist <- abs(doy - target_doy)
  dist <- pmin(dist, 365 - dist)
  x[water_year(date) %in% settings$ref_years & !is.na(x) & dist <= pool]
}

# Water-year-to-date running total; NA once more than 10% of days are missing
wy_cumsum <- function(date, x) {
  wy <- water_year(date)
  tot <- ave(ifelse(is.na(x), 0, x), wy, FUN = cumsum)
  ok <- ave(as.numeric(!is.na(x)), wy, FUN = function(v) cumsum(v) / seq_along(v))
  tot[ok < 0.9] <- NA
  tot
}

window_row <- function(key, kind, window, value, ref) {
  ref <- ref[!is.na(ref)]
  med <- if (length(ref) >= 5) stats::median(ref) else NA_real_
  data.frame(station_key = key, kind = kind, window = window, value = value,
             median = med,
             pct_of_median = ifelse(!is.na(med) & med > 0.1, 100 * value / med, NA_real_),
             pct = if (length(ref) >= 5) pct_rank(value, ref) else NA_real_,
             diff_from_median = value - med)
}

# Per-station precip totals (7, 30 days, water year to date) and SWE level and
# change, each ranked against the same day of year in the reference period.
recent_conditions <- function(gold, stations) {
  rows <- list()
  win <- settings$recent_windows

  for (k in stations$station_key[stations$role == "score_precip"]) {
    g <- gold[gold$station_key == k, ]
    g <- g[order(g$date), ]
    if (nrow(g) < 60) next
    i <- max(which(!is.na(g$value)))
    d <- day_of_wy(g$date[i])
    for (n in win) {
      tot <- roll_sum(g$value, n)
      rows[[length(rows) + 1]] <- window_row(k, "precip", paste0(n, "d"), tot[i], ref_values(g$date, tot, d))
    }
    wytd <- wy_cumsum(g$date, g$value)
    rows[[length(rows) + 1]] <- window_row(k, "precip", "wytd", wytd[i], ref_values(g$date, wytd, d, pool = 0))
  }

  for (k in stations$station_key[stations$role == "score_swe"]) {
    g <- gold[gold$station_key == k, ]
    g <- g[order(g$date), ]
    if (nrow(g) < 60) next
    i <- max(which(!is.na(g$value)))
    d <- day_of_wy(g$date[i])
    rows[[length(rows) + 1]] <- window_row(k, "swe", "now", g$value[i], ref_values(g$date, g$value, d))
    for (n in win) {
      chg <- g$value - c(rep(NA, n), utils::head(g$value, -n))
      rows[[length(rows) + 1]] <- window_row(k, "swe", paste0(n, "d_change"), chg[i], ref_values(g$date, chg, d))
    }
  }

  if (!length(rows)) return(NULL)
  out <- do.call(rbind, rows)
  out$as_of <- vapply(out$station_key, function(k) {
    as.character(max(gold$date[gold$station_key == k & !is.na(gold$value)]))
  }, "")
  rownames(out) <- NULL
  out
}

# p10/p50/p90 of the water-year-to-date precip total by day of water year,
# averaged across precip gauges, with this year's running total.
wytd_band <- function(gold, stations) {
  keys <- stations$station_key[stations$role == "score_precip"]
  per <- lapply(keys, function(k) {
    g <- gold[gold$station_key == k, ]
    if (nrow(g) < 60) return(NULL)
    g <- g[order(g$date), ]
    data.frame(date = g$date, wytd = wy_cumsum(g$date, g$value))
  })
  per <- do.call(rbind, per)
  if (is.null(per)) return(NULL)
  avg <- stats::aggregate(wytd ~ date, data = per, FUN = mean)
  avg$wy <- water_year(avg$date)
  avg$doy <- day_of_wy(avg$date)
  ref <- avg[avg$wy %in% settings$ref_years, ]
  q <- do.call(rbind, tapply(ref$wytd, ref$doy, function(v)
    stats::quantile(v, c(0.1, 0.5, 0.9), names = FALSE)))
  band <- data.frame(doy = as.integer(rownames(q)), p10 = q[, 1], p50 = q[, 2], p90 = q[, 3])
  cur <- avg[avg$wy == max(avg$wy), c("doy", "wytd")]
  merge(band, cur, by = "doy", all.x = TRUE)
}

# Points each component contributes to the score on a date (effective weight x
# percentile), so the change between two dates sums to the change in score.
score_contributions <- function(drought, date) {
  comps <- names(settings$weights)
  d <- drought[!is.na(drought$score) & drought$date <= date, ]
  if (!nrow(d)) return(NULL)
  r <- d[nrow(d), ]
  p <- unlist(r[comps])
  w <- settings$weights[comps] * !is.na(p)
  w <- w / sum(w)
  data.frame(component = comps, date = r$date, weight = unname(w), pct = unname(p),
             points = unname(ifelse(is.na(p), 0, w * p)))
}

score_attribution <- function(drought, lags = settings$recent_windows) {
  d <- drought[!is.na(drought$score), ]
  if (!nrow(d)) return(NULL)
  now <- score_contributions(d, max(d$date))
  out <- data.frame(component = now$component, weight_now = now$weight, pct_now = now$pct,
                    points_now = now$points)
  for (n in lags) {
    past <- score_contributions(d, max(d$date) - n)
    if (is.null(past)) next
    out[[paste0("pct_", n, "d")]] <- past$pct
    out[[paste0("points_change_", n, "d")]] <- now$points - past$points
  }
  out
}

pct_word <- function(p) {
  ifelse(is.na(p), "unranked",
  ifelse(p < 10, "much below normal",
  ifelse(p < 30, "below normal",
  ifelse(p <= 70, "near normal",
  ifelse(p <= 90, "above normal", "much above normal")))))
}

# Plain-language bullets for the dashboard
describe_recent <- function(recent, attribution, drought) {
  out <- character()
  avg <- function(kind, window, col) {
    x <- recent[[col]][recent$kind == kind & recent$window == window]
    if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
  }
  nwin <- max(settings$recent_windows)
  w <- paste0(nwin, "d")

  p_tot <- avg("precip", w, "value"); p_pom <- avg("precip", w, "pct_of_median")
  p_pct <- avg("precip", w, "pct")
  w7 <- paste0(min(settings$recent_windows), "d")
  p7 <- avg("precip", w7, "value"); p7_pom <- avg("precip", w7, "pct_of_median")
  y_tot <- avg("precip", "wytd", "value"); y_med <- avg("precip", "wytd", "median")
  y_pct <- avg("precip", "wytd", "pct")
  if (!is.na(p_tot)) {
    out <- c(out, sprintf(
      "**Rain:** the Mt. Hood SNOTEL gauges averaged %.1f in over the last %d days, %s of normal for this time of year (%s, %s percentile). The last %d days brought %.1f in (%s of normal).",
      p_tot, nwin, fmt_pct(p_pom), pct_word(p_pct), ordinal(p_pct),
      min(settings$recent_windows), p7, fmt_pct(p7_pom)))
  }
  if (!is.na(y_tot) && !is.na(y_med)) {
    gap <- y_tot - y_med
    out <- c(out, sprintf(
      "**Water year to date:** %.1f in vs. a median of %.1f in (%s, %s percentile). %s",
      y_tot, y_med, pct_word(y_pct), ordinal(y_pct),
      if (gap < -0.5) sprintf("That is a %.1f in deficit that later storms have to make up.", -gap)
      else if (gap > 0.5) sprintf("That is a %.1f in cushion over a typical year.", gap)
      else "That is right on a typical pace."))
  }

  s_now <- avg("swe", "now", "value"); s_med <- avg("swe", "now", "median")
  s_pct <- avg("swe", "now", "pct")
  s_chg <- avg("swe", paste0(nwin, "d_change"), "value")
  s_chg_med <- avg("swe", paste0(nwin, "d_change"), "median")
  if (!is.na(s_now)) {
    if (is.na(s_med) || s_med < 1) {
      out <- c(out, sprintf(
        "**Snow:** %.1f in of snow water equivalent on average. The snow season has barely started (median under 1 in), so snowpack does not yet count toward the score.",
        s_now))
    } else {
      out <- c(out, sprintf(
        "**Snow:** %.1f in of snow water equivalent, %s of the %.1f in median (%s, %s percentile). Over the last %d days snowpack changed by %+.1f in vs. a typical %+.1f in.",
        s_now, fmt_pct(100 * s_now / s_med), s_med, pct_word(s_pct), ordinal(s_pct),
        nwin, s_chg, s_chg_med))
    }
  }

  chg_col <- paste0("points_change_", nwin, "d")
  if (!is.null(attribution) && chg_col %in% names(attribution)) {
    d <- drought[!is.na(drought$score), ]
    s_now <- d$score[nrow(d)]
    delta <- sum(attribution[[chg_col]])
    parts <- attribution[abs(attribution[[chg_col]]) >= 0.05, ]
    parts <- parts[order(-abs(parts[[chg_col]])), ]
    lbl <- c(precip = "precipitation", flow = "streamflow", storage = "Bull Run storage", swe = "snowpack")
    by <- if (nrow(parts)) paste0(" (", paste(sprintf("%s %+.1f", lbl[parts$component], parts[[chg_col]]),
                                              collapse = ", "), ")") else ""
    direction <- if (delta > 1) "eased" else if (delta < -1) "worsened" else "held steady"
    out <- c(out, sprintf(
      "**Effect on the outlook:** drought conditions %s over the last %d days. The Local Drought Score went from %.1f to %.1f%s.",
      direction, nwin, s_now - delta, s_now, by))
  }
  out
}

fmt_pct <- function(x) ifelse(is.na(x), "n/a", paste0(round(x), "%"))

ordinal <- function(x) {
  if (is.na(x)) return("n/a")
  n <- round(x)
  suf <- if (n %% 100 %in% 11:13) "th" else c("th", "st", "nd", "rd", rep("th", 6))[n %% 10 + 1]
  paste0(n, suf)
}
