log_msg <- function(...) {
  message(format(Sys.time(), "%H:%M:%S"), "  ", paste0(...))
}

# Water year starts Oct 1; WY2027 = Oct 1 2026 - Sep 30 2027
water_year <- function(date) {
  y <- as.integer(format(date, "%Y"))
  m <- as.integer(format(date, "%m"))
  ifelse(m >= 10, y + 1L, y)
}

# Day of water year, 1..365. Feb 29 shares Feb 28's slot so each calendar
# date maps to the same index every year.
day_of_wy <- function(date) {
  md <- format(date, "%m-%d")
  md[md == "02-29"] <- "02-28"
  ref <- as.Date(paste0(ifelse(substr(md, 1, 2) >= "10", "2001-", "2002-"), md))
  as.integer(ref - as.Date("2001-10-01")) + 1L
}

# Retry wrapper for flaky public APIs
with_retry <- function(expr_fn, tries = 3, wait = 10, what = "request") {
  for (i in seq_len(tries)) {
    out <- tryCatch(expr_fn(), error = function(e) e)
    if (!inherits(out, "error")) return(out)
    log_msg("  ", what, " failed (attempt ", i, "/", tries, "): ", conditionMessage(out))
    if (i < tries) Sys.sleep(wait * i)
  }
  stop(out)
}

# Split a date range into chunks of n years to keep API responses small
date_chunks <- function(start, end, years = 10) {
  starts <- seq(start, end, by = paste(years, "years"))
  ends <- c(starts[-1] - 1, end)
  data.frame(start = starts, end = ends)
}

# Empirical percentile (0-100) of x within reference vector, mid-rank for ties
pct_rank <- function(x, ref) {
  ref <- ref[!is.na(ref)]
  if (length(ref) == 0 || is.na(x)) return(NA_real_)
  100 * (mean(ref < x) + 0.5 * mean(ref == x))
}

usdm_category <- function(pct, breaks) {
  out <- rep("None", length(pct))
  # assign from least to most severe so the most severe wins
  for (k in rev(names(breaks))) out[!is.na(pct) & pct <= breaks[[k]]] <- k
  out[is.na(pct)] <- NA_character_
  out
}
