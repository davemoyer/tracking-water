# One fetch function per source. Each returns the silver schema:
# station_key, date, value, unit, approval

empty_silver <- function() {
  data.frame(station_key = character(), date = as.Date(character()),
             value = numeric(), unit = character(), approval = character())
}

# ---- USGS Water Data APIs (via dataRetrieval >= 2.7.18) --------------------

fetch_usgs <- function(st, start, end) {
  ch <- date_chunks(start, end)
  pieces <- lapply(seq_len(nrow(ch)), function(i) {
    with_retry(function() {
      dataRetrieval::read_waterdata_daily(
        monitoring_location_id = st$site_id,
        parameter_code = st$parameter,
        statistic_id = "00003",                     # daily mean
        time = c(format(ch$start[i]), format(ch$end[i])),
        skipGeometry = TRUE
      )
    }, what = paste("USGS", st$site_id))
  })
  df <- do.call(rbind, lapply(pieces, as.data.frame))
  if (is.null(df) || nrow(df) == 0) return(empty_silver())
  data.frame(
    station_key = st$station_key,
    date = as.Date(df$time),
    value = as.numeric(df$value),
    unit = if ("unit_of_measure" %in% names(df)) df$unit_of_measure else st$unit,
    approval = if ("approval_status" %in% names(df)) df$approval_status else NA_character_
  )
}

# ---- NRCS AWDB REST API (SNOTEL) ------------------------------------------

awdb_base <- "https://wcc.sc.egov.usda.gov/awdbRestApi/services/v1"

awdb_get <- function(triplet, element, start, end) {
  resp <- with_retry(function() {
    httr2::request(paste0(awdb_base, "/data")) |>
      httr2::req_url_query(
        stationTriplets = triplet, elements = element, duration = "DAILY",
        beginDate = format(start), endDate = format(end), periodRef = "END"
      ) |>
      httr2::req_timeout(120) |>
      httr2::req_perform()
  }, what = paste("AWDB", triplet, element))
  body <- httr2::resp_body_json(resp, simplifyVector = FALSE)
  rows <- list()
  for (stn in body) for (el in stn$data) {
    vals <- el$values
    if (length(vals) == 0) next
    rows[[length(rows) + 1]] <- data.frame(
      date = as.Date(substr(vapply(vals, function(v) v$date, ""), 1, 10)),
      value = vapply(vals, function(v) if (is.null(v$value)) NA_real_ else as.numeric(v$value), 0),
      approval = vapply(vals, function(v) if (is.null(v$qcFlag)) NA_character_ else v$qcFlag, "")
    )
  }
  if (length(rows) == 0) return(NULL)
  do.call(rbind, rows)
}

fetch_nrcs <- function(st, start, end) {
  ch <- date_chunks(start, end)
  get_all <- function(el) {
    out <- lapply(seq_len(nrow(ch)), function(i) awdb_get(st$site_id, el, ch$start[i], ch$end[i]))
    do.call(rbind, out)
  }
  df <- get_all(st$parameter)

  # Some sites lack PRCP; derive daily increments from accumulated PREC instead
  if (st$parameter == "PRCP" && (is.null(df) || all(is.na(df$value)))) {
    log_msg("  ", st$site_id, ": no PRCP, deriving from PREC")
    acc <- get_all("PREC")
    if (is.null(acc)) return(empty_silver())
    acc <- acc[order(acc$date), ]
    acc$wy <- water_year(acc$date)
    acc$value <- ave(acc$value, acc$wy, FUN = function(v) c(v[1], diff(v)))
    acc$value[acc$value < 0] <- NA
    df <- acc[, c("date", "value", "approval")]
  }
  if (is.null(df)) return(empty_silver())
  data.frame(station_key = st$station_key, date = df$date, value = df$value,
             unit = st$unit, approval = df$approval)
}

# ---- US Drought Monitor (weekly DSCI by county) ----------------------------

fetch_usdm <- function(st, start, end) {
  start <- max(start, as.Date("2000-01-04"))
  url <- paste0(
    "https://usdmdataservices.unl.edu/api/CountyStatistics/GetDSCI",
    "?aoi=", st$site_id,
    "&startdate=", format(start, "%m/%d/%Y"),
    "&enddate=", format(end, "%m/%d/%Y"),
    "&statisticsType=1"
  )
  resp <- with_retry(function() {
    httr2::request(url) |> httr2::req_headers(Accept = "text/csv") |>
      httr2::req_timeout(120) |> httr2::req_perform()
  }, what = paste("USDM", st$site_id))
  df <- readr::read_csv(httr2::resp_body_string(resp), show_col_types = FALSE)
  if (nrow(df) == 0) return(empty_silver())

  # Column names are not documented; find them defensively
  nm <- names(df)
  date_col <- nm[grepl("mapdate|validstart|date", nm, ignore.case = TRUE)][1]
  val_col  <- nm[grepl("^dsci$", nm, ignore.case = TRUE)][1]
  if (is.na(date_col) || is.na(val_col)) stop("Unexpected USDM columns: ", paste(nm, collapse = ", "))
  raw_date <- as.character(df[[date_col]])
  d <- suppressWarnings(as.Date(raw_date, "%Y%m%d"))
  if (all(is.na(d))) d <- as.Date(lubridate::parse_date_time(raw_date, c("Ymd", "mdY", "Ymd HMS")))

  data.frame(station_key = st$station_key, date = d, value = as.numeric(df[[val_col]]),
             unit = "index", approval = "Published")
}

# ---- Manual entry (Portland Water Bureau) ----------------------------------

fetch_manual <- function(st, start, end) {
  path <- file.path("data", "manual", st$site_id)
  if (!file.exists(path)) return(empty_silver())
  df <- readr::read_csv(path, show_col_types = FALSE, col_types = readr::cols(.default = "c"))
  if (nrow(df) == 0) return(empty_silver())
  data.frame(station_key = st$station_key, date = as.Date(df$date),
             value = as.numeric(df[[st$parameter]]), unit = st$unit, approval = "Manual")
}

fetchers <- list(usgs = fetch_usgs, nrcs = fetch_nrcs, usdm = fetch_usdm, manual = fetch_manual)
