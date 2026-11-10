# One-time check that every station ID in config/stations.csv resolves, and a
# search for Bull Run reservoir storage gages. Run locally or via workflow_dispatch:
#   Rscript scripts/validate_stations.R

stations <- read.csv("config/stations.csv", colClasses = "character")

cat("\n== USGS sites ==\n")
usgs <- stations[stations$source == "usgs" & stations$site_id != "", ]
for (i in seq_len(nrow(usgs))) {
  ml <- tryCatch(dataRetrieval::read_waterdata_monitoring_location(
    monitoring_location_id = usgs$site_id[i], skipGeometry = TRUE), error = function(e) NULL)
  ts <- tryCatch(dataRetrieval::read_waterdata_ts_meta(
    monitoring_location_id = usgs$site_id[i], parameter_code = usgs$parameter[i],
    statistic_id = "00003", skipGeometry = TRUE), error = function(e) NULL)
  cat(sprintf("%-28s %-15s %-45s daily %s: %s\n", usgs$station_key[i], usgs$site_id[i],
              if (is.null(ml) || !nrow(ml)) "NOT FOUND" else ml$monitoring_location_name[1],
              usgs$parameter[i],
              if (is.null(ts) || !nrow(ts)) "none" else paste(ts$begin[1], "to", ts$end[1])))
}

cat("\n== Bull Run candidates (reservoir storage 00054, elevation 62614/00062) ==\n")
cand <- tryCatch(dataRetrieval::read_waterdata_monitoring_location(
  state_name = "Oregon", q = "Bull Run", skipGeometry = TRUE), error = function(e) NULL)
if (!is.null(cand) && nrow(cand)) {
  for (id in cand$monitoring_location_id) {
    ts <- tryCatch(dataRetrieval::read_waterdata_ts_meta(monitoring_location_id = id,
      skipGeometry = TRUE), error = function(e) NULL)
    if (is.null(ts) || !any(ts$parameter_code %in% c("00054", "62614", "00062"))) next
    hit <- ts[ts$parameter_code %in% c("00054", "62614", "00062"), ]
    cat(sprintf("%-15s %-45s params: %s\n", id,
                cand$monitoring_location_name[cand$monitoring_location_id == id][1],
                paste(unique(hit$parameter_code), collapse = ",")))
  }
}

cat("\n== NRCS SNOTEL triplets ==\n")
nrcs <- unique(stations$site_id[stations$source == "nrcs"])
resp <- httr2::request("https://wcc.sc.egov.usda.gov/awdbRestApi/services/v1/stations") |>
  httr2::req_url_query(stationTriplets = paste(nrcs, collapse = ",")) |>
  httr2::req_perform() |> httr2::resp_body_json()
for (s in resp) cat(sprintf("%-14s %s (elev %s ft)\n", s$stationTriplet, s$name, s$elevation))
missing <- setdiff(nrcs, vapply(resp, function(s) s$stationTriplet, ""))
if (length(missing)) cat("NOT FOUND:", paste(missing, collapse = ", "), "\n")
