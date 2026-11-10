# Project-wide settings. Edit here rather than in the pipeline code.

settings <- list(
  backfill_start   = as.Date("1990-10-01"),  # first day pulled for a new station
  repull_days      = 120,                    # re-pull window for provisional data
  ref_years        = 1991:2020,              # reference period for normals/percentiles
  doy_window       = 7,                      # +/- days pooled when computing percentiles
  min_ref_years    = 10,                     # fewer years than this -> no percentile
  flow_roll_days   = 28,
  spi_window_days  = 182,                    # ~6-month SPI-style window

  # Local Drought Score weights; snowpack drops out Jul-Oct and its weight is redistributed
  weights = c(precip = 0.30, flow = 0.30, storage = 0.25, swe = 0.15),
  swe_months = c(11, 12, 1, 2, 3, 4, 5, 6),

  # USDM percentile cutoffs (upper bound of each class)
  usdm_breaks = c(D4 = 2, D3 = 5, D2 = 10, D1 = 20, D0 = 30),

  stress_years = c(2015, 2021)
)
