# src/scripts/TrackMatchingChristopher.R

library(dplyr)
library(sf)

source("src/R/osm_transport_matching.R")

dir.create("data/processedData", recursive = TRUE, showWarnings = FALSE)

input_path <- "data/processedData/GPS_Track_compress.rds"
matched_path <- "data/processedData/GPS_Track_compress_matched.rds"
transport_share_path <- "data/processedData/christopher_transport_share.rds"

if (!file.exists(input_path)) {
  stop(
    "Missing file: ",
    input_path,
    "\nRun src/scripts/prepareChristophersData.R first."
  )
}

GPS_Track_compress <- readRDS(input_path) |>
  st_transform(2056)

osm_network <- get_osm_transport_network(
  cache_file = "data/processedData/osm_transport_network_switzerland_2056.rds",
  max_cache_age_days = 1
)

GPS_Track_compress_matched <- add_transport_and_road_type(
  points_sf = GPS_Track_compress,
  network_sf = osm_network,
  moving_col = "moving",
  max_match_dist_m = 100
)

if (!"date" %in% names(GPS_Track_compress_matched)) {
  GPS_Track_compress_matched <- GPS_Track_compress_matched |>
    mutate(date = as.Date(time))
}

christopher_transport_share <- summarise_transport_share(
  points_sf = GPS_Track_compress_matched,
  by = "date",
  wide = FALSE
)

saveRDS(
  GPS_Track_compress_matched,
  matched_path
)

saveRDS(
  christopher_transport_share,
  transport_share_path
)