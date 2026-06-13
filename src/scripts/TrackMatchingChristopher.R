source("src/R/osm_transport_matching.R")

GPS_Track_compress <- readRDS("data/processedData/GPS_Track_compress.rds")

osm_network <- get_osm_transport_network(
  cache_file = "data/processedData/osm_transport_network_switzerland_2056.rds",
  max_cache_age_days = 1
)

GPS_Track_compress <- add_transport_and_road_type(
  points_sf = GPS_Track_compress,
  network_sf = osm_network,
  moving_col = "moving",
  max_match_dist_m = 100
)

christopher_transport_share <- summarise_transport_share(
  points_sf = GPS_Track_compress,
  by = "date",
  wide = FALSE
)


# saveRDS(GPS_Track, "data/processedData/GPS_Track_processed.rds")