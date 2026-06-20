# Draft

source("src/R/fixed_areas.R")

gps <- readRDS("data/processedData/GPS_Track_compress_matched.rds")

fixed_areas <- load_fixed_areas(
  points_csv = "data/metaData/listOfStadyPoints.csv",
  zhaw_gpkg_files = c(
    "data/metaData/zhaw_gruental.gpkg",
    "data/metaData/ZHAW_Reidbach_A.gpkg"
  ),
  zhaw_buffer_m = 50
)

fixed_areas
sf::st_area(fixed_areas)

gps_tagged <- gps |>
  tag_points_with_fixed_areas(fixed_areas) |>
  smooth_fixed_area_gaps(
    max_gap_points = 2,
    max_gap_minutes = 5
  )

gps_tagged |>
  sf::st_drop_geometry() |>
  dplyr::count(area_type, area_id, sort = TRUE)
