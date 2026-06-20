# Draft

# source("src/R/fixed_areas.R")
# 
# gps <- readRDS("data/processedData/GPS_Track_compress_matched.rds")
# 
# fixed_areas <- load_fixed_areas(
#   points_csv = "data/metaData/listOfStadyPoints.csv",
#   zhaw_gpkg_files = c(
#     "data/metaData/zhaw_gruental.gpkg",
#     "data/metaData/ZHAW_Reidbach_A.gpkg"
#   ),
#   zhaw_buffer_m = 50
# )
# 
# fixed_areas
# sf::st_area(fixed_areas)
# 
# gps_tagged <- gps |>
#   tag_points_with_fixed_areas(fixed_areas) |>
#   smooth_fixed_area_gaps(
#     max_gap_points = 2,
#     max_gap_minutes = 5
#   )
# 
# gps_tagged |>
#   sf::st_drop_geometry() |>
#   dplyr::count(area_type, area_id, sort = TRUE)

# 
# christopher <- readRDS("data/processedData/christopher_plot_inputs.rds")
# 
# christopher$analysis
# 
# christopher$analysis_day
# 
# christopher$pie_data
# 
# christopher$summary_data_day
# 
# gps_tagged <- readRDS("data/processedData/GPS_Track_christopher_tagged.rds")
# 
# gps_tagged |>
#   sf::st_drop_geometry() |>
#   dplyr::count(area_type, area_id, sort = TRUE)
# 
# gps_tagged |>
#   sf::st_drop_geometry() |>
#   dplyr::summarise(
#     n_points = dplyr::n(),
#     n_valid_time_steps = sum(valid_time_step, na.rm = TRUE),
#     n_valid_movement_steps = sum(valid_movement_step, na.rm = TRUE),
#     max_speed_kmh = max(speed_kmh, na.rm = TRUE),
#     max_valid_speed_kmh = max(speed_kmh[valid_movement_step], na.rm = TRUE),
#     total_distance_km = sum(
#       dplyr::if_else(valid_movement_step, step_dist_m / 1000, 0),
#       na.rm = TRUE
#     ),
#     total_time_out_home_h = sum(
#       dplyr::if_else(valid_time_step & out_home, dt_min / 60, 0),
#       na.rm = TRUE
#     )
#   )
# 
# sum(christopher$pie_data$share, na.rm = TRUE)

library(sf)
library(dplyr)
library(lubridate)

gpx <- st_read(
  "data/rawData/Trackingdata_Christopher/20260312-172538.gpx",
  layer = "track_points",
  quiet = TRUE
)

gpx_summary <- gpx |>
  mutate(
    time_utc = ymd_hms(time, tz = "UTC"),
    time_ch = with_tz(time_utc, "Europe/Zurich")
  ) |>
  summarise(
    start_date = min(as.Date(time_ch), na.rm = TRUE),
    end_date = max(as.Date(time_ch), na.rm = TRUE),
    n_points = n(),
    n_points_rounded = round(n_points, -2)
  )

gpx_summary
