library(dplyr)

source("src/R/move_points_to_circle_exit.R")
source("src/R/dataPreparation.R")
source("src/R/gps_track_segmentation.R")
source("src/R/compress_stop_blocks.R")

GPXFileLink <- "data/rawData/Trackingdata_Christopher/20260312-172538.gpx"

GPS_Track <- load_GPX_File(GPXFile = GPXFileLink) |>
  remove_all_na_columns() |>
  select(-c( track_fid, track_seg_id, speed)) |>
  add_speed_and_accel_to_GPS_df() |>
  filter(!is.na(time)) |>
  mark_suspicious_points() |> 
  filter(bad_point != TRUE | is.na(bad_point)) |>
  detect_stops_rcpp() |>
  segment_tracks() |>
  select(
    -any_of(c(
      "dt", "dx", "dy", "dist_m", "speed_calc", "speed_kmh", "accel",
      "x_prev", "y_prev", "x_next", "y_next",
      "dist_prev", "dist_next", "dist_skip",
      "dt_prev", "dt_next"
    ))
  ) |>
  mutate(
    bad_point = coalesce(bad_point, FALSE),
    is_suspicious_spike = coalesce(is_suspicious_spike, FALSE),
    suspicious_accel = coalesce(suspicious_accel, FALSE),
    is_stop = coalesce(is_stop, FALSE),
    moving = coalesce(moving, FALSE),
    new_segment = if_else(row_number() == 1, TRUE, coalesce(new_segment, FALSE))
  )

GPS_Track_compress <- compress_stop_blocks()


saveRDS(GPS_Track, "data/processedData/GPS_Track_processed.rds")


# GPS_Track |>
#   mutate(time_parsed = ymd_hms(time, tz = "UTC", quiet = TRUE)) |>
#   summarise(
#     n_failed = sum(is.na(time_parsed)),
#     n_total = n()
#   )
# 
# GPS_Track |>
#   mutate(time_parsed = ymd_hms(time, tz = "UTC", quiet = TRUE)) |>
#   filter(is.na(time_parsed)) |>
#   st_drop_geometry() |>
#   select(time) |>
#   print(n = 30)
# GPS_Track |>
#   mutate(time_parsed = ymd_hms(time, tz = "UTC", quiet = TRUE)) |>
#   filter(is.na(time_parsed)) |>
#   st_drop_geometry() |>
#   as_tibble() |>
#   select(time) |>
#   print(n = 30)
