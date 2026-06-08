# Draft

source("src/R/move_points_to_circle_exit.R")
source("src/R/dataPreparation.R")
source("src/R/gps_track_segmentation.R")
source("src/R/compress_stop_blocks.R")
# 
# GPXFileLink <- "data/rawData/Trackingdata_Christopher/20260312-172538.gpx"
# 
# 
# 
# GPS_Track <- load_GPX_File(GPXFile = GPXFileLink) |> 
#   remove_all_na_columns() |> 
#   add_speed_and_accel_to_GSP_df() |> 
#   mark_suspicious_points()
# 
# library(dplyr)
# GPS_Track <- GPS_Track |> 
#   filter(bad_point != TRUE | is.na(bad_point))
# 
# 
# library(ggplot2)
# library(dplyr)
# library(sf)
# 
# GPS_Track_plot <- GPS_Track |>
#   mutate(
#     point_type = case_when(
#       is_suspicious_spike == TRUE & suspicious_accel == TRUE ~ "both",
#       is_suspicious_spike == TRUE ~ "spike",
#       suspicious_accel == TRUE ~ "accel",
#       TRUE ~ "normal"
#     ),
#     point_type = factor(
#       point_type,
#       levels = c("normal", "spike", "accel", "both")
#     )
#   )
# 
# ggplot(GPS_Track_plot) +
#   geom_sf(aes(color = point_type), size = 1.8, alpha = 0.8) +
#   scale_color_manual(
#     values = c(
#       "normal" = "black",
#       "spike"  = "red",
#       "accel"  = "green",
#       "both"   = "blue"
#     ),
#     labels = c(
#       "normal" = "normal",
#       "spike"  = "is_suspicious_spike",
#       "accel"  = "suspicious_accel",
#       "both"   = "beides"
#     ),
#     name = "Punkttyp"
#   ) +
#   theme_minimal() +
#   labs(
#     title = "GPS Track mit markierten verdächtigen Punkten",
#     x = "Koordinate X",
#     y = "Koordinate Y"
#   )


# 
# sum(GPS_Track$is_suspicious_spike, na.rm = TRUE)
# sum(GPS_Track$suspicious_accel, na.rm = TRUE)
# sum(GPS_Track$bad_point, na.rm = TRUE)
# 
# library(tmap)

#######################################################################3

# Datei Speichern und laden
# saveRDS(GPS_Track, "data/processedData/GPS_Track_processed.rds")

GPS_Track <- readRDS("data/processedData/GPS_Track_processed.rds")

############################################################################
# Eigen Regeln für gewisse Gebiete

# home <- st_sfc(
#   st_point(c(8.519625,47.156997)),
#   crs = 4326
# ) |> 
#   st_transform(crs =2056)
# 
# home_buffer <- st_buffer(home, 300)
# 
# GPS_Track <- GPS_Track |> 
#   mutate(
#     near_home = as.logical(st_intersects(geometry, home_buffer, sparse = FALSE)[, 1]),
#     bad_near_home = near_home & speed_kmh > 50
#   )
# 
# sum(GPS_Track$bad_near_home, na.rm = TRUE)
# 
# library(dplyr)
# GPS_Track <- GPS_Track |>
#   filter(bad_near_home != TRUE | is.na(bad_near_home))
# 
# 
# GPS_Track <- GPS_Track |>
#   mutate(
#     low_sat = sat < 6
#   )
# 
# sum(GPS_Track$low_sat, na.rm = TRUE)
# 
# GPS_Track <- GPS_Track |> 
#   filter(low_sat != TRUE | is.na(low_sat))

################################################################
# Segment






# GPS_Track <- detect_stops(GPS_Track)

test <- GPS_Track[1:10000, ]
system.time(
  result <- detect_stops(test)
)




df_test <- GPS_Track[1:10000, ]

system.time({
  result_test <- detect_stops_rcpp(df_test)
})


GPS_Track <- detect_stops_rcpp(GPS_Track)
GPS_Track <- segment_tracks(GPS_Track)

#####################################################################

# remove stop blocks

GPS_Track <- GPS_Track |>
  arrange(time) |>
  mutate(
    bad_point = coalesce(bad_point, FALSE),
    is_suspicious_spike = coalesce(is_suspicious_spike, FALSE),
    suspicious_accel = coalesce(suspicious_accel, FALSE),
    is_stop = coalesce(is_stop, FALSE),
    moving = coalesce(moving, FALSE),
    new_segment = if_else(row_number() == 1, TRUE, coalesce(new_segment, FALSE))
  )

GPS_Track_compressed <- compress_stop_blocks(GPS_Track)

nrow(GPS_Track)
nrow(GPS_Track_compressed)

GPS_Track_compressed <- GPS_Track_compressed |>
  select(
    -any_of(c(
      "dt", "dx", "dy", "dist_m", "speed_calc", "speed_kmh", "accel",
      "x_prev", "y_prev", "x_next", "y_next",
      "dist_prev", "dist_next", "dist_skip",
      "dt_prev", "dt_next"
    ))
  )
#########################################################

library(tmap)
library(colorspace)

GPS_segments <- GPS_segments |>
  mutate(segment_id = as.factor(segment_id))

segment_palette <- qualitative_hcl(
  n = nlevels(GPS_segments$segment_id),
  palette = "Dark 3"
)

tmap_mode("view")

tm_basemap("OpenStreetMap") +
  tm_shape(st_transform(GPS_segments, 4326)) +
  tm_lines(
    col = "segment_id",
    lwd = 3,
    palette = segment_palette,
    legend.col.show = FALSE,
    popup.vars = c(
      "Segment" = "segment_id",
      "Start" = "start_time",
      "Ende" = "end_time",
      "Punkte" = "n_points"
    )
  )

#############################################################
# hide_points <- get_df_of_hide_points(
#   hide_point_csv = "data/metaData/hidePoint.csv",
#   target_crs = 2056)

# if (is.null(GPS_Track)) {
#   stop("GPX-Datei konnte nicht geladen werden.")
# }

# GPS_Track_hidden <- move_points_to_circle_exit(
#   GPS_Track = GPS_Track,
#   hide_point = hide_point,
#   dist = 300
# )
# 

####################################################################
# tmap Plot
# library(tmap)
# 
# GPS_Track_line <- GPS_Track |>
#   summarise(do_union = FALSE) |>
#   st_cast("LINESTRING")
# 
# tmap_mode("view")
# 
# map <- tm_shape(GPS_Track_line) +
#   tm_lines(col = "green", lwd = 5)
# 
# print(map)
# # #######################################################################


# main <- function() {
#   
#   library(tmap)
#   
#   hide_point <- st_as_sf(
#     data.frame(
#       lon = 8 + 31/60 + 10.65/3600,
#       lat = 47 + 9/60 + 25.19/3600
#     ),
#     coords = c("lon", "lat"),
#     crs = 4326
#   ) |>
#     st_transform(crs = 2056)
#   
#   GPXFileLink <- "data/Trackingdata_Christopher/20260312-172538.gpx"
#   
#   GPS_Track <- load_GPX_File(GPXFile = GPXFileLink)
#   
#   if (is.null(GPS_Track)) {
#     stop("GPX-Datei konnte nicht geladen werden.")
#   }
#   
#   GPS_Track_hidden <- move_points_to_circle_exit(
#     GPS_Track = GPS_Track,
#     hide_point = hide_point,
#     dist = 300
#   )
#   
#   GPS_Track_line <- GPS_Track_hidden |> 
#     summarise(do_union = FALSE) |> 
#     st_cast("LINESTRING")
#   
#   tmap_mode("view")
#   
#   map <- tm_shape(GPS_Track_line) +
#     tm_lines(col = "green", lwd = 5)
#   
#   print(map)
# }
# 
# 
# main()

# if(!interactive()){
#   main()
# }

# if (sys.nframe() == 0){
#   main()
# }


# # first simple plots
# library(sf)
# library(tmap)
# library(dplyr)
# 
# FILE <- "data/Trackingdata_Christopher/20260312-172538.gpx"
# 
# hide_point <- st_as_sf(
#   data.frame(
#     lon = 8 + 31/60 + 10.65/3600, # closest bus stop
#     lat = 47 + 9/60 + 25.19/3600
#   ),
#   coords = c("lon", "lat"),
#   crs = 4326
# ) |>
#   st_transform(crs = 2056)
# 
# GPS_Track <- st_read(FILE, layer = "track_points") |> 
#   st_transform(crs = 2056) |> 
#   mutate(
#     hide = lengths(st_is_within_distance(
#       geometry,
#       st_geometry(hide_point),
#       dist = 150
#     )) > 0
#   ) |> 
#   filter(!hide) # for privacy 
# 
# GPS_Track_line <- GPS_Track |> 
#   summarise(do_union = FALSE) |> 
#   st_cast("LINESTRING")
# 
# tmap_mode("view")
# 
# tm_shape(GPS_Track_line) +
#   tm_lines(col = "blue", lwd = 5)

