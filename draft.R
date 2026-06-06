# Draft






source("src/R/move_points_to_circle_exit.R")
source("src/R/dataPreparation.R")





# library(tmap)



GPXFileLink <- "data/Trackingdata_Christopher/20260312-172538.gpx"

# GPS_Track <- load_GPX_File(GPXFile = GPXFileLink)

hide_points <- get_df_of_hide_points(
  hide_point_csv = "data/hidePoint.csv",
  target_crs = 2056)

# if (is.null(GPS_Track)) {
#   stop("GPX-Datei konnte nicht geladen werden.")
# }

# GPS_Track_hidden <- move_points_to_circle_exit(
#   GPS_Track = GPS_Track,
#   hide_point = hide_point,
#   dist = 300
# )
# 
# GPS_Track_line <- GPS_Track_hidden |>
#   summarise(do_union = FALSE) |>
#   st_cast("LINESTRING")
# 
# tmap_mode("view")
# 
# map <- tm_shape(GPS_Track_line) +
#   tm_lines(col = "green", lwd = 5)
# 
# print(map)
# 


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

