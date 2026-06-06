# Draft
library(sf)



load_GPX_File <- function(GPXFile){
  data <- tryCatch(
    {
      st_read(GPXFile, layer ="track_points") |>
        st_transform(crs = 2056)
    },
    error = function(e){
      message("Data could not be loaded", GPXFile)
      message("Error was: ", e$message)
      return(NULL)
    }
  )
  return(data)
}


source("src/R/move_points_to_circle_exit.R")

get_df_hide_point <- function(hide_point_csv, target_crs = 2056) {
  
  library(sf)
  
  csv_data <- tryCatch(
    {
      read.csv(
        hide_point_csv,
        header = TRUE,
        stringsAsFactors = FALSE
      )
    },
    error = function(e) {
      message("Data could not be loaded: ", hide_point_csv)
      message("Error was: ", e$message)
      return(NULL)
    }
  )
  
  if (is.null(csv_data)) {
    return(NULL)
  }
  
  required_cols <- c("x", "y", "crs")
  
  if (!all(required_cols %in% names(csv_data))) {
    stop(
      "CSV muss die Spalten x, y und crs enthalten. ",
      "Beispiel: x,y,crs"
    )
  }
  
  csv_data$x <- as.numeric(csv_data$x)
  csv_data$y <- as.numeric(csv_data$y)
  csv_data$crs <- as.integer(csv_data$crs)
  
  if (any(is.na(csv_data$x)) || any(is.na(csv_data$y)) || any(is.na(csv_data$crs))) {
    stop("CSV enthält ungültige Werte in x, y oder crs.")
  }
  
  hide_points_list <- lapply(seq_len(nrow(csv_data)), function(i) {
    
    point_df <- data.frame(
      x = csv_data$x[i],
      y = csv_data$y[i]
    )
    
    st_as_sf(
      point_df,
      coords = c("x", "y"),
      crs = csv_data$crs[i]
    ) |>
      st_transform(crs = target_crs)
  })
  
  hide_points <- do.call(rbind, hide_points_list)
  
  return(hide_points)
}



library(tmap)



GPXFileLink <- "data/Trackingdata_Christopher/20260312-172538.gpx"

# GPS_Track <- load_GPX_File(GPXFile = GPXFileLink)

hide_points <- get_df_hide_point(hide_point_csv = "data/hidePoint.csv")

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

