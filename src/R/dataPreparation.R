# This collection of functions helps with data preparation
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


remove_all_na_columns <- function(data) {
  data[, colSums(!is.na(data)) > 0, drop = FALSE]
}

add_speed_and_accel_to_GSP_df <- function(gpx) {
  gpx <- gpx |>
    arrange(time)
  
  coords <- st_coordinates(gpx)
  
  gpx |>
    mutate(
      x = coords[, 1],
      y = coords[, 2],
      time = ymd_hms(time, tz = "UTC"),
      dt = as.numeric(difftime(time, lag(time), units = "secs")),
      dx = x - lag(x),
      dy = y - lag(y),
      dist_m = sqrt(dx^2 + dy^2),
      speed_calc = dist_m / dt,
      speed_kmh = speed_calc * 3.6,
      accel = (speed_calc - lag(speed_calc)) / dt
    )
}
