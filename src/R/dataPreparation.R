# This collection of functions helps with data preparation
library(sf)
library(dplyr)
library(lubridate)

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

mark_suspicious_spikes <- function(df,
                                   jump_m = 80,
                                   return_m = 30,
                                   max_dt = 20) {
  df |>
    mutate(
      x_prev = lag(x),
      y_prev = lag(y),
      x_next = lead(x),
      y_next = lead(y),
      
      # Pythagoras' theorem
      dist_prev = sqrt((x - x_prev)^2 + (y - y_prev)^2),
      dist_next = sqrt((x_next - x)^2 + (y_next - y)^2),
      dist_skip = sqrt((x_next - x_prev)^2 + (y_next - y_prev)^2),
      
      dt_prev = as.numeric(difftime(time, lag(time), units = "secs")),
      dt_next = as.numeric(difftime(lead(time), time, units = "secs")),
      
      is_suspicious_spike = dist_prev > jump_m &
        dist_next > jump_m &
        dist_skip < return_m &
        dt_prev <= max_dt &
        dt_next <= max_dt
    )
}
