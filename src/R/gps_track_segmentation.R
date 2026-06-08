library(Rcpp)
library(tidyr)

# very slow!!! ~ 2'000'000 × 2'000'000 = 4'000'000'000'000 comparison

detect_stops <- function(df,
                         min_stop_time = 300,
                         stop_radius_m = 50) {
  
  n <- nrow(df)
  is_stop <- rep(FALSE, n)
  
  for (i in seq_len(n)) {
    t0 <- df$time[i]
    j <- which(df$time >= t0 & df$time <= t0 + seconds(min_stop_time))
    
    if (length(j) >= 2) {
      cx <- median(df$x[j], na.rm = TRUE)
      cy <- median(df$y[j], na.rm = TRUE)
      
      d <- sqrt((df$x[j] - cx)^2 + (df$y[j] - cy)^2)
      
      if (quantile(d, 0.9, na.rm = TRUE) <= stop_radius_m) {
        is_stop[j] <- TRUE
      }
    }
  }
  
  df$is_stop <- is_stop
  df
}

#Much Faster

sourceCpp("src/C++/detect_stops_cpp.cpp")

detect_stops_rcpp <- function(df,
                              min_stop_time = 300,
                              stop_radius_m = 50) {
  
  df <- df[order(df$time), ]
  
  time_sec <- as.numeric(df$time)
  
  df$is_stop <- detect_stops_cpp(
    time_sec = time_sec,
    x = df$x,
    y = df$y,
    min_stop_time = min_stop_time,
    stop_radius_m = stop_radius_m
  )
  
  df
}

segment_tracks <- function(df,
                           gap_app_crash_min = 60,
                           gap_tunnel_min = 20,
                           min_move_speed_kmh = 2) {
  
  df |>
    arrange(time) |>
    mutate(
      dt_min = as.numeric(difftime(time, lag(time), units = "mins")),
      
      moving = !is_stop & speed_kmh >= min_move_speed_kmh,
      
      new_segment =
        row_number() == 1 |
        dt_min > gap_app_crash_min |
        (lag(is_stop, default = TRUE) & moving),
      
      segment_id = cumsum(replace_na(new_segment, FALSE))
    )
}