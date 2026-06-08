# sehr langsam!!! ca 2'000'000 × 2'000'000 = 4'000'000'000'000 vergleiche

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
