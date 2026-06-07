get_df_of_hide_points <- function(hide_point_csv, target_crs = 2056) {
  
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