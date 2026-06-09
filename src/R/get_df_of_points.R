get_df_of_points <- function(csv_file_with_points, target_crs = 2056) {
  
  library(sf)
  
  csv_data <- tryCatch(
    {
      read.csv(
        csv_file_with_points,
        header = TRUE,
        stringsAsFactors = FALSE
      )
    },
    error = function(e) {
      message("Data could not be loaded: ", csv_file_with_points)
      message("Error was: ", e$message)
      return(NULL)
    }
  )
  
  if (is.null(csv_data)) {
    return(NULL)
  }
  
  required_cols <- c("ID", "x", "y", "r", "crs")
  
  if (!all(required_cols %in% names(csv_data))) {
    stop(
      "CSV muss die Spalten ID, x, y, r und crs enthalten. ",
      "Beispiel: ID,x,y,r,crs"
    )
  }
  
  csv_data$ID <- as.character(csv_data$ID)
  csv_data$x <- as.numeric(csv_data$x)
  csv_data$y <- as.numeric(csv_data$y)
  csv_data$r <- as.numeric(csv_data$r)
  csv_data$crs <- as.integer(csv_data$crs)
  
  if (
    any(is.na(csv_data$x)) ||
    any(is.na(csv_data$y)) ||
    any(is.na(csv_data$r)) ||
    any(is.na(csv_data$crs))
  ) {
    stop("CSV enthält ungültige Werte in x, y, r oder crs.")
  }
  
  points_list <- lapply(seq_len(nrow(csv_data)), function(i) {
    
    point_df <- data.frame(
      ID = csv_data$ID[i],
      r = csv_data$r[i],
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
  
  points <- do.call(rbind, points_list)
  
  return(points)
}