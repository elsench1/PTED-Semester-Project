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

