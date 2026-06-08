library(dplyr)
library(sf)

compress_stop_blocks <- function(df,
                                 moving_col = "moving",
                                 time_col = "time",
                                 x_col = "x",
                                 y_col = "y") {
  
  stopifnot(inherits(df, "sf"))
  stopifnot(moving_col %in% names(df))
  stopifnot(time_col %in% names(df))
  
  crs_original <- st_crs(df)
  
  df2 <- df |>
    arrange(.data[[time_col]]) |>
    mutate(
      .moving_tmp = coalesce(.data[[moving_col]], FALSE),
      .run_id = cumsum(.moving_tmp != lag(.moving_tmp, default = first(.moving_tmp)))
    )
  
  moving_parts <- df2 |>
    filter(.moving_tmp) |>
    mutate(
      compressed_type = "moving",
      n_points_compressed = 1L
    ) |>
    select(-.moving_tmp, -.run_id)
  
  stop_df <- df2 |>
    filter(!.moving_tmp) |>
    st_drop_geometry()
  
  if (nrow(stop_df) == 0) {
    return(moving_parts |> arrange(.data[[time_col]]))
  }
  
  if (!(x_col %in% names(stop_df)) || !(y_col %in% names(stop_df))) {
    coords <- st_coordinates(df2 |> filter(!.moving_tmp))
    stop_df[[x_col]] <- coords[, 1]
    stop_df[[y_col]] <- coords[, 2]
  }
  
  summarise_column <- function(v, nm) {
    
    if (nm == time_col) {
      return(first(v))
    }
    
    if (nm == moving_col) {
      return(FALSE)
    }
    
    if (nm %in% c("track_fid", "track_seg_id", "segment_id", "new_segment")) {
      return(first(v))
    }
    
    if (nm == "track_seg_point_id") {
      return(first(v))
    }
    
    if (nm %in% c("speed", "speed_calc", "speed_kmh", "dist_m", "dt", "dx", "dy")) {
      return(0)
    }
    
    if (nm == "accel") {
      return(NA_real_)
    }
    
    if (is.numeric(v)) {
      if (all(is.na(v))) return(NA_real_)
      return(median(v, na.rm = TRUE))
    }
    
    if (is.logical(v)) {
      return(any(v, na.rm = TRUE))
    }
    
    if (inherits(v, "POSIXct") || inherits(v, "POSIXt") || inherits(v, "Date")) {
      return(first(v))
    }
    
    v_non_na <- v[!is.na(v)]
    
    if (length(v_non_na) > 0) {
      return(v_non_na[1])
    }
    
    return(NA)
  }
  
  cols_to_summarise <- setdiff(
    names(stop_df),
    c(".moving_tmp", ".run_id")
  )
  
  stop_summary <- stop_df |>
    group_by(.run_id) |>
    summarise(
      across(
        .cols = all_of(cols_to_summarise),
        .fns = ~ summarise_column(.x, cur_column())
      ),
      .time_start = first(.data[[time_col]]),
      .time_end = last(.data[[time_col]]),
      n_points_compressed = n(),
      compressed_type = "stop",
      .groups = "drop"
    )
  
  stop_start <- stop_summary |>
    mutate("{time_col}" := .time_start)
  
  stop_end <- stop_summary |>
    filter(.time_end != .time_start) |>
    mutate("{time_col}" := .time_end)
  
  stop_parts <- bind_rows(stop_start, stop_end) |>
    select(-.time_start, -.time_end)
  
  stop_parts_sf <- stop_parts |>
    st_as_sf(coords = c(x_col, y_col), crs = crs_original, remove = FALSE)
  
  bind_rows(
    moving_parts,
    stop_parts_sf
  ) |>
    arrange(.data[[time_col]])
}