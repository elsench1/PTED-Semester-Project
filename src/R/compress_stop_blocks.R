library(dplyr)
library(sf)

compress_stop_blocks <- function(df,
                                 moving_col = "moving",
                                 time_col = "time",
                                 x_col = "x",
                                 y_col = "y",
                                 min_stop_points = 3L) {
  stopifnot(inherits(df, "sf"))
  stopifnot(moving_col %in% names(df))
  stopifnot(time_col %in% names(df))
  
  crs_original <- sf::st_crs(df)
  
  df2 <- df |>
    arrange(.data[[time_col]]) |>
    mutate(
      .row_id = row_number(),
      .moving_tmp = coalesce(.data[[moving_col]], FALSE),
      .run_id = cumsum(.moving_tmp != lag(.moving_tmp, default = first(.moving_tmp)))
    )
  
  # Falls x/y nicht existieren, aus der Geometrie holen
  coords <- sf::st_coordinates(df2)
  
  if (!(x_col %in% names(df2))) {
    df2[[x_col]] <- coords[, 1]
  }
  
  if (!(y_col %in% names(df2))) {
    df2[[y_col]] <- coords[, 2]
  }
  
  run_info <- df2 |>
    sf::st_drop_geometry() |>
    group_by(.run_id) |>
    summarise(
      .is_stop = !first(.moving_tmp),
      .n_run = n(),
      .start_row = first(.row_id),
      .end_row = last(.row_id),
      .median_x = median(.data[[x_col]], na.rm = TRUE),
      .median_y = median(.data[[y_col]], na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      .compress = .is_stop & .n_run >= min_stop_points
    )
  
  out <- df2 |>
    left_join(run_info, by = ".run_id") |>
    filter(
      !.compress |
        .row_id == .start_row |
        .row_id == .end_row
    ) |>
    mutate(
      compressed_type = case_when(
        .compress ~ "stop_compressed",
        .moving_tmp ~ "moving",
        TRUE ~ "stop_uncompressed"
      ),
      n_points_compressed = if_else(.compress, as.integer(.n_run), 1L),
      "{x_col}" := if_else(.compress, .median_x, .data[[x_col]]),
      "{y_col}" := if_else(.compress, .median_y, .data[[y_col]])
    ) |>
    sf::st_drop_geometry() |>
    select(
      -.row_id,
      -.moving_tmp,
      -.run_id,
      -.is_stop,
      -.n_run,
      -.start_row,
      -.end_row,
      -.median_x,
      -.median_y,
      -.compress
    )
  
  sf::st_as_sf(
    out,
    coords = c(x_col, y_col),
    crs = crs_original,
    remove = FALSE
  )
}