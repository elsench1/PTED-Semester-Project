# src/R/fixed_areas.R

library(sf)
library(dplyr)
library(readr)
library(stringr)
library(purrr)

load_home_area <- function(
    points_csv = "data/metaData/listOfStadyPoints.csv",
    home_pattern = "christopher.*home",
    target_crs = 2056
) {
  if (!file.exists(points_csv)) {
    stop("Missing metadata points CSV: ", points_csv)
  }
  
  points <- readr::read_csv(points_csv, show_col_types = FALSE)
  
  required_cols <- c("ID", "x", "y", "r", "crs")
  missing_cols <- setdiff(required_cols, names(points))
  
  if (length(missing_cols) > 0) {
    stop(
      "Metadata points CSV is missing columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  home_row <- points |>
    filter(str_detect(str_to_lower(ID), home_pattern)) |>
    slice(1)
  
  if (nrow(home_row) == 0) {
    stop("No home point found with pattern: ", home_pattern)
  }
  
  if (is.na(home_row$r[1]) || home_row$r[1] <= 0) {
    stop("Home point needs a radius r > 0.")
  }
  
  home_point <- st_as_sf(
    home_row,
    coords = c("x", "y"),
    crs = home_row$crs[1],
    remove = FALSE
  ) |>
    st_transform(target_crs)
  
  home_area <- home_point |>
    st_buffer(dist = home_row$r[1]) |>
    mutate(
      area_id = "home",
      area_type = "home"
    ) |>
    select(area_id, area_type, ID, r, geometry)
  
  home_area
}


load_zhaw_areas <- function(
    gpkg_files = c(
      "data/metaData/zhaw_gruental.gpkg",
      "data/metaData/ZHAW_Reidbach_A.gpkg"
    ),
    target_crs = 2056,
    buffer_m = 50
) {
  missing_files <- gpkg_files[!file.exists(gpkg_files)]
  
  if (length(missing_files) > 0) {
    stop(
      "Missing ZHAW GPKG file(s): ",
      paste(missing_files, collapse = ", ")
    )
  }
  
  zhaw_areas <- purrr::map_dfr(
    gpkg_files,
    function(path) {
      area_name <- tools::file_path_sans_ext(basename(path))
      
      st_read(path, quiet = TRUE) |>
        st_make_valid() |>
        st_transform(target_crs) |>
        st_union() |>
        st_as_sf() |>
        mutate(
          area_id = area_name,
          area_type = "zhaw"
        ) |>
        select(area_id, area_type, geometry)
    }
  )
  
  if (!is.null(buffer_m) && buffer_m > 0) {
    zhaw_areas <- zhaw_areas |>
      st_buffer(dist = buffer_m)
  }
  
  zhaw_areas
}


load_fixed_areas <- function(
    points_csv = "data/metaData/listOfStadyPoints.csv",
    zhaw_gpkg_files = c(
      "data/metaData/zhaw_gruental.gpkg",
      "data/metaData/ZHAW_Reidbach_A.gpkg"
    ),
    target_crs = 2056,
    zhaw_buffer_m = 50
) {
  home_area <- load_home_area(
    points_csv = points_csv,
    target_crs = target_crs
  )
  
  zhaw_areas <- load_zhaw_areas(
    gpkg_files = zhaw_gpkg_files,
    target_crs = target_crs,
    buffer_m = zhaw_buffer_m
  )
  
  fixed_areas <- bind_rows(
    home_area |> select(area_id, area_type, geometry),
    zhaw_areas |> select(area_id, area_type, geometry)
  )
  
  fixed_areas
}


tag_points_with_fixed_areas <- function(points_sf, fixed_areas) {
  if (st_crs(points_sf) != st_crs(fixed_areas)) {
    points_sf <- st_transform(points_sf, st_crs(fixed_areas))
  }
  
  points_with_id <- points_sf |>
    mutate(.point_id = row_number())
  
  hits <- st_join(
    points_with_id |> select(.point_id),
    fixed_areas |> select(area_id, area_type),
    join = st_within,
    left = FALSE
  ) |>
    st_drop_geometry() |>
    group_by(.point_id) |>
    summarise(
      area_id = first(area_id),
      area_type = first(area_type),
      .groups = "drop"
    )
  
  points_tagged <- points_with_id |>
    left_join(hits, by = ".point_id") |>
    mutate(
      in_fixed_area = !is.na(area_id),
      at_home = area_type == "home",
      at_zhaw = area_type == "zhaw"
    ) |>
    select(-.point_id)
  
  points_tagged
}


smooth_fixed_area_gaps <- function(
    points_sf,
    max_gap_points = 2,
    max_gap_minutes = 5
) {
  if (!"area_id" %in% names(points_sf)) {
    stop("points_sf must contain an area_id column.")
  }
  
  if (!"time" %in% names(points_sf)) {
    stop("points_sf must contain a time column.")
  }
  
  points_sf <- points_sf |>
    arrange(time)
  
  area_id <- points_sf$area_id
  area_type <- points_sf$area_type
  
  outside <- is.na(area_id)
  
  runs <- rle(outside)
  ends <- cumsum(runs$lengths)
  starts <- ends - runs$lengths + 1
  
  for (i in seq_along(runs$values)) {
    is_outside_run <- runs$values[i]
    
    if (!is_outside_run) next
    
    start_idx <- starts[i]
    end_idx <- ends[i]
    
    before_idx <- start_idx - 1
    after_idx <- end_idx + 1
    
    if (before_idx < 1 || after_idx > length(area_id)) next
    
    before_area <- area_id[before_idx]
    after_area <- area_id[after_idx]
    
    if (is.na(before_area) || is.na(after_area)) next
    if (before_area != after_area) next
    
    gap_points <- end_idx - start_idx + 1
    
    gap_minutes <- as.numeric(
      difftime(
        points_sf$time[after_idx],
        points_sf$time[before_idx],
        units = "mins"
      )
    )
    
    if (
      gap_points <= max_gap_points ||
      gap_minutes <= max_gap_minutes
    ) {
      area_id[start_idx:end_idx] <- before_area
      area_type[start_idx:end_idx] <- area_type[before_idx]
    }
  }
  
  points_sf |>
    mutate(
      area_id = area_id,
      area_type = area_type,
      in_fixed_area = !is.na(area_id),
      at_home = area_type == "home",
      at_zhaw = area_type == "zhaw"
    )
}