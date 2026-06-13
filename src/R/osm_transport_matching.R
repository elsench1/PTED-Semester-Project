# src/R/osm_transport_matching.R

library(sf)
library(dplyr)
library(tidyr)
library(lubridate)
library(osmextract)

default_transport_types <- function() {
  c(
    "motorway", "trunk",
    "primary", "secondary", "tertiary",
    "residential", "unclassified", "service",
    "rail", "tram", "light_rail", "subway"
  )
}


load_osm_transport_network <- function(
    place = "switzerland",
    layer = "lines",
    target_crs = 2056,
    transport_types = default_transport_types(),
    quiet = FALSE
) {
  roads <- tryCatch(
    {
      osmextract::oe_get(
        place = place,
        layer = layer,
        quiet = quiet
      )
    },
    error = function(e) {
      message("OSM data could not be loaded for: ", place)
      message("Error was: ", e$message)
      return(NULL)
    }
  )
  
  if (is.null(roads)) {
    return(NULL)
  }
  
  if (!inherits(roads, "sf")) {
    stop("OSM data must be an sf object.")
  }
  
  if (!"highway" %in% names(roads)) {
    roads$highway <- NA_character_
  }
  
  if (!"railway" %in% names(roads)) {
    roads$railway <- NA_character_
  }
  
  network_sf <- roads |>
    mutate(
      highway = as.character(.data$highway),
      railway = as.character(.data$railway),
      transport_type = coalesce(.data$highway, .data$railway)
    ) |>
    filter(.data$transport_type %in% transport_types) |>
    st_transform(crs = target_crs)
  
  return(network_sf)
}


save_osm_transport_network <- function(
    network_sf,
    cache_file = "data/processedData/osm_transport_network_switzerland_2056.rds",
    overwrite = TRUE
) {
  if (!inherits(network_sf, "sf")) {
    stop("network_sf must be an sf object.")
  }
  
  if (file.exists(cache_file) && !overwrite) {
    stop("File already exists: ", cache_file)
  }
  
  dir.create(dirname(cache_file), recursive = TRUE, showWarnings = FALSE)
  
  saveRDS(network_sf, cache_file)
  
  invisible(cache_file)
}


read_osm_transport_network <- function(
    cache_file = "data/processedData/osm_transport_network_switzerland_2056.rds"
) {
  if (!file.exists(cache_file)) {
    stop("Cached OSM file does not exist: ", cache_file)
  }
  
  readRDS(cache_file)
}


get_osm_transport_network <- function(
    cache_file = "data/processedData/osm_transport_network_switzerland_2056.rds",
    place = "switzerland",
    layer = "lines",
    target_crs = 2056,
    transport_types = default_transport_types(),
    force_download = FALSE,
    max_cache_age_days = 1,
    quiet = FALSE
) {
  if (max_cache_age_days < 0) {
    stop("max_cache_age_days must be >= 0.")
  }
  
  cache_exists <- file.exists(cache_file)
  
  cache_is_valid <- FALSE
  
  if (cache_exists && max_cache_age_days > 0) {
    cache_age_days <- as.numeric(
      difftime(
        Sys.time(),
        file.info(cache_file)$mtime,
        units = "days"
      )
    )
    
    cache_is_valid <- cache_age_days < max_cache_age_days
  }
  
  if (
    cache_exists &&
    cache_is_valid &&
    !force_download
  ) {
    message(
      "Loading cached OSM data: ",
      cache_file,
      " | cache age: ",
      round(cache_age_days, 2),
      " days"
    )
    
    return(read_osm_transport_network(cache_file))
  }
  
  if (cache_exists && max_cache_age_days == 0) {
    message("max_cache_age_days is 0. Downloading OSM data again.")
  } else if (cache_exists && !cache_is_valid) {
    message(
      "Cached OSM data is too old. Downloading again: ",
      cache_file
    )
  } else if (force_download) {
    message("force_download is TRUE. Downloading OSM data again.")
  } else {
    message("No cached OSM data found. Downloading OSM data.")
  }
  
  network_sf <- load_osm_transport_network(
    place = place,
    layer = layer,
    target_crs = target_crs,
    transport_types = transport_types,
    quiet = quiet
  )
  
  if (is.null(network_sf)) {
    return(NULL)
  }
  
  save_osm_transport_network(
    network_sf = network_sf,
    cache_file = cache_file,
    overwrite = TRUE
  )
  
  return(network_sf)
}


classify_transport_group <- function(transport_type) {
  case_when(
    transport_type %in% c("motorway", "trunk") ~ "major_road",
    transport_type %in% c("primary", "secondary", "tertiary") ~ "main_road",
    transport_type %in% c("residential", "service", "unclassified") ~ "local_road",
    transport_type %in% c("rail", "tram", "light_rail", "subway") ~ "rail",
    TRUE ~ NA_character_
  )
}


get_moving_vector <- function(
    points_sf,
    moving_col = NULL,
    static_col = "static"
) {
  if (!is.null(moving_col)) {
    if (!moving_col %in% names(points_sf)) {
      stop("moving_col does not exist in points_sf: ", moving_col)
    }
    
    return(coalesce(as.logical(points_sf[[moving_col]]), FALSE))
  }
  
  if ("moving_ext" %in% names(points_sf)) {
    return(coalesce(as.logical(points_sf$moving_ext), FALSE))
  }
  
  if ("moving" %in% names(points_sf)) {
    return(coalesce(as.logical(points_sf$moving), FALSE))
  }
  
  if (static_col %in% names(points_sf)) {
    return(!coalesce(as.logical(points_sf[[static_col]]), TRUE))
  }
  
  message(
    "No moving column found. ",
    "All points are treated as moving. ",
    "Use moving_col if this is not intended."
  )
  
  rep(TRUE, nrow(points_sf))
}


add_transport_and_road_type <- function(
    points_sf,
    network_sf,
    moving_col = NULL,
    static_col = "static",
    target_crs = 2056,
    max_match_dist_m = 100,
    bbox_buffer_m = 500,
    keep_nearest_distance = TRUE
) {
  if (!inherits(points_sf, "sf")) {
    stop("points_sf must be an sf object.")
  }
  
  if (!inherits(network_sf, "sf")) {
    stop("network_sf must be an sf object.")
  }
  
  if (is.na(st_crs(points_sf))) {
    stop("points_sf has no CRS.")
  }
  
  if (is.na(st_crs(network_sf))) {
    stop("network_sf has no CRS.")
  }
  
  points_out <- points_sf |>
    st_transform(crs = target_crs)
  
  network <- network_sf |>
    st_transform(crs = target_crs)
  
  if (!"transport_type" %in% names(network)) {
    if (!"highway" %in% names(network)) {
      network$highway <- NA_character_
    }
    
    if (!"railway" %in% names(network)) {
      network$railway <- NA_character_
    }
    
    network <- network |>
      mutate(
        highway = as.character(.data$highway),
        railway = as.character(.data$railway),
        transport_type = coalesce(.data$highway, .data$railway)
      )
  }
  
  bbox_poly <- st_as_sfc(st_bbox(points_out))
  st_crs(bbox_poly) <- st_crs(points_out)
  
  bbox_poly <- st_buffer(bbox_poly, dist = bbox_buffer_m)
  
  network_crop <- suppressWarnings(
    st_crop(network, st_bbox(bbox_poly))
  )
  
  if (nrow(network_crop) == 0) {
    stop("No OSM transport features found around the GPS points.")
  }
  
  nearest_idx <- st_nearest_feature(points_out, network_crop)
  
  nearest_dist_m <- as.numeric(
    st_distance(
      points_out,
      network_crop[nearest_idx, ],
      by_element = TRUE
    )
  )
  
  osm_type <- as.character(network_crop$transport_type[nearest_idx])
  
  if (!is.null(max_match_dist_m) && is.finite(max_match_dist_m)) {
    osm_type[nearest_dist_m > max_match_dist_m] <- NA_character_
  }
  
  moving_vec <- get_moving_vector(
    points_sf = points_out,
    moving_col = moving_col,
    static_col = static_col
  )
  
  points_out$transport_type_osm <- osm_type
  
  if (keep_nearest_distance) {
    points_out$nearest_transport_dist_m <- nearest_dist_m
  }
  
  points_out$transport_type <- osm_type
  points_out$transport_type[!moving_vec] <- "static"
  
  points_out$transport_type_move <- points_out$transport_type
  points_out$transport_type_move[!moving_vec] <- NA_character_
  
  points_out$transport_group <- classify_transport_group(
    points_out$transport_type_move
  )
  
  return(points_out)
}


summarise_transport_share <- function(
    points_sf,
    by = "date",
    group_col = "transport_group",
    time_col = "time",
    weight_col = NULL,
    max_gap_min = 120,
    wide = TRUE
) {
  if (!inherits(points_sf, "sf")) {
    stop("points_sf must be an sf object.")
  }
  
  if (!group_col %in% names(points_sf)) {
    stop("group_col does not exist in points_sf: ", group_col)
  }
  
  df <- points_sf |>
    sf::st_drop_geometry()
  
  if (!is.null(time_col) && time_col %in% names(df)) {
    df <- df |>
      dplyr::arrange(.data[[time_col]])
  }
  
  if (is.null(weight_col) && "nPlus1_time" %in% names(df)) {
    weight_col <- "nPlus1_time"
  }
  
  if (!is.null(weight_col) && weight_col %in% names(df)) {
    df$.transport_weight_min <- as.numeric(df[[weight_col]])
  } else if (!is.null(time_col) && time_col %in% names(df)) {
    df <- df |>
      dplyr::mutate(
        .transport_weight_min = as.numeric(
          difftime(
            dplyr::lead(.data[[time_col]]),
            .data[[time_col]],
            units = "mins"
          )
        )
      )
    
    df$.transport_weight_min[
      is.na(df$.transport_weight_min) |
        df$.transport_weight_min < 0 |
        df$.transport_weight_min > max_gap_min
    ] <- NA_real_
  } else {
    df$.transport_weight_min <- 1
  }
  
  if (!by %in% names(df)) {
    if (by == "date" && time_col %in% names(df)) {
      df$date <- as.Date(df[[time_col]])
    } else if (by == "day" && time_col %in% names(df)) {
      df$day <- lubridate::day(df[[time_col]])
    } else {
      stop("by column does not exist and cannot be created: ", by)
    }
  }
  
  out <- df |>
    dplyr::filter(!is.na(.data[[group_col]])) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(by, group_col)))) |>
    dplyr::summarise(
      time_min = sum(.transport_weight_min, na.rm = TRUE),
      n_points = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(by))) |>
    dplyr::mutate(
      total_time_min = sum(time_min, na.rm = TRUE),
      share = dplyr::case_when(
        total_time_min > 0 ~ time_min / total_time_min,
        TRUE ~ NA_real_
      )
    ) |>
    dplyr::ungroup()
  
  if (wide) {
    out <- out |>
      dplyr::select(
        dplyr::all_of(by),
        dplyr::all_of(group_col),
        share
      ) |>
      tidyr::pivot_wider(
        names_from = dplyr::all_of(group_col),
        values_from = share,
        values_fill = 0
      )
  }
  
  return(out)
}