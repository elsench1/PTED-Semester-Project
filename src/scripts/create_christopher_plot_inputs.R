# src/scripts/create_christopher_plot_inputs.R

library(sf)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(ggplot2)

source("src/R/fixed_areas.R")
source("src/R/plot_functions.R")

safe_min <- function(time, condition) {
  values <- time[condition & !is.na(condition)]
  
  if (length(values) == 0) {
    return(as.POSIXct(NA, tz = "Europe/Zurich"))
  }
  
  min(values, na.rm = TRUE)
}


safe_max <- function(time, condition) {
  values <- time[condition & !is.na(condition)]
  
  if (length(values) == 0) {
    return(as.POSIXct(NA, tz = "Europe/Zurich"))
  }
  
  max(values, na.rm = TRUE)
}


safe_max_before <- function(time, condition, before_time) {
  if (is.na(before_time)) {
    return(as.POSIXct(NA, tz = "Europe/Zurich"))
  }
  
  values <- time[
    condition &
      !is.na(condition) &
      time < before_time
  ]
  
  if (length(values) == 0) {
    return(as.POSIXct(NA, tz = "Europe/Zurich"))
  }
  
  max(values, na.rm = TRUE)
}


safe_min_after <- function(time, condition, after_time) {
  if (is.na(after_time)) {
    return(as.POSIXct(NA, tz = "Europe/Zurich"))
  }
  
  values <- time[
    condition &
      !is.na(condition) &
      time > after_time
  ]
  
  if (length(values) == 0) {
    return(as.POSIXct(NA, tz = "Europe/Zurich"))
  }
  
  min(values, na.rm = TRUE)
}

dir.create("data/processedData", recursive = TRUE, showWarnings = FALSE)
dir.create("chapters/plots", recursive = TRUE, showWarnings = FALSE)

mean_na <- function(x) {
  if (length(x) == 0 || all(is.na(x))) {
    return(NA_real_)
  }
  
  mean(x, na.rm = TRUE)
}

max_na <- function(x) {
  if (length(x) == 0 || all(is.na(x))) {
    return(NA_real_)
  }
  
  max(x, na.rm = TRUE)
}

# Settings
max_dt_min <- 12 * 60
max_speed_kmh <- 200

gps_path <- "data/processedData/GPS_Track_compress_matched.rds"

if (!file.exists(gps_path)) {
  stop(
    "Missing file: ",
    gps_path,
    "\nRun src/scripts/TrackMatchingChristopher.R first."
  )
}

gps <- readRDS(gps_path) |>
  st_transform(2056) |>
  arrange(time)

fixed_areas <- load_fixed_areas(
  points_csv = "data/metaData/listOfStadyPoints.csv",
  zhaw_gpkg_files = c(
    "data/metaData/zhaw_gruental.gpkg",
    "data/metaData/ZHAW_Reidbach_A.gpkg"
  ),
  zhaw_buffer_m = 50
)

gps_tagged <- gps |>
  tag_points_with_fixed_areas(fixed_areas) |>
  smooth_fixed_area_gaps(
    max_gap_points = 2,
    max_gap_minutes = 5
  ) |>
  arrange(time)

###############################################################################
# time, distance, home / ZHAW and travel metrics

if (nrow(gps_tagged) < 2) {
  stop("Not enough GPS points to calculate movement metrics.")
}

if (!inherits(gps_tagged$time, "POSIXct")) {
  gps_tagged$time <- lubridate::ymd_hms(
    gps_tagged$time,
    tz = "Europe/Zurich"
  )
}

gps_tagged <- gps_tagged |>
  dplyr::arrange(time) |>
  dplyr::mutate(
    time_local = lubridate::with_tz(time, tzone = "Europe/Zurich"),
    day = as.Date(time_local),
    moving = dplyr::coalesce(moving, FALSE),
    at_home = dplyr::coalesce(at_home, FALSE),
    at_zhaw = dplyr::coalesce(at_zhaw, FALSE)
  )

geom <- sf::st_geometry(gps_tagged)
n <- length(geom)

step_dist_m <- c(
  as.numeric(
    sf::st_distance(
      geom[-n],
      geom[-1],
      by_element = TRUE
    )
  ),
  NA_real_
)

dt_min <- c(
  as.numeric(
    difftime(
      gps_tagged$time[-1],
      gps_tagged$time[-n],
      units = "mins"
    )
  ),
  NA_real_
)

same_day <- c(
  gps_tagged$day[-1] == gps_tagged$day[-n],
  FALSE
)

home_area <- fixed_areas |>
  dplyr::filter(area_type == "home")

if (nrow(home_area) != 1) {
  stop("Expected exactly one home area.")
}

home_center <- sf::st_centroid(home_area)

dist_home_m <- as.numeric(
  sf::st_distance(
    gps_tagged,
    home_center
  )[, 1]
)

gps_tagged <- gps_tagged |>
  dplyr::mutate(
    step_dist_m = step_dist_m,
    dt_min = dt_min,
    same_day_next = same_day,
    
    at_home_next = dplyr::lead(at_home, default = dplyr::last(at_home)),
    at_zhaw_next = dplyr::lead(at_zhaw, default = dplyr::last(at_zhaw)),
    
    speed_kmh = dplyr::if_else(
      !is.na(dt_min) & dt_min > 0,
      (step_dist_m / 1000) / (dt_min / 60),
      NA_real_
    ),
    
    valid_time_step =
      same_day_next &
      !is.na(dt_min) &
      dt_min > 0 &
      dt_min <= max_dt_min,
    
    valid_movement_step =
      valid_time_step &
      !is.na(speed_kmh) &
      speed_kmh <= max_speed_kmh,
    
    dist_home = dist_home_m,
    out_home = !at_home,
    
    # Door-to-door outside-home interval:
    # count every interval that is not completely inside home.
    # This includes leaving home, travelling, waiting, changing trains,
    # and returning home.
    outside_home_interval =
      valid_time_step &
      !(at_home & at_home_next),
    
    zhaw_interval =
      valid_time_step &
      (at_zhaw | at_zhaw_next)
  )


###############################################################################
# state runs for home / ZHAW / other

travel_states <- gps_tagged |>
  sf::st_drop_geometry() |>
  dplyr::arrange(time) |>
  dplyr::mutate(
    state = dplyr::case_when(
      at_home ~ "home",
      at_zhaw ~ "zhaw",
      TRUE ~ "other"
    ),
    state_change = state != dplyr::lag(
      state,
      default = dplyr::first(state)
    ),
    state_run = cumsum(
      dplyr::coalesce(state_change, FALSE)
    ) + 1L
  )

state_runs <- travel_states |>
  dplyr::group_by(state_run) |>
  dplyr::summarise(
    state = dplyr::first(state),
    start_time = min(time, na.rm = TRUE),
    end_time = max(time, na.rm = TRUE),
    start_time_local = min(time_local, na.rm = TRUE),
    end_time_local = max(time_local, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::arrange(start_time)

state_runs <- state_runs |>
  dplyr::mutate(
    next_state = dplyr::lead(state),
    next_start_time = dplyr::lead(start_time),
    next_start_time_local = dplyr::lead(start_time_local),
    prev_state = dplyr::lag(state),
    prev_end_time = dplyr::lag(end_time),
    prev_end_time_local = dplyr::lag(end_time_local)
  )


###############################################################################
# time out of home from state runs
#
# Definition:
# time_out_home = time from leaving home until arriving home again.
#
# This includes:
# - travel time
# - waiting time
# - time at ZHAW
# - time at other places

home_departures <- state_runs |>
  dplyr::filter(
    state == "home",
    !is.na(next_state),
    next_state != "home"
  ) |>
  dplyr::transmute(
    departure_time = end_time,
    departure_time_local = end_time_local
  )

home_returns <- state_runs |>
  dplyr::filter(
    state == "home",
    !is.na(prev_state),
    prev_state != "home"
  ) |>
  dplyr::transmute(
    arrival_time = start_time,
    arrival_time_local = start_time_local
  )

out_home_periods <- tibble::tibble(
  departure_time = as.POSIXct(character(), tz = "Europe/Zurich"),
  departure_time_local = as.POSIXct(character(), tz = "Europe/Zurich"),
  arrival_time = as.POSIXct(character(), tz = "Europe/Zurich"),
  arrival_time_local = as.POSIXct(character(), tz = "Europe/Zurich"),
  date = as.Date(character()),
  duration_h = numeric()
)

if (nrow(home_departures) > 0 && nrow(home_returns) > 0) {
  for (i in seq_len(nrow(home_departures))) {
    departure_time <- home_departures$departure_time[i]
    
    next_return <- home_returns |>
      dplyr::filter(arrival_time > departure_time) |>
      dplyr::slice(1)
    
    if (nrow(next_return) == 0) {
      next
    }
    
    arrival_time <- next_return$arrival_time[1]
    
    duration_h <- as.numeric(
      difftime(
        arrival_time,
        departure_time,
        units = "hours"
      )
    )
    
    if (
      !is.na(duration_h) &&
      duration_h > 0 &&
      duration_h <= 24
    ) {
      out_home_periods <- dplyr::bind_rows(
        out_home_periods,
        tibble::tibble(
          departure_time = departure_time,
          departure_time_local = home_departures$departure_time_local[i],
          arrival_time = arrival_time,
          arrival_time_local = next_return$arrival_time_local[1],
          date = as.Date(home_departures$departure_time_local[i]),
          duration_h = duration_h
        )
      )
    }
  }
}

time_out_home_day <- out_home_periods |>
  dplyr::group_by(date) |>
  dplyr::summarise(
    time_out_home = sum(duration_h, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::rename(day = date)


###############################################################################
# door-to-door travel times between home and ZHAW
#
# Definition:
# home -> other -> zhaw
# zhaw -> other -> home
#
# Waiting time is included.
# Home -> other -> home is ignored.
# Home -> roundtrip -> home is ignored.

travel_times <- tibble::tibble(
  date = as.Date(character()),
  travel_direction = character(),
  departure_time = as.POSIXct(character(), tz = "Europe/Zurich"),
  arrival_time = as.POSIXct(character(), tz = "Europe/Zurich"),
  travel_time_min = numeric()
)

if (nrow(state_runs) >= 2) {
  for (i in seq_len(nrow(state_runs) - 1)) {
    start_state <- state_runs$state[i]
    
    if (!start_state %in% c("home", "zhaw")) {
      next
    }
    
    target_state <- dplyr::case_when(
      start_state == "home" ~ "zhaw",
      start_state == "zhaw" ~ "home",
      TRUE ~ NA_character_
    )
    
    later_runs <- state_runs[(i + 1):nrow(state_runs), ]
    
    target_idx <- which(later_runs$state == target_state)[1]
    origin_return_idx <- which(later_runs$state == start_state)[1]
    
    if (is.na(target_idx)) {
      next
    }
    
    if (!is.na(origin_return_idx) && origin_return_idx < target_idx) {
      next
    }
    
    departure_time <- state_runs$end_time[i]
    arrival_time <- later_runs$start_time[target_idx]
    
    travel_time_min <- as.numeric(
      difftime(
        arrival_time,
        departure_time,
        units = "mins"
      )
    )
    
    if (
      !is.na(travel_time_min) &&
      travel_time_min > 0 &&
      travel_time_min <= 200
    ) {
      travel_times <- dplyr::bind_rows(
        travel_times,
        tibble::tibble(
          date = as.Date(
            lubridate::with_tz(
              departure_time,
              tzone = "Europe/Zurich"
            )
          ),
          travel_direction = if (start_state == "home") {
            "travel_time_to_uni"
          } else {
            "travel_time_home"
          },
          departure_time = departure_time,
          arrival_time = arrival_time,
          travel_time_min = travel_time_min
        )
      )
    }
  }
}

home_zhaw_data <- travel_times |>
  dplyr::transmute(
    metric = "Travel time home - ZHAW (min)",
    value = travel_time_min
  )


###############################################################################
# daily metrics

analysis_day_base <- gps_tagged |>
  sf::st_drop_geometry() |>
  dplyr::group_by(day) |>
  dplyr::summarise(
    dist_day = sum(
      dplyr::if_else(
        valid_movement_step,
        step_dist_m / 1000,
        0
      ),
      na.rm = TRUE
    ),
    
    time_at_zhaw = sum(
      dplyr::if_else(
        zhaw_interval,
        dt_min / 60,
        0
      ),
      na.rm = TRUE
    ),
    
    max_radius = max_na(dist_home) / 1000,
    
    avgSpeed_day = mean_na(
      speed_kmh[valid_movement_step & moving]
    ),
    
    .groups = "drop"
  )

analysis_day <- analysis_day_base |>
  dplyr::left_join(
    time_out_home_day,
    by = "day"
  ) |>
  dplyr::mutate(
    time_out_home = tidyr::replace_na(time_out_home, 0)
  ) |>
  dplyr::select(
    day,
    dist_day,
    time_out_home,
    time_at_zhaw,
    max_radius,
    avgSpeed_day
  )

###############################################################################
# road type pie data

pie_data_raw <- gps_tagged |>
  sf::st_drop_geometry() |>
  dplyr::filter(
    valid_time_step,
    moving,
    !is.na(transport_group)
  ) |>
  dplyr::mutate(
    transport_group = dplyr::case_when(
      stringr::str_to_lower(transport_group) %in%
        c("major_road", "major road") ~ "major_road",
      stringr::str_to_lower(transport_group) %in%
        c("main_road", "main road") ~ "main_road",
      stringr::str_to_lower(transport_group) %in%
        c("local_road", "local road") ~ "local_road",
      stringr::str_to_lower(transport_group) == "rail" ~ "rail",
      TRUE ~ transport_group
    )
  ) |>
  dplyr::group_by(transport_group) |>
  dplyr::summarise(
    time_min = sum(dt_min, na.rm = TRUE),
    .groups = "drop"
  )

known_transport_groups <- tibble::tibble(
  transport_group = c(
    "major_road",
    "main_road",
    "local_road",
    "rail"
  )
)

pie_data_raw <- known_transport_groups |>
  dplyr::left_join(pie_data_raw, by = "transport_group") |>
  dplyr::mutate(
    time_min = tidyr::replace_na(time_min, 0)
  )

total_transport_time <- sum(pie_data_raw$time_min, na.rm = TRUE)

pie_data_raw$share <- if (total_transport_time > 0) {
  pie_data_raw$time_min / total_transport_time
} else {
  rep(NA_real_, nrow(pie_data_raw))
}

road_summary_wide <- pie_data_raw |>
  dplyr::select(transport_group, share) |>
  tidyr::pivot_wider(
    names_from = transport_group,
    values_from = share,
    values_fill = 0
  )

pie_data <- pie_data_raw |>
  dplyr::select(transport_group, share) |>
  dplyr::mutate(
    transport_group = dplyr::case_when(
      transport_group == "major_road" ~ "Major road",
      transport_group == "main_road" ~ "Main road",
      transport_group == "local_road" ~ "Local road",
      transport_group == "rail" ~ "Rail",
      TRUE ~ transport_group
    )
  )


###############################################################################
# summary tables

analysis <- dplyr::bind_cols(
  analysis_day |>
    dplyr::summarise(
      avgDistDay = mean_na(dist_day),
      avgTimeOutHome = mean_na(time_out_home),
      avgRadius = mean_na(max_radius)
    ),
  
  gps_tagged |>
    sf::st_drop_geometry() |>
    dplyr::summarise(
      avgSpeed = mean_na(speed_kmh[valid_movement_step & moving]),
      avgTimeZhaw = mean_na(travel_times$travel_time_min)
    ),
  
  road_summary_wide
)

summary_data <- analysis |>
  dplyr::select(
    avgDistDay,
    avgTimeOutHome,
    avgRadius,
    avgSpeed,
    avgTimeZhaw
  ) |>
  tidyr::pivot_longer(
    cols = dplyr::everything(),
    names_to = "metric",
    values_to = "value"
  ) |>
  dplyr::mutate(
    metric = dplyr::case_when(
      metric == "avgDistDay" ~ "Avg. distance/day (km)",
      metric == "avgTimeOutHome" ~ "Avg. time out of home/day (h)",
      metric == "avgRadius" ~ "Avg. max. radius (km)",
      metric == "avgSpeed" ~ "Avg. travel speed (km/h)",
      metric == "avgTimeZhaw" ~ "Avg. travel time to ZHAW (min)",
      TRUE ~ metric
    )
  )

summary_data_day <- analysis_day |>
  dplyr::select(
    day,
    dist_day,
    time_out_home,
    max_radius,
    avgSpeed_day
  ) |>
  tidyr::pivot_longer(
    cols = -day,
    names_to = "metric",
    values_to = "value"
  ) |>
  dplyr::mutate(
    metric = dplyr::case_when(
      metric == "dist_day" ~ "Total distance (km)",
      metric == "time_out_home" ~ "Time out of home (h)",
      metric == "max_radius" ~ "Max. radius (km)",
      metric == "avgSpeed_day" ~ "Avg. travel speed (km/h)",
      TRUE ~ metric
    )
  )

christopher_plot_inputs <- list(
  person = "Christopher",
  analysis = analysis,
  analysis_day = analysis_day,
  summary_data = summary_data,
  summary_data_day = summary_data_day,
  pie_data = pie_data,
  travel_times = travel_times,
  home_zhaw_data = home_zhaw_data
)

saveRDS(
  christopher_plot_inputs,
  "data/processedData/christopher_plot_inputs.rds"
)

saveRDS(
  gps_tagged,
  "data/processedData/GPS_Track_christopher_tagged.rds"
)

plot_param_sum_day_christopher <- make_param_sum_day_plot(summary_data_day)

ggsave(
  "chapters/plots/param_sum_day_christopher.png",
  plot = plot_param_sum_day_christopher,
  width = 9.5,
  height = 6
)


###############################################################################
# create Christopher movement map

gps_for_movement_plot <- gps_tagged |>
  mutate(
    static = !moving
  )

gps_moving <- gps_for_movement_plot |>
  filter(!static)

if (!"segment_id" %in% names(gps_moving)) {
  stop("Column segment_id not found in gps_moving.")
}

if (nrow(gps_moving) >= 2) {
  df_line_christopher <- gps_moving |>
    arrange(segment_id, time) |>
    group_by(segment_id) |>
    filter(n() >= 2) |>
    summarise(do_union = FALSE, .groups = "drop") |>
    st_cast("LINESTRING")
  
  plot_tm_movement_christopher <- make_tm_movement_plot(
    df_line = df_line_christopher,
    df_sf_2056 = gps_for_movement_plot
  )
  
  tmap_save(
    plot_tm_movement_christopher,
    "chapters/plots/tm_movement_christopher.png"
  )
} else {
  warning("Not enough moving GPS points to create tm_movement_christopher.png.")
}

###############################################################################
# create Christopher plots

plot_road_type_pie_christopher <- make_road_type_pie_plot(pie_data)

ggsave(
  "chapters/plots/road_type_pie_christopher.png",
  plot = plot_road_type_pie_christopher,
  width = 8,
  height = 5
)


plot_param_sum_christopher <- make_param_sum_plot(summary_data_day)

ggsave(
  "chapters/plots/param_sum_christopher.png",
  plot = plot_param_sum_christopher,
  width = 9.5,
  height = 6
)


plot_param_sum_day_christopher <- make_param_sum_day_plot(summary_data_day)

ggsave(
  "chapters/plots/param_sum_day_christopher.png",
  plot = plot_param_sum_day_christopher,
  width = 9.5,
  height = 6
)