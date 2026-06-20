# src/scripts/create_christopher_plot_inputs.R

library(sf)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(ggplot2)

source("src/R/fixed_areas.R")
source("src/R/plot_functions.R")

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

if (nrow(gps_tagged) < 2) {
  stop("Not enough GPS points to calculate movement metrics.")
}

if (!inherits(gps_tagged$time, "POSIXct")) {
  gps_tagged$time <- ymd_hms(
    gps_tagged$time,
    tz = "Europe/Zurich"
  )
}

gps_tagged <- gps_tagged |>
  mutate(
    time_local = with_tz(time, tzone = "Europe/Zurich"),
    day = as.Date(time_local),
    moving = coalesce(moving, FALSE),
    at_home = coalesce(at_home, FALSE),
    at_zhaw = coalesce(at_zhaw, FALSE)
  )

geom <- st_geometry(gps_tagged)
n <- length(geom)

step_dist_m <- c(
  as.numeric(st_distance(
    geom[-n],
    geom[-1],
    by_element = TRUE
  )),
  NA_real_
)

dt_min <- c(
  as.numeric(difftime(
    gps_tagged$time[-1],
    gps_tagged$time[-n],
    units = "mins"
  )),
  NA_real_
)

same_day <- c(
  gps_tagged$day[-1] == gps_tagged$day[-n],
  FALSE
)

home_area <- fixed_areas |>
  filter(area_type == "home")

if (nrow(home_area) != 1) {
  stop("Expected exactly one home area.")
}

home_center <- st_centroid(home_area)

dist_home_m <- as.numeric(
  st_distance(
    gps_tagged,
    home_center
  )[, 1]
)

gps_tagged <- gps_tagged |>
  mutate(
    step_dist_m = step_dist_m,
    dt_min = dt_min,
    same_day_next = same_day,
    speed_kmh = if_else(
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
    out_home = !at_home
  )

analysis_day <- gps_tagged |>
  st_drop_geometry() |>
  group_by(day) |>
  summarise(
    dist_day = sum(
      if_else(valid_movement_step, step_dist_m / 1000, 0),
      na.rm = TRUE
    ),
    time_out_home = sum(
      if_else(valid_time_step & out_home, dt_min / 60, 0),
      na.rm = TRUE
    ),
    time_at_zhaw = sum(
      if_else(valid_time_step & at_zhaw, dt_min / 60, 0),
      na.rm = TRUE
    ),
    max_radius = max_na(dist_home) / 1000,
    avgSpeed_day = mean_na(
      speed_kmh[valid_movement_step & moving]
    ),
    .groups = "drop"
  )


pie_data_raw <- gps_tagged |>
  st_drop_geometry() |>
  filter(
    valid_time_step,
    moving,
    !is.na(transport_group)
  ) |>
  mutate(
    transport_group = case_when(
      str_to_lower(transport_group) %in% c("major_road", "major road") ~ "major_road",
      str_to_lower(transport_group) %in% c("main_road", "main road") ~ "main_road",
      str_to_lower(transport_group) %in% c("local_road", "local road") ~ "local_road",
      str_to_lower(transport_group) == "rail" ~ "rail",
      TRUE ~ transport_group
    )
  ) |>
  group_by(transport_group) |>
  summarise(
    time_min = sum(dt_min, na.rm = TRUE),
    .groups = "drop"
  )

known_transport_groups <- tibble(
  transport_group = c(
    "major_road",
    "main_road",
    "local_road",
    "rail"
  )
)

pie_data_raw <- known_transport_groups |>
  left_join(pie_data_raw, by = "transport_group") |>
  mutate(
    time_min = replace_na(time_min, 0)
  )

total_transport_time <- sum(pie_data_raw$time_min, na.rm = TRUE)

pie_data_raw$share <- if (total_transport_time > 0) {
  pie_data_raw$time_min / total_transport_time
} else {
  rep(NA_real_, nrow(pie_data_raw))
}

road_summary_wide <- pie_data_raw |>
  select(transport_group, share) |>
  pivot_wider(
    names_from = transport_group,
    values_from = share,
    values_fill = 0
  )

pie_data <- pie_data_raw |>
  select(transport_group, share) |>
  mutate(
    transport_group = case_when(
      transport_group == "major_road" ~ "Major road",
      transport_group == "main_road" ~ "Main road",
      transport_group == "local_road" ~ "Local road",
      transport_group == "rail" ~ "Rail",
      TRUE ~ transport_group
    )
  )


analysis <- bind_cols(
  analysis_day |>
    summarise(
      avgDistDay = mean_na(dist_day),
      avgTimeOutHome = mean_na(time_out_home),
      avgRadius = mean_na(max_radius)
    ),
  gps_tagged |>
    st_drop_geometry() |>
    summarise(
      avgSpeed = mean_na(speed_kmh[valid_movement_step & moving]),
      avgTimeZhaw = NA_real_
    ),
  road_summary_wide
)

summary_data <- analysis |>
  select(
    avgDistDay,
    avgTimeOutHome,
    avgRadius,
    avgSpeed,
    avgTimeZhaw
  ) |>
  pivot_longer(
    cols = everything(),
    names_to = "metric",
    values_to = "value"
  ) |>
  mutate(
    metric = case_when(
      metric == "avgDistDay" ~ "Avg. distance/day (km)",
      metric == "avgTimeOutHome" ~ "Avg. time out of home/day (h)",
      metric == "avgRadius" ~ "Avg. max. radius (km)",
      metric == "avgSpeed" ~ "Avg. travel speed (km/h)",
      metric == "avgTimeZhaw" ~ "Avg. travel time to ZHAW (min)",
      TRUE ~ metric
    )
  )

summary_data_day <- analysis_day |>
  select(
    day,
    dist_day,
    time_out_home,
    max_radius,
    avgSpeed_day
  ) |>
  pivot_longer(
    cols = -day,
    names_to = "metric",
    values_to = "value"
  ) |>
  mutate(
    metric = case_when(
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
  travel_times = NULL
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