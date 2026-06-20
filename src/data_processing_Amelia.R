library(sf)
library(jsonlite)
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)
library(tmap)
library(ggplot2)
library(lubridate)
library(osmextract)
library(leaflet)
library(data.table)


source("src/R/plot_functions.R")

theme_set(theme_minimal())

###############################################################################################################################
# loading data


records_json <- jsonlite::read_json("data/rawData/Trackingdata_Amelia/Zeitachse_google_timeline.json",simplifyVector = TRUE)

records <- records_json[[1]]

semanticSegments_timeline <- records_json$semanticSegments
df_timeline <- map_dfr(semanticSegments_timeline$timelinePath, \(x)x)                


rawSignal <- records_json$rawSignals  

df <- rawSignal$position

df <- df |> 
  filter(!is.na(LatLng))  

df$time <- lubridate::ymd_hms(df$timestamp, tz = "Europe/Zurich")
df$date <- as.Date(lubridate::ymd_hms(df$timestamp, tz = "Europe/Zurich"))



#activity data
segments <- data.frame(
  start = lubridate::ymd_hms(semanticSegments_timeline$startTime, tz = "Europe/Zurich"),
  end   = lubridate::ymd_hms(semanticSegments_timeline$endTime, tz = "Europe/Zurich"),
  activity = semanticSegments_timeline$activity$topCandidate$type
)

setDT(df)
setDT(segments)

df[
  segments,
  on = .(time >= start, time <= end),
  Activity := i.activity
]


###############################################################################################################################
# transformation and error 

df <- df |> 
  separate_wider_delim(LatLng, ", ", names = c("lat","lon")) |> 
  mutate(
    lat = as.numeric(str_remove(lat, "°")),
    lon = as.numeric(str_remove(lon, "°"))
  )


df <- df |> 
  filter(time != "2026-04-11 18:37:26",
         time != "2026-04-21 18:34:16")


df_sf <- df |> 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) 



df_sf_2056 <- st_transform(df_sf, 2056)
df_sf_2056$E <- st_coordinates(df_sf_2056)[,1]   
df_sf_2056$N <- st_coordinates(df_sf_2056)[,2]   


tmap_mode("view")
# tm_shape(df_sf_2056) + tm_dots()




###############################################################################################################################
# segmentation

distance_by_element <- function(later, now) {
  as.numeric(
    st_distance(later, now, by_element = TRUE)
  )
}

time_distance_by_element <- function(later, now, units = "mins") {
  as.numeric(
    difftime(later, now, units = units)
  )
}


df_sf_2056 <- df_sf_2056 |>
  mutate(
    #nMinus2_dist = distance_by_element(lag(geometry, n = 2), geometry),  # distance to pos -2 
    nMinus1_dist = distance_by_element(lag(geometry, 1), geometry),  # distance to pos -1
    nPlus1_dist  = distance_by_element(geometry, lead(geometry, 1)) # distance to pos +1
    #nPlus2_dist  = distance_by_element(geometry, lead(geometry, 2))  # distance to pos +2
  )


df_sf_2056 <- df_sf_2056 |>
  mutate(
    #nMinus2_time = -time_distance_by_element(lag(time, 2), time),   # Zeitdifferenz zu -2
    nMinus1_time = -time_distance_by_element(lag(time, 1), time),   # Zeitdifferenz zu -1
    nPlus1_time  = -time_distance_by_element(time, lead(time, 1))  # Zeitdifferenz zu +1
    #nPlus2_time  = -time_distance_by_element(time, lead(time, 2))   # Zeitdifferenz zu +2
  )



df_sf_2056 <- df_sf_2056 |>
  rowwise() |>
  mutate(
    stepMean_dist = mean(c(nMinus1_dist, nPlus1_dist), na.rm = FALSE),
    stepMean_time = mean(c(nMinus1_time, nPlus1_time), na.rm = FALSE)
    #stepMean = mean(c(nMinus2_dist, nMinus1_dist, nPlus1_dist, nPlus2_dist), na.rm = FALSE)
  ) |>
  ungroup()


df_sf_2056 <- df_sf_2056 |> 
  mutate(day = day(time))


rle_id <- function(vec) {
  x <- rle(vec)$lengths
  as.factor(rep(seq_along(x), times = x))
}


threshold <- 100 #mean(df_sf_2056$stepMean, na.rm = TRUE)    # m


df_sf_2056 <- df_sf_2056 |>
  mutate(static = stepMean_dist < threshold)

# ggplot(df_sf_2056) +
#   geom_path(aes(E,N))+
#   geom_sf(aes(color=static))
# 


df_sf_2056 <- df_sf_2056 |>
  mutate(segment_id = rle_id(static))


df_sf_2056 <- df_sf_2056 %>%
  mutate(moving_ext = !static |
           lead(!static, default = FALSE) |
           lag(!static, default = FALSE),
         move_id = rle_id(!moving_ext)
  )



df_sf_2056 <- df_sf_2056 |>
  mutate(segment_id = rle_id(static))




###############################################################################################################################
# home range

home_est <- df_sf_2056 |>
  group_by(E, N) |>
  summarise(n = n(), .groups = "drop") |>
  slice_max(n, n = 1)


home_point <- home_est |> select(E, N)


df_sf_2056 <- df_sf_2056 |>
  mutate(
    dist_home = sqrt(
      (E - home_point$E)^2 +
        (N - home_point$N)^2
    )
  )


df_sf_2056 <- df_sf_2056 |>
  mutate(out_home = dist_home > 80)

home_point$E <- 2706285
home_point$N <- 1231915
st_geometry(home_point)[1] <- st_sfc(st_point(c(home_point$E, home_point$N)), crs = st_crs(home_point))


# tm_shape(df_sf_2056) +
#   tm_dots(col = "out_home",
#           palette = c("blue", "red"),
#           size = 0.05,
#           title = "Out of home") +
#   tm_lines()+
#   tm_shape(home_point) +
#   tm_dots(col = "black", size = 0.2)

home_geom <- st_geometry(home_point)[[1]]

df_sf_2056 <- df_sf_2056 %>%
  mutate(geometry = replace(geometry, out_home == FALSE, st_sfc(rep(list(home_geom), sum(!out_home)), crs = st_crs(df_sf_2056))))



###############################################################################################################################
# road type matching

# OSM Daten laden 

roads <- oe_get(
  place = "switzerland",
  layer = "lines"
)

network_sf <- roads |>
  mutate(transport_type = coalesce(highway, railway)) |>
  filter(transport_type %in% c(
    "motorway",
    "trunk",
    "primary",
    "secondary",
    "tertiary",
    "residential",
    "unclassified",
    "service",
    "rail", "tram", "light_rail", "subway"
  ))


pts <- st_as_sf(df_sf_2056, coords = c("E", "N"), crs = 2056)
bbox <- st_bbox(pts)


network_sf <- st_transform(network_sf, st_crs(pts))
network_sf <- st_crop(network_sf, bbox)


nearest_idx <- st_nearest_feature(pts, network_sf)


df_sf_2056$transport_type_osm <- network_sf$transport_type[nearest_idx]
df_sf_2056$transport_type <- df_sf_2056$transport_type_osm
df_sf_2056$transport_type[df_sf_2056$moving_ext == FALSE] <- "static"
df_sf_2056$transport_type_move <- df_sf_2056$transport_type
df_sf_2056$transport_type_move[df_sf_2056$moving_ext == FALSE] <- NA


df_sf_2056 <- df_sf_2056 |>
  mutate(
    transport_group = case_when(
      transport_type_move %in% c("motorway", "trunk") ~ "major_road",
      transport_type_move %in% c("primary", "secondary", "tertiary") ~ "main_road",
      transport_type_move %in% c("residential", "service", "unclassified") ~ "local_road",
      transport_type_move %in% c("rail", "tram", "light_rail", "subway") ~ "rail",
      TRUE ~ NA_character_
    )
  )


road_time_day <- df_sf_2056 |>
  filter(!is.na(transport_group)) |>
  group_by(day, transport_group) |>
  summarise(
    time_min = sum(stepMean_time, na.rm = TRUE),  
    .groups = "drop"
  )|> 
  st_drop_geometry()


road_share_day_time <- road_time_day |>
  group_by(day) |>
  mutate(
    share = time_min / sum(time_min)     
  ) |>
  st_drop_geometry()|>
  select(day, transport_group, share)



road_share_wide <- road_share_day_time |>
  pivot_wider(
    names_from = transport_group,
    values_from = share,
    values_fill = 0
  )




# road_counts_day <- df_sf_2056 |>
#   filter(!is.na(transport_group)) |>
#   group_by(day, transport_group) |>
#   summarise(n = n(), .groups = "drop") |> 
#   st_drop_geometry()


# road_share_day_count <- df_sf_2056 |>
#   filter(!is.na(transport_group)) |>
#   group_by(day, transport_group) |>
#   summarise(n = n(), .groups = "drop") |>
#   group_by(day) |>
#   mutate(share = n / sum(n)) |>
#   ungroup() |>
#   st_drop_geometry()


# road_counts_wide <- road_counts_day |>
#   pivot_wider(names_from = transport_group, values_from = n, values_fill = 0)


# road_summary <- df_sf_2056 |>
#   filter(!is.na(transport_group)) |>
#   group_by(transport_group) |>
#   summarise(n = n(), .groups = "drop") |>
#   mutate(share = n / sum(n))|>
#   st_drop_geometry()



road_summary <- df_sf_2056 |>    # oder anstatt transport_group, transport_type_move verwenden
  filter(!is.na(transport_group)) |>
  group_by(transport_group) |>
  summarise(
    time_min = sum(stepMean_time, na.rm = TRUE), 
    .groups = "drop"
  ) |>
  mutate(
    share = time_min / sum(time_min)
  )|>
  st_drop_geometry()

road_summary_wide <- road_summary |>
  select(transport_group, share) |>
  tidyr::pivot_wider(
    names_from = transport_group,
    values_from = share,
    values_fill = 0
  )
  



# visualisieren
pts$transport_group <- df_sf_2056$transport_group[match(pts$geometry, df_sf_2056$geometry)]
pts$transport_type <- network_sf$transport_type[nearest_idx]

pts_filtered <- pts |> 
 filter(moving_ext == TRUE)

pts_map <- st_transform(pts_filtered, 4326)
pts_map <- pts_map |>
  mutate(
    transport_group_label = case_when(
      transport_group == "major_road" ~ "Major road",
      transport_group == "main_road"  ~ "Main road",
      transport_group == "local_road" ~ "Local road",
      transport_group == "rail"       ~ "Rail",
      TRUE ~ transport_group)
  )

network_map <- st_transform(network_sf, 4326)

pal <- colorFactor(palette = "Set3", domain = pts_map$transport_group_label)  #oder Set20
pal2 <- colorFactor(palette = "Set3", domain = pts_map$transport_type)  #oder Set20


plot_osm_moving <- leaflet() |>
  addProviderTiles("OpenStreetMap") |>
  addPolylines(data = network_map, color = "grey70", weight = 1) |>
  addCircleMarkers(data = pts_map, radius = 4, color = "black", weight = 1, fillColor = ~pal(transport_group_label), fillOpacity = 1, stroke = TRUE) |>
  addLegend("bottomright", pal = pal, values = pts_map$transport_group_label, title = "Matched road type")
plot_osm_moving

# plot_osm_moving_type <- leaflet() |>
#   addProviderTiles("OpenStreetMap") |>
#   addPolylines(data = network_map, color = "grey70", weight = 1) |>
#   addCircleMarkers(data = pts_map, radius = 3, color = "black", weight = 1, fillColor = ~pal(transport_type), fillOpacity = 1, stroke = TRUE) |>
#   addLegend("bottomright", pal = pal2, values = pts_map$transport_type, title = "Transport type")
# 



###############################################################################################################################
# comparison road_type and google Timeline Activity
  

tab_grouped <- table(
  Activity = df_sf_2056$Activity,
  TransportGroup = df_sf_2056$transport_group,
  useNA = "no"
)

round(prop.table(tab_grouped, margin = 1), 2)




###############################################################################################################################
# traveltime from home to ZHAW

df_sel <- df_sf_2056 |>
  filter(
    (weekdays(time) == "Dienstag" &
       lubridate::hour(time) >= 12 &
       lubridate::hour(time) <= 17) |
      (weekdays(time) == "Mittwoch" &
         lubridate::hour(time) >= 7 &
         lubridate::hour(time) <= 12)
  )

uni_est <- df_sel |>
  mutate(
    E_r = round(E, 1),
    N_r = round(N, 1)
  ) |>
  group_by(E_r, N_r) |>
  summarise(n = n(), .groups = "drop") |>
  slice_max(n, n = 1)

uni_point <- uni_est |>
  summarise(
    E_uni = mean(E_r),
    N_uni = mean(N_r)
  )

df_sf_2056 <- df_sf_2056 |>
  mutate(
    dist_uni = sqrt((E - uni_point$E_uni)^2 +
                      (N - uni_point$N_uni)^2)
  )

df_sf_2056 <- df_sf_2056 |>
  mutate(
    at_home = dist_home < 250,
    at_uni  = dist_uni < 250
  )



df_zhaw <- df_sf_2056 |>
  st_drop_geometry() |>
  arrange(time) |>
  mutate(
    state = case_when(
      at_home ~ "home",
      at_uni  ~ "uni",
      TRUE ~ "other"
    )
  )


# Helper: sichere Zeit-Auswahl
safe_min <- function(time, condition) {
  vals <- time[condition]
  if (length(vals) == 0) return(NA)
  min(vals)
}

safe_max_before <- function(time, condition, before_time) {
  vals <- time[condition & time < before_time]
  if (length(vals) == 0) return(NA)
  max(vals)
}

safe_min_after <- function(time, condition, after_time) { 
  vals <- time[condition & time > after_time] 
  if (length(vals) == 0) return(NA) 
  min(vals) } 

safe_max <- function(time, condition) { 
  vals <- time[condition] 
  if (length(vals) == 0) return(NA) 
  max(vals) }


# pro Tag 
travel_times <- df_zhaw |>
  group_by(date = as.Date(time)) |>
  summarise(
    arrive_uni = safe_min(time, state == "uni"),
    depart_home = safe_max_before(time, state == "home", arrive_uni),
    travel_time_to_uni =
      as.numeric(difftime(arrive_uni, depart_home, units = "mins")),
    leave_uni = safe_max(time, state == "uni"),
    arrive_home = safe_min_after(time, state == "home", leave_uni),
    travel_time_home =
      as.numeric(difftime(arrive_home, leave_uni, units = "mins")),
    
    .groups = "drop"
  )  |> 
  mutate(
    travel_time_to_uni =
      if_else(travel_time_to_uni > 200, NA_real_, travel_time_to_uni),
    travel_time_home =
      if_else(travel_time_home > 200, NA_real_, travel_time_home)
  )

df_sf_2056 <- df_sf_2056 |>
  mutate(date = as.Date(time)) |>
  left_join(
    travel_times |> select(date, travel_time_to_uni, travel_time_home),
    by = "date"
  )




###############################################################################################################################
# other research questions


# average speed
df_sf_2056 <- df_sf_2056 |> 
  mutate(speed_after = (nPlus1_dist/1000)/(nPlus1_time/60),         #m/min -> km/h
         speed_before = (nMinus1_dist/1000)/(nMinus1_time/60),
         speed2 = (stepMean_dist/1000)/(stepMean_time/60)
  )



# distance per day
df_sf_2056 <- df_sf_2056 |>
  group_by(day) |>
  mutate(dist_day = sum(stepMean_dist/1000, na.rm = TRUE)) |>    #m -> km
  ungroup()



# time out of home
df_sf_2056 <- df_sf_2056 |>
  group_by(day) |>
  mutate(
    time_out_home = sum(nPlus1_time[out_home], na.rm = TRUE)/60   #min -> h
  ) |>
  ungroup()

#max movement radius per day / per segment
df_sf_2056 <- df_sf_2056 |>
  group_by(day) |>
  mutate(
    max_radius = max(dist_home, na.rm = TRUE)/1000   #m -> km
  ) |>
  ungroup()

df_sf_2056 <- df_sf_2056 |>
  group_by(move_id) |>
  mutate(
    max_radius_movement = max(dist_home, na.rm = TRUE)/1000   #m -> km
  ) |>
  ungroup()





analysis_day <- df_sf_2056 |> 
  group_by(day) |> 
  summarise(
    dist_day = sum(stepMean_dist / 1000, na.rm = TRUE),               #km
    time_out_home = sum(nPlus1_time[out_home], na.rm = TRUE) / 60,  #h
    max_radius = max(dist_home, na.rm = TRUE) / 1000,               #km
    max_radius_movement = max(max_radius_movement, na.rm = TRUE),   #km
    avgSpeed_day = mean(speed2[moving_ext == TRUE], na.rm = TRUE),     #km/h
    .groups = "drop"
  )|>
  left_join(road_share_wide, by = "day") |> 
  st_drop_geometry()


analysis <- bind_cols(
  analysis_day |> 
    summarise(
      avgDistDay = mean(dist_day, na.rm = TRUE),                    #km
      avgTimeOutHome = mean(time_out_home, na.rm = TRUE),           #h
      avgRadius = mean(max_radius, na.rm = TRUE)                    #km
    ) |> 
    st_drop_geometry(),
  
  df_sf_2056 |> 
    summarise(
      avgSpeed = mean(speed2[moving_ext == TRUE], na.rm = TRUE),                 #km/h
      avgTimeZhaw = mean(c(travel_time_to_uni, travel_time_home), na.rm = TRUE)  #min
    ) |> 
    st_drop_geometry(),
  
  road_summary_wide
)



###############################################################################################################################
# filter movement

df_filter <- df_sf_2056 |>
   filter(!static)

# df_filter |>
#   ggplot(aes(E, N)) +
#   geom_point(data = df_sf_2056, col = "red") +
#   geom_path() +
#   geom_point() +
#   coord_fixed() +
#   theme(legend.position = "bottom")



tmap_mode("view")


df_filter_sf <- st_as_sf(df_filter, coords = c("E", "N"), crs = 2056)


df_line <- df_filter_sf |>
  dplyr::summarise(do_union = FALSE) |>
  st_cast("LINESTRING")




###############################################################################################################################
# test plot

# test <- df_sf_2056 |> 
#   filter(segment_id == 79)
# 
# test_sf <- st_as_sf(test, coords = c("E", "N"), crs = 2056)
# 
# 
# test_line <- test_sf |>
#   dplyr::summarise(do_union = FALSE) |>
#   st_cast("LINESTRING")
# 
# tm_shape(test_line) +
#   tm_lines(col = "black", lwd = 0.8) +
#   tm_shape(test) +
#   tm_dots(
#     col = "at_uni",
#     palette = c("TRUE" = "red", "FALSE" = "blue"),
#     size = 0.3
#   )


###############################################################################################################################
# preparation visualisations

pie_data <- analysis |>
  select(any_of(c("major_road", "main_road", "local_road", "rail"))) |>
  pivot_longer(
    cols = everything(),
    names_to = "transport_group",
    values_to = "share"
  ) |>
  mutate(transport_group = case_when(
    transport_group == "major_road" ~ "Major road",
    transport_group == "main_road"  ~ "Main road",
    transport_group == "local_road" ~ "Local road",
    transport_group == "rail"       ~ "Rail",
    TRUE ~ transport_group
  ))





summary_data <- analysis |>
  select(avgDistDay, avgTimeOutHome, avgRadius, avgSpeed, avgTimeZhaw) |>
  pivot_longer(
    cols = everything(),
    names_to = "metric",
    values_to = "value"
  ) |>
  mutate(metric = case_when(
    metric == "avgDistDay"     ~ "Avg. distance/day (km)",
    metric == "avgTimeOutHome" ~ "Avg. time out of home/day (h)",
    metric == "avgRadius"      ~ "Avg. max. radius (km)",
    metric == "avgSpeed"       ~ "Avg. travel speed (km/h)",
    metric == "avgTimeZhaw"    ~ "Avg. travel time to ZHAW (min)",
    TRUE ~ metric
  ))



summary_data_day <- analysis_day |>
  select(day, dist_day, time_out_home, max_radius, avgSpeed_day) |>
  pivot_longer(
    cols = -day,
    names_to = "metric",
    values_to = "value"
  ) |>
  mutate(metric = case_when(
    metric == "dist_day"              ~ "Total distance (km)", 
    metric == "time_out_home"         ~ "Time out of home (h)",
    metric == "max_radius"            ~ "Max. radius (km)",
    metric == "avgSpeed_day"          ~ "Avg. travel speed (km/h)",
    TRUE ~ metric
  ))



###############################################################################################################################
# visualisation

# tm_shape(df_line) +
#   tm_lines(col = "black", lwd = 0.8) +
#   tm_shape(df_sf_2056) +
#   tm_dots(
#     col = "static",
#     palette = c("TRUE" = "red", "FALSE" = "blue"),
#     size = 0.3
#   )
# 
# 
# 
# tm_shape(df_line) +
#   tm_shape(df_sf_2056[df_sf_2056$static, ]) +
#   tm_dots(
#     col = "red",
#     size = 0.3
#   )
# 




# plot_param_sum <- ggplot(summary_data, aes(x = metric, y = value, fill = metric)) +
#   geom_col(width = 0.6, show.legend = FALSE) +
#   geom_text(aes(label = round(value, 1)), vjust = 1.5, color = "white", fontface = "bold") +
#   facet_wrap(~ metric, scales = "free", ncol = 3, strip.position = "bottom") +
#   labs(x = NULL, y = NULL, title = "Comparison moving parameters") +
#   theme(
#     axis.text.x = element_blank(),
#     axis.ticks.x = element_blank(),
#     strip.placement = "outside",
#     strip.text = element_text(face = "bold", size = 12),
#     plot.title = element_text(hjust = 0.5, size = 14, margin = margin(b = 20)),
#     plot.margin = margin(t = 20, r = 10, b = 10, l = 10)
#   )


plot_param_sum <- ggplot(summary_data_day, aes(x = metric, y = value, fill = metric)) +
  geom_boxplot(show.legend = FALSE, width = 0.5) +
  facet_wrap(~ metric, scales = "free", ncol = 3, strip.position = "bottom") +
  labs(x = NULL, y = NULL, title = "Comparison moving parameters") +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    strip.placement = "outside",
    strip.text = element_text(face = "bold", size = 12),
    plot.title = element_text(hjust = 0.5, size = 14, margin = margin(b = 20)),
    plot.margin = margin(t = 20, r = 10, b = 10, l = 10)
  )




 
# plot_tm_movement_2 <- tm_basemap("CartoDB.Positron") +
#   #tm_shape(df_line) +
#   #tm_lines(col = "black", lwd = 0.8) +
#   tm_shape(subset(df_sf_2056, !static)) +
#   tm_dots(
#     fill = "day",
#     fill.scale = tm_scale(values = "brewer.blues", values.range = c(0.4, 1)),
#     fill.legend = tm_legend(title = "Moving, days of tracking"),
#     size = 0.5
#   ) +
#   tm_shape(subset(df_sf_2056, static)) +
#   tm_dots(
#     fill = "day",
#     fill.scale = tm_scale(values = "brewer.reds", values.range = c(0.4, 1)),
#     fill.legend = tm_legend(title = "Static, days of tracking"),
#     size = 0.5
#   )
# 
# tmap_save(plot_tm_movement_2, "chapters/plots/tm_movement_2.png")









# Vorlagen

# tm_basemap("CartoDB.Positron")
# tm_basemap("OpenStreetMap")

# tmap_save(
#   tm        = name,
#   filename  = "karte.png",
#   width     = 10,        # Breite in Zoll (oder Pixel wenn dpi angegeben)
#   height    = 7,         # Höhe in Zoll
#   dpi       = 300,       # Auflösung (für Druck: 300+)
#   units     = "in",      # "in", "cm", "mm", "px"
#   asp       = 0          # 0 = Seitenverhältnis automatisch anpassen
# )
# 
# ggsave(
#   filename = "plot.pdf",   # Format wird aus Dateiendung erkannt
#   plot     = name,
#   width    = 10,
#   height   = 7,
#   units    = "in",         # "in", "cm", "mm", "px"
#   dpi      = 300           # irrelevant für PDF/SVG (Vektorgrafik)
# )



###############################################################################
# visualisation

dir.create("chapters/plots", recursive = TRUE, showWarnings = FALSE)

plot_tm_movement <- make_tm_movement_plot(
  df_line = df_line,
  df_sf_2056 = df_sf_2056
)

tmap::tmap_save(
  plot_tm_movement,
  "chapters/plots/tm_movement.png"
)


plot_activity_road_type <- make_activity_road_type_plot(df_sf_2056)

ggplot2::ggsave(
  "chapters/plots/activity_road_type.png",
  plot = plot_activity_road_type,
  width = 6,
  height = 5
)


plot_road_type_pie <- make_road_type_pie_plot(pie_data)

ggplot2::ggsave(
  "chapters/plots/road_type_pie.png",
  plot = plot_road_type_pie,
  width = 8,
  height = 5
)


plot_param_sum <- make_param_sum_plot(summary_data_day)


plot_param_sum_day <- make_param_sum_day_plot(summary_data_day)

ggplot2::ggsave(
  "chapters/plots/param_sum_day.png",
  plot = plot_param_sum_day,
  width = 9.5,
  height = 6
)

