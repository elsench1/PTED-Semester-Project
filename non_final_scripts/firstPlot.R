# first simple plots
library(sf)
library(tmap)
library(dplyr)

FILE <- "data/Trackingdata_Christopher/20260312-172538.gpx"

hide_point <- st_as_sf(
  data.frame(
    lon = 8 + 31/60 + 10.65/3600, # closest bus stop
    lat = 47 + 9/60 + 25.19/3600
  ),
  coords = c("lon", "lat"),
  crs = 4326
) |>
  st_transform(crs = 2056)

GPS_Track <- st_read(FILE, layer = "track_points") |> 
  st_transform(crs = 2056) |> 
  mutate(
    hide = lengths(st_is_within_distance(
      geometry,
      st_geometry(hide_point),
      dist = 150
      )) > 0
  ) |> 
  filter(!hide) # for privacy 

GPS_Track_line <- GPS_Track |> 
  summarise(do_union = FALSE) |> 
  st_cast("LINESTRING")

tmap_mode("view")

tm_shape(GPS_Track_line) +
  tm_lines(col = "blue", lwd = 5)
