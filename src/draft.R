# Draft

# source("src/R/move_points_to_circle_exit.R")
# source("src/R/dataPreparation.R")
# source("src/R/gps_track_segmentation.R")
# source("src/R/compress_stop_blocks.R")
# source("src/R/osm_transport_matching.R")

###############################################################################

# library(sf)
# library(tmap)
# 
# convert_csv_to_gpkg <- function(
#     csv_file = NULL,
#     gpkg_location = NULL
# ){
#   if(!csv_file|| !gpkg_location){
#     print("file error")
#     return(NULL)
#   }
#   # CSV korrekt laden: Semikolon als Trennzeichen, Punkt als Dezimalzeichen
#   dat <- read.csv(
#     csv_file,
#     sep = ";",
#     dec = ".",
#     stringsAsFactors = FALSE
#   )
#   
#   # Nur benötigte Koordinaten behalten
#   coords <- dat[, c("Easting", "Northing")]
#   
#   # Sicherstellen, dass die Koordinaten numerisch sind
#   coords$Easting  <- as.numeric(coords$Easting)
#   coords$Northing <- as.numeric(coords$Northing)
#   
#   # Zeilen mit fehlenden Koordinaten entfernen, falls vorhanden
#   coords <- coords[complete.cases(coords), ]
#   
#   # Polygonring schliessen, falls nötig
#   if (!all(coords[1, ] == coords[nrow(coords), ])) {
#     coords <- rbind(coords, coords[1, ])
#   }
#   
#   # Polygon erzeugen
#   poly <- st_polygon(list(as.matrix(coords)))
#   
#   # sf-Objekt mit CRS 2056
#   zhaw_gruental <- st_sf(
#     name = "ZHAW Grüental",
#     geometry = st_sfc(poly, crs = 2056)
#   )
#   
#   st_write(
#     zhaw_gruental,
#     gpkg_location,
#     delete_dsn = TRUE
#   )
#   
# }
# 
# convert_csv_to_gpkg <- function(
#     csv_file,
#     gpkg_location,
#     crs = 2056,
#     name = "ZHAW Grüental",
#     layer_name = NULL,
#     overwrite = TRUE,
#     make_valid = TRUE,
#     quiet = TRUE
# ) {
#   # Paket prüfen
#   if (!requireNamespace("sf", quietly = TRUE)) {
#     stop("Das Paket 'sf' ist nicht installiert. Bitte zuerst install.packages('sf') ausführen.")
#   }
#   
#   # Eingaben prüfen
#   if (missing(csv_file) || is.null(csv_file) || length(csv_file) != 1 || !nzchar(csv_file)) {
#     stop("'csv_file' muss ein gültiger Dateipfad sein.")
#   }
#   
#   if (missing(gpkg_location) || is.null(gpkg_location) || length(gpkg_location) != 1 || !nzchar(gpkg_location)) {
#     stop("'gpkg_location' muss ein gültiger Ausgabepfad sein.")
#   }
#   
#   if (!file.exists(csv_file)) {
#     stop("Die CSV-Datei existiert nicht: ", csv_file)
#   }
#   
#   if (dir.exists(csv_file)) {
#     stop("'csv_file' zeigt auf einen Ordner, nicht auf eine Datei: ", csv_file)
#   }
#   
#   if (!grepl("\\.gpkg$", gpkg_location, ignore.case = TRUE)) {
#     stop("'gpkg_location' sollte auf '.gpkg' enden, z.B. 'zhaw_gruental.gpkg'.")
#   }
#   
#   # Ausgabeordner prüfen / erstellen
#   output_dir <- dirname(gpkg_location)
#   
#   if (!dir.exists(output_dir)) {
#     dir_created <- dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
#     
#     if (!dir_created) {
#       stop("Ausgabeordner konnte nicht erstellt werden: ", output_dir)
#     }
#   }
#   
#   # Layername aus Dateiname ableiten, falls nicht angegeben
#   if (is.null(layer_name)) {
#     layer_name <- tools::file_path_sans_ext(basename(gpkg_location))
#   }
#   
#   # CSV lesen
#   dat <- tryCatch(
#     {
#       read.csv(
#         csv_file,
#         sep = ";",
#         dec = ".",
#         stringsAsFactors = FALSE
#       )
#     },
#     error = function(e) {
#       stop("CSV konnte nicht gelesen werden: ", conditionMessage(e))
#     }
#   )
#   
#   # Prüfen, ob benötigte Spalten vorhanden sind
#   required_cols <- c("Easting", "Northing")
#   missing_cols <- setdiff(required_cols, names(dat))
#   
#   if (length(missing_cols) > 0) {
#     stop(
#       "Folgende benötigte Spalten fehlen in der CSV: ",
#       paste(missing_cols, collapse = ", ")
#     )
#   }
#   
#   # Nur benötigte Koordinaten behalten
#   coords <- dat[, required_cols]
#   
#   # Koordinaten robust in numerisch umwandeln
#   coords$Easting <- as.numeric(
#     gsub(",", ".", gsub("'", "", trimws(as.character(coords$Easting))))
#   )
#   
#   coords$Northing <- as.numeric(
#     gsub(",", ".", gsub("'", "", trimws(as.character(coords$Northing))))
#   )
#   
#   # Ungültige Koordinaten entfernen
#   invalid_rows <- !complete.cases(coords)
#   
#   if (all(invalid_rows)) {
#     stop("Keine gültigen Koordinaten gefunden. Prüfe 'Easting' und 'Northing'.")
#   }
#   
#   if (any(invalid_rows)) {
#     warning(sum(invalid_rows), " Zeile(n) mit ungültigen Koordinaten wurden entfernt.")
#     coords <- coords[!invalid_rows, , drop = FALSE]
#   }
#   
#   # Prüfen, ob genug Punkte für ein Polygon vorhanden sind
#   if (nrow(coords) < 3) {
#     stop("Für ein Polygon werden mindestens 3 gültige Koordinatenpunkte benötigt.")
#   }
#   
#   xy <- as.matrix(coords)
#   storage.mode(xy) <- "double"
#   
#   # Prüfen, ob mindestens 3 unterschiedliche Punkte vorhanden sind
#   if (nrow(unique(xy)) < 3) {
#     stop("Für ein Polygon werden mindestens 3 unterschiedliche Punkte benötigt.")
#   }
#   
#   # Polygonring schliessen, falls nötig
#   if (!isTRUE(all.equal(xy[1, ], xy[nrow(xy), ], tolerance = 1e-8))) {
#     xy <- rbind(xy, xy[1, ])
#   }
#   
#   # Polygon erzeugen
#   poly <- sf::st_polygon(list(xy))
#   
#   # sf-Objekt mit CRS erzeugen
#   polygon_sf <- sf::st_sf(
#     name = name,
#     geometry = sf::st_sfc(poly, crs = crs)
#   )
#   
#   # Geometrie prüfen und optional reparieren
#   if (!all(sf::st_is_valid(polygon_sf))) {
#     if (make_valid) {
#       warning("Polygon war geometrisch nicht gültig und wird mit st_make_valid() repariert.")
#       polygon_sf <- sf::st_make_valid(polygon_sf)
#     } else {
#       stop("Polygon ist geometrisch nicht gültig.")
#     }
#   }
#   
#   # GeoPackage schreiben
#   tryCatch(
#     {
#       sf::st_write(
#         polygon_sf,
#         dsn = gpkg_location,
#         layer = layer_name,
#         delete_layer = overwrite,
#         quiet = quiet
#       )
#     },
#     error = function(e) {
#       stop("GeoPackage konnte nicht geschrieben werden: ", conditionMessage(e))
#     }
#   )
#   
#   message("GeoPackage erfolgreich gespeichert: ", gpkg_location)
#   
#   invisible(polygon_sf)
# }
# 
# # CSV korrekt laden: Semikolon als Trennzeichen, Punkt als Dezimalzeichen
# dat <- read.csv(
#   "data/metaData/ZHAW_Gruental.csv",
#   sep = ";",
#   dec = ".",
#   stringsAsFactors = FALSE
# )
# 
# # Nur benötigte Koordinaten behalten
# coords <- dat[, c("Easting", "Northing")]
# 
# # Sicherstellen, dass die Koordinaten numerisch sind
# coords$Easting  <- as.numeric(coords$Easting)
# coords$Northing <- as.numeric(coords$Northing)
# 
# # Zeilen mit fehlenden Koordinaten entfernen, falls vorhanden
# coords <- coords[complete.cases(coords), ]
# 
# # Polygonring schliessen, falls nötig
# if (!all(coords[1, ] == coords[nrow(coords), ])) {
#   coords <- rbind(coords, coords[1, ])
# }
# 
# # Polygon erzeugen
# poly <- st_polygon(list(as.matrix(coords)))
# 
# # sf-Objekt mit CRS 2056
# zhaw_gruental <- st_sf(
#   name = "ZHAW Grüental",
#   geometry = st_sfc(poly, crs = 2056)
# )

source("src/R/convert_csv_to_gpkg.R")

gruental <- convert_csv_to_gpkg(
  csv_file = "data/metaData/ZHAW_Gruental.csv",
  gpkg_location = "data/metaData/zhaw_gruental.gpkg"
)

reidbach <- convert_csv_to_gpkg(
  csv_file = "data/metaData/ZHAW_Reidbach_A.csv",
  gpkg_location = "data/metaData/ZHAW_Reidbach_A.gpkg"
)


# zhaw_gruental <- st_read("data/metaData/zhaw_gruental.gpkg")
# 
# Anzeigen
tmap_mode("view")

tm_shape(gruental) +
  tm_polygons(
    fill = "lightblue",
    fill_alpha = 0.4,
    col = "black"
  ) + 
  tm_shape(reidbach) +
  tm_polygons(
    fill = "lightgreen",
    fill_alpha = 0.4,
    col = "black"
  )

# st_write(
#   zhaw_gruental,
#   "data/metaData/zhaw_gruental.gpkg",
#   delete_dsn = TRUE
# )



###############################################################################
# 
# GPXFileLink <- "data/rawData/Trackingdata_Christopher/20260312-172538.gpx"
# 
# 
# 
# GPS_Track <- load_GPX_File(GPXFile = GPXFileLink) |> 
#   remove_all_na_columns() |> 
#   add_speed_and_accel_to_GSP_df() |> 
#   mark_suspicious_points()
# 
# library(dplyr)
# GPS_Track <- GPS_Track |> 
#   filter(bad_point != TRUE | is.na(bad_point))
# 
# 
# library(ggplot2)
# library(dplyr)
# library(sf)
# 
# GPS_Track_plot <- GPS_Track |>
#   mutate(
#     point_type = case_when(
#       is_suspicious_spike == TRUE & suspicious_accel == TRUE ~ "both",
#       is_suspicious_spike == TRUE ~ "spike",
#       suspicious_accel == TRUE ~ "accel",
#       TRUE ~ "normal"
#     ),
#     point_type = factor(
#       point_type,
#       levels = c("normal", "spike", "accel", "both")
#     )
#   )
# 
# ggplot(GPS_Track_plot) +
#   geom_sf(aes(color = point_type), size = 1.8, alpha = 0.8) +
#   scale_color_manual(
#     values = c(
#       "normal" = "black",
#       "spike"  = "red",
#       "accel"  = "green",
#       "both"   = "blue"
#     ),
#     labels = c(
#       "normal" = "normal",
#       "spike"  = "is_suspicious_spike",
#       "accel"  = "suspicious_accel",
#       "both"   = "beides"
#     ),
#     name = "Punkttyp"
#   ) +
#   theme_minimal() +
#   labs(
#     title = "GPS Track mit markierten verdächtigen Punkten",
#     x = "Koordinate X",
#     y = "Koordinate Y"
#   )


# 
# sum(GPS_Track$is_suspicious_spike, na.rm = TRUE)
# sum(GPS_Track$suspicious_accel, na.rm = TRUE)
# sum(GPS_Track$bad_point, na.rm = TRUE)
# 
# library(tmap)

#######################################################################3

# Datei Speichern und laden
# saveRDS(GPS_Track, "data/processedData/GPS_Track_processed.rds")

# GPS_Track_test <- readRDS("data/processedData/GPS_Track_compress.rds")

############################################################################
# Eigen Regeln für gewisse Gebiete

# home <- st_sfc(
#   st_point(c(8.519625,47.156997)),
#   crs = 4326
# ) |> 
#   st_transform(crs =2056)
# 
# home_buffer <- st_buffer(home, 300)
# 
# GPS_Track <- GPS_Track |> 
#   mutate(
#     near_home = as.logical(st_intersects(geometry, home_buffer, sparse = FALSE)[, 1]),
#     bad_near_home = near_home & speed_kmh > 50
#   )
# 
# sum(GPS_Track$bad_near_home, na.rm = TRUE)
# 
# library(dplyr)
# GPS_Track <- GPS_Track |>
#   filter(bad_near_home != TRUE | is.na(bad_near_home))
# 
# 
# GPS_Track <- GPS_Track |>
#   mutate(
#     low_sat = sat < 6
#   )
# 
# sum(GPS_Track$low_sat, na.rm = TRUE)
# 
# GPS_Track <- GPS_Track |> 
#   filter(low_sat != TRUE | is.na(low_sat))

################################################################
# Segment






# GPS_Track <- detect_stops(GPS_Track)

# test <- GPS_Track[1:10000, ]
# system.time(
#   result <- detect_stops(test)
# )
# 
# 
# 
# 
# df_test <- GPS_Track[1:10000, ]
# 
# system.time({
#   result_test <- detect_stops_rcpp(df_test)
# })
# 
# 
# GPS_Track <- detect_stops_rcpp(GPS_Track)
# GPS_Track <- segment_tracks(GPS_Track)

#####################################################################

# remove stop blocks

# GPS_Track <- GPS_Track |>
#   arrange(time) |>
#   mutate(
#     bad_point = coalesce(bad_point, FALSE),
#     is_suspicious_spike = coalesce(is_suspicious_spike, FALSE),
#     suspicious_accel = coalesce(suspicious_accel, FALSE),
#     is_stop = coalesce(is_stop, FALSE),
#     moving = coalesce(moving, FALSE),
#     new_segment = if_else(row_number() == 1, TRUE, coalesce(new_segment, FALSE))
#   )
# 
# GPS_Track_compressed <- compress_stop_blocks(GPS_Track)
# 
# nrow(GPS_Track)
# nrow(GPS_Track_compressed)
# 
# GPS_Track_compressed <- GPS_Track_compressed |>
#   select(
#     -any_of(c(
#       "dt", "dx", "dy", "dist_m", "speed_calc", "speed_kmh", "accel",
#       "x_prev", "y_prev", "x_next", "y_next",
#       "dist_prev", "dist_next", "dist_skip",
#       "dt_prev", "dt_next"
#     ))
#   )
#########################################################

# library(tmap)
# library(colorspace)
# 
# GPS_segments <- GPS_segments |>
#   mutate(segment_id = as.factor(segment_id))
# 
# segment_palette <- qualitative_hcl(
#   n = nlevels(GPS_segments$segment_id),
#   palette = "Dark 3"
# )
# 
# tmap_mode("view")
# 
# tm_basemap("OpenStreetMap") +
#   tm_shape(st_transform(GPS_segments, 4326)) +
#   tm_lines(
#     col = "segment_id",
#     lwd = 3,
#     palette = segment_palette,
#     legend.col.show = FALSE,
#     popup.vars = c(
#       "Segment" = "segment_id",
#       "Start" = "start_time",
#       "Ende" = "end_time",
#       "Punkte" = "n_points"
#     )
#   )

# library(sf)
# library(dplyr)
# 
# make_segment_lines <- function(df,
#                                segment_col = "segment_id",
#                                time_col = "time",
#                                moving_col = "moving") {
#   
#   crs_original <- st_crs(df)
#   
#   df_clean <- df |>
#     filter(.data[[moving_col]]) |>
#     arrange(.data[[segment_col]], .data[[time_col]])
#   
#   df_clean |>
#     group_by(.data[[segment_col]]) |>
#     group_modify(~ {
#       
#       x <- .x |>
#         arrange(.data[[time_col]])
#       
#       coords <- st_coordinates(x)
#       
#       if (nrow(coords) < 2) {
#         return(st_sf())
#       }
#       
#       line <- st_linestring(coords[, c("X", "Y"), drop = FALSE])
#       
#       st_sf(
#         n_points = nrow(x),
#         start_time = min(x[[time_col]], na.rm = TRUE),
#         end_time = max(x[[time_col]], na.rm = TRUE),
#         geometry = st_sfc(line, crs = crs_original)
#       )
#     }) |>
#     ungroup() |>
#     rename(segment_id = !!segment_col)
# }
# 
# GPS_segments <- make_segment_lines(GPS_Track_compressed)
# 
# GPS_segments <- GPS_segments |>
#   mutate(segment_id = as.factor(segment_id))
# 
# library(tmap)
# library(colorspace)
# 
# segment_palette <- qualitative_hcl(
#   n = nlevels(GPS_segments$segment_id),
#   palette = "Dark 3"
# )
# 
# tmap_mode("view")
# 
# tm_basemap("OpenStreetMap") +
#   tm_shape(st_transform(GPS_segments, 4326)) +
#   tm_lines(
#     col = "segment_id",
#     lwd = 3,
#     palette = segment_palette,
#     legend.col.show = FALSE
#   )
#############################################################
# hide_points <- get_df_of_hide_points(
#   hide_point_csv = "data/metaData/hidePoint.csv",
#   target_crs = 2056)

# if (is.null(GPS_Track)) {
#   stop("GPX-Datei konnte nicht geladen werden.")
# }

# GPS_Track_hidden <- move_points_to_circle_exit(
#   GPS_Track = GPS_Track,
#   hide_point = hide_point,
#   dist = 300
# )
# 

####################################################################
# tmap Plot
# library(tmap)
# 
# GPS_Track_line <- GPS_Track |>
#   summarise(do_union = FALSE) |>
#   st_cast("LINESTRING")
# 
# tmap_mode("view")
# 
# map <- tm_shape(GPS_Track_line) +
#   tm_lines(col = "green", lwd = 5)
# 
# print(map)
# # #######################################################################


# library(sf)
# library(dplyr)
# library(tmap)
# library(colorspace)
# 
# # tmap interaktiv anzeigen
# tmap_mode("view")
# 
# # Segment-Linien erstellen
# # Segmente mit nur einem Punkt werden entfernt, da daraus keine LINESTRING-Geometrie
# # gebildet werden kann.
# GPS_segments <- GPS_Track_compress %>%
#   arrange(segment_id, time) %>%
#   group_by(segment_id) %>%
#   filter(n() >= 2) %>%
#   summarise(
#     n_points = n(),
#     geometry = st_combine(geometry) |> st_cast("LINESTRING"),
#     .groups = "drop"
#   ) %>%
#   mutate(segment_id = as.factor(segment_id))
# 
# # Dynamische Farbpalette für alle vorhandenen Segmente
# seg_palette <- qualitative_hcl(
#   n = n_distinct(GPS_segments$segment_id),
#   palette = "Dark 3"
# )
# 
# # Karte anzeigen
# tm_shape(GPS_segments) +
#   tm_lines(
#     col = "segment_id",
#     palette = seg_palette,
#     lwd = 3,
#     title.col = "Segment ID"
#   ) +
#   tm_shape(GPS_Track_compress) +
#   tm_dots(
#     size = 0.03,
#     col = "black"
#   )

# 
# library(sf)
# library(dplyr)
# library(tmap)
# library(colorspace)
# 
# tmap_mode("view")
# 
# # Parameter: bei Bedarf anpassen
# max_gap_min <- 5      # Zeitlücke ab 5 Minuten trennt ein Segment
# max_dist_m  <- 50     # Distanzsprung ab 50 m trennt ein Segment
# 
# GPS_plot_points <- GPS_Track_compress %>%
#   arrange(time) %>%
#   mutate(
#     dist_to_prev_m = as.numeric(st_distance(geometry, lag(geometry), by_element = TRUE)),
#     time_gap_min = as.numeric(difftime(time, lag(time), units = "mins")),
#     
#     # Neue Unterbrechung, wenn:
#     # - es der erste Punkt ist
#     # - segment_id wechselt
#     # - new_segment schon TRUE ist
#     # - Zeitlücke zu gross ist
#     # - Distanzsprung zu gross ist
#     plot_break = is.na(lag(segment_id)) |
#       segment_id != lag(segment_id) |
#       new_segment |
#       time_gap_min > max_gap_min |
#       dist_to_prev_m > max_dist_m,
#     
#     plot_segment_id = cumsum(plot_break)
#   )
# 
# # Linien nur für Abschnitte mit mindestens 2 Punkten
# GPS_plot_lines <- GPS_plot_points %>%
#   group_by(plot_segment_id) %>%
#   filter(n() >= 2) %>%
#   summarise(
#     original_segment_id = first(segment_id),
#     n_points = n(),
#     geometry = st_linestring(do.call(rbind, st_coordinates(geometry)[, c("X", "Y"), drop = FALSE])) |> 
#       st_sfc(crs = st_crs(GPS_plot_points)),
#     .groups = "drop"
#   ) %>%
#   st_as_sf() %>%
#   mutate(plot_segment_id = as.factor(plot_segment_id))
# 
# # Einzelpunkte separat behalten
# GPS_single_points <- GPS_plot_points %>%
#   group_by(plot_segment_id) %>%
#   filter(n() == 1) %>%
#   ungroup() %>%
#   mutate(plot_segment_id = as.factor(plot_segment_id))
# 
# # Dynamische Farbpalette
# n_seg <- n_distinct(GPS_plot_lines$plot_segment_id)
# 
# seg_palette <- qualitative_hcl(
#   n = max(n_seg, 1),
#   palette = "Dark 3"
# )
# 
# # Karte
# tm_shape(GPS_plot_lines) +
#   tm_lines(
#     col = "plot_segment_id",
#     palette = seg_palette,
#     lwd = 3,
#     title.col = "Plot-Segment"
#   ) +
#   tm_shape(GPS_single_points) +
#   tm_dots(
#     col = "plot_segment_id",
#     palette = seg_palette,
#     size = 0.08,
#     title.col = "Einzelpunkt-Segment"
#   ) +
#   tm_shape(GPS_plot_points) +
#   tm_dots(
#     size = 0.02,
#     col = "black",
#     alpha = 0.4
#   )

# 
# library(sf)
# library(dplyr)
# library(tmap)
# library(colorspace)
# 
# tmap_mode("view")
# 
# # Parameter: bei Bedarf anpassen
# max_gap_min <- 5      # Zeitlücke ab 5 Minuten trennt ein Segment
# max_dist_m  <- 50     # Distanzsprung ab 50 m trennt ein Segment
# 
# # Punkte vorbereiten und zusätzliche Plot-Segmentierung erzeugen
# GPS_plot_points <- GPS_Track_compress %>%
#   arrange(time) %>%
#   mutate(
#     dist_to_prev_m = as.numeric(st_distance(geometry, lag(geometry), by_element = TRUE)),
#     time_gap_min = as.numeric(difftime(time, lag(time), units = "mins")),
#     
#     plot_break = is.na(lag(segment_id)) |
#       segment_id != lag(segment_id) |
#       new_segment |
#       time_gap_min > max_gap_min |
#       dist_to_prev_m > max_dist_m,
#     
#     plot_segment_id = cumsum(plot_break)
#   )
# 
# # Linien nur innerhalb der Plot-Segmente bauen
# GPS_plot_lines <- GPS_plot_points %>%
#   mutate(
#     X = st_coordinates(.)[, "X"],
#     Y = st_coordinates(.)[, "Y"]
#   ) %>%
#   st_drop_geometry() %>%
#   group_by(plot_segment_id) %>%
#   filter(n() >= 2) %>%
#   summarise(
#     original_segment_id = first(segment_id),
#     n_points = n(),
#     geometry = st_sfc(
#       st_linestring(as.matrix(cbind(X, Y))),
#       crs = st_crs(GPS_Track_compress)
#     ),
#     .groups = "drop"
#   ) %>%
#   st_as_sf() %>%
#   mutate(plot_segment_id = as.factor(plot_segment_id))
# 
# # Einzelpunkt-Segmente separat darstellen
# GPS_single_points <- GPS_plot_points %>%
#   group_by(plot_segment_id) %>%
#   filter(n() == 1) %>%
#   ungroup() %>%
#   mutate(plot_segment_id = as.factor(plot_segment_id))
# 
# # Dynamische Farbpalette für Linien
# seg_palette <- qualitative_hcl(
#   n = max(n_distinct(GPS_plot_lines$plot_segment_id), 1),
#   palette = "Dark 3"
# )
# 
# # Karte anzeigen
# tm_shape(GPS_plot_lines) +
#   tm_lines(
#     col = "plot_segment_id",
#     palette = seg_palette,
#     lwd = 3,
#     title.col = "Plot-Segment"
#   ) +
#   tm_shape(GPS_single_points) +
#   tm_dots(
#     col = "plot_segment_id",
#     palette = seg_palette,
#     size = 0.08,
#     title.col = "Einzelpunkt-Segment"
#   ) +
#   tm_shape(GPS_plot_points) +
#   tm_dots(
#     size = 0.02,
#     col = "black",
#     alpha = 0.4
#   )
######################################################################
# main <- function() {
#   
#   library(tmap)
#   
#   hide_point <- st_as_sf(
#     data.frame(
#       lon = 8 + 31/60 + 10.65/3600,
#       lat = 47 + 9/60 + 25.19/3600
#     ),
#     coords = c("lon", "lat"),
#     crs = 4326
#   ) |>
#     st_transform(crs = 2056)
#   
#   GPXFileLink <- "data/Trackingdata_Christopher/20260312-172538.gpx"
#   
#   GPS_Track <- load_GPX_File(GPXFile = GPXFileLink)
#   
#   if (is.null(GPS_Track)) {
#     stop("GPX-Datei konnte nicht geladen werden.")
#   }
#   
#   GPS_Track_hidden <- move_points_to_circle_exit(
#     GPS_Track = GPS_Track,
#     hide_point = hide_point,
#     dist = 300
#   )
#   
#   GPS_Track_line <- GPS_Track_hidden |> 
#     summarise(do_union = FALSE) |> 
#     st_cast("LINESTRING")
#   
#   tmap_mode("view")
#   
#   map <- tm_shape(GPS_Track_line) +
#     tm_lines(col = "green", lwd = 5)
#   
#   print(map)
# }
# 
# 
# main()

# if(!interactive()){
#   main()
# }

# if (sys.nframe() == 0){
#   main()
# }


# # first simple plots
# library(sf)
# library(tmap)
# library(dplyr)
# 
# FILE <- "data/Trackingdata_Christopher/20260312-172538.gpx"
# 
# hide_point <- st_as_sf(
#   data.frame(
#     lon = 8 + 31/60 + 10.65/3600, # closest bus stop
#     lat = 47 + 9/60 + 25.19/3600
#   ),
#   coords = c("lon", "lat"),
#   crs = 4326
# ) |>
#   st_transform(crs = 2056)
# 
# GPS_Track <- st_read(FILE, layer = "track_points") |> 
#   st_transform(crs = 2056) |> 
#   mutate(
#     hide = lengths(st_is_within_distance(
#       geometry,
#       st_geometry(hide_point),
#       dist = 150
#     )) > 0
#   ) |> 
#   filter(!hide) # for privacy 
# 
# GPS_Track_line <- GPS_Track |> 
#   summarise(do_union = FALSE) |> 
#   st_cast("LINESTRING")
# 
# tmap_mode("view")
# 
# tm_shape(GPS_Track_line) +
#   tm_lines(col = "blue", lwd = 5)

