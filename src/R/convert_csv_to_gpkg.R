# convert_csv_to_gpkg.R


library(sf)


convert_csv_to_gpkg <- function(
    csv_file,
    gpkg_location,
    crs = 2056,
    name = "ZHAW Grüental",
    layer_name = NULL,
    overwrite = TRUE,
    make_valid = TRUE,
    quiet = TRUE
) {
  # Paket prüfen
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Das Paket 'sf' ist nicht installiert. Bitte zuerst install.packages('sf') ausführen.")
  }
  
  # Eingaben prüfen
  if (missing(csv_file) || is.null(csv_file) || length(csv_file) != 1 || !nzchar(csv_file)) {
    stop("'csv_file' muss ein gültiger Dateipfad sein.")
  }
  
  if (missing(gpkg_location) || is.null(gpkg_location) || length(gpkg_location) != 1 || !nzchar(gpkg_location)) {
    stop("'gpkg_location' muss ein gültiger Ausgabepfad sein.")
  }
  
  if (!file.exists(csv_file)) {
    stop("Die CSV-Datei existiert nicht: ", csv_file)
  }
  
  if (dir.exists(csv_file)) {
    stop("'csv_file' zeigt auf einen Ordner, nicht auf eine Datei: ", csv_file)
  }
  
  if (!grepl("\\.gpkg$", gpkg_location, ignore.case = TRUE)) {
    stop("'gpkg_location' sollte auf '.gpkg' enden, z.B. 'zhaw_gruental.gpkg'.")
  }
  
  # Ausgabeordner prüfen / erstellen
  output_dir <- dirname(gpkg_location)
  
  if (!dir.exists(output_dir)) {
    dir_created <- dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    
    if (!dir_created) {
      stop("Ausgabeordner konnte nicht erstellt werden: ", output_dir)
    }
  }
  
  # Layername aus Dateiname ableiten, falls nicht angegeben
  if (is.null(layer_name)) {
    layer_name <- tools::file_path_sans_ext(basename(gpkg_location))
  }
  
  # CSV lesen
  dat <- tryCatch(
    {
      read.csv(
        csv_file,
        sep = ";",
        dec = ".",
        stringsAsFactors = FALSE
      )
    },
    error = function(e) {
      stop("CSV konnte nicht gelesen werden: ", conditionMessage(e))
    }
  )
  
  # Prüfen, ob benötigte Spalten vorhanden sind
  required_cols <- c("Easting", "Northing")
  missing_cols <- setdiff(required_cols, names(dat))
  
  if (length(missing_cols) > 0) {
    stop(
      "Folgende benötigte Spalten fehlen in der CSV: ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  # Nur benötigte Koordinaten behalten
  coords <- dat[, required_cols]
  
  # Koordinaten robust in numerisch umwandeln
  coords$Easting <- as.numeric(
    gsub(",", ".", gsub("'", "", trimws(as.character(coords$Easting))))
  )
  
  coords$Northing <- as.numeric(
    gsub(",", ".", gsub("'", "", trimws(as.character(coords$Northing))))
  )
  
  # Ungültige Koordinaten entfernen
  invalid_rows <- !complete.cases(coords)
  
  if (all(invalid_rows)) {
    stop("Keine gültigen Koordinaten gefunden. Prüfe 'Easting' und 'Northing'.")
  }
  
  if (any(invalid_rows)) {
    warning(sum(invalid_rows), " Zeile(n) mit ungültigen Koordinaten wurden entfernt.")
    coords <- coords[!invalid_rows, , drop = FALSE]
  }
  
  # Prüfen, ob genug Punkte für ein Polygon vorhanden sind
  if (nrow(coords) < 3) {
    stop("Für ein Polygon werden mindestens 3 gültige Koordinatenpunkte benötigt.")
  }
  
  xy <- as.matrix(coords)
  storage.mode(xy) <- "double"
  
  # Prüfen, ob mindestens 3 unterschiedliche Punkte vorhanden sind
  if (nrow(unique(xy)) < 3) {
    stop("Für ein Polygon werden mindestens 3 unterschiedliche Punkte benötigt.")
  }
  
  # Polygonring schliessen, falls nötig
  if (!isTRUE(all.equal(xy[1, ], xy[nrow(xy), ], tolerance = 1e-8))) {
    xy <- rbind(xy, xy[1, ])
  }
  
  # Polygon erzeugen
  poly <- sf::st_polygon(list(xy))
  
  # sf-Objekt mit CRS erzeugen
  polygon_sf <- sf::st_sf(
    name = name,
    geometry = sf::st_sfc(poly, crs = crs)
  )
  
  # Geometrie prüfen und optional reparieren
  if (!all(sf::st_is_valid(polygon_sf))) {
    if (make_valid) {
      warning("Polygon war geometrisch nicht gültig und wird mit st_make_valid() repariert.")
      polygon_sf <- sf::st_make_valid(polygon_sf)
    } else {
      stop("Polygon ist geometrisch nicht gültig.")
    }
  }
  
  # GeoPackage schreiben
  tryCatch(
    {
      sf::st_write(
        polygon_sf,
        dsn = gpkg_location,
        layer = layer_name,
        delete_layer = overwrite,
        quiet = quiet
      )
    },
    error = function(e) {
      stop("GeoPackage konnte nicht geschrieben werden: ", conditionMessage(e))
    }
  )
  
  message("GeoPackage erfolgreich gespeichert: ", gpkg_location)
  
  invisible(polygon_sf)
}