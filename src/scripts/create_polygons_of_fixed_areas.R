source("src/R/convert_csv_to_gpkg.R")

gruental <- convert_csv_to_gpkg(
  csv_file = "data/metaData/ZHAW_Gruental.csv",
  gpkg_location = "data/metaData/zhaw_gruental.gpkg"
)

reidbach <- convert_csv_to_gpkg(
  csv_file = "data/metaData/ZHAW_Reidbach_A.csv",
  gpkg_location = "data/metaData/ZHAW_Reidbach_A.gpkg"
)


library(tmap)
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