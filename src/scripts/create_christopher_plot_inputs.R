# src/scripts/create_christopher_plot_inputs.R

source("src/R/plot_functions.R")

# GPS_Track_compress <- readRDS(
#   "data/processedData/GPS_Track_compress_matched.rds"
# )

# 1. Zeit, Distanz, Speed berechnen
# 2. Home-Distanz und out_home berechnen
# 3. analysis_day bauen
# 4. analysis bauen
# 5. summary_data bauen
# 6. summary_data_day bauen
# 7. pie_data bauen
# 8. christopher_plot_inputs.rds speichern
# 9. param_sum_day_christopher.png erzeugen

# Goal -> data/processedData/christopher_plot_inputs.rds

# structure
# christopher_plot_inputs <- list(
#   person = "Christopher",
#   analysis = analysis,
#   analysis_day = analysis_day,
#   summary_data = summary_data,
#   summary_data_day = summary_data_day,
#   pie_data = pie_data
# )