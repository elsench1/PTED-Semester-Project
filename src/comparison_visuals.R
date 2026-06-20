# src/comparison_visuals.R

library(ggplot2)
library(dplyr)

source("src/R/plot_functions.R")

theme_set(theme_minimal())

dir.create("chapters/plots", recursive = TRUE, showWarnings = FALSE)

amelia_rds_path <- "data/processedData/amelia_plot_inputs.rds"
christopher <- readRDS("data/processedData/christopher_plot_inputs.rds")

if (!file.exists(amelia_rds_path)) {
  stop(
    "Missing file: ",
    amelia_rds_path,
    "\nRun src/data_processing_Amelia.R first to create the RDS file."
  )
}

amelia <- readRDS(amelia_rds_path)

required_fields <- c(
  "person",
  "summary_data_day",
  "pie_data",
  "home_zhaw_data"
)

check_plot_input <- function(x, name) {
  missing_fields <- setdiff(required_fields, names(x))
  
  if (length(missing_fields) > 0) {
    stop(
      "The ",
      name,
      " RDS file is missing required fields: ",
      paste(missing_fields, collapse = ", ")
    )
  }
}

check_plot_input(amelia, "Amelia")
check_plot_input(christopher, "Christopher")

plot_inputs <- list(
  amelia,
  christopher
)

summary_data_day_comp <- bind_rows(
  lapply(
    plot_inputs,
    function(x) {
      x$summary_data_day |>
        mutate(
          person = x$person,
          day = as.character(day),
          metric = as.character(metric),
          value = as.numeric(value)
        ) |>
        select(person, day, metric, value)
    }
  )
)

home_zhaw_comp <- bind_rows(
  lapply(
    plot_inputs,
    function(x) {
      x$home_zhaw_data |>
        mutate(
          person = x$person,
          day = NA_character_,
          metric = as.character(metric),
          value = as.numeric(value)
        ) |>
        select(person, day, metric, value)
    }
  )
)

summary_data_day_comp <- bind_rows(
  summary_data_day_comp,
  home_zhaw_comp
)

pie_data_comp <- bind_rows(
  lapply(
    plot_inputs,
    function(x) {
      x$pie_data |>
        mutate(
          person = x$person,
          transport_group = as.character(transport_group),
          share = as.numeric(share)
        )
    }
  )
)

plot_param_sum_comp <- make_param_sum_comp_plot(summary_data_day_comp)

ggsave(
  "chapters/plots/param_sum_comp.png",
  plot = plot_param_sum_comp,
  width = 9.5,
  height = 6
)

plot_road_type_pie_comp <- make_road_type_pie_comp_plot(pie_data_comp)

ggsave(
  "chapters/plots/road_type_pie_comp.png",
  plot = plot_road_type_pie_comp,
  width = 8,
  height = 5
)