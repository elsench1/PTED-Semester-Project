# src/R/plot_functions.R

library(tmap)
library(ggplot2)
library(scales)
library(dplyr)
library(tidyr)

make_tm_movement_plot <- function(df_line, df_sf_2056) {
  tm_basemap("CartoDB.Positron") +
    tm_shape(df_line) +
    tm_lines(col = "black", lwd = 0.8) +
    tm_shape(subset(df_sf_2056, !static)) +
    tm_dots(
      fill = "blue",
      size = 0.4
    ) +
    tm_shape(subset(df_sf_2056, static)) +
    tm_dots(
      fill = "red",
      size = 0.3
    )
}


make_road_type_pie_plot <- function(pie_data) {
  ggplot(
    pie_data,
    aes(x = "", y = share, fill = transport_group)
  ) +
    geom_col(width = 1, color = "white") +
    coord_polar(theta = "y") +
    geom_text(
      aes(label = percent(share, accuracy = 1)),
      position = position_stack(vjust = 0.5)
    ) +
    scale_fill_brewer(palette = "Set3") +
    labs(
      fill = "Transport type",
      title = "Share of travel time by road type"
    ) +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5)
    )
}


make_param_sum_plot <- function(summary_data_day) {
  ggplot(
    summary_data_day,
    aes(x = metric, y = value, fill = metric)
  ) +
    geom_boxplot(show.legend = FALSE, width = 0.5) +
    facet_wrap(
      ~ metric,
      scales = "free",
      ncol = 3,
      strip.position = "bottom"
    ) +
    labs(
      x = NULL,
      y = NULL,
      title = "Comparison moving parameters"
    ) +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      strip.placement = "outside",
      strip.text = element_text(face = "bold", size = 12),
      plot.title = element_text(
        hjust = 0.5,
        size = 14,
        margin = margin(b = 20)
      ),
      plot.margin = margin(t = 20, r = 10, b = 10, l = 10)
    )
}


make_param_sum_day_plot <- function(summary_data_day) {
  ggplot(
    summary_data_day,
    aes(x = factor(day), y = value, fill = metric)
  ) +
    geom_col(width = 0.6, show.legend = FALSE) +
    geom_text(
      aes(label = round(value, 0)),
      vjust = -0.5,
      color = "grey60",
      fontface = "bold",
      size = 3
    ) +
    scale_y_continuous(
      expand = expansion(mult = c(0, 0.15))
    ) +
    facet_wrap(~ metric, scales = "free", ncol = 3) +
    labs(x = NULL, y = NULL) +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      strip.text = element_text(face = "bold", size = 12),
      plot.title = element_text(
        hjust = 0.5,
        size = 14,
        margin = margin(b = 20)
      ),
      plot.margin = margin(t = 20, r = 10, b = 10, l = 10),
      panel.grid.major.x = element_blank()
    )
}


make_activity_road_type_plot <- function(df_sf_2056) {
  df_sf_2056 |>
    filter(!is.na(Activity), !is.na(transport_group)) |>
    count(Activity, transport_group) |>
    complete(Activity, transport_group, fill = list(n = 0)) |>
    group_by(Activity) |>
    mutate(prop = n / sum(n)) |>
    ggplot(
      aes(transport_group, Activity, fill = prop)
    ) +
    geom_tile() +
    geom_text(
      aes(label = percent(prop, accuracy = 1))
    ) +
    scale_fill_gradient(
      low = "#f7fcf5",
      high = "#74c476",
      limits = c(0, 1),
      na.value = "white"
    ) +
    labs(
      x = "OSM Road Type",
      y = "Google Activity",
      fill = "percentage"
    ) +
    scale_x_discrete(
      labels = c(
        "local_road" = "local road",
        "main_road" = "main road",
        "major_road" = "major road",
        "rail" = "rail"
      )
    ) +
    scale_y_discrete(
      labels = c(
        "WALKING" = "walking",
        "IN_TRAM" = "tram",
        "IN_TRAIN" = "train",
        "IN_PASSENGER_VEHICLE" = "passenger vehicle",
        "IN_BUS" = "bus",
        "CYCLING" = "cycling"
      )
    ) +
    theme(
      axis.title.x = element_text(
        margin = margin(t = 15)
      ),
      axis.title.y = element_text(
        margin = margin(r = 15)
      )
    )
}

make_param_sum_comp_plot <- function(summary_data_day_comp) {
  ggplot(
    summary_data_day_comp,
    aes(x = person, y = value, fill = person)
  ) +
    geom_boxplot(width = 0.5) +
    geom_boxplot(
      data = home_zhaw_data,
      aes(x = person, y = value, fill = person),
      width = 0.5
    ) +
    facet_wrap(
      ~ metric,
      scales = "free",
      ncol = 3,
      strip.position = "bottom"
    ) +
    scale_y_continuous(limits = c(0, NA)) +
    labs(
      x = NULL,
      y = NULL,
      fill = "",
      title = "Comparison movement parameters"
    ) +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      strip.placement = "outside",
      strip.text = element_text(face = "bold", size = 12),
      plot.title = element_text(
        hjust = 0.5,
        size = 14,
        margin = margin(b = 20)
      ),
      plot.margin = margin(t = 20, r = 10, b = 10, l = 10),
      legend.position = "bottom"
    )
}



make_road_type_pie_comp_plot <- function(pie_data_comp) {
  ggplot(
    pie_data_comp,
    aes(x = "", y = share, fill = transport_group)
  ) +
    geom_col(width = 1, color = "white") +
    coord_polar(theta = "y") +
    geom_text(
      aes(label = percent(share, accuracy = 1)),
      position = position_stack(vjust = 0.5),
      size = 3.5
    ) +
    scale_fill_brewer(palette = "Set3") +
    facet_wrap(~ person) +
    labs(
      fill = "Transport type",
      title = ""
    ) +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5),
      strip.text = element_text(face = "bold", size = 13)
    )
}