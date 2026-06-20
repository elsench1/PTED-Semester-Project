# src/R/plot_functions.R

make_tm_movement_plot <- function(df_line, df_sf_2056) {
  tmap::tm_basemap("CartoDB.Positron") +
    tmap::tm_shape(df_line) +
    tmap::tm_lines(col = "black", lwd = 0.8) +
    tmap::tm_shape(subset(df_sf_2056, !static)) +
    tmap::tm_dots(
      fill = "blue",
      size = 0.4
    ) +
    tmap::tm_shape(subset(df_sf_2056, static)) +
    tmap::tm_dots(
      fill = "red",
      size = 0.3
    )
}


make_road_type_pie_plot <- function(pie_data) {
  ggplot2::ggplot(
    pie_data,
    ggplot2::aes(x = "", y = share, fill = transport_group)
  ) +
    ggplot2::geom_col(width = 1, color = "white") +
    ggplot2::coord_polar(theta = "y") +
    ggplot2::geom_text(
      ggplot2::aes(label = scales::percent(share, accuracy = 1)),
      position = ggplot2::position_stack(vjust = 0.5)
    ) +
    ggplot2::scale_fill_brewer(palette = "Set3") +
    ggplot2::labs(
      fill = "Transport type",
      title = "Share of travel time by road type"
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5)
    )
}


make_param_sum_plot <- function(summary_data_day) {
  ggplot2::ggplot(
    summary_data_day,
    ggplot2::aes(x = metric, y = value, fill = metric)
  ) +
    ggplot2::geom_boxplot(show.legend = FALSE, width = 0.5) +
    ggplot2::facet_wrap(
      ~ metric,
      scales = "free",
      ncol = 3,
      strip.position = "bottom"
    ) +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      title = "Comparison moving parameters"
    ) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      strip.placement = "outside",
      strip.text = ggplot2::element_text(face = "bold", size = 12),
      plot.title = ggplot2::element_text(
        hjust = 0.5,
        size = 14,
        margin = ggplot2::margin(b = 20)
      ),
      plot.margin = ggplot2::margin(t = 20, r = 10, b = 10, l = 10)
    )
}


make_param_sum_day_plot <- function(summary_data_day) {
  ggplot2::ggplot(
    summary_data_day,
    ggplot2::aes(x = factor(day), y = value, fill = metric)
  ) +
    ggplot2::geom_col(width = 0.6, show.legend = FALSE) +
    ggplot2::geom_text(
      ggplot2::aes(label = round(value, 0)),
      vjust = -0.5,
      color = "grey60",
      fontface = "bold",
      size = 3
    ) +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = c(0, 0.15))
    ) +
    ggplot2::facet_wrap(~ metric, scales = "free", ncol = 3) +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", size = 12),
      plot.title = ggplot2::element_text(
        hjust = 0.5,
        size = 14,
        margin = ggplot2::margin(b = 20)
      ),
      plot.margin = ggplot2::margin(t = 20, r = 10, b = 10, l = 10),
      panel.grid.major.x = ggplot2::element_blank()
    )
}


make_activity_road_type_plot <- function(df_sf_2056) {
  df_sf_2056 |>
    dplyr::filter(!is.na(Activity), !is.na(transport_group)) |>
    dplyr::count(Activity, transport_group) |>
    tidyr::complete(Activity, transport_group, fill = list(n = 0)) |>
    dplyr::group_by(Activity) |>
    dplyr::mutate(prop = n / sum(n)) |>
    ggplot2::ggplot(
      ggplot2::aes(transport_group, Activity, fill = prop)
    ) +
    ggplot2::geom_tile() +
    ggplot2::geom_text(
      ggplot2::aes(label = scales::percent(prop, accuracy = 1))
    ) +
    ggplot2::scale_fill_gradient(
      low = "#f7fcf5",
      high = "#74c476",
      limits = c(0, 1),
      na.value = "white"
    ) +
    ggplot2::labs(
      x = "OSM Road Type",
      y = "Google Activity",
      fill = "percentage"
    ) +
    ggplot2::scale_x_discrete(
      labels = c(
        "local_road" = "local road",
        "main_road" = "main road",
        "major_road" = "major road",
        "rail" = "rail"
      )
    ) +
    ggplot2::scale_y_discrete(
      labels = c(
        "WALKING" = "walking",
        "IN_TRAM" = "tram",
        "IN_TRAIN" = "train",
        "IN_PASSENGER_VEHICLE" = "passenger vehicle",
        "IN_BUS" = "bus",
        "CYCLING" = "cycling"
      )
    ) +
    ggplot2::theme(
      axis.title.x = ggplot2::element_text(
        margin = ggplot2::margin(t = 15)
      ),
      axis.title.y = ggplot2::element_text(
        margin = ggplot2::margin(r = 15)
      )
    )
}

make_param_sum_comp_plot <- function(summary_data_day_comp) {
  ggplot2::ggplot(
    summary_data_day_comp,
    ggplot2::aes(x = person, y = value, fill = person)
  ) +
    ggplot2::geom_boxplot(width = 0.5) +
    ggplot2::facet_wrap(
      ~ metric,
      scales = "free",
      ncol = 3,
      strip.position = "bottom"
    ) +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      fill = "",
      title = "Comparison moving parameters"
    ) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      strip.placement = "outside",
      strip.text = ggplot2::element_text(face = "bold", size = 12),
      plot.title = ggplot2::element_text(
        hjust = 0.5,
        size = 14,
        margin = ggplot2::margin(b = 20)
      ),
      plot.margin = ggplot2::margin(t = 20, r = 10, b = 10, l = 10),
      legend.position = "bottom"
    )
}


make_road_type_pie_comp_plot <- function(pie_data_comp) {
  ggplot2::ggplot(
    pie_data_comp,
    ggplot2::aes(x = "", y = share, fill = transport_group)
  ) +
    ggplot2::geom_col(width = 1, color = "white") +
    ggplot2::coord_polar(theta = "y") +
    ggplot2::geom_text(
      ggplot2::aes(label = scales::percent(share, accuracy = 1)),
      position = ggplot2::position_stack(vjust = 0.5),
      size = 3.5
    ) +
    ggplot2::scale_fill_brewer(palette = "Set3") +
    ggplot2::facet_wrap(~ person) +
    ggplot2::labs(
      fill = "Transport type",
      title = ""
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      strip.text = ggplot2::element_text(face = "bold", size = 13)
    )
}