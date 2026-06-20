source("src/data_processing_Amelia.R")
theme_set(theme_minimal())


#source("src/draft.R")


summary_data_day_personB <- summary_data_day
pie_data_personB <- pie_data


summary_data_day_comp <- bind_rows(
  summary_data_day |> mutate(person = "Amelia"),
  summary_data_day_personB |> mutate(person = "Christopher")
)

pie_data_comp <- bind_rows(
  pie_data |> mutate(person = "Amelia"),
  pie_data_personB |> mutate(person = "Christopher")
)



plot_param_sum_comp <- ggplot(summary_data_day_comp, aes(x = person, y = value, fill = person)) +
  geom_boxplot(width = 0.5) +
  facet_wrap(~ metric, scales = "free", ncol = 3, strip.position = "bottom") +
  labs(x = NULL, y = NULL, fill = "", title = "Comparison moving parameters") +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    strip.placement = "outside",
    strip.text = element_text(face = "bold", size = 12),
    plot.title = element_text(hjust = 0.5, size = 14, margin = margin(b = 20)),
    plot.margin = margin(t = 20, r = 10, b = 10, l = 10),
    legend.position = "bottom"
  )

ggsave("chapters/plots/param_sum_comp.png", plot_param_sum_comp)


plot_road_type_pie_comp <- ggplot(pie_data_comp, aes(x = "", y = share, fill = transport_group)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = scales::percent(share, accuracy = 1)),
            position = position_stack(vjust = 0.5), size = 3.5) +
  scale_fill_brewer(palette = "Set3") +
  facet_wrap(~ person) +
  labs(fill = "Transport type", title = "") +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5),
    strip.text = element_text(face = "bold", size = 13)
  )

ggsave("chapters/plots/road_type_pie_comp.png", plot_road_type_pie_comp)
