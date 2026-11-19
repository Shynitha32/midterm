library(dplyr)
library(ggplot2)
library(sf)
library(tigris)
library(tidyr)

wastewater <- readRDS("data/clean/wastewater_clean.rds")

wastewater_2026 <- wastewater |>
  mutate(sample_collect_date = as.Date(sample_collect_date)) |>
  filter(
    sample_collect_date >= as.Date("2026-01-01"),
    sample_collect_date <= as.Date("2026-11-09")
  )

wastewater_2026 |>
  group_by(counties_served) |>
  summarise(
    n_samples = sum(!is.na(wval)),
    mean_wval = mean(wval, na.rm = TRUE)
  )

ga_counties <- counties(
  state = "GA",
  cb = TRUE,
  year = 2025
)
  
ggplot(ga_counties) +
  geom_sf(fill = "lightblue", color = "white") +
  theme_void() +
  labs(title = "Georgia Counties")

county_wval <- wastewater_2026 |>
  separate_rows(counties_served, sep = ",\\s*") |>
  group_by(counties_served) |>
  summarise(
    mean_wval = mean(wval, na.rm = TRUE),
    n_samples = sum(!is.na(wval)),
    .groups = "drop"
  )

ga_map <- ga_counties |>
  left_join(
    county_wval,
    by = c("NAME" = "counties_served")
  )

county_map_plot <- ggplot(ga_map) +
  geom_sf(aes(fill = mean_wval), color = "white", linewidth = 0.2) +
  scale_fill_gradient(
    low = "lightyellow",
    high = "darkred",
    na.value = "grey90",
    name = "Average WVAL"
  ) +
  labs(
    title = "COVID-19 Wastewater Viral Activity Across Georgia Counties",
    subtitle = "Average wastewater viral activity level, January-November 2026",
    caption = "Counties without available measurements are shown in gray."
  ) +
  theme_void()

ggsave(
  filename = "output/figure_one.png",
  plot = county_map_plot,
  width = 10,
  height = 7,
  dpi = 300
)
