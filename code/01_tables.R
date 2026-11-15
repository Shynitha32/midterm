here::i_am(
  "code/01_tables.R"
)

data <- readRDS(
  file =
    here::here("data/clean/wastewater_clean.rds")
)

library(gtsummary)
library(gt)
library(tidyverse)
library(knitr)

table_one <- data |>
  select(
    pathogen_target,
    population_served,
    site_wval,
    site_wval_category
  ) |>
  tbl_summary(
    by = pathogen_target,
    statistic = list(
      population_served ~ "{median} ({p25}, {p75})",
      site_wval ~ "{median} ({p25}, {p75})",
      site_wval_category ~ "{n} ({p}%)"
    ),
    label = list(
      population_served ~ "Population Served",
      site_wval ~ "Wastewater Viral Activity Level",
      site_wval_category ~ "Viral Activity Category"
    ),
    missing = "no"
  ) |>
  add_overall() |>
  bold_labels() |>
  modify_caption(
  "**Table 1. Wastewater Surveillance Characteristics by Pathogen Target**"
)

table_one

saveRDS(
  table_one,
  file =
    here::here("output/table_one.rds")
)

table_two <- data |>
  group_by(pathogen_target) |>
  summarize(
    `Number of Observations` = n(),
    `Unique Sites` = n_distinct(site),
    `States/Territories Represented` = n_distinct(state_territory),
    .groups = "drop"
  )

table_two <- table_two |>
  gt() |>
  cols_label(
    pathogen_target = "Pathogen"
  ) |>
  tab_header(
    title = "Wastewater Surveillance Coverage by Pathogen"
  )

table_two

saveRDS(
  table_two,
  file =
    here::here("output/table_two.rds")
)