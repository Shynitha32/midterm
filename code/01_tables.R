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
    pcr_target,
    population_served,
    wval,
    wval_category
  ) |>
  tbl_summary(
    by = pcr_target,
    statistic = list(
      population_served ~ "{median} ({p25}, {p75})",
      wval ~ "{median} ({p25}, {p75})",
      wval_category ~ "{n} ({p}%)"
    ),
    label = list(
      population_served ~ "Population Served",
      wval ~ "Wastewater Viral Activity Level",
      wval_category ~ "Viral Activity Category"
    ),
    missing = "no"
  ) |>
#  add_overall() |>
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
  group_by(pcr_target) |>
  summarize(
    `Number of Observations` = n(),
    `Unique Sites` = n_distinct(site),
    `States/Territories Represented` = n_distinct(state_territory),
    .groups = "drop"
  )

table_two <- table_two |>
  gt() |>
  cols_label(
    pcr_target = "Pathogen"
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