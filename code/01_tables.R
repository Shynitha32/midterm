here::i_am(
  "code/01_tables.R"
)

data <- readRDS(
  file =
    here::here("data/clean/wastewater_clean.rds")
)

library(gtsummary)
library(tidyverse)
library(knitr)

wastewater_table <- data |>
  mutate(
    wval_category = factor(
      wval_category,
      levels = c(
        "Very Low",
        "Low",
        "Moderate",
        "High",
        "Very High"
      )
    ),
    sample_matrix = recode(
      sample_matrix,
      "post grit removal" = "Post Grit Removal",
      "primary sludge" = "Primary Sludge",
      "raw wastewater" = "Raw Wastewater"
    )
  )

table_one <- wastewater_table |>
  select(
    wval,
    wval_category,
    pcr_target_detect,
    sample_matrix
  ) |>
  tbl_summary(
    statistic = list(
      wval ~ "{median} ({p25}, {p75})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    label = list(
      wval ~ "Wastewater Viral Activity Level",
      wval_category ~ "Viral Activity Category",
      pcr_target_detect ~ "SARS-CoV-2 Detected",
      sample_matrix ~ "Sample Matrix"
    ),
    missing = "ifany"
  ) |>
  bold_labels() |>
  modify_caption(
    "**Table 1. Characteristics of SARS-CoV-2 Wastewater Surveillance in Georgia**"
  )

saveRDS(
  table_one,
  file =
    here::here("output/table_one.rds")
)