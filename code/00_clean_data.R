#Set working directory 

#libs and global settings 
library(tidyverse)
library(gtsummary)
library(knitr)
library(janitor)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)


#code location 
here::i_am("code/00_clean_data.R")


## Load data 
df_raw <- read_csv("data/raw/ga_sars_cov2_wastewater.csv", show_col_types = FALSE)


to_num <- function(x) {
  as.numeric(str_remove_all(as.character(x), ","))
}

#create wval calculation
df_raw <- df_raw %>%
  mutate(
    sample_date = parse_date_time(sample_collect_date, orders = c("dmy", "mdy", "ymd")),
    conc_lin = to_num(pcr_target_avg_conc_lin),
    flowpop_lin = to_num(pcr_target_flowpop_lin),
    hum_conc = to_num(hum_frac_mic_conc),
    norm_conc = if_else(!is.na(flowpop_lin), flowpop_lin, conc_lin / hum_conc),
    log_conc = log(norm_conc),
    anchor_year = if_else(month(sample_date) >= 8, year(sample_date), year(sample_date) - 1),
    baseline_anchor = make_date(anchor_year, 8, 1),
    week_end = ceiling_date(as_date(sample_date), unit = "week", week_start = 7) - days(1)
  )


group_cols <- c(
  "site",
  "source",
  "major_lab_method",
  "pcr_type",
  "extraction_method",
  "concentration_method",
  "sample_matrix"
)

df_raw <- df_raw %>%
  group_by(across(all_of(group_cols))) %>%
  mutate(
    p99 = quantile(log_conc, probs = 0.99, na.rm = TRUE),
    log_conc_capped = pmin(log_conc, p99)
  ) %>%
  ungroup()

# 
df_wval <- df_raw %>%
  group_by(across(all_of(group_cols))) %>%
  group_modify(~ {
    grp <- .x %>% arrange(sample_date)
    
    grp %>%
      group_by(baseline_anchor) %>%
      group_modify(function(sub, key) {
        anchor <- key$baseline_anchor
        window_start <- anchor %m-% months(24)
        
        hist <- grp %>%
          filter(sample_date >= window_start, sample_date < anchor) %>%
          pull(log_conc_capped)
        
        if (length(hist) < 8) {
          hist <- grp$log_conc_capped
        }
        
        baseline <- quantile(hist, probs = 0.10, na.rm = TRUE)
        sd_val <- sd(hist, na.rm = TRUE)
        
        z <- if (!is.na(sd_val) && sd_val > 0) {
          (sub$log_conc_capped - baseline) / sd_val
        } else {
          NA_real_
        }
        
        sub %>% mutate(sample_wval = exp(z))
      }) %>%
      ungroup()
  }) %>%
  ungroup()

#define Wval caregories 
df_wval <- df_wval %>%
  group_by(across(all_of(c(group_cols, "week_end")))) %>%
  mutate(wval = round(mean(sample_wval, na.rm = TRUE), 2)) %>%
  ungroup() %>%
  mutate(
    wval_category = case_when(
      is.na(wval) ~ NA_character_,
      wval <= 1.7 ~ "Very Low",
      wval <= 3.4 ~ "Low",
      wval <= 5.4 ~ "Moderate",
      wval <= 8.1 ~ "High",
      TRUE        ~ "Very High"
    )
  )


#select final columns 
wastewater_clean <- df_wval %>%
  select(
    record_id, 
    sample_collect_date, 
    site, 
    state_territory, 
    source, 
    counties_served, 
    population_served, 
    pcr_target, 
    sample_location, 
    pcr_target_detect,
    sample_matrix, 
    wval,
    wval_category
  )


write_csv(
  wastewater_clean,
  file = here::here( "Data", "clean", "wastewater_clean.csv")
)

saveRDS(
  wastewater_clean,
  file = here::here("data/clean/wastewater_clean.rds")
)

print("00_clean worked.")

