library(here)
library(tidyverse)
library(dplyr)
library(kableExtra)

data <- read.csv(here("./data/reg_data.csv"))

data <- data |> filter(flag_female == 1)


data_agg <- data |> mutate(
  flag_working  = 100 * flag_working,
  flag_black    = 100 * flag_black,
  flag_hispanic = 100 * flag_hispanic,
  flag_married  = 100 * flag_married,
  flag_divorce  = 100 * flag_divorce,
) |> select(wage, fte_experience, education, age, education_dad, education_mom, flag_working, flag_black, flag_hispanic, flag_married, flag_divorce, drug_use_score, school_quality, num_children) |> pivot_longer(
  cols = everything(), 
  names_to = "Statistic", 
  values_to = "Value"
  ) |> group_by(Statistic) |> mutate(
    min = min(Value, na.rm = TRUE),
    p25 = quantile(Value, 0.25, na.rm = TRUE),
    mean = mean(Value, na.rm = TRUE),
    median = median(Value, na.rm = TRUE),
    p75 = quantile(Value, 0.75, na.rm = TRUE),
    max = max(Value, na.rm = TRUE)
  ) |> select(-c(Value)) |> distinct() |>
  # Rename the statistics to be more readable
  mutate(Statistic = case_when(
    Statistic == "wage"             ~ "Wage",
    Statistic == "fte_experience"   ~ "Work Experience",
    Statistic == "education"        ~ "Years of Education",
    Statistic == "age"              ~ "Age",
    Statistic == "education_dad"    ~ "Dad's Education",
    Statistic == "education_mom"    ~ "Mom's Education",
    Statistic == "flag_black"       ~ "% Black",
    Statistic == "flag_hispanic"    ~ "% Hispanic",
    Statistic == "flag_married"     ~ "% Married",
    Statistic == "flag_divorce"     ~ "% Parents Divorced",
    Statistic == "drug_use_score"   ~ "Drug Intensity",
    Statistic == "school_quality"   ~ "School Quality",
    Statistic == "num_children"     ~ "Number of Children",
    Statistic == "flag_working"     ~ "% Working",
    TRUE ~ Statistic
  ))

# Output as a enhanced LaTeX table
latex_table <- data_agg %>%
  kable(format = "latex", 
        digits = 1,
        booktabs = TRUE, 
        caption = "Summary Statistics",
        col.names = c("Statistic", "Min", "25%", "Mean", "Median", "75%", "Max"),)

# Save the table to a .tex file
save_kable(latex_table, file = here("output/summary_statistics_table.tex"))
