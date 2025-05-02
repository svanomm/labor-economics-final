library(here)
library(stargazer)
library(gridExtra)
library(tidyverse)
library(dplyr)
library(kableExtra)
library(ivreg)

# load the rds file
data <- readRDS(here("./data/NLS_data.rds"))

# Create new variables ----
data <- data |> mutate(
  stolen_school       = ifelse(ifelse(is.na(`YSCH-35900_1997`), 0, `YSCH-35900_1997`) > 0, 1, 0),
  threatened_school   = ifelse(ifelse(is.na(`YSCH-36000_1997`), 0, `YSCH-36000_1997`) > 0, 1, 0),
  fight_school        = ifelse(ifelse(is.na(`YSCH-36100_1997`), 0, `YSCH-36100_1997`) > 0, 1, 0),
  late_school         = ifelse(ifelse(is.na(`YSCH-36200_1997`), 0, `YSCH-36200_1997`) > 0, 1, 0),
  absent_school       = ifelse(ifelse(is.na(`YSCH-36300_1997`), 0, `YSCH-36300_1997`) > 0, 1, 0),
  teachers_good       = ifelse(ifelse(is.na(`YSCH-36400_1997`), 0, `YSCH-36400_1997`) < 3, 1, 0),
  teachers_interested = ifelse(ifelse(is.na(`YSCH-36500_1997`), 0, `YSCH-36500_1997`) < 3, 1, 0),
  disrupt_learning    = ifelse(ifelse(is.na(`YSCH-36600_1997`), 0, `YSCH-36600_1997`) < 3, 1, 0),
  graded_fairly       = ifelse(ifelse(is.na(`YSCH-36700_1997`), 0, `YSCH-36700_1997`) < 3, 1, 0),
  cheating_school     = ifelse(ifelse(is.na(`YSCH-36800_1997`), 0, `YSCH-36800_1997`) < 3, 1, 0),
  discipline_fair     = ifelse(ifelse(is.na(`YSCH-36900_1997`), 0, `YSCH-36900_1997`) < 3, 1, 0),
  safe_school         = ifelse(ifelse(is.na(`YSCH-37000_1997`), 0, `YSCH-37000_1997`) < 3, 1, 0),
  
  drug_use_score = 
    ifelse(is.na(`YSAQ-361_1997`), 0, `YSAQ-361_1997`) + 
    ifelse(is.na(`YSAQ-365_1997`), 0, `YSAQ-365_1997`) + 
    ifelse(is.na(`YSAQ-371_1997`), 0, `YSAQ-371_1997`) + 
    (15*ifelse(is.na(`YSAQ-372B_1998`), 0, `YSAQ-372B_1998`)),
  # I multiply by 15 because the last question is a binary 1-0.
  
  flag_female = ifelse(KEY_SEX_1997 == 2, 1, 0),
  flag_black = ifelse(KEY_RACE_ETHNICITY_1997==1, 1, 0),
  flag_hispanic = ifelse(KEY_RACE_ETHNICITY_1997==2, 1, 0),
  flag_divorce = ifelse(ifelse(is.na(`YHEA-3000_2002`), 0, `YHEA-3000_2002`)==1, 1, 0),
  
  # Proxy for school "quality" or experience
  school_quality = 
    # add "good" qualities
    teachers_good 
  + teachers_interested
  + graded_fairly
  + discipline_fair    
  + safe_school   
  # subtract "bad" qualities
  - stolen_school 
  - threatened_school
  - fight_school  
  - late_school  
  - absent_school
  - disrupt_learning   
  - cheating_school 
)

# Drop variables ----
data <- data |> select(
  -c(
    `YSCH-35900_1997`,
    `YSCH-36000_1997`,
    `YSCH-36100_1997`,
    `YSCH-36200_1997`,
    `YSCH-36300_1997`,
    `YSCH-36400_1997`,
    `YSCH-36500_1997`,
    `YSCH-36600_1997`,
    `YSCH-36700_1997`,
    `YSCH-36800_1997`,
    `YSCH-36900_1997`,
    `YSCH-37000_1997`,
    
    # Drug use variables
    `YSAQ-361_1997`, 
    `YSAQ-365_1997`, 
    `YSAQ-371_1997`, 
    `YSAQ-372B_1998`,
    KEY_SEX_1997,KEY_RACE_ETHNICITY_1997,`YHEA-3000_2002`,
    stolen_school,      
    threatened_school,  
    fight_school,       
    late_school,        
    absent_school,      
    teachers_good,      
    teachers_interested,
    disrupt_learning,   
    graded_fairly,      
    cheating_school,    
    discipline_fair,    
    safe_school,
    CV_CENSUS_REGION_1997,
    CV_SAMPLE_TYPE_1997
  )
)

# Rename the 1997 columns
data <- data |> rename(
  education_dad = CV_HGC_BIO_DAD_1997,
  education_mom = CV_HGC_BIO_MOM_1997,
  id = PUBID_1997,
  birth_year = KEY_BDATE_Y_1997,
  birth_month = KEY_BDATE_M_1997,
)

# Rename some columns to fix the year pattern
rename_columns <- function(df) {
  old_names <- names(df)
  new_names <- sapply(old_names, function(name) {
    if (grepl("CVC_HOURS_WK_YR_ALL\\.[0-9]{2}_XRND", name)) {
      year_digits <- sub(".*\\.([0-9]{2})_XRND", "\\1", name)
      full_year <- ifelse(as.numeric(year_digits) > 50, 
                          paste0("19", year_digits), 
                          paste0("20", year_digits))
      new_name <- sub("\\.([0-9]{2})_XRND", paste0(".", full_year), name)
      return(new_name)
    } else {
      return(name)
    }
  })
  names(df) <- new_names
  return(df)
}

data <- rename_columns(data)

# For all columns CVC_HOURS_WK_YR_ALL.2002:CVC_HOURS_WK_YR_ALL.2021, cap them at 4000 hours
data <- data |> mutate(across(starts_with("CVC_HOURS_WK_YR_ALL"), ~ ifelse(. > 4000, 4000, .)))

# Create variables for cumulative hours worked since 2002 (at least 18)
data$hours_worked_since02_2010 <- data |> select(CVC_HOURS_WK_YR_ALL.2002:CVC_HOURS_WK_YR_ALL.2010) |> rowSums(na.rm = TRUE)
data$hours_worked_since02_2011 <- data |> select(CVC_HOURS_WK_YR_ALL.2002:CVC_HOURS_WK_YR_ALL.2011) |> rowSums(na.rm = TRUE)
data$hours_worked_since02_2013 <- data |> select(CVC_HOURS_WK_YR_ALL.2002:CVC_HOURS_WK_YR_ALL.2013) |> rowSums(na.rm = TRUE)
data$hours_worked_since02_2015 <- data |> select(CVC_HOURS_WK_YR_ALL.2002:CVC_HOURS_WK_YR_ALL.2015) |> rowSums(na.rm = TRUE)
data$hours_worked_since02_2017 <- data |> select(CVC_HOURS_WK_YR_ALL.2002:CVC_HOURS_WK_YR_ALL.2017) |> rowSums(na.rm = TRUE)
data$hours_worked_since02_2019 <- data |> select(CVC_HOURS_WK_YR_ALL.2002:CVC_HOURS_WK_YR_ALL.2019) |> rowSums(na.rm = TRUE)
data$hours_worked_since02_2021 <- data |> select(CVC_HOURS_WK_YR_ALL.2002:CVC_HOURS_WK_YR_ALL.2021) |> rowSums(na.rm = TRUE)
data <- data |> select(-c(CVC_HOURS_WK_YR_ALL.2000:CVC_HOURS_WK_YR_ALL.1999))

# Reshape the data long by year
data <- data |> pivot_longer(
  cols = matches("_20"),
  names_to = c(".value", "year"),
  names_pattern = "(.+)_(\\d+)$"
) 


# More renaming
data <- data |> rename(
  education = CV_HGC_EVER_EDT
) |> relocate(
  c(CV_MARSTAT, CV_BIO_CHILD_HH, CV_BIO_CHILD_HH_U18), .before = CV_HRLY_PAY.01
)

# Calculate wage (maximum across all reported jobs in the year)
data <- data |> rowwise(id) |> 
  mutate(
    wage = max(c_across(CV_HRLY_PAY.01:CV_HRLY_PAY.15), na.rm = TRUE),
  ) |> ungroup() |>
  select(-c(CV_HRLY_PAY.01:CV_HRLY_PAY.15))

data <- data |> mutate(
  wage = ifelse(is.infinite(wage), NA, wage/100),
  log_wage = log(wage),
  fte_experience = hours_worked_since02 / 2000
)

# Data Filters ----
data <- data |> filter(
  education_dad != 95, # we need dad's education
  education_mom != 95,
  education != 95, # this also removes years after a respondent drops out
  (is.na(wage) | (wage > 5 & wage < 250)) # remove outlier wages
)

# Create new variables
data <- data |> mutate(
  flag_working = ifelse(!is.na(wage), 1, 0),
  flag_married = ifelse(CV_MARSTAT %in% c(3,4), 1, 0),
  num_children = ifelse(is.na(CV_BIO_CHILD_HH), 0, CV_BIO_CHILD_HH),
  fte_experience_sq = fte_experience^2,
  age = as.numeric(year) - birth_year
) |> select(
  -c(CVC_WKSWK_ADULT2_ALL_XRND, CVC_RND_XRND, CV_MARSTAT, CV_BIO_CHILD_HH, CV_BIO_CHILD_HH_U18)
)

# Save as CSV file
write_csv(data, here("./data/reg_data.csv"))
