library(here)
library(stargazer)
library(gridExtra)
library(tidyverse)
library(dplyr)
library(kableExtra)
library(ivreg)

# load the rds file
data <- readRDS(here("./data/NLS_data.rds"))

# Filter the data
data <- data |> filter(
  CV_HGC_BIO_DAD_1997 != 95,
  CV_HGC_BIO_MOM_1997 != 95,
  CV_HGC_EVER_EDT_2021 != 95,
#  !is.na(CV_HRLY_PAY.01_2021),
#  CV_HRLY_PAY.01_2021>100, # wage earners only
#  !is.na(CVC_WKSWK_ADULT2_ALL_XRND),
  !is.na(`CV_URBAN-RURAL_2021`),
  `CV_URBAN-RURAL_2021` != 2
)

# Create new variables
data <- data |> mutate(
  flag_working = ifelse(
    !is.na(CV_HRLY_PAY.01_2021) &
    CV_HRLY_PAY.01_2021>100 &
    !is.na(CVC_WKSWK_ADULT2_ALL_XRND),
    1, 0
  ),
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
  
  parent_educ_score = CV_HGC_BIO_DAD_1997+CV_HGC_BIO_MOM_1997,
  
  flag_black = ifelse(KEY_RACE_ETHNICITY_1997==1, 1, 0),
  flag_hispanic = ifelse(KEY_RACE_ETHNICITY_1997==2, 1, 0),
  flag_divorce = ifelse(ifelse(is.na(`YHEA-3000_2002`), 0, `YHEA-3000_2002`)==1, 1, 0),
  
  log_wage = log(CV_HRLY_PAY.01_2021/100),
  
  flag_urban = ifelse(`CV_URBAN-RURAL_2021`==1, 1, 0),
  
  years_experience = CVC_WKSWK_ADULT2_ALL_XRND / 52,
  years_experience_sq = years_experience^2,
  
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

# Save as CSV file
write_csv(data, here("./data/reg_data.csv"))
