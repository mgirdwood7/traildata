### Check interstate groups (separate)
## Imaging and bloods date needed in fusion
## Imaging time from DICOM

# Withdrawals smartabase

# Dashboard?

# Goose
# - 6-monthly dashboard
# - 4 yr imaging dashboard
# add imaging/boods in fusion
# talk to MK about training for Fusion
# - Monthly injury, 1 reminder only
# - Chorus/Strava connection

## Training load
# - snapshot of whether gathering data ? 2-3 month

# Load

library(tidyverse)
library(lubridate)
library(readxl)
library(openxlsx)
library(neon)

# Variable Names 
path <- "data/raw/varnames.xlsx" # set path for read in later

varnames <- path %>%
  excel_sheets() %>% # read in all sheets
  set_names() %>% # set names based on name of sheet
  map(read_xlsx, path = path) %>% # read in each sheet to a list
  map(~mutate(., question = ifelse(str_detect(question, "\\r"), str_replace(question, "\\r", ""), question))) # correcting where returned lines labelled as \r\n to \n

 names(varnames) <- paste0(names(varnames), "names") # paste 'names' to end of each variable name 
 list2env(varnames, envir = .GlobalEnv) # add each element of the list to the environment as its own df.

# New method as of December 2022
# Using Fusion API
# Fusion direct download
library(neon)

# Pull Data from Fusion/Smartabase

koos <- pull_smartabase(
  form = "3. Knee Injury Outcome Score (KOOS)",
  start_date = "01/06/2020",
  end_date = as.character(format(as.Date(Sys.Date()), "%d/%m/%Y")))

tampa <- pull_smartabase(
  form = "6. Tampa Scale for Kinesiophobia (TSK)",
  start_date = "01/06/2020",
  end_date = as.character(format(as.Date(Sys.Date()), "%d/%m/%Y")))

spex <- pull_smartabase(
  form = "4. The medical examination sports injury surveillance questionnaire (SPEX)",
  start_date = "01/06/2020",
  end_date = as.character(format(as.Date(Sys.Date()), "%d/%m/%Y")))

phonescreen <- pull_smartabase(
  form = "TRAIL Phone Screening",
  start_date = "01/06/2020",
  end_date = as.character(format(as.Date(Sys.Date()), "%d/%m/%Y")))

pass <- pull_smartabase(
  form = "8. Patient Acceptable Symptom State (PASS)",
  start_date = "01/06/2020",
  end_date = as.character(format(as.Date(Sys.Date()), "%d/%m/%Y")))

lab <- pull_smartabase(
  form = "TRAIL_Lab Testing",
  start_date = "01/06/2020",
  end_date = as.character(format(as.Date(Sys.Date()), "%d/%m/%Y")))

kses <- pull_smartabase(
  form = "7. Knee Self-Efficacy Scale (K-SES)",
  start_date = "01/06/2020",
  end_date = as.character(format(as.Date(Sys.Date()), "%d/%m/%Y")))

baselineq <- pull_smartabase(
  form = "1. TRAIL Baseline Questionnaire",
  start_date = "01/06/2020",
  end_date = as.character(format(as.Date(Sys.Date()), "%d/%m/%Y")))

assq <- pull_smartabase(
  form = "5. Sleep Survery (ASSQ)",
  start_date = "01/06/2020",
  end_date = as.character(format(as.Date(Sys.Date()), "%d/%m/%Y")))

visaa <- pull_smartabase(
  form = "9. Achilles questionnaire (VISA-A)",
  start_date = "01/06/2020",
  end_date = as.character(format(as.Date(Sys.Date()), "%d/%m/%Y")))
  


