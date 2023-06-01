
## 400 ish numbers TP1 is too close to T00 and script is missing the 2 separate timepoints. 
## Need to fix so that they have a Tp1 and a T00 

## 159 Ginny is the exception - was in study originally then didn't fill out any forms, but 3 years later actually came i.




# 411 - DNC additional lab/T00 PROMS - was delayed for TP PROMs too

# 417 - no lab/T00 PROMs, 7 week gap?

# 426 - no lab/T00 PROMs,  10 week gap?

# 428 - no TP PROMs, but did her baselineq at T00

# 430 - no lab/T00 PROMS, 10 week gap?

# 433 - no TP PROMs, but did her baselineq TP

## 330 - no lab/T00 proms? 120 week gap

# 159 - no TP PROMS, baselineq and all proms done at T00



# Function to apportion timepoints for each data entry per participant
traildates <- function(data) {
  data <- get(data) # get relevant data
  
  data %>%
    test <- koos2 %>% left_join(id, ., by = c("UUID")) %>% # link to id dataframe with baseline dates
    mutate(temptime = difftime(Date, labtest_date, units = "weeks")) %>%
    filter(!UUID %in% c("0000005768", "008")) %>%
    rowwise() %>%
    group_by(UUID) %>%
    mutate(timepoint = case_when( # compute timepoint based on following conditions
      is.na(labtest_date) ~ "TP", # if no lav testing then
      temptime == min(temptime) ~ "TP", # the earliest value is the pre-baseline timepoint
      abs(temptime) == min(abs(temptime - 0)) ~ "T00", # take the timepoint closest to zero and use this as baseline (T00)
      is.na(labtest_date) ~ "TP", # if hasn't completed baseline yet then all timepoints are pre-baseline
      difftime(Date, labtest_date, units = "weeks") > 21 & difftime(Date, labtest_date, units = "weeks") < 36 ~ "T06", # -5 weeks, +10 weeks
      difftime(Date, labtest_date, units = "weeks") > 47 & difftime(Date, labtest_date, units = "weeks") < 62 ~ "T12",
      difftime(Date, labtest_date, units = "weeks") > 73 & difftime(Date, labtest_date, units = "weeks") < 88 ~ "T18",
      difftime(Date, labtest_date, units = "weeks") > 99 & difftime(Date, labtest_date, units = "weeks") < 114 ~ "T24",
      difftime(Date, labtest_date, units = "weeks") > 125 & difftime(Date, labtest_date, units = "weeks") < 140 ~ "T30",
      difftime(Date, labtest_date, units = "weeks") > 151 & difftime(Date, labtest_date, units = "weeks") < 166 ~ "T36",
      difftime(Date, labtest_date, units = "weeks") > 177 & difftime(Date, labtest_date, units = "weeks") < 192 ~ "T42",
      difftime(Date, labtest_date, units = "weeks") > 203 & difftime(Date, labtest_date, units = "weeks") < 218 ~ "T48"
    )) %>%
    ungroup() %>%
    select(-temptime) %>%
    arrange(timepoint, Date) %>% # arranging and grouping to allocate sequential number to pre-baseline timepoints
    group_by(UUID, timepoint) %>%
    mutate(timepoint = case_when(
      timepoint == "TP" ~ paste0(timepoint, row_number()), # for anyone with multiple pre-baseline timepoints, name by sequence.
      timepoint != "TP" ~ timepoint)) %>% # no change to regular timepoints
    ungroup() %>%
    group_by(UUID) %>% # now to get unique timepoint for each person (i.e. remove accidental duplicates completed by participant)
    arrange(timepoint, Date) %>%
    distinct(timepoint, .keep_all = TRUE) %>%
    ungroup() %>%
    select(UUID, timepoint, Date, everything()) %>% 
    arrange(UUID, timepoint)
}


