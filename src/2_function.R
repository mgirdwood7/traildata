# Function to rename variables based on list of same name - e.g. rename koos with koosnames list
renamevariables <- function(data) {
  title <- paste0(data, "names") # create string with variable name + "names" 
  name <- get(title) # get relevant name list
  data <- get(data) # get relevant data
  
  data %>% # use relevant data
    select(-c(user_id, form)) %>%
    rename_with(~name$codename, name$question) %>% # replace all var names
    rename_at(vars(ends_with("?")), # remove any question marks not matched
              ~str_replace(., "\\?", "")) %>%
    rename(Date = start_date) %>%
    mutate(Date = dmy(Date)) %>%  # change to date formate
    select(-matches("HIDDEN|Index|about|by|start_time|end_date|end_time|event_id", ignore.case = FALSE)) # remove unneeded variables

}  


# Function to apportion timepoints for each data entry per participant
traildates <- function(data) {
  data <- get(data) # get relevant data
  
  data %>%
    left_join(id, ., by = c("UUID")) %>% # link to id dataframe with baseline dates
    rowwise() %>%
    mutate(timepoint = case_when( # compute timepoint based on following conditions
      is.na(labtest_date) ~ "TP", # if hasn't completed baseline yet then all timepoints are pre-baseline
      difftime(Date, labtest_date, units = "weeks") < -4 ~ "TP", # for those with a baseline timepoint
      difftime(Date, labtest_date, units = "weeks") > -4 & difftime(Date, labtest_date, units = "weeks") < 10 ~ "T00", 
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

