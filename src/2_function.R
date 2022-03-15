# Function to rename variables based on list of same name - e.g. rename koos with koosnames list

renamevariables <- function(data) {
  title <- paste0(data, "names") # create string with variable name + "names" 
  name <- get(title) # get relevant name list
  data <- get(data) # get relevant data
  
  data %>% # use relevant data
    rename_with(~name$codename, name$question) %>% # replace all var names
    rename_at(vars(ends_with("?")), # remove any question marks not matched
              ~str_replace(., "\\?", "")) %>%
    select(-matches("HIDDEN|Index|About|by", ignore.case = FALSE)) # remove unneeded variables

}  



traildates <- function(data) {
  data <- get(data) # get relevant data
  
  data %>%
    left_join(id, ., by = c("UUID")) %>% # link to id dataframe with baseline dates
    rowwise() %>%
    mutate(timepoint = case_when( # compute timepoint based on following conditions
      is.na(labtest_date) ~ "TP", # if hasn't completed baseline yet then all timepoints are pre-baseline
      difftime(Date, labtest_date, units = "weeks") < -3 ~ "TP", # for those with a baseline timepoint
      difftime(Date, labtest_date, units = "weeks") > -3 & difftime(Date, labtest_date, units = "weeks") < 10 ~ "T00", 
      difftime(Date, labtest_date, units = "weeks") > 21 & difftime(Date, labtest_date, units = "weeks") < 36 ~ "T06", # -5 weeks, +10 weeks
      difftime(Date, labtest_date, units = "weeks") > 47 & difftime(Date, labtest_date, units = "weeks") < 62 ~ "T12",
      difftime(Date, labtest_date, units = "weeks") > 73 & difftime(Date, labtest_date, units = "weeks") < 88 ~ "T18",
      difftime(Date, labtest_date, units = "weeks") > 99 & difftime(Date, labtest_date, units = "weeks") < 114 ~ "T24",
      difftime(Date, labtest_date, units = "weeks") > 125 & difftime(Date, labtest_date, units = "weeks") < 140 ~ "T30",
      difftime(Date, labtest_date, units = "weeks") > 151 & difftime(Date, labtest_date, units = "weeks") < 166 ~ "T36"
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

