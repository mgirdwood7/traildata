# Clean File

# lab testing
# This one first as has the lab testing date which is used to calculate/extract timepoint.
lab <- renamevariables("lab") %>%
  mutate(across(c(msi_weight_score, msi_stackheight_score, msi_heeltoedrop_score, msi_stability_score),
                ~ifelse(is.na(.x), NA, str_sub(.x, 1,1)))) %>% # change score based msi variables to just the numeric value and not descriptor
  mutate(across(c(r_medial_palp, l_medial_palp, r_lateral_palp, l_lateral_palp, r_standing_crepitus, l_standing_crepitus),
                ~case_when(
                  .x == "Negative" ~ 0, # change to numeric values
                  .x == "Positive" ~ 1,
                ))) %>%
  mutate(nacount = rowSums(is.na(.))) %>% # sum NA count to help with deciding which entry to keep below
  group_by(UUID) %>% # need to remove duplicated data entries 
  arrange(nacount, .by_group = TRUE) %>% # 
  slice(1) %>% # take the instance with most complete data
  ungroup() %>%
  select(-nacount)

# extract baseline date
baselinedates <- lab %>%
  select(UUID, labtest_date) # get ids and labdates from labtest data
# get all ids 
trailid <- baselineq %>%
  select(UUID, Date) %>% # get all ids from baselineq data
  rename(studyentry_date = Date) %>%
  distinct(UUID, .keep_all = TRUE)

# create id dataframe with baseline date.
id <- left_join(trailid, baselinedates, by = c("UUID")) # join together, NA for those not yet completed lab testing

# create demographic info dataframe
demo <- lab %>%
  select(UUID, knee_reference, height, weight, dominantlimb) %>%
  left_join(dob %>% select(UUID, dob, sex, group, age), ., by = "UUID") %>%
  left_join(id, ., by = "UUID") %>%
  mutate(age = trunc((dob %--% studyentry_date) / years(1))) # recalcualte age field from study entry date

lab <- lab %>% select(!c(labtest_date, sex, group, knee_reference, height, weight, dominantlimb)) # remove variables saved separately in demographic df.
lab <- traildates("lab") 


# KOOS
# note koos_A6 question is missing - was not entered into smartabase
koos <- renamevariables("koos") %>% # run rename variable function
  select(!koos_total) %>% 
  mutate(across(c(koos_P1, koos_Q1, koos_PF2:koos_PF3), # recode factors to numeric values
            ~factor(.x, levels = c("Never", "Monthly", "Weekly", "Daily", "Always"), 
                   labels = c(0:4)))) %>%
  mutate(across(c(koos_P2:koos_P9, koos_S1:koos_S2, koos_A1:koos_A17, koos_SP1:koos_SP5, koos_Q4, koos_PF1, koos_PF4:koos_PF10), 
                ~factor(.x, levels = c("None", "Mild", "Moderate", "Severe", "Extreme"), 
                        labels = c(0:4)))) %>%
  mutate(across(c(koos_S3:koos_S5), 
                ~factor(.x, levels = c("Never", "Rarely", "Sometimes", "Often", "Always"), 
                        labels = c(0:4)))) %>%
  mutate(across(c(koos_S6:koos_S7), 
                ~factor(.x, levels = c("Always", "Often", "Sometimes", "Rarely", "Never"), 
                        labels = c(0:4)))) %>%
  mutate(across(c(koos_Q2:koos_Q3, koos_PF11), 
                ~factor(.x, levels = c("Not at all", "Mildly", "Moderately", "Severely", "Totally"), 
                        labels = c(0:4)))) %>%
  mutate(across(c(koos_P1:koos_P9, koos_S1:koos_S7, koos_A1:koos_A17, koos_SP1:koos_SP5, koos_Q1:koos_Q4, koos_PF1:koos_PF11), 
                ~as.numeric(as.character(.x)))) %>% # convert factors to numeric for calculation of totals
  rowwise() %>% # to calculate totals per participant
  mutate(koos_P_total = 100 - (mean(c_across(koos_P1:koos_P9), na.rm = TRUE) * 100) / 4, # formula for calculating koos subscale totals
         koos_S_total = 100 - (mean(c_across(koos_S1:koos_S7), na.rm = TRUE) * 100) / 4,
         koos_A_total = 100 - (mean(c_across(koos_A1:koos_A17), na.rm = TRUE) * 100) / 4,
         koos_SP_total = 100 - (mean(c_across(koos_SP1:koos_SP5), na.rm = TRUE) * 100) /4,
         koos_Q_total = 100 - (mean(c_across(koos_Q1:koos_Q4), na.rm = TRUE) * 100) / 4,
         koos_PF_total = 100 - (mean(c_across(koos_PF1:koos_PF11), na.rm = TRUE) * 100) / 4,
         koos_4_total = mean(c(koos_P_total, koos_S_total, koos_SP_total, koos_Q_total), na.rm = T)) %>% # koos 4 total = total of all subscales except A and PF
  ungroup()
koos <- traildates("koos")

# SPEX
spex <- renamevariables("spex")
spex <- traildates("spex")

# ASSQ
assq <- renamevariables("assq") 
assq <- traildates("assq")

# Tampa
tampa <- renamevariables("tampa") %>% 
  mutate(across(c(tsk_1:tsk_3, tsk_5:tsk_7, tsk_9:tsk_11, tsk_13:tsk_15, tsk_17), # recode factors to numeric values
                ~factor(.x, levels = c("Strongly disagree", "Disagree", "Agree", "Strongly agree"), 
                        labels = c(1:4)))) %>%
  mutate(across(c(tsk_4, tsk_8, tsk_12, tsk_16), # inverse for tsk 4, 8, 12, 16
                ~factor(.x, levels = c("Strongly disagree", "Disagree", "Agree", "Strongly agree"), 
                        labels = c(4:1)))) %>%
  mutate(across(c(tsk_1:tsk_17),
                ~as.numeric(as.character(.x)))) %>%
  rowwise() %>%
  mutate(tsk_total17 = sum(c_across(tsk_1:tsk_17), na.rm = TRUE), # calculate total for original tampa scale
         tsk_total11 = sum(c_across(c(tsk_1:tsk_3, tsk_5:tsk_7, tsk_10, tsk_11, tsk_13, tsk_15, tsk_17)), na.rm = TRUE)) %>% # calc TSK 11 total
  ungroup() %>%
  select(-tsk_total) # remove original total value from export which is incorrect (doesn't inverse 4, 8, 12, 16)
tampa <- traildates("tampa")

# Pass
pass <- renamevariables("pass")
pass <- traildates("pass")

# Visa-a
visaa <- renamevariables("visaa") %>%
  mutate(visaa_1 = case_when(
    visaa_1 == "0 mins" ~ 10,
    visaa_1 == "10 mins" ~ 9,
    visaa_1 == "20 mins" ~ 8,
    visaa_1 == "30mins" ~ 7, # note the typo here
    visaa_1 == "40 mins" ~ 6,
    visaa_1 == "50 mins" ~ 5,
    visaa_1 == "60 mins" ~ 4,
    visaa_1 == "70 mins" ~ 3,
    visaa_1 == "80 mins" ~ 2,
    visaa_1 == "90 mins" ~ 1,
    visaa_1 == "100 mins" ~ 0,
  )) %>%
  mutate(across(c(visaa_2, visaa_3, visaa_4, visaa_5),
                ~case_when(
                  .x == "No pain" ~ 10,
                  .x == "Strong severe pain" ~ 0,
                  .x != "No pain|Strong severe pain" ~ as.numeric(.x)
                )))  %>%
  mutate(visaa_7 = case_when(
    visaa_7 == "Not at all" ~ 0,
    visaa_7 == "Modified training Â± modified competition" ~ 4, 
    visaa_7 == "Full training Â± competition but not at same level as when symptoms began" ~ 7,
    visaa_7 == "Competing at the same or higher level as when Symptoms began" ~ 10
  )) %>%
  mutate(visaa_8a = case_when(
    visaa_8a == ">30 mins" ~ 30,
    visaa_8a == "21-30 mins" ~ 21,
    visaa_8a == "11-20 mins" ~ 14,
    visaa_8a == "1-10 mins" ~ 7,
    visaa_8a == "NIL" ~ 0
  )) %>%
  mutate(visaa_8b = case_when(
    visaa_8b == ">30 mins" ~ 20,
    visaa_8b == "21-30 mins" ~ 14,
    visaa_8b == "11-20 mins" ~ 10,
    visaa_8b == "1-10 mins" ~ 4,
    visaa_8b == "NIL" ~ 0
  )) %>%
  mutate(visaa_8c = case_when(
    visaa_8c == ">30 mins" ~ 10,
    visaa_8c == "21-30 mins" ~ 7,
    visaa_8c == "11-20 mins" ~ 5,
    visaa_8c == "1-10 mins" ~ 2,
    visaa_8c == "NIL" ~ 0
  )) 
visaa <- traildates("visaa")

# Trail baseline
# need to remove full stop column manually.
baselineq <- renamevariables("baselineq") %>%
  select(c(-remove, -Hidden_Shoes, -Hidden_Shoes_Type)) %>% # remove duplicated and blank variables
  mutate(across(c(employment, koabeliefs_8, koabeliefs_9, koabeliefs_11:koabeliefs_13, 
                  knee_medication, women_cycle_change, women_contraception_reason, shoe_brand, 
                  shoe_type, shoe_factors, supports), # select variables which are bracketed by "[]"
                ~str_extract(., "(?<=\\[).*(?=\\])"))) %>% # extract string from within the brackets
  mutate(across(c(employment, koabeliefs_8, koabeliefs_9, koabeliefs_11:koabeliefs_13, 
                  knee_medication, women_cycle_change, women_contraception_reason, shoe_brand, 
                  shoe_type, shoe_factors, supports), # select the same variables again
                na_if, "")) %>% # if extracted string is blank label as NA
  mutate(across(c(l_swelling, r_swelling, l_stiffness, r_stiffness, l_crepitus, r_crepitus, xray_oa, mri_oa),
                ~case_when(
                  .x == "No" ~ 0, # change to numeric values
                  .x == "Yes" ~ 1,
                ))) %>%
  mutate(kneepain_running = ifelse(kneepain_running == "0 - No Pain", 0, kneepain_running)) %>% # change 0 value to numeric
  select(!c(sex, height, weight)) # remove duplicate variables from other questionnaires.
baselineq <- traildates("baselineq")

# phone Screening / Injury Data - Will sit as separate data frame
phonescreen <- renamevariables("phonescreen") %>%
  select(c(-kneesurgery_days, -kneesurgery_years, -contains("llsurgery_"), -postcode, -sex, -group, -finalmessage,
           -phonenumber, -email)) %>% # remove duplicated, identifying or empty variables
  mutate(running_device = ifelse(running_device == "Other", running_device_other, running_device)) %>% # combine running_device into one variable
  select(-running_device_other) %>% # remove "other" variable as now combined.
  select(Date, UUID, contains("kneesurgery"), contains("llsurgery"), contains("kneeinjury"), otherinjury_hx, contains("llinjury"))

# Separate data frames out into each type of info - knee surgery, knee injury, llinjury
kneesurgery <- phonescreen %>%
  select(Date, UUID, starts_with("kneesurgery")) %>%
  rename_at(vars(starts_with("kneesurgery")),
            ~str_replace(., "kneesurgery_", "")) %>%
  mutate(variable = "Knee Sugery")

kneeinjury <- phonescreen %>%
  select(Date, UUID, starts_with("kneeinjury")) %>%
  rename_at(vars(starts_with("kneeinjury")),
            ~str_replace(., "kneeinjury_", "")) %>%
  mutate(variable = "Knee Injury")

llinjury <- phonescreen %>%
  select(Date, UUID, starts_with("llinjury")) %>%
  rename_at(vars(starts_with("llinjury")),
            ~str_replace(., "llinjury_", "")) %>%
  mutate(variable = "Lower Limb Injury")

# join together individual dataframes as long form
injuryinfo <- bind_rows(kneesurgery, kneeinjury) %>%
  bind_rows(., llinjury) %>%
  rename(injurydate = date) %>% # so as not to confuse with Date of collection
  mutate(timepoint = "TP1", 
         grindem = str_extract(grindem, "\\d"), # extract numeric grindem level
         osics_code = str_extract(osics, "[:upper:]{4}")) %>% # extract 4 letter osics code
  select(UUID, -Date, timepoint, variable, everything()) %>%
  filter(!is.na(id), 
         !is.na(injurydate)) %>% # remove empty lines
  arrange(UUID, id)

#need to get unique timpoint.
traildatabase <- reduce(list(koos, spex, assq, tampa, pass, visaa, baselineq, lab), left_join, by = c("UUID", "timepoint", "studyentry_date", "labtest_date")) %>%
  left_join(demo, ., by = c("UUID", "labtest_date", "studyentry_date")) %>%
  rename(timepoint_date = Date.x, 
         id = UUID) %>% # better names
  select(!contains("Date.")) %>%
  filter(!timepoint %in% c("TP2", "TP3", "TP4", "TP5", "TP6")) %>% # remove timepoints after study entry but before lab test (not needed).
  arrange(id)

# Write to csv
#- write_csv(traildatabase, "data/processed/Trail Database v1 090322.csv")
#- wriet_csv(injuryinfo, "data/processed/Trail Injury History v1 090322.csv")

# Export
sheets <- list("Database" = traildatabase, "Injury History" = injuryinfo) # list of different excel sheets
write.xlsx(sheets, "data/processed/Trail Data.xlsx", keepNA = TRUE, na.string = "NA") # write to xlsx file with 2 sheets.

# need to look at training load - summary data

# change names - Date to timepointdate?
# UUID to id?
