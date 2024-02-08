# Clean File

# lab testing
# This one first as has the lab testing date which is used to calculate/extract timepoint.
lab <- renamevariables("lab") %>%
  mutate(UUID = str_pad(UUID, width = 3, side = "left", pad = 0)) %>% # need to pad out id names to a character string to match other forms
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

# get dob, sex and group from phonescreen form 
dob <- phonescreen %>%
  filter(Eligibility != "Ineligible") %>%
  select(`Research Participant Number`, `Date of Birth`, Eligibility, Sex, Age) %>%
  rename(UUID = `Research Participant Number`,
         dob = `Date of Birth`, 
         sex = Sex,
         group = Eligibility,
         age = Age) %>%
  distinct(UUID, .keep_all = TRUE) %>% 
  filter(!is.na(UUID))

# extract baseline date
baselinedates <- lab %>%
  select(UUID, labtest_date) %>% # get ids and labdates from labtest data
  mutate(labtest_date = dmy(labtest_date),
         UUID = str_pad(UUID, width = 3, side = "left", pad = 0))
# get all ids 
trailid <- baselineq %>%
  select(UUID, start_date) %>% # get all ids from baselineq data
  rename(studyentry_date = start_date) %>%
  mutate(studyentry_date = dmy(studyentry_date),
         UUID = str_pad(UUID, width = 3, side = "left", pad = 0)) %>%
  distinct(UUID, .keep_all = TRUE)

# create id dataframe with baseline date.
id <- left_join(trailid, baselinedates, by = c("UUID")) %>% # join together, NA for those not yet completed lab testing
  filter(!UUID %in% c("4444", "8888888", "0000005768"),
         !is.na(UUID))

# create demographic info dataframe
demo <- lab %>%
  select(UUID, knee_reference, height, weight, dominantlimb) %>%
  left_join(dob %>% select(UUID, dob, sex, group, age), ., by = "UUID") %>%
  left_join(id, ., by = "UUID") %>%
  mutate(dob = dmy(dob),
         age = trunc((dob %--% studyentry_date) / years(1))) # recalcualte age field from study entry date

lab <- lab %>% select(!c(labtest_date, sex, group, knee_reference, height, weight, dominantlimb)) # remove variables saved separately in demographic df.
lab <- traildates("lab") %>%
  filter(!is.na(labtest_date)) %>%
  mutate(timepoint = "T00")


####
# Trail baseline
baselineq <- renamevariables("baselineq") %>%
  select(c(-Hidden_Shoes, -Hidden_Shoes_Type)) %>% # remove duplicated and blank variables
  mutate(across(c(employment, koabeliefs_8, koabeliefs_9, koabeliefs_11, koabeliefs_12, koabeliefs_13, 
                  knee_medication, women_cycle_change, women_contraception_reason, shoe_brand, 
                  shoe_type, shoe_factors, supports), # select variables which are bracketed by "[]"
                ~str_extract(.x, "(?<=\\[).*(?=\\])"))) %>% # extract string from within the brackets
  mutate(across(c(employment, koabeliefs_8, koabeliefs_9, koabeliefs_11, koabeliefs_12, koabeliefs_13, 
                  knee_medication, women_cycle_change, women_contraception_reason, shoe_brand, 
                  shoe_type, shoe_factors, supports), # select the same variables again
                ~na_if(.x, ""))) %>% # if extracted string is blank label as NA
  mutate(across(c(l_swelling, r_swelling, l_stiffness, r_stiffness, l_crepitus, r_crepitus, xray_oa, mri_oa),
                ~case_when(
                  .x == "No" ~ 0, # change to numeric values
                  .x == "Yes" ~ 1,
                ))) %>%
  mutate(kneepain_running = ifelse(kneepain_running == "0 - No Pain", 0, kneepain_running)) %>% # change 0 value to numeric
  select(!c(sex, height, weight)) # remove duplicate variables from other questionnaires.
baselineq <- traildates("baselineq") %>%
  select(UUID:mri_oa, koabeliefs_1:koabeliefs_3, koabeliefs_4, koabeliefs_5:koabeliefs_11, koabeliefs_12, koabeliefs_13, everything())


# KOOS
# note koos_A6 question is missing - was not entered into smartabase initially
koos <- renamevariables("koos") %>% # run rename variable function
  select(!koos_total) %>% 
  select(Date, UUID, koos_p1, koos_p2, koos_p3, koos_p4, koos_p5, koos_p6, koos_p7, koos_p8, koos_p9, # order data per questionnaire, as fusion has jumbled
         koos_s1, koos_s2, koos_s3, koos_s4, koos_s5, koos_s6, koos_s7,
         koos_a1, koos_a2, koos_a3, koos_a4, koos_a5, koos_a6, koos_a7, koos_a8, koos_a9, koos_a10,
         koos_a11, koos_a12, koos_a13, koos_a14, koos_a15, koos_a16, koos_a17,
         koos_sp1, koos_sp2, koos_sp3, koos_sp4, koos_sp5,
         koos_q1, koos_q2, koos_q3, koos_q4, 
         koos_pf1, koos_pf2, koos_pf3, koos_pf4, koos_pf5, koos_pf6, koos_pf7, koos_pf8, koos_pf9, koos_pf10, koos_pf11, 
         contains("total")) %>%
  mutate(across(c(koos_p1, koos_q1, koos_pf2, koos_pf3), # recode factors to numeric values
            ~factor(.x, levels = c("Never", "Monthly", "Weekly", "Daily", "Always"), 
                   labels = c(0:4)))) %>%
  mutate(across(c(koos_p2:koos_p9, koos_s1:koos_s2, koos_a1:koos_a17, koos_sp1:koos_sp5, koos_q4, koos_pf1, koos_pf4:koos_pf10), 
                ~factor(.x, levels = c("None", "Mild", "Moderate", "Severe", "Extreme"), 
                        labels = c(0:4)))) %>%
  mutate(across(c(koos_s3, koos_s4, koos_s5), 
                ~factor(.x, levels = c("Never", "Rarely", "Sometimes", "Often", "Always"), 
                        labels = c(0:4)))) %>%
  mutate(across(c(koos_s6, koos_s7), 
                ~factor(.x, levels = c("Always", "Often", "Sometimes", "Rarely", "Never"), 
                        labels = c(0:4)))) %>%
  mutate(across(c(koos_q2, koos_q3, koos_pf11), 
                ~factor(.x, levels = c("Not at all", "Mildly", "Moderately", "Severely", "Totally"), 
                        labels = c(0:4)))) %>%
  mutate(across(c(koos_p1:koos_p9, koos_s1:koos_s7, koos_a1:koos_a17, koos_sp1:koos_sp5, koos_q1:koos_q4, koos_pf1:koos_pf11), 
                ~as.numeric(as.character(.x)))) %>% # convert factors to numeric for calculation of totals
  rowwise() %>% # to calculate totals per participant
  mutate(koos_p_total = 100 - (mean(c_across(koos_p1:koos_p9), na.rm = TRUE) * 100) / 4, # formula for calculating koos subscale totals
         koos_s_total = 100 - (mean(c_across(koos_s1:koos_s7), na.rm = TRUE) * 100) / 4,
         koos_a_total = 100 - (mean(c_across(koos_a1:koos_a17), na.rm = TRUE) * 100) / 4,
         koos_sp_total = 100 - (mean(c_across(koos_sp1:koos_sp5), na.rm = TRUE) * 100) /4,
         koos_q_total = 100 - (mean(c_across(koos_q1:koos_q4), na.rm = TRUE) * 100) / 4,
         koos_pf_total = 100 - (mean(c_across(koos_pf1:koos_pf11), na.rm = TRUE) * 100) / 4,
         koos_4_total = mean(c(koos_p_total, koos_s_total, koos_sp_total, koos_q_total), na.rm = T)) %>% # koos 4 total = total of all subscales except A and PF
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

# K-SES
kses <- renamevariables("kses")
kses <- traildates("kses")

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
                  TRUE ~ as.numeric(.x)
                )))  %>%
  mutate(visaa_7 = case_when(
    visaa_7 == "Not at all" ~ 0,
    visaa_7 == "Modified training ± modified competition" ~ 4, 
    visaa_7 == "Full training ± competition but not at same level as when symptoms began" ~ 7,
    visaa_7 == "Competing at the same or higher level as when Symptoms began" ~ 10,
    is.na(visaa_7) ~ NA_real_
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

# phone Screening / Injury Data - Will sit as separate data frame
phonescreen <- phonescreen %>%
  group_by(user_id, start_date) %>%
  fill(`Research Participant Number`, .direction = "down") %>%
  ungroup()

phonescreen <- renamevariables("phonescreen") %>%
  select(c(-kneesurgery_days, -kneesurgery_years, -contains("llsurgery_"), -postcode, -sex, -group, -finalmessage,
           -phonenumber, -email)) %>% # remove duplicated, identifying or empty variables
  mutate(running_device = ifelse(running_device == "Other", running_device_other, running_device)) %>% # combine running_device into one variable
  select(-running_device_other) %>% # remove "other" variable as now combined.
  select(Date, UUID, contains("kneesurgery"), contains("llsurgery"), contains("kneeinjury"), otherinjury_hx, contains("llinjury"))

idlist <- id


# Separate data frames out into each type of info - knee surgery, knee injury, llinjury
kneesurgery <- phonescreen %>%
  select(Date, UUID, starts_with("kneesurgery")) %>%
  rename_at(vars(starts_with("kneesurgery")),
            ~str_replace(., "kneesurgery_", "")) %>%
  mutate(variable = "Knee Surgery")

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
  left_join(., dob %>% select(UUID, group), by = "UUID") %>%
  rename(injury_id = id, 
         id = UUID) %>% # more intuative names
  select(id, group, everything()) %>%
  arrange(id, variable, injury_id) %>%
  filter(!id %in% c("4444", "88818", "0006"), 
         !is.na(id)) %>%
  filter(id %in% c(idlist$UUID))


# Clean injury data to get type of surgery, and date of surgery
# Need to define based ont type of surgery first, as may have had subsequent surgeries etc
# Split groups based on - ACLR, Meniscus or other

injuryclean <- injuryinfo %>%
  filter(variable %in% c("Knee Surgery", "Knee Injury")) %>% # discard all llinjury ifnormaiotn
  mutate(aclr = case_when(
    osics_code == "YKLA" ~ 1, # ACL reconstructive surgery code
    TRUE ~ NA_real_),
    aclrinjury = case_when(
      osics_code %in% c("KJAX","KJAG", "KJAR", "KJAC", "KJBX") ~ 1, # ACL injury codes
      TRUE ~ NA_real_
    )) %>%
  group_by(id) %>% # use fill to add above values to all rows per participan
  fill(aclr, .direction = "downup") %>%
  fill(aclrinjury, .direction = "downup") %>%
  ungroup() %>%
  mutate(aclr = case_when(# now need to check that if had 'reconstructive surgery'  that it related to ACL (and not patellar dislocation e.g.)
    aclr == 1 ~ 1, 
    aclrinjury == 1 & osics_code == "YKLX" ~ 1, # "YKLX = "Knee reconstruction' code (not specific to what)
    TRUE ~ NA_real_
  ),
  meniscal = case_when(
    osics_code %in% c("YKCR", "YKCM") ~ 1,
    TRUE ~ NA_real_)) %>%
  group_by(id) %>% # now fill down and up again 
  fill(aclr, .direction = "downup") %>%
  fill(aclrinjury, .direction = "downup") %>%
  fill(meniscal, .direction = "downup") %>%
  ungroup() %>%
  mutate(surgerytype = case_when(
    aclr == 1 ~ "ACLR",
    meniscal == 1 ~ "Meniscal",
    group == "Control" ~ NA_character_,
    TRUE ~ "Other"
  )) %>%
  select(!c(aclrinjury, aclr, meniscal)) %>%
  filter(id %in% c(idlist$UUID))
  

# ACLR subgroup
aclsx <- injuryclean %>%
  mutate(injurydate = dmy(injurydate)) %>%
  filter(group == "Surgery",
         surgerytype == "ACLR", # select those with an ACLR
         variable == "Knee Surgery",
         osics_code %in% c("YKLA", "YKLX")) %>% # select only the row with ACLR data 
  group_by(id) %>%
  arrange(desc(injurydate), .by_group = TRUE) %>%
  slice(1) %>% # take the most recent surgery of this type (e.g. if had multiple ACLR, take most recent)
  ungroup() %>%
  mutate(date_surgerytype = injurydate) %>%
  select(id, surgerytype, date_surgerytype)

# Meniscal/Cartilage subgroup
meniscalsx <- injuryclean %>%
  mutate(injurydate = dmy(injurydate)) %>%
  filter(group == "Surgery",
         surgerytype == "Meniscal",
         variable == "Knee Surgery",
         osics_code %in% c("YKCR", "YKCM")) %>%
  group_by(id) %>%
  arrange(desc(injurydate), .by_group = TRUE) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(date_surgerytype = injurydate) %>%
  select(id, surgerytype, date_surgerytype)

# Other subgroup
othersx <- injuryclean %>%
  mutate(injurydate = dmy(injurydate)) %>%
  filter(group == "Surgery",
         surgerytype == "Other",
         variable == "Knee Surgery") %>%
  group_by(id) %>%
  arrange(desc(injurydate), .by_group = TRUE) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(date_surgerytype = injurydate) %>%
  select(id, surgerytype, date_surgerytype)

# Data-frame of all surgery dates for surgical group
sxdates <- bind_rows(aclsx, meniscalsx) %>%
  bind_rows(., othersx) 

# Individual participant fixes
# 411 complete no pre-baseline (TP1) Proms, and also completed their proms over multiple days so timepoints don't join up. Have to manually tweal
# only affects koos and spex.
# koos <- koos %>%
#  mutate(timepoint = case_when(UUID == "411" & timepoint == "TP1" ~ "T00", TRUE ~ timepoint))
# spex <- spex %>%
#  mutate(timepoint = case_when(UUID == "411" & timepoint == "TP1" ~ "T00", TRUE ~ timepoint))

# 417 also did not complete a pre-baseline timepoint, however all have been allocated "TP1". Will also temporarily change lab to "TP1", so
# they can all join together as one entry. Will then change to T00 once joined.
# lab <- lab %>%
#  mutate(timepoint = case_when(UUID == "417" & timepoint == "T00" ~ "TP1", TRUE ~ timepoint))


#####
## Pre-trail data

# Function to extract the first data instance and join together as pre-baseline timepoint.
predatafunction <- function(x){
 x %>% 
    group_by(UUID) %>%
    arrange(Date) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(timepoint = "TP1")
}

# Data for the Pre-trail timepoint (including all including if not in trail proper)
traildatabase_pre <- list(koos, spex, assq, tampa, pass, visaa, kses, baselineq) %>% 
  map(~predatafunction(.x)) %>% # apply above function to all data parts
  reduce(., left_join, by = c("UUID", "timepoint", "studyentry_date", "labtest_date")) %>%
  left_join(demo, ., by = c("UUID", "labtest_date", "studyentry_date")) %>%
  rename(timepoint_date = Date.x, 
         id = UUID) %>% # better names
  select(!contains("Date.")) %>%
  left_join(., sxdates, by = "id") %>%
  select(id:group, surgerytype, date_surgerytype, everything()) %>%
  arrange(id) %>%
  filter(!id %in% c("4444", "8888888", "0000005768"),
         !is.na(id))


## real trail data
traildatabase <- reduce(list(koos, spex, assq, tampa, pass, visaa, kses, baselineq, lab), left_join, by = c("UUID", "timepoint", "studyentry_date", "labtest_date")) %>%
  bind_rows(., lab %>% filter(UUID %in% c("330")) %>% mutate(Date.x = labtest_date)) %>% # this id didn't complete proms at their lab test, so won't join in properly, have to manually add this timepoint
  left_join(demo, ., by = c("UUID", "labtest_date", "studyentry_date")) %>%
  rename(timepoint_date = Date.x, 
         id = UUID) %>% # better names
  select(!contains("Date.")) %>%
  filter(!timepoint %in% c("TP1", "TP2", "TP3", "TP4", "TP5", "TP6", "TP7", "TP8", "TP9", "TP10")) %>% # remove timepoints after study entry but before lab test (not needed) 
  left_join(., sxdates, by = "id") %>%
  select(id:group, surgerytype, date_surgerytype, everything()) %>%
  arrange(id, factor(timepoint, levels = c("T00", "T06", "T12", "T18", "T24", "T30", "T36", "T42", "T48", "T54", "T60"))) %>%
  filter(!id %in% c("4444", "8888888", "0000005768"),
         !is.na(id),
         !is.na(labtest_date))

monthlypain <- renamevariables("monthlypain") %>%
  select(!ends_with("_x")) %>% # remove duplicated variables
  left_join(., dob %>% select(UUID, group), by = "UUID") %>%
  rename(id = UUID) %>%
  select(id, group, everything()) %>%
  arrange(id) %>%
  filter(!id %in% c("4444", "0000005768"),
         !is.na(id))


# Write to csv
#- write_csv(traildatabase, "data/processed/Trail Database v1 090322.csv")
#- write_csv(injuryinfo, "data/processed/Trail Injury History v1 090322.csv")

# Export
openxlsx_setOp("dateFormat", value = "yyyy-mm-dd") # set date format for openxlsx to write in iso format
sheets <- list("Database" = traildatabase, "Injury History" = injuryinfo, "Montly Pain" = monthlypain, "Pre-Trail" = traildatabase_pre) # list of different excel sheets
write.xlsx(sheets, "data/processed/Trail Database.xlsx", keepNA = TRUE, na.string = "NA") # write to xlsx file with 4 sheets.

# Make file read only
file_path <- "data/processed/Trail Database.xlsx"
Sys.chmod(file_path, "444")
  
  
  
