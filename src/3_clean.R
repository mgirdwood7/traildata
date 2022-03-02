# Clean File

# KOOS
# note koos_A6 question is missing - was not entered into smartabase

koos <- renamevariables("koos") %>% # run rename variable function
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

# SPEX

spex <- renamevariables(spex)

# ASSQ

assq <- renamevariables(assq)

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

# Pass

pass <- renamevariables("pass")

# Visa-a

visaa <- renamevariables("visaa")

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
                na_if, "")) # if extracted string is blank label as NA


# phone Screening
## need to remove days since surgery column as auto calculated
## remove postcode due to double up in baseline questionnaire
## remove sex, height, weight, group
## remove finalmessage
## remove contains('llsurgery')

# lab testing and biodex
