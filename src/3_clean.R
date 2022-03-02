# Clean File

# Variable renaming

# KOOS
# note koos_A6 is missing - was not entered into smartabase

koos <- renamefunction('koos') %>%
  mutate(across(c(koos_P1, koos_Q1, koos_PF2:koos_PF3), 
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
                ~as.numeric(as.character(.x)))) %>%
  rowwise() %>%
  mutate(koos_P_total = 100 - (mean(c_across(koos_P1:koos_P9), na.rm = TRUE) * 100) / 4,
         koos_S_total = 100 - (mean(c_across(koos_S1:koos_S7), na.rm = TRUE) * 100) / 4,
         koos_A_total = 100 - (mean(c_across(koos_A1:koos_A17), na.rm = TRUE) * 100) / 4,
         koos_SP_total = 100 - (mean(c_across(koos_SP1:koos_SP5), na.rm = TRUE) * 100) /4,
         koos_Q_total = 100 - (mean(c_across(koos_Q1:koos_Q4), na.rm = TRUE) * 100) / 4,
         koos_PF_total = 100 - (mean(c_across(koos_PF1:koos_PF11), na.rm = TRUE) * 100) / 4,
         koos_4_total = mean(c(koos_P_total, koos_S_total, koos_SP_total, koos_Q_total), na.rm = T)) %>%
  ungroup()

# SPEX

# ? write as a function  

# ASSQ

# Tampa

## Code factor
## Need to recalculate total? and check inverse, also calculate tsk_11 total

# Pass

# Visa-a

# Trail baseline
## Shoes question double up? 
## and need to remove '.' column
# remove Hidden_Shoes 
# remove Hidden_Shoes_Type


# phone Screening
## need to remove days since surgery column as auto calculated
## remove postcode due to double up in baseline questionnaire
## remove sex, height, weight, group
## remove finalmessage
## remove contains('llsurgery')

# lab testing and biodex
