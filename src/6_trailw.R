# Trail W

library(tidyverse)

trailw <- read_csv("data/raw/trailw_20241015.csv")

data <- trailw %>%
  filter(!is.na(trailw_timestamp)) %>%
  mutate(id = str_extract(participant_group, "\\d+")) %>%
  filter(!is.na(id)) %>%
  rename(trailw_date = trailw_timestamp) %>%
  select(id, everything()) %>%
  select(-c(record_id, consenting_date_phone, participant_code_timestamp, ends_with("complete"), 
            ends_with("timestamp"), redcap_survey_identifier, participant_group)) %>%
  select_if(~ !all(is.na(.)))

data <- data %>%
  mutate(gender_identity___1 = case_match(gender_identity___1, 1 ~ "Man", 0 ~ NA_character_),
         gender_identity___2 = case_match(gender_identity___2, 1 ~ "Woman", 0 ~ NA_character_),
         gender_identity___3 = case_match(gender_identity___3, 1 ~ "Gender diverse", 0 ~ NA_character_),
         gender_identity___4 = case_match(gender_identity___4, 1 ~ "Gender not specified", 0 ~ NA_character_),
         gender_identity___5 = case_match(gender_identity___5, 1 ~ "Prefer not to answer", 0 ~ NA_character_)) %>%
  unite("gender", c(gender_identity___1, gender_identity___2, gender_identity___3, gender_identity___4, gender_identity___5),
        na.rm = TRUE, remove = TRUE, sep = "; ")

leaf_score <- data %>%
  select(id, starts_with("leaf")) %>%
  mutate(leaf_3_2a = case_match(leaf_3_2a,
                                c(1,2,4) ~ 0,
                                3 ~ 1,
                                5 ~ 8),
         leaf_3_2b = case_match(leaf_3_2b,
                                1 ~ 0,
                                c(2,3) ~ 1),
         leaf_3_2b1 = case_match(leaf_3_2b1,
                                 c(1,2,3,4) ~ 1),
         leaf_3_2c = case_match(leaf_3_2c,
                                1 ~ 0, 
                                2 ~ 2,
                                3 ~ 1),
         leaf_3_2c1 = leaf_3_2c1 - 1,
         leaf_3_2c3 = case_match(leaf_3_2c3,
                                 1 ~ 1,
                                 c(2,3,4,5) ~ 0),
         leaf_3_2c5 = leaf_3_2c5 - 1,
         leaf_3_2c6 = case_match(leaf_3_2c6,
                                 1 ~ 1,
                                 2 ~ 2,
                                 3 ~ 3,
                                 4 ~ 0),
         leaf_3_2e1___3 = case_match(leaf_3_2e1___3,
                                     1 ~ 2,
                                     0 ~ 0)) %>%
  rowwise() %>%
  mutate(leaf_total = sum(c(leaf_1a, leaf_1a1, leaf_2a, leaf_2b, leaf_2c, leaf_2d,
                            leaf_3_1a1___5, leaf_3_2a, leaf_3_2b,
                          leaf_3_2b1, leaf_3_2c, leaf_3_2c1, leaf_3_2c2, leaf_3_2c3,
                          leaf_3_2c5, leaf_3_2c6, leaf_3_2d, leaf_3_2e, leaf_3_2e1___1,
                          leaf_3_2e1___2, leaf_3_2e1___3), na.rm = TRUE)) %>%
  ungroup() %>%
  select(id, leaf_total)

data <- left_join(data, leaf_score, by = "id") %>%
  select(id:leaf_3_2e1___5, leaf_total, everything())

trail <- readxl::read_xlsx("data/processed/Trail Database v4.6 20240208.xlsx", sheet = 1) %>%
  filter(timepoint == "T00") %>%
  select(id, group, age)

data <- right_join(trail, data, by = "id") %>%
  mutate(across(c(run_alone, run_withother, run_withgroup, run_toilet, run_safety), ~case_match(.x, 
                                                                                                0 ~ "Never",
                                                                                                1 ~ "Sometimes",
                                                                                                2 ~ "Always")))
