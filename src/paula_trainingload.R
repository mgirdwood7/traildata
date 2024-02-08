
# Read in 2 different activity files
tl1 <- read_csv("data/processed/training_load/garmin_activity_1.csv") %>% select(-1)
tl2 <- read_csv("data/processed/training_load/garmin_activity_3.csv")
tl3 <- read_csv("data/processed/training_load/garmin_activity_4.csv")
tl4 <- read_csv("data/processed/training_load/garmin_activity_5.csv")


apple1 <- read_csv("data/processed/training_load/Apple Daily Summary.csv")
apple2 <- read_csv("data/processed/training_load/Apple Daily Summary2.csv")


tl1 <- tl1 %>%
  mutate(start_date = dmy(start_date)) %>%
  filter(start_date < "2022-12-01")

tl2 <- tl2 %>%
  mutate(start_date = dmy(start_date))

tl4 <- tl4 %>%
  mutate(start_date = dmy(start_date)) %>%
  filter(start_date > "2022-11-30")

# Bind Together
trainingload <- bind_rows(tl1, tl4)

# Get study entry date
pretrail <- readxl::read_xlsx("data/processed/Trail Database v4.5 20231218.xlsx", sheet = 4, na = "") %>%
  select(id, studyentry_date)

paula <- trainingload %>%
  filter(str_detect(`Activity Type`, "running|RUNNING")) %>% # select only running activities
  select(1:6, `Total Distance`) # take id variabiles and total distance only

joined <- left_join(pretrail, paula, by = "id") %>% # join together to study_entry date for each participant
  filter(str_detect(`Summary Id`, "detail")) %>% # remove duplicate entries (i.e. where garmin and anotehr device duplicate)
  mutate(diff = as.numeric(difftime(start_date, ymd(studyentry_date), units="days"))) %>% # difference between studyentry date and obs date
  filter(diff < 57 & diff >= 0) %>% # remove those outside of the 8 week window
  mutate(week = factor(trunc((diff-1)/7), levels = c("0", "1", "2", "3", "4", "5", "6", "7"))) # convert weeks to factor (for use later)

frequency <- joined %>%
  group_by(id, week) %>%
  summarise(freq = length(`Total Distance`), # calculate week based summaries
            weekdistance = sum(`Total Distance`, na.rm = TRUE)) %>%
  complete(week, fill = list(freq = 0, weekdistance = 0)) %>% # complete fills in the 'blank' weeks where there is no data with 0
  ungroup() %>%
  group_by(id) %>%
  summarise(mean_freq = mean(freq), # now calculate overall aggregates for each participant
            mean_weekdistance = mean(weekdistance, na.rm = TRUE),
            max_weekdistance = max(weekdistance, na.rm = TRUE),
            n_runs = sum(freq)) %>%
  ungroup()

# Calculate max distance across the entire period
maxdistance <- joined %>%
  group_by(id) %>%
  summarise(maxdistance = max(`Total Distance`, na.rm = TRUE)) %>%
  ungroup()

data <- left_join(frequency, maxdistance, by = "id") # join together

data_date <- left_join(data, pretrail, by = "id") %>%
  select(id, studyentry_date, everything())

ogdata <- read_csv("data/processed/training_load/paulacheck.csv") %>%
  mutate(id = str_pad(id, 3, "left", "0"))

datacheck <- left_join(data_date, ogdata, by = "id")


### APPLE

apple <- bind_rows(apple1, apple2)

apple <- apple %>%
  filter(str_detect(`Activity Name`, "running|Running")) %>% # select only running activities
  select(1, start_date, `Total Distance`, `Data Source`, start_time) # take id variabiles and total distance only

apple2 <- apple %>%
  group_by(id, start_date, start_time) %>%
  slice(1) %>%
  ungroup()

apple2_1 <- apple2 %>%
  group_by(id, start_date) %>%
  filter(length(unique(`Data Source`)) == 2) %>%
  slice(1) %>%
  ungroup()

apple3 <- bind_rows(apple2 %>%   group_by(id, start_date) %>%
            filter(length(unique(`Data Source`)) == 1),
          apple2_1)


applejoined <- right_join(pretrail, apple3, by = "id") %>% # join together to study_entry date for each participant
  mutate(diff = as.numeric(difftime(dmy(start_date), ymd(studyentry_date), units="days"))) %>% # difference between studyentry date and obs date
  filter(diff < 57 & diff >= 0) %>% # remove those outside of the 8 week window
  mutate(week = factor(trunc((diff-1)/7), levels = c("0", "1", "2", "3", "4", "5", "6", "7"))) # convert weeks to factor (for use later)

applejoined <- applejoined %>% filter(id != "006")

frequency <- applejoined %>%
  group_by(id, week) %>%
  summarise(freq = length(`Total Distance`), # calculate week based summaries
            weekdistance = sum(`Total Distance`, na.rm = TRUE)) %>%
  complete(week, fill = list(freq = 0, weekdistance = 0)) %>% # complete fills in the 'blank' weeks where there is no data with 0
  ungroup() %>%
  group_by(id) %>%
  summarise(mean_freq = mean(freq), # now calculate overall aggregates for each participant
            mean_weekdistance = mean(weekdistance, na.rm = TRUE),
            max_weekdistance = max(weekdistance, na.rm = TRUE),
            n_runs = sum(freq)) %>%
  ungroup()

# Calculate max distance across the entire period
maxdistance <- applejoined %>%
  group_by(id) %>%
  summarise(maxdistance = max(`Total Distance`, na.rm = TRUE)) %>%
  ungroup()

data <- left_join(frequency, maxdistance, by = "id") # join together

data_date <- left_join(data, pretrail, by = "id") %>%
  select(id, studyentry_date, everything())

