library(tidyverse)
library(lubridate)

monthly <- readxl::read_xlsx("data/processed/Trail Data.xlsx", sheet = 3, na = "NA")

monthlydate <- monthly %>%
  group_by(id) %>%
  arrange(id, Date) %>%
  slice(1) %>%
  ungroup() %>%
  select(id, Date) %>%
  rename(first_date = Date) 

monthly_join <- left_join(monthly, monthlydate, by = "id") %>%
  mutate(diff = interval(first_date, Date),
         new1 = as.numeric(diff, "months"),
         month = round(as.numeric(diff, "months")),
         ostrc_pain_knee = factor(ostrc_pain_knee, levels = 
                                    c("No knee pain when running in the past 7 days",
                                      "Mild knee pain when running in the past 7 days",
                                      "Moderate knee pain when running in the past 7 days",
                                      "Severe knee pain when running in the past 7 days",
                                      "Could not participate in running at all in the past 7 days due to knee pain")),
         ostrc_achilles = factor(ostrc_achilles, levels = 
                                    c("No Achilles pain when running in the past 7 days",
                                      "Mild Achilles pain when running in the past 7 days",
                                      "Moderate Achilles pain when running in the past 7 days",
                                      "Severe Achilles pain when running in the past 7 days",
                                      "Could not participate in running at all in the past 7 days due to Achilles pain")),
         ostrc_pain_other = factor(ostrc_pain_other, levels = 
                                   c("No other pain when running in the past 7 days",
                                     "Mild other pain when running in the past 7 days",
                                     "Moderate other pain when running in the past 7 days",
                                     "Severe other pain when running in the past 7 days",
                                     "Could not participate in running at all in the past 7 days due to other pain"))) %>%
  mutate(fill = "yes") 

ggplot(monthly_join, aes(x = month, y = id, fill = ostrc_pain_knee)) + 
  geom_tile(colour = "white") +
  scale_fill_manual(values = c("#1a9641", "#a6d96a", "#ffffbf", "#fdae61" , "#d7191c"), guide = "none") +
  labs(title = "Knee Pain")

ggplot(monthly_join, aes(x = month, y = id, fill = ostrc_achilles)) + 
  geom_tile(colour = "white") +
  scale_fill_manual(values = c("#1a9641", "#a6d96a", "#ffffbf", "#fdae61" , "#d7191c"), guide = "none") +
  labs(title = "Achilles Pain")

ggplot(monthly_join, aes(x = month, y = id, fill = ostrc_pain_other)) + 
  geom_tile(colour = "white") +
  scale_fill_manual(values = c("#1a9641", "#a6d96a", "#ffffbf", "#fdae61" , "#d7191c"), guide = "none") +
  labs(title = "Other Pain")


monthly_join %>%
  group_by(id) %>%
  distinct(month, .keep_all = TRUE) %>%
  ggplot(., aes(x = month)) + 
  geom_bar()


test <- monthly_join %>%
  mutate(across(c(ostrc_pain_knee, ostrc_pain_other, ostrc_achilles), as.numeric)) %>%
  mutate(ostrc_pain_knee_1 = if_else(ostrc_pain_knee > 3, "Yes", "No"),
         ostrc_pain_other_1 = if_else(ostrc_pain_other > 3, "Yes", "No"),
         ostrc_achilles_1 = if_else(ostrc_achilles > 3, "Yes", "No")) %>%
  group_by(id) %>%
  distinct(month, .keep_all = TRUE) %>%
  ungroup() %>%
  filter(ostrc_pain_knee_1 == "Yes" | ostrc_pain_other_1 == "Yes" | ostrc_achilles == "Yes") %>%
  group_by(month) %>%
  summarise(n = length(id)) %>%
  summarise(mean = mean(n))


  

as.numeric(difftime(date2, date1, units = "months"))