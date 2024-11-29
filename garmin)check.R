 garminactivity <- pull_smartabase(
       form = "Garmin Activity Summary",
       start_date = "01/11/2023",
       end_date = "30/11/2023") %>%
       inner_join(., fusion_id, by = "user_id") %>% # join trail ids
       select(id, everything(), -c(about, UUID))


 garminactivity2 <- pull_smartabase(
   form = "Garmin Activity Summary",
   start_date = "25/12/2022",
   end_date = "25/01/2023") %>%
   inner_join(., fusion_id, by = "user_id") %>% # join trail ids
   select(id, everything(), -c(about, UUID))
 
 

olddata <- read_csv("data/processed/training_load/garmin_activity_20240411.csv")

olddata2 <- read_csv("data/processed/training_load/garmin_activity_20240604_partial.csv")

data <- olddata %>%
  mutate(start_date = as.Date(start_date, format = "%d/%m/%Y")) %>%
  group_by(start_date) %>%
  distinct(id, .keep_all = TRUE) %>%
  summarise(n = length(id)) %>%
  filter((start_date > as.Date("2022-12-25") & start_date < as.Date("2023-01-25"))|(start_date > as.Date("2023-11-01") & start_date < as.Date("2023-12-01")) 
         ) %>%
  mutate(facet = case_when(
    (start_date > as.Date("2022-12-25") & start_date < as.Date("2023-01-25")) ~ "Jan 2023",
    TRUE ~ "Nov 2023"
  ))

data2 <- olddata2 %>%
  mutate(start_date = as.Date(start_date, format = "%d/%m/%Y")) %>%
  group_by(start_date) %>%
  distinct(id, .keep_all = TRUE) %>%
  summarise(n = length(id)) 

newdata <- garminactivity %>%
  mutate(start_date = as.Date(start_date, format = "%d/%m/%Y")) %>%
  group_by(start_date) %>%
  distinct(id, .keep_all = TRUE) %>%
  summarise(n = length(id))  %>%
  filter((start_date > as.Date("2022-12-25") & start_date < as.Date("2023-01-25"))|(start_date > as.Date("2023-11-01") & start_date < as.Date("2023-12-01")) 
  )

newdata2 <- garminactivity2 %>%
  mutate(start_date = as.Date(start_date, format = "%d/%m/%Y")) %>%
  group_by(start_date) %>%
  distinct(id, .keep_all = TRUE) %>%
  summarise(n = length(id))  %>%
  filter((start_date > as.Date("2022-12-25") & start_date < as.Date("2023-01-25"))|(start_date > as.Date("2023-11-01") & start_date < as.Date("2023-12-01")) 
  )

newdata3 <- bind_rows(newdata, newdata2) %>%
  mutate(facet = case_when(
    (start_date > as.Date("2022-12-25") & start_date < as.Date("2023-01-25")) ~ "Jan 2023",
    TRUE ~ "Nov 2023"
  ))


ggplot(data, aes(x = start_date, y = n)) +
  geom_col(newdata3, mapping = aes(x = start_date, y = n), inherit.aes = FALSE, fill = "green") +
  geom_col() +
  facet_wrap(~facet, scales = "free")

ggsave("traildatacheck_20240701.png", width = 6, height = 4)