check <- traildatabase %>% 
  filter(!is.na(timepoint)) %>%
  arrange(id, desc(factor(timepoint, levels = c("TP1", "T00", "T06", "T12", "T18", "T24", "T30", "T36", "T42", "T48", "T54", "T60")))) %>%
  group_by(id) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(currenttime = as.numeric(difftime(Sys.Date(), labtest_date, units = "weeks")),
         timepoint_new = cut(currenttime, 
                         breaks = c(-3, 10, 36, 62, 88, 115, 140, 166),
                         labels = c("T00", "T06", "T12", "T18", "T24", "T30", "T36")
                         ),
         timepoint_ready = case_when(
           timepoint_new == "T06" & currenttime > 22 ~ 'Yes',
           timepoint_new == "T12" & currenttime > 48 ~ 'Yes',
           timepoint_new == "T18" & currenttime > 74 ~ 'Yes',
           timepoint_new == "T24" & currenttime > 100 ~ 'Yes',
           timepoint_new == "T30" & currenttime > 126 ~ 'Yes',
           timepoint_new == "T36" & currenttime > 152 ~ 'Yes',
           TRUE ~ "No"
         ),
         timepoint_overdue = case_when(
           timepoint_new == "T06" & currenttime > 26 ~ 'Overdue',
           timepoint_new == "T12" & currenttime > 52 ~ 'Overdue',
           timepoint_new == "T18" & currenttime > 78 ~ 'Overdue',
           timepoint_new == "T24" & currenttime > 104 ~ 'Overdue',
           timepoint_new == "T30" & currenttime > 130 ~ 'Overdue',
           timepoint_new == "T36" & currenttime > 156 ~ 'Overdue',
           TRUE ~ "No"
         )) %>%
  select(id, group, labtest_date, timepoint, currenttime, timepoint_new, timepoint_ready, timepoint_overdue) %>%
  filter(!(timepoint == timepoint_new),
         timepoint_ready == "Yes")