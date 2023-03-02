# Data checks
library(tidyverse)
library(d)

data <- readxl::read_xlsx("data/processed/Trail Data.xlsx", sheet = 1, na = "NA")

data %>%
  #select(id, biodex_seatheight:r_biodex_hs_pain) %>%
  select(where(is.numeric)) %>%
  pivot_longer(everything(), "var", "value") %>%
  ggplot(., aes(x = value)) + 
  geom_histogram() +
  facet_wrap(~var, scales = "free")

data %>%
  select(id, l_leg_length, r_leg_length, l_thigh_length, r_thigh_length) %>%
  filter(l_leg_length < 70)


heatmap <- traildatabase %>%
  #filter(!id %in% c("888888","0000005768")) %>%
  select(id, studyentry_date, labtest_date, sex, group, timepoint, timepoint_date) %>%
  pivot_wider(id_cols = c(id, studyentry_date, labtest_date, sex, group),
              names_from = timepoint,
              values_from = timepoint_date)



heatmap2 <- heatmap %>%
  select(id, TP1, labtest_date, T00, T06, T12, T18) %>%
  mutate(across(c(TP1:T18), ~case_when(
    is.na(.) ~ 0,
    TRUE ~ 1
  )))

  mutate(sex = as.numeric(factor(sex)),
         group = as.numeric(factor(group)),
         T00 = as.numeric(T00)) %>%
  drop_na()

d3heatmap(heatmap2,
          fontsize = 6, 
          #col=c("#d53e4f", "#99d594"),
          height = 2000,
          dendrogram = "none",
          labRow = heatmap2$id,
          na.rm = TRUE)