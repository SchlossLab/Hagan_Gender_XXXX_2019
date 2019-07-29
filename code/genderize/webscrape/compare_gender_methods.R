library(tidyverse)

gender_join <- read_csv("Processed_Data/B_C_auth_genderize_join.csv")

#calculated a modified probability from genderize p & c -- Pmod = pc+2/c+4 -- cut off set at Pmod >= 0.85 (ref 41)

summary <- gender_join %>% group_by(match) %>% summarise(n = n()) %>% 
  mutate(proportion = round(n/nrow(gender_join), digits = 2)) %>% 
  mutate(match = case_when(match == "0" ~ "incorrect",
                           match == "1" ~ "correct",
                           is.na(match) ~ "not predicted"))

prob_summ <- gender_join %>% group_by(match, probability) %>% summarise(n = n())

gender_compare <- gender_join %>% 
  mutate(mod.probability = ((probability*count+2)/(count+4)))

gender_compare %>% filter(mod.probability >= 0.85) %>% 
  group_by(match) %>% 
  summarise(n = n())

gender_compare %>% filter(probability >= 0.65) %>% group_by(match) %>% 
  summarise(n = n())

