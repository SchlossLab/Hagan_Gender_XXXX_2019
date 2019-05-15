library(tidyverse)
library(lubridate)

source("code/analysis_functions.R") #functions used during analysis
source("code/get_plot_options.R") #plotting preferences & variables

manu_data <- read_csv("data/2018_manu_ready.csv")

people_data <- read_csv("data/2018_people_ready.csv")

reviews_data <- read_csv("data/2018_reviews_ready.csv")

data <- left_join(manu_data, reviews_data, by = c("random.manu.num", "grouped.random")) %>% 
  left_join(., people_data, by = c("grouped.random", "random.manu.num")) %>% 
  rename("reviewer.country" = "country.x", "reviewer.institution" = "institution.x", "reviewer.gender" = "gender.x", "reviewer.random.id" = "random.person.id.x") %>% #rename.x person info to reviewer info
  select(-role.x) %>% #drop unneeded role.x column (b/c all reviewer)
  filter(year(submitted.date) >= "2011") #drop anything submitted in 2011

carnegie_class <- read_csv("data/carnegie_class.csv") %>% 
  select(name, city, state, Basic)

test <- data %>% 
  left_join(., carnegie_class, by = c("institution.y" = "name")) %>% 
  mutate(
    US.inst = if_else(country.y == "United States", "yes", "no"),
    US.inst.type = case_when(
    !is.na(state) & Basic == "Doctoral Universities: Very High Research Activity" ~ "R1", 
    !is.na(state) & Basic == "Doctoral Universities: High Research Activity" ~ "R2",
    !is.na(state) & !is.na(Basic) ~ "Low research"))

US_ungrouped <- test %>% filter(US.inst == "yes") %>% filter(author.corres == TRUE) %>% select(institution.y, US.inst.type) %>% group_by(US.inst.type, institution.y) %>% summarise(n = n()) %>% filter(is.na(US.inst.type))

write_csv(US_ungrouped, path = "data/ungrouped_inst.csv")