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

#below is commented out b/c I already did this when I generated the 2018_*_ready.csv files----
#research_articles <- c("Full-length text", "Full-Length Text", "New-Data Letter", "Observation", "Research Article", "Short Form", "Short-Form Paper", "Opinion/Hypothesis", "AAM Contribution-Observation", "AAM Contribution-Research Article")

#research_only <- data %>% filter(manuscript.type %in% research_articles)
