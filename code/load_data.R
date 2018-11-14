library(tidyverse)
library(lubridate)

manu_data <- read_csv("data/2018_manu_ready.csv")

people_data <- read_csv("data/2018_people_ready.csv")

reviews_data <- read_csv("data/2018_reviews_ready.csv")

data <- left_join(manu_data, reviews_data, by = c("random.manu.num", "grouped.random")) %>% left_join(., people_data, by = c("grouped.random", "random.manu.num")) %>% 
  filter(year(submitted.date) >= "2011")

#research_articles <- c("Full-length text", "Full-Length Text", "New-Data Letter", "Observation", "Research Article", "Short Form", "Short-Form Paper", "Opinion/Hypothesis", "AAM Contribution-Observation", "AAM Contribution-Research Article")

#research_only <- data %>% filter(manuscript.type %in% research_articles)
