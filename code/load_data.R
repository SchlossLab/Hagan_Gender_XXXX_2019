library(tidyverse)

manu_data <- read_csv("Processed_Data/grouped_manus_2016.csv")

people_data <- read_csv("Processed_Data/grouped_people_2016.csv")

reviews_data <- read_csv("Processed_Data/grouped_reviews_2016.csv")

data <- left_join(manu_data, reviews_data, by = "manuscript.number") %>% left_join(., people_data, by = "manuscript.number")

research_articles <- c("Full-length text", "Full-Length Text", "New-Data Letter", "Observation", "Research Article", "Short Form", "Short-Form Paper", "Opinion/Hypothesis", "AAM Contribution-Observation", "AAM Contribution-Research Article")

research_only <- data %>% filter(manuscript.type %in% research_articles)
