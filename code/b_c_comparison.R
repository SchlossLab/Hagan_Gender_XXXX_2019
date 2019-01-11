library(tidyverse)
library(caret)
library(e1071)
library(pROC)

#function to convert male/female into binary (1/0)
convert_gender <- function(x){
  case_when(
    x == "male" ~ "0",
    x == "female" ~ "1"
  )
}

#compare genderize outcomes to nichole's data----
gender_data <- read_csv("data/B_C_auth_genderize_join.csv") %>% 
  select(contains("gender"), probability, count) %>% 
  mutate(pmod = (probability*count+2)/(count+4)) %>% 
  filter(!is.na(actual.gender) & !is.na(genderize.gender)) %>% 
  mutate(actual.gender = map(actual.gender, convert_gender) %>% unlist() %>% as.factor()) %>% 
  mutate(genderize.gender = map(genderize.gender, convert_gender) %>% unlist() %>% as.factor())

gender_matrix <- confusionMatrix(gender_data$genderize.gender, gender_data$actual.gender)

#compare genderize outcomes using country codes to nichole's data----
country_gender_data <- read_csv("data/B_C_auth_country_genderize_join.csv") %>% 
  select(contains("gender"), probability, count, match, code) %>% 
  mutate(pmod = (probability*count+2)/(count+4)) %>% 
  filter(!is.na(actual.gender) & !is.na(genderize.gender)) %>% 
  mutate(actual.gender = map(actual.gender, convert_gender) %>% unlist() %>% as.factor()) %>% 
  mutate(genderize.gender = map(genderize.gender, convert_gender) %>% unlist() %>% as.factor())

country_matrix <- confusionMatrix(country_gender_data$genderize.gender, country_gender_data$actual.gender)
