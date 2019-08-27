#setup (libraries & data import)----
library(tidyverse)
library(lubridate)
library(rlang)

source("../code/analysis_functions.R") #functions used during analysis
source("../code/get_plot_options.R") #plotting preferences & variables

manu_data <- read_csv("../data/2018_manu_ready.csv")

people_data <- read_csv("../data/2018_people_ready.csv") #%>% 
  #select(-number_authors)

reviews_data <- read_csv("../data/2018_reviews_ready.csv")

eic_data <- read_csv("../data/eic_genders.csv")

gender_reviews <- people_data %>% 
  select(-role, -contains("auth"), -random.manu.num, -grouped.random, -title) %>% 
  left_join(reviews_data, ., by = "random.person.id") %>% distinct() %>% 
  rename("reviewer.country" = "country", "reviewer.institution" = "institution", "reviewer.gender" = "gender", "reviewer.random.id" = "random.person.id") #rename.x person info to reviewer info

data <- left_join(manu_data, gender_reviews, 
                  by = c("random.manu.num", "grouped.random")) %>% 
  left_join(., people_data, by = c("grouped.random", "random.manu.num")) %>% 
  distinct() %>% 
  filter(year(submitted.date) >= "2011") #drop anything submitted in 2011

#bin US institutions w. carnegie classifications----
source("../code/institution_bins.R")

#merge final dataset ----
decisions <- c("Withdrawn", "Reject", "Revise and re-review",
               "Revise only", "Accept, no revision")

data <- data %>% 
  mutate(institution = str_to_lower(institution),
    US.inst = if_else(country == "United States", "yes", "no")) %>% 
  left_join(., binned_inst, by = "institution") %>% distinct() %>% 
  mutate(gender = fct_explicit_na(gender, na_level = "none"),
         reviewer.gender = fct_explicit_na(reviewer.gender, na_level = "none"),
         US.inst.type = fct_explicit_na(US.inst.type, na_level = "Non-US Inst"),
         EJP.decision = factor(EJP.decision, levels = decisions)) %>% 
  filter(!is.na(year))

#ensure ordered levels  
data$US.inst <- fct_relevel(data$US.inst, inst_list)  

#bias analysis dataset  
bias_data <- data %>% 
  select(-number_authors) %>% 
  filter(author.corres == TRUE) %>% 
  filter(gender != "none") %>% 
  filter(!is.na(gender)) %>% 
  filter(!is.na(journal)) %>% 
  distinct()

#representation analysis datasets
source("../code/author_setup.R")
source("../code/gatekeeper_setup.R")

mjourns <- c("mBio", "mSphere", "mSystems")