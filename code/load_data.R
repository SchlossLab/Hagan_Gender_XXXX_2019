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

us_industries <- read_csv("data/us_industries.csv") %>% 
  pull(institution.name) %>% 
  str_to_lower(.) %>% unique()

fed_labs <- read_csv("data/fed_state_labs.csv") %>% 
  pull(institution.name) %>% 
  str_to_lower(.) %>% unique()

R1 <- carnegie_class %>% 
  filter(Basic == "Doctoral Universities: Very High Research Activity") %>% 
  pull(name) %>% str_to_lower(.)

R2 <- carnegie_class %>% 
  filter(Basic == "Doctoral Universities: High Research Activity") %>% 
  pull(name) %>% str_to_lower(.)

low_research <- carnegie_class %>% 
  filter(str_detect(Basic, "High Research") == FALSE) %>% 
  pull(name) %>% str_to_lower(.)

test <- data %>% 
  mutate(institution.y = str_to_lower(institution.y)) %>% 
  mutate(
    US.inst = if_else(country.y == "United States", "yes", "no"),
    US.inst.type = case_when(
      institution.y %in% R1 | str_detect(institution.y, ) ~ "R1",
      institution.y %in% R2 ~ "R2",
      institution.y %in% low_research ~ "Low research",
      institution.y %in% fed_labs ~ "Federal",
      institution.y %in% us_industries ~ "Industry"
))

industry <- c("abbott", "abbvie", "accelerate", "achaogen", "achillion", "amgen",
              "allergan", "amplyx", "pharmaceuticals", "astrazeneca", "avidbiotics",
              "biopharma", "diagnostics", "bd", "beckman coulter", "bio rad", 
              "biorad", "biomerieux", "myers squibb", "chembio", "cellex",
              "therapeutics", "dupont", "genentech")

govt <- c("usda", "agricultural research service", "air force", 
          "regional healthcare", "state health", "armed forces", "nasa", 
          "department of public health", "fda", "cdc", "national cancer institute", 
          "food and drug administration", "cleveland clinic", "health department",
          "homeland security", "veterans affairs", "environmental protection agency",
          "epa", "national lab")

r1 <- c("harvard", "binghamton", "boston children's hospital",
        "george washington", "brigham and women's", "boston university", 
        "cornell university", "cuny graduate", "dana farber", "dartmouth",
        "broad institute", "case western", "children's hospital", "children's mercy",
        "university of michigan", "clemson", "columbia university", "duke",
        "emory university", "auburn", "georgetown university")

r2 <- c("cuny city", "east tennessee state", "baylor", "eastern michigan")

low <- c("baptist health", "california state")

US_ungrouped <- test %>% filter(US.inst == "yes") %>% 
  filter(author.corres == TRUE) %>% 
  select(institution.y, US.inst.type) %>% 
  group_by(US.inst.type, institution.y) %>% 
  filter(is.na(US.inst.type)) %>% 
  summarise(n = n()) %>% View()

#write_csv(US_ungrouped, path = "data/ungrouped_inst.csv")