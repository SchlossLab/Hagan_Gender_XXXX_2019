#generate gatekeeper-level analysis of data - requires: 
# 1) load_data.R sourced data & library(lubridate) 
# 2) get_plot_options.R 
# 3) analysis_functions.R

#editor data----
editor_data <- data %>% filter(str_detect(role, "editor")) %>% 
  mutate(year = year(submitted.date)) %>% 
  select(year, grouped.random, random.person.id, country, gender, 
         role, journal) %>% 
  filter(gender == "male" | gender == "female") %>% 
  distinct()

editors <- editor_data %>% 
  filter(role == "editor")

senior_ed <- editor_data %>% 
  filter(role == "senior.editor")

#reviewer data----
reviewer_data <- data %>% filter(role == "reviewer") %>% 
  mutate(year = year(submitted.date)) %>% 
  select(year, grouped.random, random.person.id, country, gender, 
         role, journal) %>%
  distinct()

#potential reviewer data----
pot_rev_data <- data %>% 
  filter(role == "potential.reviewer") %>% 
  mutate(year = year(submitted.date)) %>% 
  distinct()

#metadata----
years <- reviewer_data %>% pull(year) %>% unique()

journal_list <- data %>% pull(journal) %>% unique()

#eic data ----
eic_data <- eic_data %>% 
  select(-EiC) %>% 
  filter(journal %in% journal_list) %>% 
  rename(gender = EiC.gender)

