#generate author-level analysis of data - requires: 
# 1) load_data.R sourced data & library(lubridate) 
# 2) get_plot_options.R 
# 3) analysis_functions.R

#generate dataset of unqiue authors that submitted each year----
uniq_author_data <- data %>% filter(role == "author") %>% 
  mutate(year = year(submitted.date)) %>% #new column with the year of submission
  select(year, region, GDP, random.person.id, gender, contains("auth"), journal) %>% #restrict to year, region, person, gender & author status
  filter(!is.na(year)) %>% #drop data w/o sub year
  mutate(gender = fct_explicit_na(gender, na_level = "none")) %>% #refactor na values so that they can be plotted correctly
  distinct()#single entry for each year/author combo

 years <- uniq_author_data %>% pull(year) %>% unique()#vector of all years in dataset

#break down uniq_auth_data by type of authorship 
mid_auth <- get_auth_type("middle", uniq_author_data)

corres_auth <- get_auth_type("corres", uniq_author_data)

first_auth <- get_auth_type("first", uniq_author_data)

last_auth <- get_auth_type("last", uniq_author_data)

#dataset of authors published----
pub_author_data <- data %>% 
  filter(role == "author") %>% 
  filter(published == "yes") %>% 
  mutate(year = year(submitted.date)) %>% 
  mutate(gender = fct_explicit_na(gender, na_level = "none")) %>% 
  select(year, region, GDP, random.person.id, gender, contains("auth"), journal, random.manu.num, grouped.random)

mS_years <- pub_author_data %>% 
  filter(journal == "mSystems" | journal == "mSphere") %>% 
  pull(year) %>% unique()

journals <- pub_author_data %>% pull(journal) %>% unique()

#break down pub_auth_data by type of authorship 
pub_mid_auth <- get_auth_type("middle", pub_author_data)

pub_corres_auth <- get_auth_type("corres", pub_author_data)

pub_first_auth <- get_auth_type("first", pub_author_data)

pub_last_auth <- get_auth_type("last", pub_author_data)

#dataset of authors submitted----
sub_author_data <- data %>% 
  filter(role == "author") %>% 
  mutate(year = year(submitted.date)) %>% 
  mutate(gender = fct_explicit_na(gender, na_level = "none")) %>% 
  select(year, random.person.id, gender, country, region, GDP, contains("auth"), journal, random.manu.num, grouped.random)

#break down sub_auth_data by type of authorship 
sub_mid_auth <- get_auth_type("middle", sub_author_data)

sub_corres_auth <- get_auth_type("corres", sub_author_data)

sub_first_auth <- get_auth_type("first", sub_author_data)

sub_last_auth <- get_auth_type("last", sub_author_data)

