library(tidyverse)
library(GenderGuesser) #genderize package
library(jsonlite) #dependency for genderize

source("Code/genderize_functions.R") #source functions for interacting w. genderize

#load("Processed_Data/B_C_authors_2018-11-15.Rdata") #environment for failed csv

author_list <- read_csv("Processed_Data/B_C_author_list.csv") #saved csv from get_names_B_C_analysis

#B_C_gender_tidy <- B_C_gender_tidy[-860,] #csv failed b/c this URL wouldn't evaluate, pulled it from get_names_B_C_analysis

#Generate df that has two columns: names & actual gender (based on Nicholes data)----
author_list_df <- author_list %>% 
  #cbind(B_C_gender_tidy, authors) %>% #bind webscraping results to url & gender data -- if using .Rdata file
  separate(auth.order, into = c("first.co", "second.co"), sep = ",") %>% #split genders into separate columns
  separate(value, into = c("first.co.name", "second.co.name"), sep = ",", extra = "drop") #split names into sep columns

first_co <- author_list_df %>% select(country, contains("first")) %>% 
  rename(., "actual.gender" = "first.co", "first.name" = "first.co.name") #df of first co-author names/gender/country

second_co <- author_list_df %>% select(country, contains("second")) %>% 
  rename(., "actual.gender" = "second.co", "first.name" = "second.co.name") #df of 2nd co-author names/gender/country

auths_real_gender <- rbind(first_co, second_co) %>% #merge first & 2nd co-author data
  mutate(first.name = str_extract(first.name, "\\w+(?=\\s\\w)")) %>% #separate first names from last
  filter(!is.na(first.name)) %>% #drop any entry that didn't work
  filter(str_detect(first.name, "[:upper:]\\b") == FALSE) %>% #drop initials
  mutate(first.name = iconv(first.name, from = 'UTF-8', to = 'ASCII//TRANSLIT')) %>% 
  distinct() %>% 
  mutate(actual.gender = case_when(str_detect(actual.gender, "m") ~ paste("male"), 
                                   str_detect(actual.gender, "f") ~ paste("female"))) 

#Generate vector of names for genderize w/o country data----
auths_for_genderize <- auths_real_gender %>% select(-actual.gender) %>% #drop actual genders
  filter(str_detect(first.name, "author") == FALSE) %>% #drop values that returned errors
  pull(first.name) %>%  #convert to vector for genderize
  unique()

api <- "" #current api key

genderize_auths <- guessGender(nameVector = auths_for_genderize, apiKey = api) #get gender for names, no country IDs

clean_genderize_auths <- genderize_auths %>% 
  rename("genderize.gender" = "gender")

#df containing names, predicted & actual genders----
gender_join <- full_join(auths_real_gender, clean_genderize_auths, by = c("first.name" = "name")) %>% 
  distinct() %>% 
  mutate(match = if_else(actual.gender == genderize.gender, "1", "0"))

write_csv(gender_join, "Processed_Data/b_c_gender_data/B_C_auth_genderize_join_nosp.csv")

#Check gender based on countries----
auths_by_country <- auths_real_gender %>% 
  mutate(country = fct_collapse(country,
                              "United States" = c("USA", "UCLA"),
                              "Austria" = c("Austria", "Austria/Germany"),
                              "France" = c("France", "Framce", "France/Germany"),
                              "Israel" = c("Israel", "Isral"),
                              "Netherlands" = c("Netherlands", "Nethelands", "USA/Netherlands"),
                              "Sweden" = c("Sweden", "Sweeden", "UK/Sweden"),
                              "Czech Republic" = c("Czech Republic", "CzechR/USA"),
                              "Denmark" = c("Demmark", "Denmark"),
                              "Portugal" = c("Portugal", "Portugal/Hungary"),
                              "United Kingdom" = c("UK", "United Kingdom", "USA/UK"),
                              "Switzerland" = c("Switzerland", "USA/Swi", "USA/Switzerland"),
                              "Australia" = c("Australia", "UK/AUS"),
                              "China" = c("China", "UK/CH"),
                              "Finland" = c("Finland", "UK/Finland", "USA/Finlad"),
                              "Panama" = c("UK/Panama", "Panama"),
                              "Canada" = c("Canada", "USA/Canada"),
                              "Germany" = c("Germany", "USA/Germany"),
                              "Greece" = c("USA/Greece", "Greece"),
                              "Hungary" = c("Hungary", "USA/Hungary"),
                              "Italy" = c("Italy", "USA/Italy"),
                              "Japan" = c("Japan", "USA/Japan"),
                              "Singapore" = c("USA/Singapore", "Singapore"),
                              "Spain" = c("Spain", "USA/Spain"),
                              "Taiwan, , Province of China" = c("Taiwan", "USA/Taiwan"),
                              "Korea, Republic of" = c("Korea", "South Korea"),
                              "Russian Federation" = c("Russia")
                              )) %>% 
  left_join(., iso_code, by = c("country" = "Name"))

auths_for_country_genderize <- auths_by_country %>% 
  mutate(first.name = iconv(first.name, from = 'UTF-8', to = 'ASCII//TRANSLIT')) %>% 
  select(-actual.gender, -country) %>% 
  distinct()

#generate list of all countries in the current dataset  ----
Code_list <- auths_for_country_genderize %>% filter(!is.na(Code)) %>% #remove rows with no ISO code from list of names
  group_by(Code) %>% summarise(n = n()) %>% #group by code & condense into table with summarize
  pull(Code) #generate a list of ISO-coded countries present in this dataset

#df of gender assigned names after looping through---- 
gender_by_country <- map_dfr(Code_list, function(x) get_country_gender(country = x, for_genderize_df = auths_for_country_genderize))

clean_country_genderize_auths <- gender_by_country %>% 
  rename("genderize.gender" = "gender")

country_gender_join <- full_join(auths_by_country, clean_country_genderize_auths, by = c("first.name" = "name", "Code" = "country_id")) %>% 
  distinct() %>% 
  mutate(match = if_else(actual.gender == genderize.gender, "1", "0")) %>% 
  rename("code" = "Code")

write_csv(country_gender_join, "Processed_Data/b_c_gender_data/B_C_auth_country_genderize_join_nosp.csv")
