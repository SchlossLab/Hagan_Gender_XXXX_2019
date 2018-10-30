library(tidyverse)

source("code/analysis_functions.R") 

people <- read_csv("data/2018_people_ready.csv")

results <- people %>% select(random.person.id, country, gender) %>% distinct()

num_unique <- nrow(results)

num_na_gender <- results %>% filter(is.na(gender)) %>% nrow()

#breakdown of people by country
country <- results %>% group_by(country) %>% summarise(n = n()) %>% arrange(desc(n)) %>% 
  as.data.frame() %>% mutate(percent = get_percent(n, num_unique))

#breakdown of genders within each country, ranked highest to lowest
gen_by_count <- results %>% group_by(country, gender) %>% summarise(n = n()) %>% 
  as.data.frame() %>% mutate(percent = get_percent(n, num_unique)) %>% arrange(desc(percent))

#breakdown of people by gender
gender <- results %>% group_by(gender) %>% summarise(n = n()) %>% 
  as.data.frame() %>% mutate(percent = get_percent(n, num_unique))

#which countries have the most "unknown" gender names?
unknown_by_count <- results %>% filter(is.na(gender)) %>% group_by(country) %>% summarise(n = n()) %>% 
  as.data.frame() %>% mutate(percent = get_percent(n, num_na_gender)) %>% arrange(desc(percent))

#% authors gender assigned by country ----

country_list <- results %>% filter(!is.na(country)) %>% pull(country) %>% unique()

percent_assigned <- map_df(country_list, function(x){
  
  num_country <- results %>% filter(country == x) %>% nrow()
  
  gender_df <- results %>% filter(country == x) %>% group_by(gender) %>% summarise(n = n()) %>% 
  as.data.frame() %>% mutate(percent = get_percent(n, num_country))
  
  country <- x
  
  country_gender <- cbind(country, gender_df)
  
  return(country_gender)
}
)

num_na_country <- results %>% filter(is.na(country)) %>% nrow()

na_country_assigned <- results %>% filter(is.na(country)) %>% group_by(gender) %>% 
  summarise(n = n()) %>% as.data.frame() %>% mutate(percent = get_percent(n, num_na_country)) %>% 
  cbind(country = NA, .)

percent_assigned <- percent_assigned %>% rbind(na_country_assigned)
