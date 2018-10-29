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

#% authors gender assigned by country

country_list <- results %>% pull(country) %>% unique()

percent_assigned <- map_df(country_list, function(x){
  num_country <- results %>% filter(country == x) %>% nrow()
  
  gender_df <- results %>% filter(country == x) %>% group_by(gender) %>% summarise(n = n()) %>% 
  as.data.frame() %>% mutate(percent = get_percent(n, num_country))
  
  country_gender <- cbind(country = paste(x), gender_df) #can't get this to work...
  
  return(country_gender)
}
)

#US authors gender assigned
num_us <- results %>% filter(country == "United States") %>% nrow()

us_gender <- results %>% filter(country == "United States") %>% group_by(gender) %>% summarise(n = n()) %>% 
  as.data.frame() %>% cbind(country = "United States", .) %>% mutate(percent = get_percent(n, num_us))

#Japan authors gender assigned
num_japan <- results %>% filter(country == "Japan") %>% nrow()

japan_gender <- results %>% filter(country == "Japan") %>% group_by(gender) %>% summarise(n = n()) %>% 
  as.data.frame() %>% mutate(percent = get_percent(n, num_japan))
