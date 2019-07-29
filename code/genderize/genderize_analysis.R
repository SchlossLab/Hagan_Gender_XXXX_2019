library(tidyverse)

source("../code/analysis_functions.R") 

people <- read_csv("../data/2018_people_ready.csv")

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

percent_assigned <- percent_assigned %>% rbind(na_country_assigned) %>% 
  mutate(bin = case_when(
    n > 10000 ~ ">10,000",
    n <= 10000 & n >= 5001 ~ "5,001 - 10,000",
    n <= 5000 & n >= 1001 ~ "1,001 - 5,000",
    n <= 1000 & n >= 501 ~ "501 - 1,000",
    n <= 500 & n >= 101 ~ "101 - 500",
    n <= 100 & n >= 50 ~ "50 - 100",
    TRUE ~ "<50"
  )) 

percent_assigned %>% 
  ggplot()+
  geom_tile(aes(x = bin, y = gender, fill = percent))+
  scale_x_discrete(limits = c("<50", "50 - 100", "101 - 500", "501 - 1,000", "1,001 - 5,000", "5,001 - 10,000", ">10,000"))
