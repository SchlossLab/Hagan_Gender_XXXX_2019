#library(tidyverse)

#full b_c dataset w. country data
ASM_country_impact_data <- people %>% 
  select(random.person.id, gender, country) %>% distinct() %>% 
  mutate(predicted = if_else(is.na(gender) == TRUE, "no", "yes") %>% as.factor()) %>% #add binary factor for prediction
  mutate(country = as.factor(country)) %>% #convert countries to factors
  distinct() #unique entries

#summary stats----
ASM_country_list <- ASM_country_impact_data %>% 
  pull(country) %>% unique() #all countries

ASM_num_countries <- length(ASM_country_list) #number of countries

#df of names w. country data that weren't assigned a gender
ASM_na_obs <- ASM_country_impact_data %>% 
  filter(is.na(gender)) %>% distinct()

ASM_num_obs <- ASM_country_impact_data %>% nrow()

ASM_num_na_countries <- ASM_na_obs %>% pull(country) %>% 
  unique() %>% length() #countries associated w. na-gender names

ASM_num_na_obs <- ASM_na_obs %>% nrow() #total na-gender names

#total names assigned gender
ASM_num_predicted <- ASM_country_impact_data %>% 
  filter(!is.na(gender)) %>% nrow()

#percent of names w. country data assigned "na" gender
ASM_percent_unpredicted <- get_percent(ASM_num_na_obs, ASM_num_obs)

#how many names from each country had genders predicted?----
ASM_predictions_by_country <- table(ASM_country_impact_data$country, #count prediction values for each country
                                    ASM_country_impact_data$predicted) %>% 
  as_tibble(., .name_repair = "universal") #convert to tibble

colnames(ASM_predictions_by_country)[1:2] <- c("country", "prediction")

ASM_predictions_by_country <- ASM_predictions_by_country %>% 
  spread(., key = prediction, value = n) %>% #transform table for country column
  mutate(total = yes + no) %>% #calculate total names per country
  mutate(percent = get_percent(no, total)) %>% #calculate per country % na-gendered
  mutate(impact = round(((percent - ASM_percent_unpredicted)*(total/ASM_num_obs))/ASM_percent_unpredicted, digits = 4)) #calculate per country impact on overall na-gendered %

ASM_top_five_num <- list_to_sent(df = ASM_predictions_by_country, 
                             sort = "total", n = 5, pull = "country")

ASM_top_five_na <- list_to_sent(df = ASM_predictions_by_country, 
                            sort = "percent", n = 5, pull = "country")

ASM_max_names_five_na <- ASM_predictions_by_country %>% arrange(desc(percent)) %>% 
  head(n = 5) %>% summarise(max = max(total)) %>% unlist() %>% unname()

ASM_min_names_five_na <- ASM_predictions_by_country %>% arrange(desc(percent)) %>% 
  head(n = 5) %>% summarise(min = min(total)) %>% unlist() %>% unname()

ASM_top_five_impact <- ASM_predictions_by_country %>% filter(impact > 0) %>% 
  list_to_sent(df = ., sort = "impact", n = 5, pull = "country") 

plot_ASM_country_impact <- ASM_predictions_by_country %>% 
  filter(impact > 0) %>% #remove countries w. positive impact on prediction
  ggplot(., aes(x = reorder(country, impact), y = impact))+
  geom_col()+
  geom_text(aes(label = total), vjust = .5, hjust = -.15)+
  coord_flip()+
  labs(x = "Publication Country", y = "Negative Impact on Overall Gender Prediction",
       caption = "Number indicates total number of names associated with the country")+
  my_theme_horiz
