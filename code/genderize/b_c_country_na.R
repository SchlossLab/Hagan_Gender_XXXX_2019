#full b_c dataset w. country data
country_impact_data <- read_csv("../data/B_C_auth_country_genderize_join_nosp.csv") %>% 
  filter(!is.na(country)) %>% #drop data w/o associated country
  filter(!is.na(code)) %>% #drop data w countries that aren't supported by genderize
  filter(str_detect(country, "/") == FALSE) %>% #drop data w. 2+ countries
  mutate(predicted = if_else(is.na(genderize.gender) == TRUE, "no", "yes") %>% as.factor()) %>% #add binary factor for prediction
  mutate(country = as.factor(country)) %>% #convert countries to factors
  distinct() #unique entries

#summary stats----
country_list <- country_impact_data %>% 
  pull(country) %>% unique() #all countries

num_countries <- length(country_list) #number of countries

#df of names w. country data that weren't assigned a gender
na_obs <- country_impact_data %>% 
  filter(is.na(genderize.gender) & !is.na(code)) %>% distinct()

num_obs <- country_impact_data %>% nrow()

num_na_countries <- na_obs %>% pull(country) %>% 
  unique() %>% length() #countries associated w. na-gender names

num_na_obs <- na_obs %>% nrow() #total na-gender names

#total names assigned gender
num_predicted <- country_impact_data %>% 
  filter(!is.na(genderize.gender) & !is.na(code)) %>% nrow()

#percent of names w. country data assigned "na" gender
percent_unpredicted <- get_percent(num_na_obs, num_obs)

#how many names from each country had genders predicted?----
predictions_by_country <- table(country_impact_data$country, #count prediction values for each country
                                country_impact_data$predicted) %>% 
  as_tibble(., .name_repair = "universal") #convert to tibble

colnames(predictions_by_country)[1:2] <- c("country", "prediction")

predictions_by_country <- predictions_by_country %>% 
  spread(., key = prediction, value = n) %>% #transform table for country column
  mutate(total = yes + no) %>% #calculate total names per country
  mutate(percent = get_percent(no, total)) %>% #calculate per country % na-gendered
  mutate(impact = round(((percent - percent_unpredicted)*(total/num_obs))/percent_unpredicted, digits = 4)) #calculate per country impact on overall na-gendered %

top_five_num <- list_to_sent(df = predictions_by_country, 
                             sort = "total", n = 5, pull = "country")
  
top_five_na <- list_to_sent(df = predictions_by_country, 
                            sort = "percent", n = 5, pull = "country")

max_names_five_na <- predictions_by_country %>% arrange(desc(percent)) %>% 
  head(n = 5) %>% summarise(max = max(total)) %>% unlist() %>% unname()

min_names_five_na <- predictions_by_country %>% arrange(desc(percent)) %>% 
  head(n = 5) %>% summarise(min = min(total)) %>% unlist() %>% unname()

top_five_impact <- predictions_by_country %>% filter(impact > 0) %>% 
  list_to_sent(df = ., sort = "impact", n = 5, pull = "country") 

plot_country_impact <- predictions_by_country %>% 
  filter(impact > 0) %>% #remove countries w. positive impact on prediction
  ggplot(., aes(x = reorder(country, impact), y = impact))+
  geom_col()+
  geom_text(aes(label = total), vjust = .5, hjust = -.15)+
  coord_flip()+
  labs(x = "Publication Country", y = "Negative Impact on Overall Gender Prediction",
       caption = "Number indicates total number of names associated with the country")+
  my_theme_horiz

#which countries have the most "unknown" gender names?
#na_by_country <- na_obs %>%
#  group_by(country) %>% summarise(n = n()) %>% 
#  as.data.frame() %>% mutate(percent = get_percent(n, num_na_obs)) %>% arrange(desc(percent))
#
#ggplot(na_by_country, aes(x = reorder(country, percent), y = percent))+
#  geom_col()+
#  geom_text(aes(label = n), vjust = -0.5)+
#  labs(x = "Publication Country", y = "Percent of Unpredicted Genders",
#       caption = "Number at top of bar indicates number of names lacking gender predictions")+
#  my_theme
