#generate plots of geographic origin of first authors over time - requires: 
# 1) load_data.R sourced data & library(lubridate) 
# 2) get_plot_options.R 
# 3) analysis_functions.R

#generate dataset of authors that submitted each year - requires data & lubridate from load_data.R
  first_author_origin <- data %>% filter(role == "author") %>% 
  mutate(year = year(submitted.date)) %>% #new column with the year of submission
  select(year, region, random.person.id, country, contains("auth")) %>% #restrict to year, person, country & author status
  filter(!is.na(year)) %>% #drop data w/o sub year
  #mutate(role = fct_explicit_na(role, na_level = "none")) %>% #refactor na values so that they can be plotted correctly
  distinct() #single entry for each year/editor combo

#plot number of first authors by country & year
first_author_origin %>% 
  filter(author.last != "true" & author.seq == "1") %>% #select first authors (not last or middle)
  ggplot()+
  stat_count(aes(x = region), geom = "bar")+
  geom_bar(aes(x = region, fill = region))+
  facet_grid(~year)+ 
  #scale_fill_manual(values = cbPalette)+ #set fill colors
  #scale_x_discrete(labels = gen_labels)+ #set gender labels
  labs(x = "Origin", y ="Number of First Authors")+
  my_theme_leg

years <- first_author_origin %>% pull(year) %>% unique() #vector of all years in dataset

#generate dataframe summarising the proportion of each country that were first authors for each year
first_w_prop <- map_dfr(years, function(x){ #map through the vector of years to generate a dataset
  
  df <- first_author_origin %>% 
    filter(author.last != "true" & author.seq == "1") %>% #select first authors
    filter(year == x) %>% #restrict to single year
    group_by(country) %>% summarise(n = n()) %>% #calculate number of each country in that year
    mutate(proportion = get_percent(n, sum(n))) %>% #add column calculating the proportions for the year, requires analysis_functions.R
    cbind(year = x, .) #add column specifying the year
  
  return(df)
}) #%>% filter(gender == "female") %>% arrange(year) #in case I only want to report the proportion of women

#plot summaries by year
ggplot(first_w_prop) + 
  geom_line(aes(x = year, y = proportion, linetype = country))+
  coord_cartesian(ylim = c(0, 25))+
  labs(x = "Year", y = "Proportion of First Authors")+
  my_theme

ggsave(device = png, filename = paste0(name, "first_origin_time.png"),
       path = "results/geographic/figures")