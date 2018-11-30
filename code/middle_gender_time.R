#generate plots of middle author genders over time - requires: 
# 1) load_data.R sourced data & library(lubridate) 
# 2) get_plot_options.R 
# 3) analysis_functions.R

#generate dataset of authors that submitted each year - requires data & lubridate from load_data.R
author_data <- data %>% filter(role.y == "author") %>% 
  mutate(year = year(submitted.date)) %>% #new column with the year of submission
  select(year, random.person.id.y, gender.y, contains("auth")) %>% #restrict to year, person, gender & author status
  filter(!is.na(year)) %>% #drop data w/o sub year
  mutate(gender.y = fct_explicit_na(gender.y, na_level = "none")) %>% #refactor na values so that they can be plotted correctly
  distinct() #single entry for each year/author combo

#plot number of middle authors by gender & year
author_data %>% 
  filter(author.last != "true" & author.seq != "1") %>% #select middle authors (not last or first)
  ggplot()+
  #stat_count(aes(x = gender.y), geom = "bar")+
  geom_bar(aes(x = gender.y, fill = gender.y))+
  facet_grid(~year)+ 
  scale_fill_manual(values = gen_colors)+ #set fill colors
  scale_x_discrete(labels = gen_labels)+ #set gender labels
  labs(x = "Presenting Gender", y ="Number of Middle Authors")+
  my_theme_leg

years <- author_data %>% pull(year) %>% unique() #vector of all years in dataset

#generate dataframe summarising the proportion of each gender that were middle authors for each year
middle_w_prop <- map_dfr(years, function(x){ #map through the vector of years to generate a dataset
  
  df <- author_data %>% 
    filter(author.last != "true" & author.seq != "1") %>% #select middle authors
    filter(year == x) %>% #restrict to single year
    group_by(gender.y) %>% summarise(n = n()) %>% #calculate number of each gender in that year
    mutate(proportion = get_percent(n, sum(n))) %>% #add column calculating the proportions for the year, requires analysis_functions.R
    cbind(year = x, .) #add column specifying the year
  
  return(df)
}) #%>% filter(gender.y == "female") %>% arrange(year) #in case I only want to report the proportion of women

#plot summaries by year
ggplot(middle_w_prop) + 
  geom_line(aes(x = year, y = proportion, linetype = gender.y))+
  coord_cartesian(ylim = c(0, 100))+
  labs(x = "Year", y = "Proportion of Middle Authors")+
  annotate(geom = "text", x = 2018, y = 34, label = "Women")+ #add text labels to each line
  annotate(geom = "text", x = 2018, y = 45, label = "Men")+
  annotate(geom = "text", x = 2018, y = 25, label = "Unclear")+
  my_theme

