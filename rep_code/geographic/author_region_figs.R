#Figures showing regions with author types over time
#Region on y-axis, x axis time; faceted by author type
#need author data


#Notes: Need to create a vector (region_list) for it to create plots for each region containing the different author types
#Will need the df to contain authortype

#Setup----

region_list <- c("South Asia", "Europe & Central Asia", "Middle East & North America", "Sub-Saharan Africa", "Latin America & Caribbean", "East Asia & Pacific", "North America")

#create general dataframe to loop each author type through

region_df <- get(each_region) #pull this df from the global environment

#orig is the author type, i.e. uniq_author_data
orig <- paste0(each_region) #isolate the name of the df as a string

print(orig)

region <- case_when( #identify regions being examined, i.e. South Asia, Europe & Central Asia, etc.
  str_detect(orig, "South Asia") ~ "South Asia",
  str_detect(orig, "Europe & Central Asia") ~ "Europe & Central Asia",
  str_detect(orig, "Middle East & North America") ~ "Middle East & North America",
  str_detect(orig, "Sub-Saharan Africa") ~ "Sub-Saharan Africa",
  str_detect(orig, "Latin America & Caribbean") ~ "Latin America & Caribbean",
  str_detect(orig, "East Asia & Pacific") ~ "East Asia & Pacific",
  str_detect(orig, "North America") ~ "North America",
  TRUE ~ "All"
)

#type of manuscript i.e. submitting or publishing
manu_type <- case_when( #identify type of manuscript (if any)
  str_detect(name, "sub") ~ "Submitting",
  str_detect(name, "pub") ~ "Publishing",
  TRUE ~ "Unique"
)

region_df <- region_df %>% 
  filter(GDP %in% gdp_list)

print(manu_type)

#generate dataframe summarising the proportion of each author country of origin for each year----
author_region_prop <- map_dfr(years, function(x){ #map through the vector of years to generate a dataset

 region_df <- author_origin_data %>%
    filter(year == x) %>% #restrict to single year
    group_by(random.person.id) %>% summarise(n = n()) %>% #calculate number of authors per origin in that year
    mutate(proportion = get_percent(n, sum(n))) %>% #add column calculating the proportions for the year, requires analysis_functions.R
    cbind(year = x, .) #add column specifying the year
  
  return(region_df)
})


#figure 1. Proportion of Authors over time in South Asia
ggplot(author_region_prop) +
  geom_line(aes(x = year, y = proportion, linetype = region))+
  coord_cartesian(ylim = c(0, 25))+
  labs(x = "Year", y = "Proportion of Authors in South Asia")+
  scale_x_continuous(limits = c(2012, 2018))+ 
  scale_y_continuous(limits = c(10, 75))+
  my_theme_leg_horiz

ggsave(device = png, filename = paste0(name, "author_region_figs.png"),
       path = "results/geographic/figures")

#figure 2. Proportion of Authors over time in Europe & Central Asia

ggplot(author_region_prop) +
  geom_line(aes(x = year, y = proportion, linetype = region))+
  coord_cartesian(ylim = c(0, 25))+
  labs(x = "Year", y = "Proportion of Authors in Europe & Central Asia")+
  scale_x_continuous(limits = c(2012, 2018))+ 
  scale_y_continuous(limits = c(10, 75))+
  my_theme_leg_horiz

ggsave(device = png, filename = paste0(name, "author_region_figs.png"),
       path = "results/geographic/figures")


#figure 3. Proportion of Authors over time in Middle East & North America

ggplot(author_region_prop) +
  geom_line(aes(x = year, y = proportion, linetype = region))+
  coord_cartesian(ylim = c(0, 25))+
  labs(x = "Year", y = "Proportion of Authors in Middle East & North America")+
  scale_x_continuous(limits = c(2012, 2018))+ 
  scale_y_continuous(limits = c(10, 75))+
  my_theme_leg_horiz

ggsave(device = png, filename = paste0(name, "author_region_figs.png"),
       path = "results/geographic/figures")


#figure 4. Proportion of Authors over time in Middle East & North Africa
ggplot(author_region_prop) +
  geom_line(aes(x = year, y = proportion, linetype = region))+
  coord_cartesian(ylim = c(0, 25))+
  labs(x = "Year", y = "Proportion of Authors in Middle East & North Africa")+
  scale_x_continuous(limits = c(2012, 2018))+ 
  scale_y_continuous(limits = c(10, 75))+
  my_theme_leg_horiz

ggsave(device = png, filename = paste0(name, "author_region_figs.png"),
       path = "results/geographic/figures")



#Figure 5. Proportion of Authors over time in Latin America & Caribbean

ggplot(author_region_prop) +
  geom_line(aes(x = year, y = proportion, linetype = region))+
  coord_cartesian(ylim = c(0, 25))+
  labs(x = "Year", y = "Proportion of Authors in Latin America & Caribbean")+
  scale_x_continuous(limits = c(2012, 2018))+ 
  scale_y_continuous(limits = c(10, 75))+
  my_theme_leg_horiz

ggsave(device = png, filename = paste0(name, "author_region_figs.png"),
       path = "results/geographic/figures")



#Figure 6. Proportion of Authors over time in East Asia & Pacific

ggplot(author_region_prop) +
  geom_line(aes(x = year, y = proportion, linetype = region))+
  coord_cartesian(ylim = c(0, 25))+
  labs(x = "Year", y = "Proportion of Authors in East Asia & Pacific")+
  scale_x_continuous(limits = c(2012, 2018))+ 
  scale_y_continuous(limits = c(10, 75))+
  my_theme_leg_horiz

ggsave(device = png, filename = paste0(name, "author_region_figs.png"),
       path = "results/geographic/figures")



#Figure 7. Proportion of Authors over time in North America
ggplot(author_region_prop) +
  geom_line(aes(x = year, y = proportion, linetype = region))+
  coord_cartesian(ylim = c(0, 25))+
  labs(x = "Year", y = "Proportion of Authors in North America")+
  scale_x_continuous(limits = c(2012, 2018))+ 
  scale_y_continuous(limits = c(10, 75))+
  my_theme_leg_horiz

ggsave(device = png, filename = paste0(name, "author_region_figs.png"),
       path = "results/geographic/figures")




