#generate figures to summarize reviewer data

#A. Proportion of Potential Reviewers suggested each Year---
pot_rev_w_prop <- map_dfr(years, function(x){
  get_prop_by_yr(x, pot_rev_data, "region", "All")})

#figure out which year is the last & isolate the proportion values
text_values <- get_gen_prop_text(pot_rev_w_prop, 7, "region")

max_value <- get_ymax(pot_rev_w_prop) 

#line plot of all journals combined by year
reviewer_A <- geographic_line_plot(pot_rev_w_prop, max_value) + 
  labs(x = "Year", y = paste("Proportion of Potential Reviewers"),
       caption = paste("Proportion of potential reviewers for submitted manuscripts each year. Each person is counted once per year"))

#B. Number of papers reviewed by Region----
reviewer_C <- data %>% filter(role == "reviewer") %>% 
  mutate(year = year(submitted.date)) %>% 
  select(year, random.person.id, grouped.random, region) %>% 
  filter(!is.na(year)) %>% 
  mutate(region = fct_explicit_na(region, na_level = "none")) %>% 
  distinct() %>% #doesn't have the manuscript ids
  group_by(random.person.id, region) %>% 
  summarise(n = n()) %>%
  ggplot()+
  geom_boxplot(aes(x = region, group = region, y = n, fill = region))+
  scale_y_log10()+
  #coord_cartesian(ylim = c(0, 8))+
  coord_flip()+
  scale_fill_manual(values = gen_colors)+
  labs(x = "Reviewer Region", y = "Number of Papers Reviewed")+
  my_theme_horiz  #figure out how to add n of individuals
