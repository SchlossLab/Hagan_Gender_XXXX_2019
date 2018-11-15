

reviewer_data <- data %>% filter(str_detect(role.y, "reviewer")) %>% 
  mutate(year = year(submitted.date)) %>% select(year, random.person.id.y, gender.y, role.y) %>% 
  filter(!is.na(year)) %>% 
  mutate(gender.y = fct_explicit_na(gender.y, na_level = "none")) %>% 
  distinct()

reviewer_data %>% 
  ggplot()+
  #stat_count(aes(x = gender.y), geom = "bar")+
  geom_bar(aes(x = gender.y, fill = gender.y))+
  facet_grid(~year)+
  scale_fill_manual(values = gen_colors)+
  scale_x_discrete(labels = gen_labels)+
  labs(x = "Presenting Gender", y ="Number of Reviewers")+
  my_theme_leg

years <- reviewer_data %>% pull(year) %>% unique()

reviewer_w_prop <- map_dfr(years, function(x){
  df <- reviewer_data %>% 
    filter(year == x) %>% 
    group_by(gender.y) %>% summarise(n = n()) %>% 
    mutate(proportion = get_percent(n, sum(n))) %>% 
    cbind(year = x, .)
  
  return(df)
}) #%>% filter(gender.y == "female") %>% arrange(year)

ggplot(reviewer_w_prop) + 
  geom_line(aes(x = year, y = proportion, linetype = gender.y))+
  coord_cartesian(ylim = c(0, 100))+
  labs(x = "Year", y = "Proportion of Reviewers")+
  annotate(geom = "text", x = 2018, y = 31, label = "Women")+
  annotate(geom = "text", x = 2018, y = 64, label = "Men")+
  annotate(geom = "text", x = 2018, y = 14, label = "Unclear")+
  my_theme
