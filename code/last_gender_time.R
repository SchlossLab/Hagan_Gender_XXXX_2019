
author_data <- data %>% filter(role.y == "author") %>% 
  mutate(year = year(submitted.date)) %>% 
  select(year, random.person.id.y, gender.y, contains("auth")) %>% 
  filter(!is.na(year)) %>% 
  mutate(gender.y = fct_explicit_na(gender.y, na_level = "none")) %>% 
  distinct()

author_data %>% 
  filter(author.last == "true") %>% 
  ggplot()+
  #stat_count(aes(x = gender.y), geom = "bar")+
  geom_bar(aes(x = gender.y, fill = gender.y))+
  facet_grid(~year)+
  scale_fill_manual(values = gen_colors)+
  scale_x_discrete(labels = gen_labels)+
  labs(x = "Presenting Gender", y ="Number of Last Authors")+
  my_theme_leg

years <- reviewer_data %>% pull(year) %>% unique()

last_w_prop <- map_dfr(years, function(x){
  df <- author_data %>% 
    filter(author.last == "true") %>% 
    filter(year == x) %>% 
    group_by(gender.y) %>% summarise(n = n()) %>% 
    mutate(proportion = get_percent(n, sum(n))) %>% 
    cbind(year = x, .)
  
  return(df)
}) #%>% filter(gender.y == "female") %>% arrange(year)

ggplot(last_w_prop) + 
  geom_line(aes(x = year, y = proportion, linetype = gender.y))+
  coord_cartesian(ylim = c(0, 100))+
  labs(x = "Year", y = "Proportion of Last Authors")+
  annotate(geom = "text", x = 2018, y = 22, label = "Women")+
  annotate(geom = "text", x = 2018, y = 46, label = "Men")+
  annotate(geom = "text", x = 2018, y = 29, label = "Unclear")+
  my_theme
