
author_data <- data %>% filter(role.y == "author") %>% 
  mutate(year = year(submitted.date)) %>% 
  select(year, random.person.id.y, gender.y, contains("auth")) %>% 
  filter(!is.na(year)) %>% 
  mutate(gender.y = fct_explicit_na(gender.y, na_level = "none")) %>% 
  distinct()

author_data %>% 
  filter(author.seq == "1") %>% 
  ggplot()+
  #stat_count(aes(x = gender.y), geom = "bar")+
  geom_bar(aes(x = gender.y, fill = gender.y))+
  facet_grid(~year)+
  scale_fill_manual(values = gen_colors)+
  scale_x_discrete(labels = gen_labels)+
  labs(x = "Presenting Gender", y ="Number of First Authors")+
  my_theme_leg

years <- reviewer_data %>% pull(year) %>% unique()

first_w_prop <- map_dfr(years, function(x){
  df <- author_data %>% 
    filter(author.seq == "1") %>% 
    filter(year == x) %>% 
    group_by(gender.y) %>% summarise(n = n()) %>% 
    mutate(proportion = get_percent(n, sum(n))) %>% 
    cbind(year = x, .)
  
  return(df)
}) #%>% filter(gender.y == "female") %>% arrange(year)

ggplot(first_w_prop) + 
  geom_line(aes(x = year, y = proportion, linetype = gender.y))+
  coord_cartesian(ylim = c(0, 100))+
  labs(x = "Year", y = "Proportion of First Authors")+
  annotate(geom = "text", x = 2017, y = 33, label = "Women")+
  annotate(geom = "text", x = 2017, y = 42, label = "Men")+
  annotate(geom = "text", x = 2018, y = 25, label = "Unclear")+
  my_theme
