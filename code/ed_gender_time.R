#gender of all editors over time

editor_data <- data %>% filter(str_detect(role.y, "editor")) %>% 
  mutate(year = year(submitted.date)) %>% select(year, random.person.id.y, gender.y, role.y) %>% 
  filter(!is.na(year)) %>% 
  distinct()


editor_data %>% 
  filter(role.y == "senior.editor") %>% 
  ggplot()+
  #stat_count(aes(x = gender.y), geom = "bar")+
  geom_bar(aes(x = gender.y))+
  facet_grid(~year)+
  geom_text()
labs(x = "Presenting Gender", y ="Number of Senior Editors")+
  my_theme_leg

years <- editor_data %>% pull(year) %>% unique()

sen_ed_w_prop <- map_dfr(years, function(x){
  df <- editor_data %>% 
      filter(role.y == "senior.editor") %>% 
      filter(year == x) %>% 
      group_by(gender.y) %>% summarise(n = n()) %>% 
      mutate(proportion = get_percent(n, sum(n))) %>% 
      cbind(year = x, .)
  
  return(df)
}) #%>% filter(gender.y == "female") %>% arrange(year)

ggplot(sen_ed_w_prop) + 
  geom_line(aes(x = year, y = proportion, linetype = gender.y))+
  coord_cartesian(ylim = c(0, 100))+
  labs(x = "Year", y = "Proportion of Senior Editors")+
  annotate(geom = "text", x = 2018, y = 31, label = "Women")+
  annotate(geom = "text", x = 2018, y = 64, label = "Men")+
  my_theme
  
editor_data %>% 
  filter(role.y == "editor") %>% 
  ggplot()+
  geom_bar(aes(x = gender.y))+
  facet_grid(~year)+
labs(x = "Presenting Gender", y ="Number of Editors")+
  my_theme_leg

ed_w_prop <- map_dfr(years, function(x){
  df <- editor_data %>% 
    filter(role.y == "editor") %>% 
    filter(year == x) %>% 
    group_by(gender.y) %>% summarise(n = n()) %>% 
    mutate(proportion = get_percent(n, sum(n))) %>% 
    cbind(year = x, .)
  
  return(df)
}) #%>% filter(gender.y == "female") %>% select(-gender.y) %>% arrange(year)


ggplot(ed_w_prop) + 
  geom_line(aes(x = year, y = proportion, linetype = gender.y))+
  coord_cartesian(ylim = c(0, 100))+
  labs(x = "Year", y = "Proportion of Editors")+
  annotate(geom = "text", x = 2018, y = 31, label = "Women")+
  annotate(geom = "text", x = 2018, y = 64, label = "Men")+
  my_theme

