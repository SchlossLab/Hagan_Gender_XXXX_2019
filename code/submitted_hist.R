# The number of submitted manuscripts by gender & journal - a histogram

data %>% filter(role.y == "author") %>% 
  select(grouped.random, gender.y, random.person.id.y, journal) %>% distinct() %>% 
  group_by(journal, random.person.id.y, gender.y) %>% summarise(n = n()) %>% 
  ggplot()+
  geom_histogram(aes(x = n, fill = gender.y), stat = "count")+
  facet_wrap(~journal, scales = "free_x")+
  scale_y_log10()

# the number of published manuscripts
data %>% filter(role.y == "author") %>% filter(published == "yes") %>% 
  select(grouped.random, gender.y, random.person.id.y, journal) %>% distinct() %>% 
  group_by(journal, random.person.id.y, gender.y) %>% summarise(n = n()) %>% 
  ggplot()+
  geom_histogram(aes(x = n, fill = gender.y), stat = "count")+
  facet_wrap(~journal, scales = "free_x")+
  scale_y_log10()
#%>% summarise(n = n()) %>% 
