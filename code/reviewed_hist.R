# the number of manuscripts an individual has reviewed

data %>% filter(role.y == "reviewer") %>% 
  select(grouped.random, gender.y, random.person.id.y, journal) %>% distinct() %>%
  group_by(journal, random.person.id.y, gender.y) %>% summarise(n = n()) %>% 
  ggplot()+
  geom_histogram(aes(x = n, fill = gender.y), stat = "count")+
  facet_wrap(~journal, scales = "free_x")+
  scale_y_log10()

data %>% filter(role.y == "reviewer") %>% 
  select(grouped.random, gender.y, random.person.id.y, journal) %>% distinct() %>%
  group_by(random.person.id.y, gender.y) %>% summarise(n = n()) %>%
  ggplot()+
  geom_histogram(aes(x = n, fill = gender.y), stat = "count")+
  facet_wrap(~gender.y, scales = "free_x")+
  scale_y_log10()

data %>% filter(role.y == "reviewer") %>% 
  select(grouped.random, gender.y, random.person.id.y, journal) %>% distinct() %>%
  group_by(journal, random.person.id.y, gender.y) %>% summarise(n = n()) %>% 
  ggplot()+
  geom_raster(aes(x = n, y = gender.y, fill = "stat"), stat = "count")
  #facet_wrap(~journal, scales = "free_x")+
  #scale_y_log10()