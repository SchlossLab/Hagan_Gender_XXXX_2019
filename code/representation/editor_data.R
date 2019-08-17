#Calculate editor stats for manuscript

#EIC----

prop_eic <- eic_data %>% group_by(gender) %>% 
  summarise(n = n()) %>% 
  mutate(prop = get_percent(n, sum(n)))

num_eic <- sum(prop_eic$n)

#total editors----
prop_ed <- editor_data %>% 
  select(gender, random.person.id) %>% 
  distinct() %>% 
  group_by(gender) %>% 
  summarise(n = n()) %>% 
  mutate(prop = get_percent(n, sum(n)))

num_ed <- sum(prop_ed$n)