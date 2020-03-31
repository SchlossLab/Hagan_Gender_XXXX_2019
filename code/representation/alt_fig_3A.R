n2_US_stats <- inst_stats_data %>% 
  group_by(role, gender, US.inst.type) %>% 
  summarise(n = n()) %>% 
  spread(key = gender, value = n) %>% 
  mutate(sum_inst = female+male+none) %>% 
  mutate_at(vars("female", "male", "none"), funs(get_percent(., sum_inst))) %>% 
  gather(female:none, key = gender, value = percent) 

n2_US_stats %>% 
  filter(role == "author") %>% 
  ggplot()+
  geom_col(aes(y = percent, x = US.inst.type, fill = gender), position = "dodge")+
  coord_flip()
