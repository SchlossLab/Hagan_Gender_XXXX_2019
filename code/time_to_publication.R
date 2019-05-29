published <- bias_data %>% 
  filter(published == "yes") %>% 
  select(version, grouped.random, random.manu.num, gender, 
         EJP.decision, contains("days"), journal,
         num.versions, -days.to.review) %>% 
  distinct()

accepted_data <- published %>% 
  filter(version == "0") %>% 
  select(-num.versions) %>% 
  distinct()

#days from submission to production
accepted_data %>% 
  ggplot(aes(x = gender, y = days.pending))+
  geom_boxplot()+
  scale_fill_manual(values = gen_colors)+
  my_theme_horiz

accepted_data %>% 
  ggplot(aes(x = days.pending, fill = gender))+
  geom_density(alpha = 0.5)+
  coord_cartesian(xlim = c(0, 200))+
  scale_fill_manual(values = gen_colors)+
  facet_wrap(~journal)+
  my_theme_horiz


#number of revisions

vers_data <- final_decision %>% 
  select(grouped.random, num.versions) %>% 
  distinct() %>% 
  left_join(accepted_data, ., by = "grouped.random") %>% 
  select(gender, num.versions, grouped.random, journal) %>% 
  distinct()

vers_data %>% 
  ggplot(aes(x = gender, y = num.versions))+
  geom_boxplot()+
  scale_fill_manual(values = gen_colors)+
  my_theme_horiz

vers_data %>% 
  ggplot(aes(x = gender, y = num.versions))+
  geom_boxplot()+
  facet_wrap(~journal)+
  scale_fill_manual(values = gen_colors)+
  my_theme_horiz
