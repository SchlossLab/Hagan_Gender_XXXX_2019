acc_data <- bias_data %>% 
  select(grouped.random, gender, journal) %>% 
  distinct() %>% 
  filter(journal %in% c("mBio", "mSphere")) %>% 
  group_by(journal, gender) %>% 
  summarise(n = n()) %>% 
  spread(key = journal, value = n) %>% 
  mutate(prop_mBio = get_percent(mBio, mBio + mSphere))

factors_E <- ggplot(acc_data, 
                    aes(x = gender, y = prop_mBio, fill = gender))+
  geom_col(position = "dodge")+
  coord_cartesian(ylim = c(0,100))+
  scale_fill_manual(labels = gen_ed_labels, values = gen_ed_colors)+
  labs(x = "Author Gender", y = "Proportion Submitted to mBio", fill = "Gender")+
  my_theme_horiz
