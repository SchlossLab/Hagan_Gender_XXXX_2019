# the number of manuscripts an individual has been assigned
editor_data %>% 
  group_by(random.person.id, gender) %>% 
  summarise(n = n()) %>% 
  ggplot()+
    geom_boxplot(aes(x = gender, group = gender, y = n, fill = gender))+
    scale_y_log10()+
    coord_flip()+
    scale_x_discrete(labels = gen_labels)+
    scale_fill_manual(values = gen_colors)+
    labs(x = "Editor Gender", y = "Number of Papers Handled")+
    my_theme_horiz  #figure out how to add n of individuals

ggsave(filename = "assigned_ed_boxplot.png", path = "results/gender/figures/")

editor_data %>% 
  group_by(journal, random.person.id, gender) %>% 
  summarise(n = n()) %>% 
  ggplot()+
  geom_boxplot(aes(x = gender, group = gender, y = n, fill = gender))+
  facet_wrap(~journal)+
  scale_y_log10()+
  coord_flip()+
  scale_x_discrete(labels = gen_labels)+
  scale_fill_manual(values = gen_colors)+
  labs(x = "Editor Gender", y = "Number of Papers Handled")+
  my_theme_horiz

ggsave(filename = "j_assigned_ed_boxplot.png", path = "results/gender/figures/")

#editor_data %>% 
#  group_by(random.person.id, gender) %>% 
#  summarise(n = n()) %>% 
#  group_by(gender) %>% 
#  summarise(n_gen = n(), n_papers = sum(n)) %>%
#  mutate(ratio = n_papers/n_gen) %>%
#  ggplot()+
#  geom_boxplot(aes(x = gender, group = gender, y = ratio, fill #= gender))+
#  coord_flip()+
#  scale_x_discrete(labels = gen_labels)+
#  scale_fill_manual(values = gen_colors)+
#  labs(x = "Editor Gender", y = "Number of Papers Handled")+
#  my_theme_horiz
