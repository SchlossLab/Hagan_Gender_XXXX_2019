# the number of manuscripts an individual has reviewed
reviewer_data %>% 
  group_by(random.person.id, gender) %>% 
  summarise(n = n()) %>% 
  ggplot()+
    geom_boxplot(aes(x = gender, group = gender, y = n, fill = gender))+
    scale_y_log10()+
    coord_flip()+
    scale_x_discrete(labels = gen_labels)+
    scale_fill_manual(values = gen_colors)+
    labs(x = "Reviewer Gender", y = "Number of Papers Reviewed")+
    my_theme_horiz  #figure out how to add n of individuals

ggsave(filename = "repeat_rev_boxplot.png", path = "results/gender/figures/")

reviewer_data %>% 
  group_by(journal, random.person.id, gender) %>% 
  summarise(n = n()) %>% 
  ggplot()+
  geom_boxplot(aes(x = gender, group = gender, y = n, fill = gender))+
  facet_wrap(~journal)+
  scale_y_log10()+
  coord_flip()+
  scale_x_discrete(labels = gen_labels)+
  scale_fill_manual(values = gen_colors)+
  labs(x = "Reviewer Gender", y = "Number of Papers Reviewed")+
  my_theme_horiz

ggsave(filename = "j_repeat_rev_boxplot.png", path = "results/gender/figures/")
