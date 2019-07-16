# the number of manuscripts an individual has been assigned by region
editor_data %>% 
  group_by(random.person.id, region) %>% 
  summarise(n = n()) %>% 
  filter(region != "NA") %>%
  ggplot()+
  geom_boxplot(aes(x = region, group = region, y = n, fill = region))+
  #stat_summary(fun.y = median, fun.ymax = length,
              # geom = "text", aes(label = ..ymax..), vjust = -1)+
  scale_y_log10()+
  coord_flip()+
  #scale_colour_discrete(drop = FALSE)+
  #scale_fill_manual(labels = gen_labels)+
  labs(x = "Editor Region", y = "Number of Papers Handled")+
  my_theme_horiz  #figure out how to add n of individuals

ggsave(filename = "assigned_ed_boxplot_geo.png", path = "results/geographic/figures/")

editor_data %>% 
  group_by(journal, random.person.id, region) %>% 
  summarise(n = n()) %>%
  filter(region != "NA") %>%
  ggplot()+
  geom_boxplot(aes(x = region, group = region, y = n, fill = region))+
  #stat_summary(fun.y = median, fun.ymax = length,
               #geom = "text", aes(label = ..ymax..), vjust = -1)+
  facet_wrap(~journal)+
  scale_y_log10()+
  coord_flip()+
  #scale_x_discrete(labels = gen_labels)+
  #scale_fill_manual(values = gen_colors)+
  labs(x = "Editor Region", y = "Number of Papers Handled")+
  my_theme_horiz

ggsave(filename = "j_assigned_ed_boxplot_geo.png", path = "results/geographic/figures/")
