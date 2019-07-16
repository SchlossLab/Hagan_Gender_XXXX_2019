# the number of manuscripts an individual has been assigned by GDP
#write GDP as "Economy"

editor_data %>% 
  group_by(random.person.id, GDP) %>% 
  summarise(n = n()) %>%
  filter(GDP != "NA") %>%
  ggplot()+
  geom_boxplot(aes(x = GDP, group = GDP, y = n, fill = GDP, color = "grey"))+
  scale_y_log10()+
  coord_flip()+
  #scale_x_discrete(labels = gen_labels)+
  #scale_fill_manual(values = gen_colors)+
  labs(x = "Editor Economy", y = "Number of Papers Handled")+
  #stat_summary(fun.y = median, fun.ymax = length,
      #         geom = "text", aes(label = ..ymax..), vjust = -1)+
  my_theme_horiz  #figure out how to add n of individuals

ggsave(filename = "assigned_ed_gdp_boxplot_geo.png", path = "results/geographic/figures/")

editor_data %>% 
  group_by(journal, random.person.id, GDP) %>% 
  summarise(n = n()) %>% 
  filter(GDP != "NA") %>%
  ggplot()+
  geom_boxplot(aes(x = GDP, group = GDP, y = n, fill = GDP))+
  facet_wrap(~journal)+
  #stat_summary(fun.y = median, fun.ymax = length,
  #          geom = "text", aes(label = ..ymax..), vjust = -1)+
  scale_y_log10()+
  coord_flip()+
  #scale_x_discrete(labels = gen_labels)+
  #scale_fill_manual(values = gen_colors)+
  labs(x = "Editor Economy", y = "Number of Papers Handled")+
  my_theme_horiz

ggsave(filename = "j_assigned_ed_gdp_boxplot_geo.png", path = "results/geographic/figures/")

#Input code to assign color to only North America and High GDP in Box Plots