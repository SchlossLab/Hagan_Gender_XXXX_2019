# The number of submitted manuscripts by region & journal - a histogram
sub_author_data %>% 
  group_by(journal, random.person.id, region) %>% summarise(n = n()) %>% 
  ggplot()+
  geom_boxplot(aes(x = region, y = n, fill = region))+
  facet_wrap(~journal)+
  scale_y_log10()+
  coord_flip()+
  #scale_x_discrete(labels = gen_labels)+
  #scale_fill_manual(values = gen_colors)+
  labs(x = "Author Region", y = "Number of Unique Papers Submitted")+
  my_theme_horiz


# sub_author_data %>% 
#  select(region) %>% distinct() %>% 
# group_by(region) %>%
# summarise()

ggsave(filename = "repeat_subs_by_journ_boxplot_geo.png", path = "results/geographic/figures/")

sub_author_data %>% 
  group_by(random.person.id, region) %>% summarise(n = n()) %>% 
  ggplot()+
  geom_boxplot(aes(x = region, y = n, fill = region))+
  scale_y_log10()+
  coord_flip()+
  #scale_x_discrete(labels = gen_labels)+
  #scale_fill_manual(values = gen_colors)+
  labs(x = "Author Region", y = "Number of Unique Papers Submitted")+
  my_theme_horiz

ggsave(filename = "repeat_subs_boxplot_geo.png", path = "results/geographic/figures/")

# the number of published manuscripts by journal
pub_author_data %>%  
  group_by(journal, random.person.id, region) %>% summarise(n = n()) %>% 
  ggplot()+
  geom_boxplot(aes(x = region, y = n, fill = region))+
  facet_wrap(~journal)+
  scale_y_log10()+
  coord_flip()+
  #scale_x_discrete(labels = gen_labels)+
  #scale_fill_manual(values = gen_colors)+
  labs(x = "Author Region", y = "Number of Unique Papers Published")+
  my_theme_horiz

ggsave(filename = "repeat_pubs_by_journ_boxplot_geo.png", path = "results/geographic/figures/")

pub_author_data %>%  
  group_by(random.person.id, region) %>% summarise(n = n()) %>% 
  ggplot()+
  geom_boxplot(aes(x = region, y = n, fill = region))+
  scale_y_log10()+
  coord_flip()+
  #scale_x_discrete(labels = gen_labels)+
  #scale_fill_manual(values = gen_colors)+
  labs(x = "Author Region", y = "Number of Unique Papers Published")+
  my_theme_horiz

ggsave(filename = "repeat_pubs_boxplot_geo.png", path = "results/geographic/figures/")