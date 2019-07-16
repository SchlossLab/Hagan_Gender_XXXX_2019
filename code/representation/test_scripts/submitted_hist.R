# The number of submitted manuscripts by gender & journal - a histogram
sub_author_data %>% 
  group_by(journal, random.person.id, gender) %>% summarise(n = n()) %>% 
  ggplot()+
  geom_boxplot(aes(x = gender, y = n, fill = gender))+
  facet_wrap(~journal)+
  scale_y_log10()+
  coord_flip()+
  scale_x_discrete(labels = gen_labels)+
  scale_fill_manual(values = gen_colors)+
  labs(x = "Author Gender", y = "Number of Unique Papers Submitted")+
  my_theme_horiz

ggsave(filename = "repeat_subs_by_journ_boxplot.png", path = "results/gender/figures/")

sub_author_data %>% 
  group_by(random.person.id, gender) %>% summarise(n = n()) %>% 
  ggplot()+
  geom_boxplot(aes(x = gender, y = n, fill = gender))+
  scale_y_log10()+
  coord_flip()+
  scale_x_discrete(labels = gen_labels)+
  scale_fill_manual(values = gen_colors)+
  labs(x = "Author Gender", y = "Number of Unique Papers Submitted")+
  my_theme_horiz

ggsave(filename = "repeat_subs_boxplot.png", path = "results/gender/figures/")

# the number of published manuscripts by journal
pub_author_data %>%  
  group_by(journal, random.person.id, gender) %>% summarise(n = n()) %>% 
  ggplot()+
  geom_boxplot(aes(x = gender, y = n, fill = gender))+
  facet_wrap(~journal)+
  scale_y_log10()+
  coord_flip()+
  scale_x_discrete(labels = gen_labels)+
  scale_fill_manual(values = gen_colors)+
  labs(x = "Author Gender", y = "Number of Unique Papers Published")+
  my_theme_horiz

ggsave(filename = "repeat_pubs_by_journ_boxplot.png", path = "results/gender/figures/")

pub_author_data %>%  
  group_by(random.person.id, gender) %>% summarise(n = n()) %>% 
  ggplot()+
  geom_boxplot(aes(x = gender, y = n, fill = gender))+
  scale_y_log10()+
  coord_flip()+
  scale_x_discrete(labels = gen_labels)+
  scale_fill_manual(values = gen_colors)+
  labs(x = "Author Gender", y = "Number of Unique Papers Published")+
  my_theme_horiz

ggsave(filename = "repeat_pubs_boxplot.png", path = "results/gender/figures/")