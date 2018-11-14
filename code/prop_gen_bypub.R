source("code/load_data.R")

published_authors <- data %>% filter(role.y == "author" & published == "yes") %>% #select authors whose papers were published
  select(random.person.id.y, gender.y, grouped.random) %>% distinct() #narrow to the grouped.manu to ensure authors only counted once per publication

unpublished_authors <- data %>% filter(role.y == "author" & published == "no") %>% 
  select(random.person.id.y, gender.y, grouped.random) %>% distinct()

num_published <- published_authors %>% nrow() #total published

num_unpublished <- unpublished_authors %>% nrow() #total rejected

prop_gen_pub <- published_authors %>% group_by(gender.y) %>% summarise(n = n()) %>% as.tibble() %>% #summary based on gender
  mutate(total = num_published) %>% mutate(prop = round((n/total)*100, 2)) %>% mutate(published = "yes") #calculate percent of all published

prop_gen_unpub <- unpublished_authors %>% group_by(gender.y) %>% summarise(n = n()) %>% as.tibble() %>% #summary based on gender
  mutate(total = num_unpublished) %>% mutate(prop = round((n/total)*100, 2)) %>% mutate(published = "no") #% of all rejected

prop_gen_bypub <- rbind(prop_gen_pub, prop_gen_unpub) %>% #summary table
  mutate(gender.y = fct_explicit_na(gender.y, na_level = "none"))

pub_status <- c(no = "Rejected", yes = "Published")

prop_gen_bypub %>% 
  ggplot(aes(x = gender.y, y = prop, fill = gender.y)) +
  geom_col()+
  facet_grid(~published, labeller = labeller(published = pub_status))+
  scale_x_discrete(labels = gen_labels)+
  scale_fill_manual(values = gen_colors)+
  labs(x = "Predicted gender", y = "Proportion")+
  my_theme_horiz

ggsave("results/prop_gen_bypub.jpg")
