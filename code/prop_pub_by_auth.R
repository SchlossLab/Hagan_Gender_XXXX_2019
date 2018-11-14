source("code/manu_meta.R")

#select authorship roles----
first_auth <- data %>% filter(author.seq == "1")

corres_auth <- data %>% filter(author.corres == "true")

last_auth <- data %>% filter(author.last == "true")

middle_auth <- data %>% filter(author.seq != "1" & author.last == "false")

#count number of unique individuals in each role----
num_first <- first_auth %>% select(random.person.id.y) %>% distinct() %>% nrow()

num_corres <- corres_auth %>% select(random.person.id.y) %>% distinct() %>% nrow()

num_last <- last_auth %>% select(random.person.id.y) %>% distinct() %>% nrow()

num_middle <- middle_auth %>% select(random.person.id.y) %>% distinct() %>% nrow()

#summary of submitting indivduals in each role by gender (per grouped manuscripts)----
first_auth_gen <- first_auth %>% group_by(gender.y) %>% distinct(grouped.random) %>% summarise(n = n())

corres_auth_gen <- corres_auth %>% group_by(gender.y) %>% distinct(grouped.random) %>% summarise(n = n())

last_auth_gen <- corres_auth %>% group_by(gender.y) %>% distinct(grouped.random) %>% summarise(n = n())

middle_auth_gen <- middle_auth %>% group_by(gender.y) %>% distinct(grouped.random) %>% summarise(n = n())

#summary of published individuals in each role by gender (per grouped manu)----
first_auth_pub <- data %>% filter(published == "yes" & author.seq == "1") %>% 
  group_by(gender.y) %>% distinct(grouped.random) %>% summarise(n = n()) %>% as.tibble()

corres_auth_pub <- data %>% filter(published == "yes" & author.corres == "true") %>% 
  group_by(gender.y) %>% distinct(grouped.random) %>% summarise(n = n()) %>% as.tibble()

last_auth_pub <- data %>% filter(published == "yes" & author.last == "true") %>% 
  group_by(gender.y) %>% distinct(grouped.random) %>% summarise(n = n()) %>% as.tibble()

middle_auth_pub <- data %>% filter(published == "yes" & author.last == "false" & author.seq != "1") %>% 
  group_by(gender.y) %>% distinct(grouped.random) %>% summarise(n = n()) %>% as.tibble()

#summary of published vs rejected authors in each role----
num_first_auth <- first_auth %>% group_by(published) %>% distinct(grouped.random) %>% summarise(n = n())

num_corres_auth <- corres_auth %>% group_by(published) %>% distinct(grouped.random) %>% summarise(n = n())

num_last_auth <- last_auth %>% group_by(published) %>% distinct(grouped.random) %>% summarise(n = n())

num_middle_auth <- middle_auth %>% group_by(published) %>% distinct(grouped.random) %>% summarise(n = n())

#proportion of submitted manuscripts accepted by authorship & gender (e.g., 50% of M sub accepted, vs 35% F)----
prop_first_auth_gen <- first_auth_gen %>% cbind(n_pub = first_auth_pub$n) %>% 
  mutate(prop = round((n_pub/n)*100, 2)) %>% mutate(author = "first")

prop_corres_auth_gen <- corres_auth_gen %>% cbind(n_pub = corres_auth_pub$n) %>% 
  mutate(prop = round((n_pub/n)*100, 2)) %>% mutate(author = "corresponding")

prop_last_auth_gen <- last_auth_gen %>% cbind(n_pub = last_auth_pub$n) %>% 
  mutate(prop = round((n_pub/n)*100, 2)) %>% mutate(author = "last")

prop_middle_auth_gen <- middle_auth_gen %>% cbind(n_pub = middle_auth_pub$n) %>% 
  mutate(prop = round((n_pub/n)*100, 2)) %>% mutate(author = "middle")

prop_pub_by_auth <- rbind(prop_corres_auth_gen, prop_first_auth_gen, prop_last_auth_gen, prop_middle_auth_gen) %>% 
  mutate(gender.y = fct_explicit_na(gender.y, na_level = "none"))

#plot ----
prop_pub_by_auth %>% 
  ggplot(aes(x = gender.y, y = prop, fill = gender.y)) +
  geom_col()+
  facet_grid(~author, labeller = labeller(author = str_to_title))+
  geom_hline(yintercept = percent_pubbed)+
  scale_fill_manual(values = gen_colors)+
  scale_x_discrete(labels = gen_labels)+
  labs(x = "Predicted Gender of Author", y = "Proportion of Submitted Manuscripts Published",
       caption = paste0("Black line indicates the overall publication rate of ", percent_pubbed, "%"))+
  my_theme_horiz

ggsave("results/prop_pub_by_auth.jpg")
