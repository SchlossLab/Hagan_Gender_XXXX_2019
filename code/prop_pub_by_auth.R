source(code/manu_meta.R)

first_auth <- data %>% filter(author.seq == "1")

corres_auth <- data %>% filter(author.corres == "true")

#middle

num_first <- first_auth %>% select(random.person.id.y) %>% distinct() %>% nrow()

num_corres <- corres_auth %>% select(random.person.id.y) %>% distinct() %>% nrow()

first_auth_gen <- first_auth %>% group_by(gender.y) %>% distinct(grouped.random) %>% summarise(n = n())

corres_auth_gen <- corres_auth %>% group_by(gender.y) %>% distinct(grouped.random) %>% summarise(n = n())

first_auth_pub <- data %>% filter(published == "yes" & author.seq == "1") %>% 
  group_by(gender.y) %>% distinct(grouped.random) %>% summarise(n = n()) %>% as.tibble()

corres_auth_pub <- data %>% filter(published == "yes" & author.corres == "true") %>% 
  group_by(gender.y) %>% distinct(grouped.random) %>% summarise(n = n()) %>% as.tibble()

num_first_auth <- first_auth %>% group_by(published) %>% distinct(grouped.random) %>% summarise(n = n())

num_corres_auth <- corres_auth %>% group_by(published) %>% distinct(grouped.random) %>% summarise(n = n())

prop_first_auth_gen <- first_auth_gen %>% cbind(n_pub = first_auth_pub$n) %>% 
  mutate(prop = round((n_pub/n)*100, 2)) %>% mutate(author = "first")

prop_corres_auth_gen <- corres_auth_gen %>% cbind(n_pub = corres_auth_pub$n) %>% 
  mutate(prop = round((n_pub/n)*100, 2)) %>% mutate(author = "corresponding")

prop_pub_by_auth <- rbind(prop_corres_auth_gen, prop_first_auth_gen)

prop_pub_by_auth %>% 
  ggplot(aes(x = gender.y, y = prop, fill = gender.y)) +
  geom_col()+
  facet_grid(~author)+
  geom_hline(yintercept = percent_pubbed)

ggsave("results/prop_pub_by_auth.jpg")
