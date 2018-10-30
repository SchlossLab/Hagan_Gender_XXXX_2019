source("code/load_data.R")

published_authors <- data %>% filter(role.y == "author" & published == "yes") %>% select(random.person.id.y, gender.y, grouped.random) %>% distinct()

unpublished_authors <- data %>% filter(role.y == "author" & published == "no") %>% select(random.person.id.y, gender.y, grouped.random) %>% distinct()

num_published <- published_authors %>% nrow()

num_unpublished <- unpublished_authors %>% nrow()

prop_gen_pub <- published_authors %>% group_by(gender.y) %>% summarise(n = n()) %>% as.tibble() %>% 
  mutate(total = num_published) %>% mutate(prop = round((n/total)*100, 2)) %>% mutate(published = "yes")

prop_gen_unpub <- unpublished_authors %>% group_by(gender.y) %>% summarise(n = n()) %>% as.tibble() %>% 
  mutate(total = num_unpublished) %>% mutate(prop = round((n/total)*100, 2)) %>% mutate(published = "no")

prop_gen_bypub <- rbind(prop_gen_pub, prop_gen_unpub)

prop_gen_bypub %>% 
  ggplot(aes(x = gender.y, y = prop, fill = gender.y)) +
  geom_col()+
  facet_grid(~published)

ggsave("results/prop_gen_bypub.jpg")
