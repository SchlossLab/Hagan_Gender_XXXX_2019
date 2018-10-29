source("Code/load_data.R")

people_doi <- research_only %>% 
  mutate(published = if_else(!is.na(doi), "yes", "no"))

published_authors <- people_doi %>% filter(role == "author" & published == "yes") %>% select(person.id.y, gender, grouped.manu.number) %>% distinct()

unpublished_authors <- people_doi %>% filter(role == "author" & published == "no") %>% select(person.id.y, gender, grouped.manu.number) %>% distinct()

num_published <- published_authors %>% nrow()

num_unpublished <- unpublished_authors %>% nrow()

prop_gen_pub <- published_authors %>% group_by(gender) %>% summarise(n = n()) %>% as.tibble() %>% 
  mutate(total = num_published) %>% mutate(prop = round((n/total)*100, 2)) %>% mutate(published = "yes")

prop_gen_unpub <- unpublished_authors %>% group_by(gender) %>% summarise(n = n()) %>% as.tibble() %>% 
  mutate(total = num_unpublished) %>% mutate(prop = round((n/total)*100, 2)) %>% mutate(published = "no")

prop_gen_bypub <- rbind(prop_gen_pub, prop_gen_unpub)

prop_gen_bypub %>% 
  ggplot(aes(x = gender, y = prop, fill = gender)) +
  geom_col()+
  facet_grid(~published)

ggsave("Results/Plots/prop_gen_bypub.jpg")
