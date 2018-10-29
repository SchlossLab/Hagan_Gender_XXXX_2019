source("Code/load_data.R")
source("Results/manu_meta.R")

people_doi <- research_only %>% 
  mutate(published = if_else(!is.na(doi), "yes", "no"))

first_auth <- people_doi %>% filter(author.seq == "1")

corres_auth <- people_doi %>% filter(author.corres == "true")

middle

num_first <- first_auth %>% select(person.id.y) %>% distinct() %>% nrow()

num_corres <- corres_auth %>% select(person.id.y) %>% distinct() %>% nrow()

first_auth_gen <- first_auth %>% group_by(gender) %>% distinct(grouped.manu.number) %>% summarise(n = n())

corres_auth_gen <- corres_auth %>% group_by(gender) %>% distinct(grouped.manu.number) %>% summarise(n = n())

first_auth_pub <- people_doi %>% filter(published == "yes" & author.seq == "1") %>% 
  group_by(gender) %>% distinct(grouped.manu.number) %>% summarise(n = n()) %>% as.tibble()

corres_auth_pub <- people_doi %>% filter(published == "yes" & author.corres == "true") %>% 
  group_by(gender) %>% distinct(grouped.manu.number) %>% summarise(n = n()) %>% as.tibble()

num_first_auth <- first_auth %>% group_by(published) %>% distinct(grouped.manu.number) %>% summarise(n = n())

num_corres_auth <- corres_auth %>% group_by(published) %>% distinct(grouped.manu.number) %>% summarise(n = n())

prop_first_auth_gen <- first_auth_gen %>% cbind(n_pub = first_auth_pub$n) %>% 
  mutate(prop = round((n_pub/n)*100, 2)) %>% mutate(author = "first")

prop_corres_auth_gen <- corres_auth_gen %>% cbind(n_pub = corres_auth_pub$n) %>% 
  mutate(prop = round((n_pub/n)*100, 2)) %>% mutate(author = "corresponding")

prop_pub_by_auth <- rbind(prop_corres_auth_gen, prop_first_auth_gen)

prop_pub_by_auth %>% 
  ggplot(aes(x = gender, y = prop, fill = gender)) +
  geom_col()+
  facet_grid(~author)+
  geom_hline(yintercept = percent_pubbed)

ggsave("Results/Plots/prop_pub_by_auth.jpg")
