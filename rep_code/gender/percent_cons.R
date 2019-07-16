senior_authors <- people_data %>% 
  filter(role == "author") %>% 
  filter(author.corres == "true" | author.last == "true") %>% 
  select(gender, random.person.id) %>% distinct() %>% 
  rename("author.gender" = "gender")

pot_reviewers <- people_data %>% 
  filter(role == "reviewer" | role == "potential.reviewer") %>% 
  select(gender, random.person.id) %>% distinct() %>% 
  rename("reviewer.gender" = "gender")

joined <- left_join(senior_authors, pot_reviewers, by = "random.person.id")

female <- joined %>% filter(author.gender == "female") %>% 
  group_by(reviewer.gender) %>% summarise(n = n()) %>% 
  mutate(percent = get_percent(n, sum(n)))

male <- joined %>% filter(author.gender == "male") %>% 
  group_by(reviewer.gender) %>% summarise(n = n()) %>% 
  mutate(percent = get_percent(n, sum(n)))

summary <- rbind(female, male) %>% 
  filter(!is.na(reviewer.gender))

reviewer_C <- ggplot(summary)+
  geom_col(aes(x = reviewer.gender, y = percent, fill = reviewer.gender))+
  scale_fill_manual(values = gen_ed_colors)+
  scale_x_discrete(breaks = gen_breaks, labels = gen_labels)+
  labs(x = "Gender", y = "Percent Considered as Reviewers")+
  my_theme_horiz
