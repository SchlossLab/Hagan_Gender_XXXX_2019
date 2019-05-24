#Does reviewer acceptance depend on gender of editor/reviewer? -- need potential reviewer dataset
people <- people_data %>% select(-role, -contains("auth"), 
                                 -grouped.random, -random.manu.num) %>%
  distinct() %>% 
  mutate(gender = fct_explicit_na(gender, na_level = "none"))

rev_resp <- read_csv("data/2018_rev_resp_ready.csv") %>% 
    left_join(., people, by = "random.person.id")

rev_resp %>% #collapse "contacted" into "no response"
  ggplot()+
  geom_bar(aes(x = gender, fill = gender), position = "dodge")+
  facet_wrap(~status)
