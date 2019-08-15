#gender split by U.S. institution type for authors vs reviewers vs editors:

inst_stats_data <- data %>% 
  filter(!is.na(US.inst.type)) %>% 
  filter(gender != "none") %>% 
  mutate(role = fct_collapse(role,
                             "reviewer" = c("reviewer", "potential.reviewer"),
                             "editor" = c("editor", "senior.editor"))) %>% 
  filter(author.corres == "TRUE" | author.last == "TRUE" |
           role == "reviewer" | role == "editor") %>% 
  select(random.person.id, role, gender,
         US.inst.type) %>% distinct() 

gender_n <- inst_stats_data %>% 
  group_by(role, gender) %>% 
  summarise(total = n())
  
roles <- c("author", "reviewer", "editor")

summ_US_stats <- inst_stats_data %>% 
  group_by(role, gender, US.inst.type) %>% 
  summarise(n = n()) %>% 
  left_join(., gender_n, by = c("role", "gender")) %>% 
  spread(key = US.inst.type, value = n) %>%
  mutate_at(vars(`Federal Research`:`R2  Institution`), 
         list(~ get_percent(., total))
    ) %>% 
  select(-total) %>% 
  na.omit() %>% 
  gather(`Federal Research`:`R2  Institution`, 
         key = US.inst.type, value = percent) 

summ_US_stats$role <- fct_relevel(summ_US_stats$role, roles)
