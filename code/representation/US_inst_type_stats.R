#gender split by U.S. institution type for authors vs reviewers vs editors:

inst_stats_data <- data %>% 
  filter(!is.na(US.inst.type)) %>% 
  select(random.person.id, role, gender,
         US.inst.type) %>% 
  filter(gender != "none") %>% 
  mutate(role = fct_collapse(role,
                             "reviewer" = c("reviewer", "potential.reviewer"),
                             "editor" = c("editor", "senior.editor"))) %>% distinct() 

gender_n <- inst_stats_data %>% 
  group_by(role, gender) %>% 
  summarise(total = n())
  
roles <- c("author", "reviewer", "editor")

summ_stats <- inst_stats_data %>% 
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

summ_stats$role <- fct_relevel(summ_stats$role, roles)

summ_stats %>% 
  ggplot()+
  geom_col(aes(fill = gender, y = percent, x = role),
           position = "dodge")+
  facet_wrap(~US.inst.type, scales = "free_y")+
  scale_fill_manual(values = gen_ed_colors)+
  labs(x = "Role", y = "Percent of Gender")+
  my_theme_horiz
