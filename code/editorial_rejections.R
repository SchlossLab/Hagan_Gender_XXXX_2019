#Do women recieve proportionally more editorial rejections than men?
ed_genders <- data %>% 
  filter(role == "editor") %>% 
  select(random.manu.num, gender) %>% 
  distinct() %>% 
  rename("editor.gender" = "gender")

ed_rejs <- bias_data %>% 
  filter(published == "no") %>% 
  filter(EJP.decision == "Reject" & is.na(days.to.review)) %>% 
  select(grouped.random, random.manu.num, gender, 
         EJP.decision, contains("days"), journal,
         num.versions, -days.to.review) %>% 
  distinct() %>% 
  left_join(., ed_genders, by = "random.manu.num") %>% 
  filter(editor.gender %in% c("female", "male")) %>% 
  distinct()

#percent of submissions that are editorial rejections by gender
ASM_summary <- bias_data %>% 
  select(gender, grouped.random) %>% distinct() %>% 
  group_by(gender) %>% summarise(n = n())

ASM_ed_rej <- ed_rejs %>% 
  group_by(gender) %>% 
  summarise(n = n()) %>% 
  mutate(prop_rej = get_percent(n, ASM_summary$n)) %>% 
  select(-n) %>% 
  spread(key = gender, value = prop_rej) %>% 
  mutate(performance = male - female) %>% 
  cbind(journal = "All Combined", .) %>% as_tibble()

#percent of submissions that are editorial rejections by gender & journal
journal_summary <- bias_data %>% 
  select(journal, gender, grouped.random) %>% distinct() %>% 
  group_by(journal, gender) %>% summarise(total = n())

ed_rejections_A <- ed_rejs %>% 
  group_by(journal, gender) %>% 
  summarise(n = n()) %>% 
  left_join(., journal_summary, by = c("journal", "gender")) %>% 
  distinct() %>% 
  mutate(prop_rej = get_percent(n, total)) %>% 
  select(-n, -total) %>% 
  spread(key = gender, value = prop_rej) %>% 
  mutate(performance = male - female) %>%  
  ggplot() +
  geom_col(aes(x = fct_reorder(journal, performance), 
               y = performance, fill = performance)) + 
  coord_flip()+
  gen_gradient+
  geom_hline(data = ASM_ed_rej, aes(yintercept = performance))+
  annotate(geom = "text", x = 12, y = -2.95, label = "All Journals")+
  labs(x = "Journal", 
       y = "\nDifference in Editorial Rejections")+
  my_theme_horiz

#editor recommendations by gender----
fem_ed <- ed_rejs %>% 
  select(random.manu.num, gender, 
         editor.gender) %>%
  distinct() %>% 
  group_by(editor.gender, gender) %>% 
  summarise(n = n()) %>% as_tibble() %>% 
  spread(key = gender, value = n) %>% 
  filter(editor.gender == "female") %>% 
  mutate(female = get_percent(female, ASM_summary[1,2]),
         male = get_percent(male, ASM_summary[2,2])) %>%
  mutate(overperformance = male - female) 

men_ed <- ed_rejs %>% 
  select(random.manu.num, gender, 
         editor.gender) %>%
  distinct() %>% 
  group_by(editor.gender, gender) %>% 
  summarise(n = n()) %>% as_tibble() %>% 
  spread(key = gender, value = n) %>%
  filter(editor.gender == "male") %>% 
  mutate(female = get_percent(female, ASM_summary[1,2]),
         male = get_percent(male, ASM_summary[2,2])) %>% 
  mutate(overperformance = male - female) 

summary_gen_ed <- rbind(fem_ed, men_ed)

ed_rejections_E <- summary_gen_ed %>% 
  ggplot(aes(x = editor.gender, 
             y = overperformance, fill = overperformance))+
  geom_col(position = "dodge")+
  gen_x_replace+
  gen_gradient+
  coord_flip()+
  labs(x = "Editor Gender", 
       y = "\nDifference in Editorial Rejections")+
  my_theme_horiz
