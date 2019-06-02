#Do women recieve proportionally more editorial rejections than men?

ed_rejs <- bias_data %>% 
  filter(published == "no") %>% 
  filter(EJP.decision == "Reject" & is.na(days.to.review)) %>% 
  select(grouped.random, random.manu.num, gender, 
         EJP.decision, contains("days"), journal,
         num.versions, -days.to.review) %>% 
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
  mutate(performance = male - female) %>% as_tibble() %>% 
  rbind(ASM_ed_rej) %>% 
  ggplot() +
  geom_col(aes(x = journal, y = performance, fill = performance)) + 
  #scale_fill_manual(values = gen_colors)+
  coord_flip()+
  gen_gradient+
  labs(x = "Journal", 
       y = "\nDifference in Percent Editorial Rejections\n(Men - Women)")+
  my_theme_horiz
