cat_data <- bias_data %>% 
  filter(journal == "JVI") %>% 
  #filter(journal %in% c("JVI", "JCM", "AAC", "AEM", "IAI")) %>% 
  filter(!is.na(category))

#editorial rejections----


#editor decisions----
cat_ed_dec_data <- cat_data %>% 
  filter(version.reviewed == 0) %>% 
  filter(grouped.vers == 1) %>% 
  select(gender, category, grouped.random, EJP.decision, version) %>% 
  filter(EJP.decision %in% c("Accept", "Reject", "Revise")) %>% 
  distinct()

cat_ASM_summary_dec <- cat_data %>% 
  filter(version.reviewed == 0) %>% 
  filter(grouped.vers == 1) %>% 
  select(gender, grouped.random, EJP.decision) %>% distinct() %>% 
  group_by(gender) %>% summarise(total = n())

cat_dec <- cat_ed_dec_data %>% 
  group_by(gender, EJP.decision) %>% 
  summarise(n = n()) %>% 
  left_join(., US_ASM_summary_dec, by = "gender") %>% 
  mutate(prop_dec = get_percent(n, total)) %>%
  select(-n, -total) %>% distinct() %>% 
  spread(key = gender, value = prop_dec) %>% 
  mutate(performance = male - female)

cat_summary <- cat_ed_dec_data %>% 
  group_by(category, gender) %>% summarise(total = n())

cat_dec_summary <- cat_ed_dec_data %>% 
  group_by(category, EJP.decision) %>% 
  summarise(n = n())

cat_ed_dec_data %>% 
  group_by(category, gender, EJP.decision) %>% 
  summarise(n = n()) %>% 
  left_join(., cat_summary, 
            by = c("category", "gender")) %>% 
  distinct() %>% 
  mutate(prop_rej = get_percent(n, total)) %>%
  select(-n, -total) %>% 
  spread(key = gender, value = prop_rej) %>% 
  mutate(performance = male - female) %>% 
  left_join(., cat_dec_summary, 
            by = c("category", "EJP.decision")) %>% 
  ggplot() +
  geom_col(aes(x = fct_reorder(category, performance), 
               y = performance, fill = performance)) + 
  facet_wrap(~EJP.decision)+
  coord_flip()+
  gen_gradient+
  #geom_hline(data = US_j_dec, aes(yintercept = performance))+
  #geom_text(aes(x = journal, y = 1.5, label = n))+
  labs(x = "\nJournal", 
       y = "Difference in Decision after First Review\n")+
  my_theme_horiz
