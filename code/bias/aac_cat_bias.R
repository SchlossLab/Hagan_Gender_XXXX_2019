cat_data <- bias_data %>% 
  filter(journal == "IAI") %>% 
  #filter(journal %in% c("JVI", "JCM", "AAC", "AEM", "IAI")) %>% 
  filter(!is.na(category))

#editorial rejections----
cat_ed_rejs <- cat_data %>% 
  filter(EJP.decision == "Reject" & is.na(days.to.review)) %>% 
  select(grouped.random, gender, 
         EJP.decision, contains("days"), category,
         num.versions, -days.to.review) %>% 
  distinct()

#percent of submissions that are editorial rejections by gender
num_sub_summary <- cat_data %>% 
  select(gender, grouped.random) %>% distinct() %>% 
  group_by(gender) %>% summarise(n = n())

num_ed_rej <- cat_ed_rejs %>% 
  group_by(gender) %>% 
  summarise(n = n()) %>% 
  mutate(prop_rej = get_percent(n, num_sub_summary$n)) %>% 
  select(-n) %>% 
  spread(key = gender, value = prop_rej) %>% 
  mutate(performance = male - female) %>% 
  cbind(category = "All Combined", .) %>% as_tibble()

#percent of submissions that are editorial rejections by gender & journal
cat_summary <- cat_data %>% 
  select(category, gender, grouped.random) %>% distinct() %>% 
  group_by(category, gender) %>% summarise(total = n())

cat_ed_rejections <- cat_ed_rejs %>% 
  group_by(category, gender) %>% 
  summarise(n = n()) %>% 
  left_join(., cat_summary, by = c("category", "gender")) %>% 
  distinct() %>% 
  mutate(prop_rej = get_percent(n, total)) 

cat_ed_reject_n <- cat_ed_rejections %>% 
  select(-total, -prop_rej) %>% 
  spread(key = gender, value = n) %>% 
  mutate(n = male + female) %>% 
  select(-male, -female)

Fig_cat_A <- cat_ed_rejections %>% select(-n, -total) %>% 
  spread(key = gender, value = prop_rej) %>% 
  mutate(performance = male - female) %>% 
  left_join(., cat_ed_reject_n, by = "category") %>% 
  ggplot() +
  geom_col(aes(x = fct_reorder(category, performance), 
               y = performance, fill = performance)) + 
  coord_flip()+
  gen_gradient+
  geom_hline(data = num_ed_rej, aes(yintercept = performance))+
  #annotate(geom = "text", x = 12, y = -2.5, label = "All Journals")+
  #geom_text(aes(x = journal, y = 0.75, label = n))+
  labs(x = "\n", 
       y = "Difference in Editorial Rejections")+
  my_theme_horiz

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

Fig_cat_B <- cat_ed_dec_data %>% 
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

plot_grid(Fig_cat_A, Fig_cat_B, 
          labels = c('IAI_A', 'IAI_B'), 
          label_size = 18, nrow = 2)

ggsave("IAI_cat.png", device = 'png', 
       path = 'submission', width = 9, height = 6)