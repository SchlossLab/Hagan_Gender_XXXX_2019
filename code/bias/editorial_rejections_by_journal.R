#break decisions after review down by journal
j_ed_dec_data <- bias_data %>% 
#  filter(!(country %in% c("Japan", "Taiwan, Province of China", 
#                          "China", "Singapore", "Hong Kong", 
#                          "Korea, Republic of"))) %>% 
  filter(version.reviewed == 0) %>% 
  filter(grouped.vers == 1) %>% 
  #filter(US.inst == "yes") %>% 
  select(gender, journal, grouped.random, EJP.decision, version) %>% 
  filter(EJP.decision %in% c("Accept, no revision",
                             "Reject", "Revise only")) %>% 
  distinct()

ASM_summary_dec <- bias_data %>% 
  filter(version.reviewed == 0) %>% 
  filter(grouped.vers == 1) %>% 
  select(gender, grouped.random, EJP.decision) %>% distinct() %>% 
  group_by(gender) %>% summarise(total = n())

ASM_dec <- j_ed_dec_data %>% 
  group_by(gender, EJP.decision) %>% 
  summarise(n = n()) %>% 
  left_join(., ASM_summary_dec, by = "gender") %>% 
  mutate(prop_dec = get_percent(n, total)) %>%
  select(-n, -total) %>% distinct() %>% 
  spread(key = gender, value = prop_dec) %>% 
  mutate(performance = male - female)

journal_summary <- j_ed_dec_data %>% 
  group_by(journal, gender) %>% summarise(total = n())

journal_dec_summary <- j_ed_dec_data %>% 
  group_by(journal, EJP.decision) %>% 
  summarise(n = n())

ed_rejections_E <- j_ed_dec_data %>% 
  group_by(journal, gender, EJP.decision) %>% 
  summarise(n = n()) %>% 
  left_join(., journal_summary, 
            by = c("journal", "gender")) %>% 
  distinct() %>% 
  mutate(prop_rej = get_percent(n, total)) %>%
  select(-n, -total) %>% 
  spread(key = gender, value = prop_rej) %>% 
  mutate(performance = male - female) %>% 
  left_join(., journal_dec_summary, 
            by = c("journal", "EJP.decision")) %>% 
  ggplot() +
  geom_col(aes(x = fct_reorder(journal, performance), 
               y = performance, fill = performance)) + 
  facet_wrap(~EJP.decision)+
  coord_flip()+
  gen_gradient+
  geom_hline(data = ASM_dec, aes(yintercept = performance))+
  geom_text(aes(x = journal, y = 1.5, label = n))+
  labs(x = "Journal", 
       y = "\nDifference in Decision after First Review",
       caption = "Vertical line indicates value for 
       all journals combined")+
  my_theme_horiz

#ggsave("results/first_decision_j.png")