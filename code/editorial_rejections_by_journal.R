#break decisions after review down by journal
j_ed_dec_data <- bias_data %>% 
#  filter(!(country %in% c("Japan", "Taiwan, Province of China", 
#                          "China", "Singapore", "Hong Kong", 
#                          "Korea, Republic of"))) %>% 
  filter(version.reviewed == 0) %>% 
  filter(version == 0) %>% 
  select(gender, journal, grouped.random, EJP.decision, version) %>% 
  filter(EJP.decision %in% c("Accept, no revision",
                             "Reject", "Revise only")) %>% 
  distinct()

ASM_summary_dec <- bias_data %>% 
  filter(version.reviewed == 0) %>% 
  filter(version == 0) %>% 
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

journal_summary <- bias_data %>% 
  filter(version.reviewed == 0) %>% 
  filter(version == 0) %>% 
  select(journal, gender, grouped.random, EJP.decision) %>% 
  distinct() %>% 
  group_by(journal, gender) %>% summarise(total = n())

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
  ggplot() +
  geom_col(aes(x = fct_reorder(journal, performance), 
               y = performance, fill = performance)) + 
  facet_wrap(~EJP.decision)+
  coord_flip()+
  gen_gradient+
  geom_hline(data = ASM_dec, aes(yintercept = performance))+
  #annotate(geom = "text", x = 12, y = -2.5, label = "All Journals")+
  labs(x = "Journal", 
       y = "\nDifference in Decision after First Review",
       caption = "Vertical line indicates value for 
       all journals combined")+
  my_theme_horiz

#ggsave("results/first_decision_j.png")