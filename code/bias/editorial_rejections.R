#Do women recieve proportionally more editorial rejections than men?
ed_rejs <- bias_data %>% 
#  filter(!(country %in% c("Japan", "Taiwan, Province of China", 
#                          "China", "Singapore", "Hong Kong", 
#                          "Korea, Republic of"))) %>% 
  filter(published == "no") %>% 
  filter(EJP.decision == "Reject" & is.na(days.to.review)) %>% 
  select(grouped.random, gender, 
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

ed_rejections <- ed_rejs %>% 
  group_by(journal, gender) %>% 
  summarise(n = n()) %>% 
  left_join(., journal_summary, by = c("journal", "gender")) %>% 
  distinct() %>% 
  mutate(prop_rej = get_percent(n, total)) 

ed_reject_n <- ed_rejections %>% 
  select(-total, -prop_rej) %>% 
  spread(key = gender, value = n) %>% 
  mutate(n = male + female) %>% 
  select(-male, -female)

ed_rejections_A <- ed_rejections %>% select(-n, -total) %>% 
  spread(key = gender, value = prop_rej) %>% 
  mutate(performance = male - female) %>% 
  left_join(., ed_reject_n, by = "journal") %>% 
  ggplot() +
  geom_col(aes(x = fct_reorder(journal, performance), 
               y = performance, fill = performance)) + 
  coord_flip()+
  gen_gradient+
  geom_hline(data = ASM_ed_rej, aes(yintercept = performance))+
  annotate(geom = "text", x = 12, y = -2.5, label = "All Journals")+
  geom_text(aes(x = journal, y = 0.75, label = n))+
  labs(x = "Journal", 
       y = "\nDifference in Editorial Rejections",
       caption = "Value equals Total N of editorial rejections")+
  my_theme_horiz

#ggsave("results/j_ed_rej_no_china.png", ed_rejections_A)

#editor recommendations by gender---- Doesn't work bc editors aren't assigned in all cases
