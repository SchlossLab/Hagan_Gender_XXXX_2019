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

ed_rejs %>% 
  group_by(gender) %>% 
  summarise(n = n()) %>% 
  mutate(prop_rej = get_percent(n, ASM_summary$n)) %>% 
  ggplot()+
  geom_col(aes(x = gender, y = prop_rej, fill = gender))+
  scale_fill_manual(values = gen_colors)+
  labs(x = "Gender", y = "Percent Editorial Rejections")+
  my_theme_horiz

#percent of submissions that are editorial rejections by gender & journal
journal_summary <- bias_data %>% 
  select(journal, gender, grouped.random) %>% distinct() %>% 
  group_by(journal, gender) %>% summarise(total = n())

ed_rejs %>% 
  group_by(journal, gender) %>% 
  summarise(n = n()) %>% 
  left_join(., journal_summary, by = c("journal", "gender")) %>% 
  distinct() %>% 
  mutate(prop_rej = get_percent(n, total)) %>%
  ggplot() +
  geom_col(aes(x = gender, y = prop_rej, fill = gender)) + 
  facet_wrap(~journal)+
  scale_fill_manual(values = gen_colors)+
  labs(x = "Gender", y = "Percent Editorial Rejections")+
  my_theme_horiz
