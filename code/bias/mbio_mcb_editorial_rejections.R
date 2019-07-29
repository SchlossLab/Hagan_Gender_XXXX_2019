ed_rejs <- bias_data %>% 
  filter(published == "no") %>% 
  filter(EJP.decision == "Reject" & is.na(days.to.review)) %>% 
  select(grouped.random, gender, 
         journal, decision.date) %>% 
  mutate(decision.date = year(decision.date)) %>% 
  distinct()

mBio_summary <- bias_data %>% 
  filter(journal == "mBio") %>%
  mutate(decision.date = year(decision.date)) %>%
  select(decision.date, gender, grouped.random) %>% distinct() %>% 
  filter(!is.na(decision.date)) %>% 
  filter(decision.date != "2011") %>% 
  group_by(decision.date, gender) %>% summarise(total = n())

ed_rejs %>% 
  filter(journal == "mBio") %>% 
  group_by(decision.date, gender) %>% 
  summarise(ed_rej = n()) %>% 
  cbind(., total = mBio_summary$total) %>% 
  mutate(percent = get_percent(ed_rej, total)) %>% 
  ggplot(aes(x = decision.date, y = percent, fill = gender))+
  geom_col(position = "dodge") +
  scale_fill_manual(breaks = gen_levels, labels = gen_labels,
                    values = gen_colors)+
  geom_text(aes(label = ed_rej, y = percent), 
            position = position_dodge(0.9), vjust = -0.5)+
  labs(x = "Year of Decision", 
       y = "mBio Editorial Rejections by Gender (%)",
       fill = "Author\nGender",
       caption = "N of editorial rejections at the top of each bar")+
  my_theme_leg_horiz

ggsave("results/mbio_editorial_rejections.png")

mcb_summary <- bias_data %>% 
  filter(journal == "MCB") %>%
  mutate(decision.date = year(decision.date)) %>%
  select(decision.date, gender, grouped.random) %>% distinct() %>% 
  filter(!is.na(decision.date)) %>% 
  filter(decision.date != "2011") %>% 
  group_by(decision.date, gender) %>% summarise(total = n())

ed_rejs %>% 
  filter(journal == "MCB") %>% 
  group_by(decision.date, gender) %>% 
  summarise(ed_rej = n()) %>% 
  cbind(., total = mcb_summary$total) %>% 
  mutate(percent = get_percent(ed_rej, total)) %>% 
  ggplot(aes(x = decision.date, y = percent, fill = gender))+
  geom_col(position = "dodge") +
  scale_fill_manual(breaks = gen_levels, labels = gen_labels,
                    values = gen_colors)+
  geom_text(aes(label = ed_rej, y = percent), 
            position = position_dodge(0.9), vjust = -0.5)+
  labs(x = "Year of Decision", 
       y = "MCB Editorial Rejections by Gender (%)",
       fill = "Author\nGender",
       caption = "N of editorial rejections at the top of each bar")+
  my_theme_leg_horiz

ggsave("results/mcb_editorial_rejections.png")