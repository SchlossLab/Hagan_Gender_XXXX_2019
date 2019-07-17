#are papers authored by all gendered rejected at equivalent rates?

#setup----
acc_rej_data <- data %>% 
  filter(EJP.decision == "Accept, no revision" | EJP.decision == "Reject") 

auth_types <- c("first", "middle", "last", "corres")

#rejection rates by author type----
rej_by_auth <- map_df(auth_types, function(x){
  
  get_auth_type(x, acc_rej_data) %>% 
  filter(role == "author") %>% 
  select(gender, EJP.decision, grouped.random) %>% distinct() %>% 
  group_by(gender, EJP.decision) %>% summarise(n = n()) %>% 
  spread(key = EJP.decision, value = n) %>% 
  mutate(prop_rej = round((Reject/(Reject + `Accept, no revision`))*100, digits = 2)) %>% 
    mutate(., auth_type = x)
})

#cross ASM rejection rate
ASM_rej_rate <- acc_rej_data %>% 
  select(EJP.decision, grouped.random) %>% distinct() %>% 
  group_by(EJP.decision) %>% 
  summarise(n = n()) %>% 
  spread(key = EJP.decision, value = n) %>%
  mutate(prop_rej = round((Reject/(Reject + `Accept, no revision`))*100, digits = 2)) 

#theme_set(theme_cowplot(font_size=12))
auth_type_A <- rej_by_auth %>% 
  ggplot() + 
  geom_col(aes(x = gender, y = prop_rej, fill = gender)) +
  facet_wrap(~auth_type)+
  scale_fill_manual(values = gen_colors)+
  scale_x_discrete(breaks = gen_levels,
                    labels = gen_labels)+
  annotate(geom = "text", x = 2, 
           y = (ASM_rej_rate[[3]]+3), label = "ASM rejection rate")+
  geom_hline(data = ASM_rej_rate, aes(yintercept = prop_rej))+
  labs(x = "Predicted Gender\n", y = "Percent of Manuscripts Rejected")+
  my_theme_horiz

#rejection rates by gender and journal----
rej_by_journ <- map_df(auth_types, function(x){
  
  journ_prop <- get_auth_type(x, acc_rej_data) %>% 
    filter(role == "author") %>% 
    filter(gender != "none") %>% 
    select(journal, gender, EJP.decision, grouped.random) %>% 
    distinct() %>% 
    group_by(journal, gender, EJP.decision) %>% 
    summarise(n = n()) %>% 
    spread(key = EJP.decision, value = n) %>% 
    mutate(prop_rej = round((Reject/(Reject + `Accept, no revision`))*100, digits = 2)) %>% 
    select(-Reject, -`Accept, no revision`) %>% 
    spread(key = gender, value = prop_rej) %>% 
    mutate(difference = male - female) %>% 
    mutate(., auth.type = x)
})

journals <- acc_rej_data %>% pull(journal) %>% unique()

journ_rej_rates <- map_df(journals, function(x){
  
  acc_rej_data %>% 
  filter(journal == x) %>% 
  select(EJP.decision, grouped.random) %>% distinct() %>% 
  group_by(EJP.decision) %>% 
  summarise(n = n()) %>% 
  spread(key = EJP.decision, value = n) %>%
  mutate(prop_rej = round((Reject/(Reject + `Accept, no revision`))*100, digits = 2)) %>% 
  mutate(., journal = x)
})

auth_type_B <- rej_by_journ %>% 
  ggplot() + 
  geom_col(aes(x = journal, y = difference, fill = difference)) +
  coord_flip()+
  facet_wrap(~auth.type)+
  gen_gradient+
  labs(x = "Journal", y = "Difference in Percent Rejection\n",
       fill = "% Points\nDifference")+
  my_theme_leg_horiz

#Do women recieve proportionally more editorial rejections than men?
ed_rejs <- bias_data %>% 
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
  #geom_text(aes(x = journal, y = 0.75, label = n))+
  labs(x = "Journal", 
       y = "Difference in Editorial Rejections")+
  my_theme_horiz

#break decisions after review down by journal
j_ed_dec_data <- bias_data %>% 
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
  #geom_text(aes(x = journal, y = 1.5, label = n))+
  labs(x = "Journal", 
       y = "Difference in Decision after First Review")+
  my_theme_horiz

plot_grid(auth_type_A, auth_type_B, ed_rejections_A, ed_rejections_E, 
          labels = c('A', 'B', 'C', 'D'), label_size = 18)

ggsave("Figure_6.png", device = 'png', 
       path = 'submission/', width = 12, height = 12)
