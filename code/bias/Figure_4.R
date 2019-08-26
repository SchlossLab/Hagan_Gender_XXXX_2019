#are papers authored by all gendered rejected at equivalent rates?

#setup----
acc_rej_data <- data %>% 
  filter(version == 0) %>% 
  filter(EJP.decision == "Accept, no revision" | EJP.decision == "Reject") 

auth_types <- c("first", "middle", "last", "corres")

#rejection rates by author type----
rej_by_auth <- map_df(auth_types, function(x){
  
  get_auth_type(x, acc_rej_data) %>% 
    filter(role == "author") %>% 
    filter(!is.na(gender)) %>% 
    select(gender, EJP.decision, grouped.random) %>% distinct() %>% 
    group_by(gender, EJP.decision) %>% summarise(n = n()) %>% 
    spread(key = EJP.decision, value = n) %>% 
    mutate(prop_rej = round((Reject/(Reject + `Accept, no revision`))*100, digits = 2)) %>% 
    mutate(., auth.type = x) #%>% 
    #select(-Reject, -`Accept, no revision`) %>% 
})

#cross ASM rejection rate by author type
ASM_rej_rate <- rej_by_auth %>% 
  select(-Reject, -`Accept, no revision`) %>% 
  spread(key = gender, value = prop_rej) %>% 
  mutate(performance = male - female)

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

figure_5A <- rej_by_journ %>% 
  ggplot() + 
  geom_col(aes(x = journal, y = difference, fill = difference)) +
  coord_flip()+
  facet_wrap(~auth.type, nrow = 1)+
  gen_gradient+
  geom_hline(data = ASM_rej_rate, aes(yintercept = performance))+
  labs(x = "\nJournal", y = "Difference in Percent Rejection\n",
       fill = "% Points\nDifference")+
  my_theme_leg_horiz

#B. Do women recieve proportionally more editorial rejections than men?----
fig5_ed_rejs <- bias_data %>% 
  filter(EJP.decision == "Reject" & is.na(days.to.review)) %>% 
  select(grouped.random, gender, 
         EJP.decision, contains("days"), journal,
         num.versions, -days.to.review) %>% 
  distinct()

#percent of submissions that are editorial rejections by gender
fig5_ASM_summary <- bias_data %>% 
  select(gender, grouped.random) %>% distinct() %>% 
  group_by(gender) %>% summarise(n = n())

fig5_ASM_ed_rej <- fig5_ed_rejs %>% 
  group_by(gender) %>% 
  summarise(n = n()) %>% 
  mutate(prop_rej = get_percent(n, fig5_ASM_summary$n)) %>% 
  select(-n) %>% 
  spread(key = gender, value = prop_rej) %>% 
  mutate(performance = male - female) %>% 
  cbind(journal = "All Combined", .) %>% as_tibble()

#percent of submissions that are editorial rejections by gender & journal
fig5_journal_summary <- bias_data %>% 
  select(journal, gender, grouped.random) %>% distinct() %>% 
  group_by(journal, gender) %>% summarise(total = n())

fig5_ed_rejections <- fig5_ed_rejs %>% 
  group_by(journal, gender) %>% 
  summarise(n = n()) %>% 
  left_join(., fig5_journal_summary, by = c("journal", "gender")) %>% 
  distinct() %>% 
  mutate(prop_rej = get_percent(n, total)) 

fig5_ed_reject_n <- fig5_ed_rejections %>% 
  select(-total, -prop_rej) %>% 
  spread(key = gender, value = n) %>% 
  mutate(n = male + female) %>% 
  select(-male, -female)

figure_5B <- fig5_ed_rejections %>% select(-n, -total) %>% 
  spread(key = gender, value = prop_rej) %>% 
  mutate(performance = male - female) %>% 
  left_join(., fig5_ed_reject_n, by = "journal") %>% 
  ggplot() +
  geom_col(aes(x = fct_reorder(journal, performance), 
               y = performance, fill = performance)) + 
  coord_flip()+
  gen_gradient+
  geom_hline(data = fig5_ASM_ed_rej, aes(yintercept = performance))+
  #annotate(geom = "text", x = 12, y = -2.5, label = "All Journals")+
  #geom_text(aes(x = journal, y = 0.75, label = n))+
  labs(x = "\nJournal", 
       y = "Difference in Editorial Rejections")+
  my_theme_horiz

#C. break decisions after review down by journal----
fig5_j_ed_dec_data <- bias_data %>% 
  filter(version.reviewed == 0) %>% 
  filter(version == 0) %>% 
  select(gender, journal, grouped.random, EJP.decision, version) %>% 
  filter(EJP.decision %in% c("Accept, no revision",
                             "Reject", "Revise only")) %>% 
  distinct()

fig5_ASM_summary_dec <- bias_data %>% 
  filter(version.reviewed == 0) %>% 
  filter(version == 0) %>% 
  select(gender, grouped.random, EJP.decision) %>% distinct() %>% 
  group_by(gender) %>% summarise(total = n())

fig5_ASM_dec <- fig5_j_ed_dec_data %>% 
  group_by(gender, EJP.decision) %>% 
  summarise(n = n()) %>% 
  left_join(., fig5_ASM_summary_dec, by = "gender") %>% 
  mutate(prop_dec = get_percent(n, total)) %>%
  select(-n, -total) %>% distinct() %>% 
  spread(key = gender, value = prop_dec) %>% 
  mutate(performance = male - female)

fig5_journal_summary <- fig5_j_ed_dec_data %>% 
  group_by(journal, gender) %>% summarise(total = n())

fig5_journal_dec_summary <- fig5_j_ed_dec_data %>% 
  group_by(journal, EJP.decision) %>% 
  summarise(n = n())

figure_5C <- fig5_j_ed_dec_data %>% 
  group_by(journal, gender, EJP.decision) %>% 
  summarise(n = n()) %>% 
  left_join(., fig5_journal_summary, 
            by = c("journal", "gender")) %>% 
  distinct() %>% 
  mutate(prop_rej = get_percent(n, total)) %>%
  select(-n, -total) %>% 
  spread(key = gender, value = prop_rej) %>% 
  mutate(performance = male - female) %>% 
  left_join(., fig5_journal_dec_summary, 
            by = c("journal", "EJP.decision")) %>% 
  ggplot() +
  geom_col(aes(x = fct_reorder(journal, performance), 
               y = performance, fill = performance)) + 
  facet_wrap(~EJP.decision)+
  coord_flip()+
  gen_gradient+
  geom_hline(data = fig5_ASM_dec, aes(yintercept = performance))+
  #geom_text(aes(x = journal, y = 1.5, label = n))+
  labs(x = "\nJournal", 
       y = "Difference in Decision\nafter First Review")+
  my_theme_horiz

row2 <- plot_grid(figure_5B, figure_5C, 
          labels = c('B', 'C'), label_size = 18, nrow = 1)

plot_grid(figure_5A, row2, labels = 'A', label_size = 18, nrow = 2)

ggsave("Figure_5.png", device = 'png', 
       path = '../submission/', width = 9, height = 6)
