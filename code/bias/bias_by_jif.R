
jif_data <- read_csv("data/IF_2018.csv", n_max = 13) %>% 
  separate(`JCR Abbreviation;ISSN;Total Cites;Journal Impact Factor;5 Year Impact Factor;Immediacy Index;Eigenfactor`, 
           into = c("journal", "ISSN", "Total_Cites", "JIF", "5_Yr_IF", 
                    "Immediacy_Index", "Eigenfactor"), sep = ";")

ed_rej_jif <- fig4_ed_rejections %>% select(-n, -total) %>% 
  spread(key = gender, value = prop_rej) %>% 
  mutate(performance = male - female) %>% 
  left_join(., fig4_ed_reject_n, by = "journal") %>% 
  mutate(EJP.decision = "Editorial Rejection") %>% 
  select(-female, -male, -n)

first_rev_jif <- fig4_j_ed_dec_data %>% 
  group_by(journal, gender, EJP.decision) %>% 
  summarise(n = n()) %>% 
  left_join(., fig4_journal_summary, 
            by = c("journal", "gender")) %>% 
  distinct() %>% 
  mutate(prop_rej = get_percent(n, total)) %>%
  select(-n, -total) %>% 
  spread(key = gender, value = prop_rej) %>% 
  mutate(performance = male - female) %>% 
  left_join(., fig4_journal_dec_summary, 
            by = c("journal", "EJP.decision")) %>% 
  select(-female, -male, -n)

jif_summary <- jif_data %>% 
  select(journal, JIF) %>% 
  left_join(rbind(ed_rej_jif, first_rev_jif), ., by = "journal")

#plot
jif_plot <- jif_summary %>% 
  ggplot(aes(x = as.numeric(JIF), y = performance))+
  geom_smooth(method="lm",se=FALSE) +
  geom_point()

#used Adjusted R-squared
stats <- summary(lm(jif_summary$performance~as.numeric(jif_summary$JIF)))
