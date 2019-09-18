#Does the geographical origin & institution of the corresponding author matter?

#A. editoral rejections for U.S. only----
#Do women recieve proportionally more editorial rejections than men?
US_ed_rejs <- bias_data %>% 
  filter(EJP.decision == "Reject" & is.na(days.to.review)) %>%
  filter(US.inst == "yes") %>% 
  select(grouped.random, gender, 
         EJP.decision, contains("days"), journal,
         num.versions, -days.to.review) %>% 
  distinct()

#percent of submissions that are editorial rejections by gender
fig6_ASM_summary <- bias_data %>% 
  filter(US.inst == "yes") %>% 
  select(gender, grouped.random) %>% distinct() %>% 
  group_by(gender) %>% summarise(n = n())

fig6_ASM_ed_rej <- US_ed_rejs %>% 
  group_by(gender) %>% 
  summarise(n = n()) %>% 
  mutate(prop_rej = get_percent(n, fig6_ASM_summary$n)) %>% 
  select(-n) %>% 
  spread(key = gender, value = prop_rej) %>% 
  mutate(performance = male - female) 

#percent of submissions that are editorial rejections by gender & journal
fig6_journal_summary <- bias_data %>% 
  filter(US.inst == "yes") %>% 
  select(journal, gender, grouped.random) %>% distinct() %>% 
  group_by(journal, gender) %>% summarise(total = n())

fig6_ed_rejections <- US_ed_rejs %>% 
  group_by(journal, gender) %>% 
  summarise(n = n()) %>% 
  left_join(., fig6_journal_summary, by = c("journal", "gender")) %>% 
  distinct() %>% 
  mutate(prop_rej = get_percent(n, total)) 

fig6_ed_reject_n <- fig6_ed_rejections %>% 
  select(-total, -prop_rej) %>% 
  spread(key = gender, value = n) %>% 
  mutate(n = male + female) %>% 
  select(-male, -female)

figure_6A <- fig6_ed_rejections %>% select(-n, -total) %>% 
  spread(key = gender, value = prop_rej) %>% 
  mutate(performance = male - female) %>% 
  left_join(., fig6_ed_reject_n, by = "journal") %>% 
  ggplot() +
  geom_col(aes(x = fct_reorder(journal, performance), 
               y = performance, fill = performance)) + 
  coord_flip()+
  gen_gradient+
  geom_hline(data = fig6_ASM_ed_rej, aes(yintercept = performance))+
  annotate(geom = "text", x = 12, y = -2.5, label = "All Journals")+
  #geom_text(aes(x = journal, y = 0.75, label = n))+
  labs(x = "\nJournal", 
       y = "Difference in Editorial Rejections\n")+
  my_theme_horiz

#B. decisions following review for U.S. only----
#break decisions after review down by journal
US_j_ed_dec_data <- bias_data %>% 
  filter(US.inst == "yes") %>% 
  filter(version.reviewed == 0) %>% 
  filter(grouped.vers == 1) %>% 
  select(gender, journal, grouped.random, EJP.decision, version) %>% 
  filter(EJP.decision %in% c("Accept", "Reject", "Revise")) %>% 
  distinct()

US_ASM_summary_dec <- bias_data %>% 
  filter(US.inst == "yes") %>% 
  filter(version.reviewed == 0) %>% 
  filter(grouped.vers == 1) %>% 
  select(gender, grouped.random, EJP.decision) %>% distinct() %>% 
  group_by(gender) %>% summarise(total = n())

US_j_dec <- US_j_ed_dec_data %>% 
  group_by(gender, EJP.decision) %>% 
  summarise(n = n()) %>% 
  left_join(., US_ASM_summary_dec, by = "gender") %>% 
  mutate(prop_dec = get_percent(n, total)) %>%
  select(-n, -total) %>% distinct() %>% 
  spread(key = gender, value = prop_dec) %>% 
  mutate(performance = male - female)

US_journal_summary <- US_j_ed_dec_data %>% 
  group_by(journal, gender) %>% summarise(total = n())

US_journal_dec_summary <- US_j_ed_dec_data %>% 
  group_by(journal, EJP.decision) %>% 
  summarise(n = n())

figure_6B <- US_j_ed_dec_data %>% 
  group_by(journal, gender, EJP.decision) %>% 
  summarise(n = n()) %>% 
  left_join(., US_journal_summary, 
            by = c("journal", "gender")) %>% 
  distinct() %>% 
  mutate(prop_rej = get_percent(n, total)) %>%
  select(-n, -total) %>% 
  spread(key = gender, value = prop_rej) %>% 
  mutate(performance = male - female) %>% 
  left_join(., US_journal_dec_summary, 
            by = c("journal", "EJP.decision")) %>% 
  ggplot() +
  geom_col(aes(x = fct_reorder(journal, performance), 
               y = performance, fill = performance)) + 
  facet_wrap(~EJP.decision)+
  coord_flip()+
  gen_gradient+
  geom_hline(data = US_j_dec, aes(yintercept = performance))+
  #geom_text(aes(x = journal, y = 1.5, label = n))+
  labs(x = "\nJournal", 
       y = "Difference in Decision after First Review\n")+
  my_theme_horiz

#C. acceptance & editorial rejection rates by U.S. insitution----
fig6c_ed_rejs <- bias_data %>% 
  filter(US.inst == "yes") %>% 
  filter(!is.na(US.inst.type)) %>% 
  filter(EJP.decision == "Reject" & is.na(days.to.review)) %>% 
  select(-days.to.review, -contains("version")) %>% 
  distinct() #editorially rejected manuscripts

#count all submissions
fig6c_ASM_subs <- bias_data %>% 
  filter(US.inst == "yes") %>% 
  filter(!is.na(US.inst.type)) %>% 
  select(gender, grouped.random, US.inst.type) %>% 
  distinct() %>% 
  group_by(US.inst.type, gender) %>% summarise(total = n()) 

#count editorial rejections
fig6c_ed_rej_subs <- fig6c_ed_rejs %>% 
  select(gender, grouped.random, US.inst.type) %>% 
  distinct() %>% 
  group_by(US.inst.type, gender) %>% 
  summarise(ed.rejections = n())

#calculate percentage point difference in editorial rejections
fig6c_editorial_rej_per <- left_join(fig6c_ASM_subs, fig6c_ed_rej_subs, 
                               by = c("US.inst.type", "gender")) %>% 
  mutate(prop.ed.rej = get_percent(ed.rejections, total)) %>% 
  select(-total, -ed.rejections) %>% 
  spread(key = gender, value = prop.ed.rej) %>% 
  mutate(performance = male - female,
         rate = "Editorial Rejection") %>% 
  select(-male, -female) 

#accepted---
fig6c_acc <- bias_data %>% 
  filter(EJP.decision == "Accept") %>% 
  filter(US.inst == "yes") %>% 
  filter(!is.na(US.inst.type)) %>%
  select(-days.to.review, contains("version")) %>% 
  distinct()

fig6c_acc_subs <- fig6c_acc %>% 
  select(gender, grouped.random, US.inst.type) %>% 
  distinct() %>% 
  group_by(US.inst.type, gender) %>% summarise(accepted = n())

#calculate percentage point difference in acceptance
fig6c_acc_per <- left_join(fig6c_ASM_subs, fig6c_acc_subs, 
                               by = c("US.inst.type", "gender")) %>% 
  mutate(prop.acc = get_percent(accepted, total)) %>% 
  select(-total, -accepted) %>% 
  spread(key = gender, value = prop.acc) %>% 
  mutate(performance = male - female,
         rate = "Acceptance") %>% 
  select(-male, -female) 

fig6c_inst_rates <- rbind(fig6c_editorial_rej_per, fig6c_acc_per)

#plot
figure_6C <- fig6c_inst_rates %>% 
  ggplot()+
  geom_col(aes(x = US.inst.type, y = performance, fill = performance))+
  coord_flip()+
  facet_wrap(~rate, nrow = 2)+
  gen_gradient+
  labs(x = "\nInstitution Type", 
       y = "Difference")+
  my_theme_horiz

#D. acceptance by editor gender and institution type after 1st review----
fig6d_ed_genders <- editor_data %>%  
  select(grouped.random, gender) %>% 
  distinct() %>% 
  rename("editor.gender" = "gender")

fig6d_ed_dec_data <- bias_data %>% 
  filter(version.reviewed == 0) %>% 
  filter(grouped.vers == 1) %>% 
  select(gender, journal, grouped.random, 
         US.inst, US.inst.type, EJP.decision) %>% 
  filter(US.inst == "yes") %>% 
  filter(!is.na(US.inst.type)) %>% 
  filter(EJP.decision %in% c("Accept", "Revise", "Reject")) %>% 
  left_join(., fig6d_ed_genders, by = "grouped.random") %>%
  distinct()

fig6d_sub_inst_gen <- fig6d_ed_dec_data %>% 
  select(grouped.random, US.inst.type, editor.gender,
         EJP.decision, gender) %>% 
  distinct() %>% 
  group_by(editor.gender, US.inst.type, gender) %>% 
  summarise(total = n())

fig6d_summ_inst <- fig6d_ed_dec_data %>% 
  select(grouped.random, editor.gender, US.inst.type, 
         EJP.decision, gender) %>% 
  distinct() %>% 
  group_by(editor.gender, US.inst.type, gender, 
           EJP.decision) %>% 
  summarise(n = n()) %>% as_tibble() %>% 
  left_join(., fig6d_sub_inst_gen, by = c("editor.gender",
                                    "US.inst.type", 
                                    "gender")) %>% 
  mutate(percent = get_percent(n, total)) %>% 
  select(-n, -total) %>% 
  spread(key = gender, value = percent) %>% 
  mutate(overperform = male - female)

#plot
figure_6D <- fig6d_summ_inst %>% 
  filter(EJP.decision == "Reject") %>% 
  ggplot(aes(x = US.inst.type, fill = overperform,
             y = overperform))+
  geom_col(position = "dodge")+
  gen_gradient+
  coord_flip()+
  facet_wrap(~gen_ed_facet(editor.gender), ncol = 1)+
  labs(x = "\nInstitution Type", 
       y = "Difference in Rejection\nby Editor Gender")+
  my_theme_horiz

#generate & save figure----
plot_grid(figure_6A, figure_6B, figure_6C, figure_6D,
          labels = c('A', 'B', 'C', 'D'), 
          label_size = 18, ncol = 2)

ggsave("Figure_6.png", device = 'png', 
       path = 'submission', width = 12, height = 9)
