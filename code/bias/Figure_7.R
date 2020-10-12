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
fig7_ASM_summary <- bias_data %>% 
  filter(US.inst == "yes") %>% 
  select(gender, grouped.random) %>% distinct() %>% 
  group_by(gender) %>% summarise(n = n())

fig7_ASM_ed_rej <- US_ed_rejs %>% 
  group_by(gender) %>% 
  summarise(n = n()) %>% 
  mutate(prop_rej = get_percent(n, fig7_ASM_summary$n)) %>% 
  select(-n) %>% 
  spread(key = gender, value = prop_rej) %>% 
  mutate(performance = male - female) 

#percent of submissions that are editorial rejections by gender & journal
fig7_journal_summary <- bias_data %>% 
  filter(US.inst == "yes") %>% 
  select(journal, gender, grouped.random) %>% distinct() %>% 
  group_by(journal, gender) %>% summarise(total = n())

fig7_ed_rejections <- US_ed_rejs %>% 
  group_by(journal, gender) %>% 
  summarise(n = n()) %>% 
  left_join(., fig7_journal_summary, by = c("journal", "gender")) %>% 
  distinct() %>% 
  mutate(prop_rej = get_percent(n, total)) 

fig7_ed_reject_n <- fig7_ed_rejections %>% 
  select(-total, -prop_rej) %>% 
  spread(key = gender, value = n) %>% 
  mutate(n = male + female) %>% 
  select(-male, -female)

figure_7A_data <- fig7_ed_rejections %>% select(-n, -total) %>% 
  spread(key = gender, value = prop_rej) %>% 
  mutate(performance = male - female) %>% 
  left_join(., fig7_ed_reject_n, by = "journal")

plot_breaks <- pretty(figure_7A_data$performance, n = 7)  
  
figure_7A <- figure_7A_data %>%   
    ggplot() +
  geom_col(aes(x = fct_reorder(journal, performance), 
               y = performance, fill = performance)) + 
  coord_flip()+
  gen_gradient+
  geom_hline(data = fig7_ASM_ed_rej, aes(yintercept = performance))+
  scale_y_continuous(breaks = plot_breaks,
                     labels = abs(plot_breaks))+
  annotate(geom = "text", x = 12, y = -4, label = "All Journals")+
  #geom_text(aes(x = journal, y = 0.75, label = n))+
  labs(x = "\n", 
       y = "Difference in Editorial Rejections",
       caption = expression("Men" %<-% "Favored Gender" %->% "Women"))+
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

figure_7B_data <- US_j_ed_dec_data %>% 
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
            by = c("journal", "EJP.decision"))

plot_breaks <- pretty(figure_7B_data$performance, n = 7)

figure_7B <- figure_7B_data %>% 
  ggplot() +
  geom_col(aes(x = fct_reorder(journal, performance), 
               y = performance, fill = performance)) + 
  facet_wrap(~EJP.decision)+
  scale_y_continuous(breaks = plot_breaks,
                     labels = abs(plot_breaks))+
  coord_flip()+
  gen_gradient+
  geom_hline(data = US_j_dec, aes(yintercept = performance))+
  #geom_text(aes(x = journal, y = 1.5, label = n))+
  labs(x = "\n", 
       y = "Difference in Decision after First Review\n\n")+
  my_theme_horiz

#C. acceptance & editorial rejection rates by U.S. insitution----
fig7c_ed_rejs <- bias_data %>% 
  filter(US.inst == "yes") %>% 
  filter(!is.na(US.inst.type)) %>% 
  filter(EJP.decision == "Reject" & is.na(days.to.review)) %>% 
  select(-days.to.review, -contains("version")) %>% 
  distinct() #editorially rejected manuscripts

#count all submissions
fig7c_ASM_subs <- bias_data %>% 
  filter(US.inst == "yes") %>% 
  filter(!is.na(US.inst.type)) %>% 
  select(gender, grouped.random, US.inst.type) %>% 
  distinct() %>% 
  group_by(US.inst.type, gender) %>% summarise(total = n()) 

#count editorial rejections
fig7c_ed_rej_subs <- fig7c_ed_rejs %>% 
  select(gender, grouped.random, US.inst.type) %>% 
  distinct() %>% 
  group_by(US.inst.type, gender) %>% 
  summarise(ed.rejections = n())

fig7c_edrej_sum_inst <- fig7c_ed_rej_subs %>% 
  group_by(US.inst.type) %>% 
  summarise(total = sum(ed.rejections))

#calculate percentage point difference in editorial rejections
fig7c_editorial_rej_per <- left_join(fig7c_ASM_subs, fig7c_ed_rej_subs, 
                               by = c("US.inst.type", "gender")) %>% 
  mutate(prop.ed.rej = get_percent(ed.rejections, total)) %>% 
  select(-total, -ed.rejections) %>% 
  spread(key = gender, value = prop.ed.rej) %>% 
  mutate(performance = male - female,
         rate = "Editorial Rejection") %>% 
  select(-male, -female) %>% 
  left_join(., fig7c_edrej_sum_inst, by = "US.inst.type")

#accepted---
fig7c_acc <- bias_data %>% 
  filter(EJP.decision == "Accept") %>% 
  filter(US.inst == "yes") %>% 
  filter(!is.na(US.inst.type)) %>%
  select(-days.to.review, contains("version")) %>% 
  distinct()

fig7c_acc_subs <- fig7c_acc %>% 
  select(gender, grouped.random, US.inst.type) %>% 
  distinct() %>% 
  group_by(US.inst.type, gender) %>% summarise(accepted = n())

fig7c_acc_sum_inst <- fig7c_acc_subs %>% 
  group_by(US.inst.type) %>% 
  summarise(total = sum(accepted))

#calculate percentage point difference in acceptance
fig7c_acc_per <- left_join(fig7c_ASM_subs, fig7c_acc_subs, 
                               by = c("US.inst.type", "gender")) %>% 
  mutate(prop.acc = get_percent(accepted, total)) %>% 
  select(-total, -accepted) %>% 
  spread(key = gender, value = prop.acc) %>% 
  mutate(performance = male - female,
         rate = "Acceptance") %>% 
  select(-male, -female) %>% 
  left_join(., fig7c_acc_sum_inst, by = "US.inst.type")

fig7c_inst_rates <- rbind(fig7c_editorial_rej_per, fig7c_acc_per) %>% 
  as.data.frame() %>% 
  mutate(US.inst.type = paste0(US.inst.type, " (N=", total, ")"),
         rate = as.factor(rate))

#plot
plot_breaks <- pretty(fig7c_inst_rates$performance, n = 16)

figure_7C <- fig7c_inst_rates %>% 
  ggplot(aes(x = fct_reorder(US.inst.type, desc(total)), 
                                          y = performance, fill = performance))+
  geom_col()+
  coord_flip()+
  facet_wrap(~rate, scales = "free_y", nrow = 2)+
  scale_y_continuous(breaks = plot_breaks,
                     labels = abs(plot_breaks))+
  gen_gradient+
  labs(x = "\n", y = "Difference in Decision\n")+
  my_theme_horiz

#D. acceptance by editor gender and institution type after 1st review----
fig7d_ed_genders <- editor_data %>%  
  select(grouped.random, gender) %>% 
  distinct() %>% 
  rename("editor.gender" = "gender")

fig7d_ed_dec_data <- bias_data %>% 
  filter(version.reviewed == 0) %>% 
  filter(grouped.vers == 1) %>% 
  select(gender, journal, grouped.random, 
         US.inst, US.inst.type, EJP.decision) %>% 
  filter(US.inst == "yes") %>% 
  filter(!is.na(US.inst.type)) %>% 
  filter(EJP.decision %in% c("Accept", "Revise", "Reject")) %>% 
  left_join(., fig7d_ed_genders, by = "grouped.random") %>%
  distinct()

fig7d_sub_inst_gen <- fig7d_ed_dec_data %>% 
  select(grouped.random, US.inst.type, editor.gender,
         EJP.decision, gender) %>% 
  distinct() %>% 
  group_by(editor.gender, US.inst.type, gender) %>% 
  summarise(total = n())

fig7d_summ_inst <- fig7d_ed_dec_data %>% 
  select(grouped.random, editor.gender, US.inst.type, 
         EJP.decision, gender) %>% 
  distinct() %>% 
  group_by(editor.gender, US.inst.type, gender, 
           EJP.decision) %>% 
  summarise(n = n()) %>% as_tibble() %>% 
  left_join(., fig7d_sub_inst_gen, by = c("editor.gender",
                                    "US.inst.type", 
                                    "gender")) %>% 
  mutate(percent = get_percent(n, total)) %>% 
  select(-n, -total) %>% 
  spread(key = gender, value = percent) %>% 
  mutate(overperform = male - female)

fig7d_inst_total <- fig7d_sub_inst_gen %>% 
  group_by(editor.gender, US.inst.type) %>% 
  summarise(total = sum(total)) %>% 
  left_join(., fig7d_summ_inst, 
            by = c("editor.gender", "US.inst.type")) %>% 
  as.data.frame() %>% 
  filter(EJP.decision == "Reject") %>% 
  mutate(US.inst.type = paste0(US.inst.type, " (N=", total, ")"))

#plot
plot_breaks <- pretty(fig7d_inst_total$overperform, n = 16)

figure_7D <- fig7d_inst_total %>% 
  ggplot(aes(x = fct_reorder(US.inst.type, desc(total)), fill = overperform,
             y = overperform))+
  geom_col()+
  gen_gradient+
  scale_y_continuous(breaks = plot_breaks,
                     labels = abs(plot_breaks))+
  coord_flip()+
  facet_wrap(~gen_ed_facet(editor.gender), ncol = 1,
             scales = "free_y")+
  labs(x = "\n", 
       y = "Difference in Rejection Decision",
       caption = expression("Men" %<-% "Favored Gender" %->% "Women"))+
  my_theme_horiz

#generate & save figure----
Fig7_legend <- get_legend(ed_rejections_legend)

blank <- ggplot()+theme_void()

plot_legend <- plot_grid(blank, Fig7_legend, blank, nrow = 1)

fig_7_row1 <- plot_grid(figure_7A, figure_7B,
                        labels = c('A', 'B'), 
                        rel_widths = c(1.5, 2),
                        label_size = 18, ncol = 2)

plot_grid(plot_legend, fig_7_row1, figure_7C, figure_7D,
          labels = c('','', 'C', 'D'), 
          rel_heights = c(0.25, 1, 1, 1),
          label_size = 18, nrow = 4)

ggsave("Figure_7.tiff", device = 'tiff', units = "in", scale = 1.75,
       path = 'submission', width = 6.8, height = 9)
