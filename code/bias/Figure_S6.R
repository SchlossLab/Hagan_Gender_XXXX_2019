#Does the institution of the corresponding author matter? Supplementary
#S6A. difference in editorial rejection by journal & inst type----
ed_rejs <- bias_data %>% 
  filter(published == "no") %>% 
  filter(EJP.decision == "Reject" & is.na(days.to.review)) %>% 
  select(-days.to.review, -contains("version")) %>% 
  distinct() 

#editorial rejections by journal--
ASM_subs_j <- bias_data %>% 
  filter(US.inst == "yes") %>% 
  filter(!is.na(US.inst.type)) %>% 
  select(journal, gender, grouped.random, US.inst.type) %>% 
  distinct() %>% 
  group_by(journal, US.inst.type, gender) %>% summarise(total = n()) 

ed_rej_subs_j <- ed_rejs %>% 
  filter(US.inst == "yes") %>% 
  filter(!is.na(US.inst.type)) %>% 
  select(journal, gender, grouped.random, US.inst.type) %>% 
  distinct() %>% 
  group_by(journal, US.inst.type, gender) %>% 
  summarise(ed.rejections = n())

Figure_S6A <- left_join(ASM_subs_j, ed_rej_subs_j, 
          by = c("US.inst.type", "gender", "journal")) %>% 
  mutate(prop.rejected = get_percent(ed.rejections, total)) %>% 
  select(-total, -ed.rejections) %>% 
  spread(key = gender, value = prop.rejected) %>% 
  mutate(performance = male - female) %>% 
  ggplot()+
  geom_col(aes(x = journal, y = performance, fill = performance))+
  #scale_fill_manual(labels = gen_ed_labels, values = gen_ed_colors)+
  facet_wrap(~US.inst.type)+
  coord_flip()+
  gen_gradient+
  labs(x = "\nJournal", 
       y = "Difference in Editorial Rejections\n")+
  my_theme_horiz

#S6B. Difference in accepted rates by journal & inst type----
acc <- bias_data %>% 
  filter(EJP.decision == "Accept, no revision") %>% 
  filter(US.inst == "yes") %>% 
  filter(!is.na(US.inst.type)) %>%
  select(-days.to.review, contains("version")) %>% 
  distinct()

ASM_subs_j <- bias_data %>% 
  filter(US.inst == "yes") %>% 
  filter(!is.na(US.inst.type)) %>% 
  select(journal, gender, grouped.random, US.inst.type) %>% 
  distinct() %>% 
  group_by(journal, US.inst.type, gender) %>% summarise(total = n()) 

acc_subs_j <- acc %>% 
  select(journal, gender, grouped.random, US.inst.type) %>% 
  distinct() %>% 
  group_by(journal, US.inst.type, gender) %>% 
  summarise(accepted = n())

Figure_S6B <- left_join(ASM_subs_j, acc_subs_j, 
          by = c("US.inst.type", "gender", "journal")) %>% 
  mutate(prop.accepted = get_percent(accepted, total)) %>% 
  select(-total, -accepted) %>% 
  spread(key = gender, value = prop.accepted) %>% 
  mutate(performance = male - female) %>% 
  ggplot()+
  geom_col(aes(x = journal, y = performance, fill = performance))+
  #scale_fill_manual(labels = gen_ed_labels, values = gen_ed_colors)+
  facet_wrap(~US.inst.type)+
  coord_flip()+
  gen_gradient+
  labs(x = "\nJournal", 
       y = "Difference in Acceptance Rates\n")+
  my_theme_horiz

#S6C. Difference in review recommendation by institution type----
rev_rec_inst <- bias_data %>% 
  filter(!is.na(review.recommendation)) %>% 
  filter(US.inst == "yes") %>% 
  filter(!is.na(US.inst.type)) %>% 
  select(grouped.random, gender, 
         US.inst.type, review.recommendation) %>% 
  filter(review.recommendation %in% c("Revise only", "Reject", 
                                      "Accept, no revision")) %>% 
  distinct()

US_inst_totals <- rev_rec_inst %>% 
  group_by(US.inst.type, gender) %>% 
  summarise(n = n())

Figure_S6C <- rev_rec_inst %>%   
  group_by(US.inst.type, review.recommendation, gender) %>% 
  summarise(n = n()) %>% as_tibble() %>% 
  spread(key = review.recommendation, value = n) %>% 
  mutate_if(is.numeric, 
            funs(get_percent(., US_inst_totals$n))) %>% 
  gather(`Accept, no revision`:`Revise only`,
         key = review.recommendation, value = percent) %>% 
  spread(key = gender, value = percent) %>% 
  mutate(overperformance = male - female) %>%
  ggplot(aes(x = US.inst.type, 
             y = overperformance, fill = overperformance))+
  geom_col(position = "dodge")+
  facet_wrap(~review.recommendation, ncol = 1)+
  gen_gradient+
  coord_flip()+
  labs(x = "\nInstitution Type", 
       y = "Difference in Review Recommendation")+
  my_theme_horiz

#S6D. difference in acceptance recommendation by reviewer gender----
rev_rec_data <- bias_data %>% 
  filter(version.reviewed == 0) %>% 
  filter(version == 0) %>% 
  select(gender, journal, published, review.recommendation, 
         reviewer.gender, reviewer.random.id, random.manu.num, version.reviewed, 
         US.inst, US.inst.type) %>% distinct() %>% 
  filter(review.recommendation %in% c("Revise only", "Reject", "Accept, no revision")) %>%
  distinct()

sub_inst_gen <- rev_rec_data %>% 
  filter(US.inst == "yes") %>% 
  filter(!is.na(US.inst.type)) %>% 
  filter(review.recommendation != "Revise only") %>% 
  select(random.manu.num, US.inst.type, reviewer.gender,
         review.recommendation, gender) %>% 
  distinct() %>% 
  group_by(reviewer.gender, US.inst.type, gender) %>% 
  summarise(total = n())

summ_inst <- rev_rec_data %>% 
  filter(US.inst == "yes") %>% 
  filter(!is.na(US.inst.type)) %>% 
  filter(review.recommendation != "Revise only") %>% 
  filter(reviewer.gender %in% c("female", "male")) %>% 
  select(random.manu.num, reviewer.gender, US.inst.type, review.recommendation, gender) %>% 
  distinct() %>% 
  group_by(reviewer.gender, US.inst.type, gender, 
           review.recommendation) %>% 
  summarise(n = n()) %>% as_tibble() %>% 
  left_join(., sub_inst_gen, by = c("reviewer.gender",
                                    "US.inst.type", 
                                    "gender")) %>% 
  mutate(percent = get_percent(n, total)) %>% 
  select(-n, -total) %>% 
  spread(key = gender, value = percent) %>% 
  mutate(overperform = male - female)

Figure_S6D <- summ_inst %>% 
  filter(review.recommendation == "Accept, no revision") %>% 
  ggplot(aes(x = US.inst.type, fill = overperform,
             y = overperform))+
  geom_col(position = "dodge")+
  scale_fill_gradient2(low = "#D55E00", mid='snow3', 
                       high = "#0072B2", space = "Lab")+
  coord_flip()+
  facet_wrap(~gen_ed_facet(reviewer.gender), ncol = 1)+
  labs(x = "\nInstitution Type", 
       y = "Difference in Acceptance Recomendation\nby Reviewer Gender")+
  my_theme_horiz

#generate & save figure----
plot_grid(Figure_S6A, Figure_S6B, Figure_S6C, Figure_S6D,
          labels = c('A', 'B', 'C', 'D'),
          label_size = 18,
          nrow = 2)

ggsave("Figure_S6.png", device = 'png', 
       path = '../submission', width = 16, height = 13)
