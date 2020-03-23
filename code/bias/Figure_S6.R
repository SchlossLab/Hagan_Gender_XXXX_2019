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

Fig_S6A_data <- left_join(ASM_subs_j, ed_rej_subs_j, 
          by = c("US.inst.type", "gender", "journal")) %>% 
  mutate(prop.rejected = get_percent(ed.rejections, total)) %>% 
  select(-total, -ed.rejections) %>% 
  spread(key = gender, value = prop.rejected) %>% 
  mutate(performance = male - female)
  
Figure_S6A <- Fig_S6A_data %>% 
  ggplot(aes(x = journal, y = performance, fill = performance))+
  geom_col()+
  facet_wrap(~US.inst.type, scales = "free", ncol = 2)+
  coord_flip()+
  gen_gradient+
  labs(x = "\n", 
       y = "Difference in Editorial Rejections\n")+
  my_theme_horiz

#S6B. Difference in accepted rates by journal & inst type----
acc <- bias_data %>% 
  filter(EJP.decision == "Accept") %>% 
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

Fig_S6B_data <- left_join(ASM_subs_j, acc_subs_j, 
          by = c("US.inst.type", "gender", "journal")) %>% 
  mutate(prop.accepted = get_percent(accepted, total)) %>% 
  select(-total, -accepted) %>% 
  spread(key = gender, value = prop.accepted) %>% 
  mutate(performance = male - female)

Figure_S6B <- Fig_S6B_data %>% 
  ggplot(aes(x = journal, y = performance, fill = performance))+
  geom_col()+
  facet_wrap(~US.inst.type, scales = "free", ncol = 2)+
  coord_flip()+
  gen_gradient+
  labs(x = "\n", 
       y = "Difference in Acceptance Rates\n")+
  my_theme_horiz

#FormerS6C. Difference in review recommendation by institution type----
rev_rec_inst <- bias_data %>% 
  filter(!is.na(review.recommendation)) %>% 
  filter(US.inst == "yes") %>% 
  filter(!is.na(US.inst.type)) %>% 
  select(grouped.random, gender, 
         US.inst.type, review.recommendation) %>% 
  filter(review.recommendation %in% c("Revise", "Reject", 
                                      "Accept")) %>% 
  distinct()

US_inst_totals <- rev_rec_inst %>% 
  group_by(US.inst.type, gender) %>% 
  summarise(n = n())

#Figure_S6C <- rev_rec_inst %>%   
#  group_by(US.inst.type, review.recommendation, gender) %>% 
#  summarise(n = n()) %>% as_tibble() %>% 
#  spread(key = review.recommendation, value = n) %>% 
#  mutate_if(is.numeric, 
#            funs(get_percent(., US_inst_totals$n))) %>%
#  gather(`Accept, no revision`:`Revise only`,
#         key = review.recommendation, value = percent) %>% 
#  spread(key = gender, value = percent) %>% 
#  mutate(overperformance = male - female) %>%
#  ggplot(aes(x = US.inst.type, 
#             y = overperformance, fill = overperformance))+
#  geom_col(position = "dodge")+
#  facet_wrap(~review.recommendation, ncol = 1)+
#  gen_gradient+
#  coord_flip()+
#  labs(x = "\nInstitution Type", 
#       y = "Difference in Review Recommendation")+
#  my_theme_horiz

#S6C. difference in acceptance recommendation by reviewer gender----
rev_rec_data <- bias_data %>% 
  filter(version.reviewed == 0) %>% 
  filter(version == 0) %>% 
  select(gender, journal, published, review.recommendation, 
         reviewer.gender, reviewer.random.id, random.manu.num, version.reviewed, 
         US.inst, US.inst.type) %>% distinct() %>% 
    filter(review.recommendation %in% c("Revise", "Reject", 
                                        "Accept")) %>%
  distinct()

sub_inst_gen <- rev_rec_data %>% 
  filter(US.inst == "yes") %>% 
  filter(!is.na(US.inst.type)) %>% 
  filter(review.recommendation != "Revise") %>% 
  select(random.manu.num, US.inst.type, reviewer.gender,
         review.recommendation, gender) %>% 
  distinct() %>% 
  group_by(reviewer.gender, US.inst.type, gender) %>% 
  summarise(total = n())

summ_inst_num <- rev_rec_data %>% 
  filter(US.inst == "yes") %>% 
  filter(!is.na(US.inst.type)) %>% 
  filter(review.recommendation != "Revise") %>% 
  filter(reviewer.gender %in% c("female", "male")) %>% 
  select(random.manu.num, reviewer.gender, US.inst.type, review.recommendation, gender) %>% 
  distinct() %>% 
  group_by(reviewer.gender, US.inst.type, gender, 
           review.recommendation) %>% 
  summarise(n = n()) %>% as_tibble() %>% 
  left_join(., sub_inst_gen, by = c("reviewer.gender",
                                    "US.inst.type", 
                                    "gender")) %>% 
  mutate(percent = get_percent(n, total))

rev_inst_totals <- summ_inst_num %>% 
  select(reviewer.gender, US.inst.type, gender, total) %>% distinct() %>% 
  spread(gender, total) %>% 
  mutate(total_sub = male + female) %>% select(-male, -female)

summ_inst <- summ_inst_num %>% 
  select(-n, -total) %>% 
  spread(key = gender, value = percent) %>% 
  mutate(overperform = male - female) %>% 
  select(-male, -female) %>% 
  left_join(., rev_inst_totals, by = c("reviewer.gender", "US.inst.type")) %>% 
  mutate(US.inst.type = paste0(US.inst.type, " (N=", total_sub, ")"))

Figure_S6C <- summ_inst %>% 
  filter(review.recommendation == "Accept") %>% 
  ggplot(aes(x = fct_reorder(US.inst.type, desc(total_sub)), 
             fill = overperform,
             y = overperform))+
  geom_col(position = "dodge")+
  scale_fill_gradient2(low = "#D55E00", mid='snow3', 
                       high = "#0072B2", space = "Lab")+
  coord_flip()+
  facet_wrap(~gen_rev_facet(reviewer.gender), ncol = 1, 
             scales = "free_y")+
  labs(x = "\n", 
       y = "Difference in Acceptance Recommendation
       by Reviewer Gender")+
  my_theme_horiz

#S6D. important features for editorial rejection----
Figure_S6D <- plot_feature_ranks(ranked_weights)+
  labs(y = "Importance Rank", color = "Direction\nof Impact",
       x = "\nFeature")

#generate & save figure----
Fig_S6AB <- plot_grid(Figure_S6A, Figure_S6B,
          labels = c('A', 'B'),
          label_size = 18,
          ncol = 2)

Fig_S6CD <- plot_grid(Figure_S6C, Figure_S6D,
                      labels = c('C', 'D'),
                      label_size = 18,
                      ncol = 2)

plot_grid(Fig_S6AB, Fig_S6CD,
          rel_heights = c(2, 1),
          nrow = 2)

ggsave("Figure_S6.png", device = 'png', 
       path = 'submission', width = 14, height = 16)
