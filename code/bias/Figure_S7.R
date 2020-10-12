#Does the institution of the corresponding author matter? Supplementary
#S7A. difference in editorial rejection by journal & inst type----
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

Fig_S7A_data <- left_join(ASM_subs_j, ed_rej_subs_j, 
          by = c("US.inst.type", "gender", "journal")) %>% 
  mutate(prop.rejected = get_percent(ed.rejections, total)) %>% 
  select(-total, -ed.rejections) %>% 
  spread(key = gender, value = prop.rejected) %>% 
  mutate(performance = male - female)

break_list <- pretty(Fig_S7A_data$performance, n = 7)

Figure_S7A <- Fig_S7A_data %>% 
  ggplot(aes(x = journal, y = performance, fill = performance))+
  geom_col()+
  facet_wrap(~US.inst.type, scales = "free", ncol = 2)+
  coord_flip()+
  gen_gradient_40+
  scale_y_continuous(breaks = break_list,
                     labels = abs(break_list))+
  labs(x = "\n",
       y = "Difference in Editorial Rejections",
       fill = "% Points\nDifference",
       caption = expression("Men" %<-% "Favored Gender" %->% "Women"))+
  my_theme_horiz+
  theme(legend.position = c(0.8,0.1))

#S7B. Difference in accepted rates by journal & inst type----
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

Fig_S7B_data <- left_join(ASM_subs_j, acc_subs_j, 
          by = c("US.inst.type", "gender", "journal")) %>% 
  mutate(prop.accepted = get_percent(accepted, total)) %>% 
  select(-total, -accepted) %>% 
  spread(key = gender, value = prop.accepted) %>% 
  mutate(performance = male - female)

break_list <- pretty(Fig_S7B_data$performance, n = 7)

Figure_S7B <- Fig_S7B_data %>% 
  ggplot(aes(x = journal, y = performance, fill = performance))+
  geom_col()+
  facet_wrap(~US.inst.type, scales = "free", ncol = 2)+
  coord_flip()+
  gen_gradient_40+
  scale_y_continuous(breaks = break_list,
                     labels = abs(break_list))+
  labs(x = "\n", 
       y = "Difference in Acceptance Rates", 
       fill = "% Points\nDifference",
       caption = expression("Women" %<-% "Favored Gender" %->% "Men"))+
  my_theme_horiz+
  theme(legend.position = c(0.8,0.1))

#Former S7C. Difference in review recommendation by institution type----
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

#Figure_S7C <- rev_rec_inst %>%   
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

#S7C. difference in acceptance recommendation by reviewer gender----
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

break_list <- pretty(summ_inst$overperform, n = 7)

Figure_S7C <- summ_inst %>% 
  filter(review.recommendation == "Accept") %>% 
  ggplot(aes(x = fct_reorder(US.inst.type, desc(total_sub)), 
             fill = overperform,
             y = overperform))+
  geom_col(position = "dodge")+
  gen_gradient+
  coord_flip()+
  facet_wrap(~gen_rev_facet(reviewer.gender), ncol = 1, 
             scales = "free_y")+
  scale_y_continuous(breaks = break_list,
                     labels = abs(break_list))+
  labs(x = "\n", 
       y = "Difference in Acceptance
       Recommendation
       by Reviewer Gender", fill = "% Points\nDifference",
       caption = expression("Women" %<-% "Favored Gender" %->% "Men"))+
  my_theme_leg_horiz+
  theme(legend.position = "left")

#S7D. important features for editorial rejection----
Figure_S7D <- plot_feature_ranks(ranked_weights)+
  labs(y = "Importance Rank", color = "Direction\nof Impact",
       x = "\nFeature")

#generate & save figure----
Fig_S7AB <- plot_grid(Figure_S7A, Figure_S7B,
          labels = c('A', 'B'),
          label_size = 18,
          ncol = 2)

Fig_S7CD <- plot_grid(Figure_S7C, Figure_S7D,
                      labels = c('C', 'D'),
                      label_size = 18,
                      ncol = 2)

plot_grid(Fig_S7AB, Fig_S7CD,
          rel_heights = c(2, 1),
          nrow = 2)

ggsave("Figure_S7.tiff", device = 'tiff', units = "in", scale = 2,
       path = 'submission', width = 6.8, height = 8)
