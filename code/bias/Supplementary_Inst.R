ed_genders <- data %>% 
  filter(role == "editor") %>% 
  select(grouped.random, gender) %>% 
  distinct() %>% 
  rename("editor.gender" = "gender")

ed_dec_data <- bias_data %>% 
  filter(version.reviewed == 0) %>% 
  filter(version == 0) %>% 
  select(gender, journal, grouped.random, 
         US.inst, US.inst.type, EJP.decision) %>% 
  filter(EJP.decision %in% c("Accept, no revision",
                             "Reject", "Revise only")) %>% 
  left_join(., ed_genders, by = "grouped.random") %>% 
  filter(editor.gender %in% c("female", "male")) %>% 
  distinct()

sub_inst_gen <- ed_dec_data %>% 
  filter(US.inst == "yes") %>% 
  filter(!is.na(US.inst.type)) %>% 
  filter(EJP.decision != "Revise only") %>% 
  select(grouped.random, US.inst.type, editor.gender,
         EJP.decision, gender) %>% 
  distinct() %>% 
  group_by(editor.gender, US.inst.type, gender) %>% 
  summarise(total = n())

summ_inst <- ed_dec_data %>% 
  filter(US.inst == "yes") %>% 
  filter(!is.na(US.inst.type)) %>% 
  filter(EJP.decision != "Revise only") %>% 
  select(grouped.random, editor.gender, US.inst.type, 
         EJP.decision, gender) %>% 
  distinct() %>% 
  group_by(editor.gender, US.inst.type, gender, 
           EJP.decision) %>% 
  summarise(n = n()) %>% as_tibble() %>% 
  left_join(., sub_inst_gen, by = c("editor.gender",
                                    "US.inst.type", 
                                    "gender")) %>% 
  mutate(percent = get_percent(n, total)) %>% 
  select(-n, -total) %>% 
  spread(key = gender, value = percent) %>% 
  mutate(overperform = male - female)

ed_rejections_D <- summ_inst %>% 
  filter(EJP.decision == "Accept, no revision") %>% 
  ggplot(aes(x = US.inst.type, fill = overperform,
             y = overperform))+
  geom_col(position = "dodge")+
  gen_gradient+
  coord_flip()+
  facet_wrap(~gen_ed_facet(editor.gender), 
             ncol = 1)+
  labs(x = "\nInstitution Type", 
       y = "Difference in Acceptance by Editor Gender\n")+
  my_theme_horiz

ed_rejs <- bias_data %>% 
  filter(published == "no") %>% 
  filter(EJP.decision == "Reject" & is.na(days.to.review)) %>% 
  select(-days.to.review, -contains("version")) %>% 
  distinct() 

ASM_subs <- bias_data %>% 
  filter(US.inst == "yes") %>% 
  filter(!is.na(US.inst.type)) %>% 
  select(gender, grouped.random, US.inst.type) %>% 
  distinct() %>% 
  group_by(US.inst.type, gender) %>% summarise(total = n()) 

ed_rej_subs <- ed_rejs %>% 
  filter(US.inst == "yes") %>% 
  filter(!is.na(US.inst.type)) %>% 
  select(gender, grouped.random, US.inst.type) %>% 
  distinct() %>% 
  group_by(US.inst.type, gender) %>% summarise(ed.rejections = n())

editorial_rej_per <- left_join(ASM_subs, ed_rej_subs, 
                               by = c("US.inst.type", "gender")) %>% 
  mutate(prop.ed.rej = get_percent(ed.rejections, total)) %>% 
  select(-total, -ed.rejections) %>% 
  spread(key = gender, value = prop.ed.rej) %>% 
  mutate(performance = male - female,
         rate = "Editorial Rejection")

#accepted----
acc <- bias_data %>% 
  filter(EJP.decision == "Accept, no revision") %>% 
  filter(US.inst == "yes") %>% 
  filter(!is.na(US.inst.type)) %>%
  select(-days.to.review, contains("version")) %>% 
  distinct()

acc_subs <- acc %>% 
  select(gender, grouped.random, US.inst.type) %>% 
  distinct() %>% 
  group_by(US.inst.type, gender) %>% summarise(accepted = n())

ed_rejections_C <- left_join(ASM_subs, acc_subs, 
                             by = c("US.inst.type", "gender")) %>% 
  mutate(prop.accepted = get_percent(accepted, total)) %>% 
  select(-total, -accepted) %>% 
  spread(key = gender, value = prop.accepted) %>% 
  mutate(performance = male - female,
         rate = "Acceptance") %>% 
  rbind(editorial_rej_per) %>% 
  ggplot()+
  geom_col(aes(x = US.inst.type, 
               y = performance, fill = performance))+
  facet_wrap(~rate, ncol = 1)+
  coord_flip()+
  gen_gradient+
  labs(x = "Institution Type", 
       y = "Difference\n")+
  my_theme_horiz

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

reviewer_D <- summ_inst %>% 
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

#inst type in US----  
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

reviewer_B <- rev_rec_inst %>%   
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
  labs(x = "Institution Type", 
       y = "Difference in Review Recommendation")+
  my_theme_horiz

source("code/bias/supp_inst_AB.R")

plot_grid(ed_rejections_C, Supplementary_A, 
          Supplementary_B,
          ed_rejections_D,
          reviewer_B, reviewer_D,
          labels = c('A', 'B', 'C', 'D', 'E', 'F'),
          label_size = 18,
          nrow = 3)


ggsave("Supp_inst.png", device = 'png', 
       path = 'submission/', width = 15, height = 15)
