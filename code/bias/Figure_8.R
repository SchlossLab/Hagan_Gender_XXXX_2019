#Are papers authored by women ranked differently by reviewers?
rev_rec_data <- bias_data %>% 
  filter(version.reviewed == 0) %>% 
  filter(version == 0) %>% 
  select(gender, journal, published, review.recommendation, 
         reviewer.gender, 
         reviewer.random.id, random.manu.num, version.reviewed, 
         EJP.decision, doi, US.inst, US.inst.type) %>% distinct() %>% 
  filter(reviewer.gender != "none") %>% 
  filter(review.recommendation %in% c("Revise only", "Reject", "Accept, no revision")) %>% distinct()

gender_totals <- rev_rec_data %>% 
  select(random.manu.num, gender, review.recommendation) %>%
  distinct() %>% 
  group_by(gender) %>% 
  summarise(n = n())

#graphs of review scores by journal & gender----
reviewer_A <- rev_rec_data %>% 
  select(random.manu.num, gender, review.recommendation) %>%
  distinct() %>% 
  group_by(review.recommendation, gender) %>% 
  summarise(n = n()) %>% as_tibble() %>% 
  spread(key = review.recommendation, value = n) %>% 
  mutate_if(is.numeric, 
            funs(get_percent(., gender_totals$n))) %>%
  gather(`Accept, no revision`:`Revise only`,
         key = review.recommendation, value = percent) %>% 
  spread(key = gender, value = percent) %>%
  mutate(overperformance = male - female) %>% 
  ggplot(aes(x = review.recommendation, 
             y = overperformance, fill = overperformance))+
  geom_col(position = "dodge")+
  gen_gradient+
  coord_flip()+
  labs(x = "Review Recommendation", 
       y = "Difference in Review Recommendation\n")+
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
  labs(x = "\nInstitution Type", 
       y = "Difference in Review Recommendation\n")+
  my_theme_horiz

rev_rec_data <- bias_data %>% 
  filter(version.reviewed == 0) %>% 
  filter(version == 0) %>% 
  select(gender, journal, published, review.recommendation, 
         reviewer.gender, reviewer.random.id, random.manu.num, version.reviewed, 
         US.inst, US.inst.type) %>% distinct() %>% 
  filter(review.recommendation %in% c("Revise only", "Reject", "Accept, no revision")) %>%
  distinct()

rev_gen <- rev_rec_data %>% 
  select(random.manu.num, reviewer.gender, review.recommendation) %>%
  distinct() %>% 
  group_by(reviewer.gender) %>% 
  summarise(n = n())

sub_gen <- rev_rec_data %>% 
  select(random.manu.num, gender, review.recommendation) %>%
  distinct() %>% 
  group_by(gender) %>% 
  summarise(n = n())


#reviewer recommendations by gender----
fem_rev <- rev_rec_data %>% 
  select(random.manu.num, gender, review.recommendation, reviewer.gender) %>%
  distinct() %>% 
  group_by(reviewer.gender, review.recommendation, gender) %>% 
  summarise(n = n()) %>% as_tibble() %>% 
  spread(key = gender, value = n) %>%
  filter(reviewer.gender == "female") %>% 
  mutate(female = get_percent(female, sub_gen[1,2]),
         male = get_percent(male, sub_gen[2,2])) %>% 
  mutate(overperformance = male - female) 

men_rev <- rev_rec_data %>% 
  select(random.manu.num, gender, review.recommendation, reviewer.gender) %>%
  distinct() %>% 
  group_by(reviewer.gender, review.recommendation, gender) %>% 
  summarise(n = n()) %>% as_tibble() %>% 
  spread(key = gender, value = n) %>%
  filter(reviewer.gender == "male") %>% 
  mutate(female = get_percent(female, sub_gen[1,2]),
         male = get_percent(male, sub_gen[2,2])) %>% 
  mutate(overperformance = male - female) 

summary_gen_rev <- rbind(fem_rev, men_rev)

reviewer_C <- summary_gen_rev %>% 
  ggplot(aes(x = review.recommendation, 
             y = overperformance, fill = overperformance))+
  geom_col(position = "dodge")+
  gen_gradient+
  coord_flip()+
  facet_wrap(~gen_ed_facet(reviewer.gender), ncol = 1)+
  labs(x = "Review Recommendation", 
       y = "Difference by Reviewer Gender")+
  my_theme_horiz

#reviewer recommendations by institution----

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
  labs(x = "\nUS Institution Type", 
       y = "Difference in Acceptance Recomendation\nby Reviewer Gender")+
  my_theme_horiz

plot_grid(reviewer_A, reviewer_B, reviewer_C, reviewer_D, labels = c('A', 'B', 'C', 'D'), label_size = 18)

ggsave("Figure_8.png", device = 'png', 
       path = 'submission/', width = 15, height = 12)
