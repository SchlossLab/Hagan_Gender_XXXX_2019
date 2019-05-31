rev_rec_data <- bias_data %>% 
  select(gender, journal, published, review.recommendation, 
         reviewer.gender, reviewer.random.id, random.manu.num, version.reviewed, 
         US.inst, US.inst.type) %>% distinct() %>% 
  filter(version.reviewed == 0) %>% 
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

summary_gen_rev %>% 
  ggplot(aes(x = review.recommendation, 
             y = overperformance, fill = overperformance))+
  geom_col(position = "dodge")+
  scale_fill_gradient2(low = "#D55E00", mid='snow3', 
                       high = "#0072B2", space = "Lab")+
  coord_flip()+
  facet_wrap(~reviewer.gender)+
  labs(x = "Review Recommendation", 
       y = "Disparity in Percent of Recommendations")+
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

summ_inst %>% 
  filter(review.recommendation == "Accept, no revision") %>% 
  ggplot(aes(x = US.inst.type, fill = overperform,
             y = overperform))+
  geom_col(position = "dodge")+
  scale_fill_gradient2(low = "#D55E00", mid='snow3', 
                       high = "#0072B2", space = "Lab")+
  coord_flip()+
  facet_wrap(~reviewer.gender)+
  labs(x = "US Institution Type", 
       y = "Acceptance Disparity by Reviewer Gender")+
  my_theme_horiz


