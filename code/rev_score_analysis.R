#Are papers authored by women ranked differently by reviewers?
rev_rec_data <- bias_data %>% 
  select(gender, journal, published, review.recommendation, 
         reviewer.gender, 
         reviewer.random.id, random.manu.num, version.reviewed, 
         EJP.decision, doi, US.inst, US.inst.type) %>% distinct() %>% 
  filter(version.reviewed == 0) %>% 
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
       y = "\nDifference by Author Gender\n(% Men - % Women)")+
  my_theme_horiz


#us vs not inst by review recommendation & gender----
#rev_rec_us <- bias_data %>% 
#  filter(!is.na(review.recommendation)) %>% 
#  filter(!is.na(US.inst)) %>% 
#  select(random.manu.num, US.inst, gender, 
#         review.recommendation) %>% 
#  filter(review.recommendation 
#         %in% c("Revise only", "Reject", 
#                "Accept, no revision")) %>% 
#  distinct()
#
#US_totals <- rev_rec_us %>% 
#  group_by(US.inst, gender) %>% 
#  summarise(n = n())
#
#rev_rec_us %>%
#  group_by(US.inst, review.recommendation, gender) %>% 
#  summarise(n = n()) %>% as_tibble() %>% 
#  spread(key = review.recommendation, value = n) %>% 
#  mutate_if(is.numeric, 
#            funs(get_percent(., US_totals$n))) %>% 
#  gather(`Accept, no revision`:`Revise only`,
#         key = review.recommendation, value = percent) %>% 
#  spread(key = gender, value = percent) %>% 
#  mutate(overperformance = male - female) %>%
#  ggplot(aes(x = US.inst, 
#             y = overperformance, fill = overperformance))+
#  geom_col(position = "dodge")+
#    facet_wrap(~review.recommendation)+
#  scale_fill_gradient2(low = "#D55E00", mid='snow3', 
#                       high = "#0072B2", space = "Lab")+
#  coord_flip()+
#  labs(x = "Review recommendation", y = "Percent disparity")+
#  my_theme_horiz

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
  labs(x = "Institution Type\n", 
       y = "\nDifference by Author Gender\n(% Men - % Women)")+
  my_theme_horiz
