##How does the gatekeeper gender affect decisions and review reccommendations

####Editors----

#Get editors and their genders
ed_genders <- data %>% 
  filter(role == "editor") %>% 
  select(grouped.random, gender) %>% 
  distinct() %>% 
  rename("editor.gender" = "gender")

#editor decision data
ed_dec_data <- bias_data %>% 
  filter(version.reviewed == 0) %>% 
  filter(grouped.vers == 1) %>% 
  select(gender, journal, grouped.random, EJP.decision) %>% 
  filter(EJP.decision %in% c("Accept",
                             "Reject", "Revise")) %>% 
  left_join(., ed_genders, by = "grouped.random") %>% 
  filter(editor.gender %in% c("female", "male")) %>% 
  distinct()

#how many of each decision was made by each editor gender
ed_gen <- ed_dec_data %>% 
  select(grouped.random, editor.gender, 
         EJP.decision) %>%
  distinct() %>% 
  group_by(editor.gender) %>% 
  summarise(n = n())

#how did those decisions break down according to author gender
fem_ed_sub_gen <- ed_dec_data %>% 
  filter (editor.gender == "female") %>% 
  select(grouped.random, gender, EJP.decision) %>%
  distinct() %>% 
  group_by(gender) %>% 
  summarise(n = n())

men_edsub_gen <- ed_dec_data %>% 
  filter (editor.gender == "male") %>% 
  select(grouped.random, gender, EJP.decision) %>%
  distinct() %>% 
  group_by(gender) %>% 
  summarise(n = n())

#calculate women editor recommendations by author gender---
fem_ed <- ed_dec_data %>% 
  select(grouped.random, gender, EJP.decision, 
         editor.gender) %>%
  distinct() %>% 
  group_by(editor.gender, EJP.decision, gender) %>% 
  summarise(n = n()) %>% as_tibble() %>% 
  spread(key = gender, value = n) %>%
  filter(editor.gender == "female") %>% 
  mutate(female = get_percent(female, fem_ed_sub_gen[1,2]),
         male = get_percent(male, fem_ed_sub_gen[2,2])) %>% 
  mutate(overperformance = male - female) 

#calculate men editor reccomendations by author gender
men_ed <- ed_dec_data %>% 
  select(grouped.random, gender, EJP.decision, 
         editor.gender) %>%
  distinct() %>% 
  group_by(editor.gender, EJP.decision, gender) %>% 
  summarise(n = n()) %>% as_tibble() %>% 
  spread(key = gender, value = n) %>%
  filter(editor.gender == "male") %>% 
  mutate(female = get_percent(female, men_edsub_gen[1,2]),
         male = get_percent(male, men_edsub_gen[2,2])) %>% 
  mutate(overperformance = male - female) 

summary_gen_ed <- rbind(fem_ed, men_ed)

#plot differences in editor decisions by gender (6A)
ed_rejections_B <- summary_gen_ed %>% 
  ggplot(aes(x = EJP.decision, 
             y = overperformance, fill = overperformance))+
  geom_col(position = "dodge")+
  gen_gradient+
  coord_flip()+
  facet_wrap(~gen_ed_facet(editor.gender), ncol = 1)+
  labs(x = "\n", 
       y = "Difference Following Review by Editor Gender\n",
       fill = "Percentage Point\nDifference")+
  my_theme_leg_horiz

####Reviewers----

#reviewer data
rev_rec_data <- bias_data %>% 
  filter(version.reviewed == 0) %>% 
  filter(grouped.vers == 1) %>% 
  select(gender, journal, review.recommendation, 
         reviewer.gender, reviewer.random.id, random.manu.num) %>% distinct() %>% 
  filter(review.recommendation %in% c("Revise", "Reject", "Accept")) %>%
  distinct()

#how many reviewers of each gender
rev_gen <- rev_rec_data %>% 
  select(random.manu.num, reviewer.gender, review.recommendation) %>%
  distinct() %>% 
  group_by(reviewer.gender) %>% 
  summarise(n = n())

#number of manuscripts by author gender that were reviewed by women
fem_rev_sub_gen <- rev_rec_data %>% 
  filter(reviewer.gender == "female") %>% 
  select(random.manu.num, gender, review.recommendation) %>%
  distinct() %>% 
  group_by(gender) %>% 
  summarise(n = n())

men_rev_sub_gen <- rev_rec_data %>% 
  filter(reviewer.gender == "male") %>% 
  select(random.manu.num, gender, review.recommendation) %>%
  distinct() %>% 
  group_by(gender) %>% 
  summarise(n = n())

#women reviewer suggestions by author gender
fem_rev <- rev_rec_data %>% 
  select(random.manu.num, gender, review.recommendation, reviewer.gender) %>%
  distinct() %>% 
  group_by(reviewer.gender, review.recommendation, gender) %>% 
  summarise(n = n()) %>% as_tibble() %>% 
  spread(key = gender, value = n) %>%
  filter(reviewer.gender == "female") %>% 
  mutate(female = get_percent(female, fem_rev_sub_gen[1,2]),
         male = get_percent(male, fem_rev_sub_gen[2,2])) %>% 
  mutate(overperformance = male - female) 

#men reviewer suggestions by author gender
men_rev <- rev_rec_data %>% 
  select(random.manu.num, gender, review.recommendation, reviewer.gender) %>%
  distinct() %>% 
  group_by(reviewer.gender, review.recommendation, gender) %>% 
  summarise(n = n()) %>% as_tibble() %>% 
  spread(key = gender, value = n) %>%
  filter(reviewer.gender == "male") %>% 
  mutate(female = get_percent(female, men_rev_sub_gen[1,2]),
         male = get_percent(male, men_rev_sub_gen[2,2])) %>% 
  mutate(overperformance = male - female) 

summary_gen_rev <- rbind(fem_rev, men_rev)

#Plot reviewer recomendation outcomes by reviewer gender
reviewer_C <- summary_gen_rev %>% 
  ggplot(aes(x = review.recommendation, 
             y = overperformance, fill = overperformance))+
  geom_col(position = "dodge")+
  gen_gradient+
  coord_flip()+
  facet_wrap(~gen_rev_facet(reviewer.gender), ncol = 1)+
  labs(x = "\n", 
       y = "Difference in Review\nRecommendation")+
  my_theme_horiz

#Are papers authored by women ranked differently by reviewers?
gender_totals <- rev_rec_data %>% 
  select(random.manu.num, gender, review.recommendation) %>%
  distinct() %>% 
  group_by(gender) %>% 
  summarise(n = n())

#graphs of review scores by gender----
reviewer_A_data <- rev_rec_data %>% 
  select(random.manu.num, gender, review.recommendation) %>%
  distinct() %>% 
  group_by(review.recommendation, gender) %>% 
  summarise(n = n()) %>% as_tibble() %>% 
  spread(key = review.recommendation, value = n) %>% 
  mutate_if(is.numeric, 
            funs(get_percent(., gender_totals$n))) %>% 
  gather(Reject:Accept,
         key = review.recommendation, value = percent) %>% 
  spread(key = gender, value = percent) %>%
  mutate(overperformance = male - female) 

reviewer_A_data$review.recommendation <- fct_relevel(reviewer_A_data$review.recommendation, decisions)

#plot impact of author gender on review suggestions (B)
reviewer_A <- reviewer_A_data %>% 
  ggplot(aes(x = review.recommendation, 
             y = overperformance, fill = overperformance))+
  geom_col(position = "dodge")+
  gen_gradient+
  coord_flip()+
  labs(x = "\n", 
       y = "Difference in Review Recommendation")+
  my_theme_horiz

blank <- ggplot()

plot_A <- plot_grid(ed_rejections_B, blank, labels = c('A'), 
                    label_size = 18, rel_widths = c(2,1))

plot_BC <- plot_grid(reviewer_A, reviewer_C, labels = c('B', 'C'), label_size = 18)

plot_grid(plot_A, plot_BC, nrow = 2)

ggsave("Figure_5.png", device = 'png', 
       path = 'submission', width = 12, height = 7)
