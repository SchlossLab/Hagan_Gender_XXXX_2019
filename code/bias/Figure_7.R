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

ed_gen <- ed_dec_data %>% 
  select(grouped.random, editor.gender, 
         EJP.decision) %>%
  distinct() %>% 
  group_by(editor.gender) %>% 
  summarise(n = n())

sub_gen <- ed_dec_data %>% 
  select(grouped.random, gender, EJP.decision) %>%
  distinct() %>% 
  group_by(gender) %>% 
  summarise(n = n())


#editor recommendations by gender----
fem_ed <- ed_dec_data %>% 
  select(grouped.random, gender, EJP.decision, 
         editor.gender) %>%
  distinct() %>% 
  group_by(editor.gender, EJP.decision, gender) %>% 
  summarise(n = n()) %>% as_tibble() %>% 
  spread(key = gender, value = n) %>%
  filter(editor.gender == "female") %>% 
  mutate(female = get_percent(female, sub_gen[1,2]),
         male = get_percent(male, sub_gen[2,2])) %>% 
  mutate(overperformance = male - female) 

men_ed <- ed_dec_data %>% 
  select(grouped.random, gender, EJP.decision, 
         editor.gender) %>%
  distinct() %>% 
  group_by(editor.gender, EJP.decision, gender) %>% 
  summarise(n = n()) %>% as_tibble() %>% 
  spread(key = gender, value = n) %>%
  filter(editor.gender == "male") %>% 
  mutate(female = get_percent(female, sub_gen[1,2]),
         male = get_percent(male, sub_gen[2,2])) %>% 
  mutate(overperformance = male - female) 

summary_gen_ed <- rbind(fem_ed, men_ed)

ed_rejections_B <- summary_gen_ed %>% 
  ggplot(aes(x = EJP.decision, 
             y = overperformance, fill = overperformance))+
  geom_col(position = "dodge")+
  gen_gradient+
  coord_flip()+
  facet_wrap(~gen_ed_facet(editor.gender), ncol = 1)+
  labs(x = "Decision", 
       y = "Difference Following Review by Editor Gender\n")+
  my_theme_horiz

#reviewer recommendations by gender----
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
  labs(x = "\nReview Recommendation", 
       y = "Difference by Reviewer Gender")+
  my_theme_horiz


#Are papers authored by women ranked differently by reviewers?
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
  labs(x = "\nReview Recommendation", 
       y = "Difference in Review Recommendation")+
  my_theme_horiz

blank <- ggplot()

plot_A <- plot_grid(ed_rejections_B, blank, labels = c('A'), 
                    label_size = 18)

plot_BC <- plot_grid(reviewer_A, reviewer_C, labels = c('B', 'C'), label_size = 18)

plot_grid(plot_A, plot_BC, nrow = 2)

ggsave("Figure_7.png", device = 'png', 
       path = 'submission/', width = 12, height = 6)
