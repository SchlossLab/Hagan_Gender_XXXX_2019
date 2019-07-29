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
       y = "\nDifference Following Review by Editor Gender")+
  my_theme_horiz

#editor recommendations by institution----

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
  labs(x = "US Institution Type", 
       y = "\nDifference in Acceptance by Editor Gender")+
  my_theme_horiz
