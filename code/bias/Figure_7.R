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
       y = "Difference in Acceptance by Editor Gender")+
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
  labs(x = "\nInstitution Type", 
       y = "Difference")+
  my_theme_horiz

plot_grid(ed_rejections_B, ed_rejections_C, ed_rejections_D, labels = c('A', 'B', 'C'), label_size = 18)

ggsave("Figure_7.png", device = 'png', 
       path = 'submission/', width = 9, height = 6)
