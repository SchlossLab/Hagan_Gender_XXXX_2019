#Does reviewer acceptance depend on gender of editor/reviewer? -- need potential reviewer dataset
people <- people_data %>% select(-role, -contains("auth"), 
                                 -grouped.random, -random.manu.num) %>%
  distinct() %>% 
  mutate(gender = fct_explicit_na(gender, na_level = "none"))

editor_dat <- data %>% filter(role == "editor") %>% 
  mutate(year = year(submitted.date)) %>% 
  select(year, random.manu.num, gender, journal) %>% 
  filter(!is.na(year)) %>% 
  rename("editor.gender" = "gender") %>% 
  distinct()

authors <- people_data %>% filter(role == "editor") %>% 
  select(random.manu.num, gender) %>%
  rename("editor.gender" = "gender") %>% distinct() 

rev_resp <- read_csv("data/2018_rev_resp_ready.csv") %>% 
  left_join(., people, by = "random.person.id") %>% 
  rename("reviewer.gender" = "gender") %>% 
  left_join(., editor_dat, by = "random.manu.num") %>% 
  filter(!is.na(random.person.id)) %>% 
  filter(editor.gender != "none") %>% 
  filter(status != "Withdrawn") %>% 
  select(-suggested.include, -suggested.exclude, -title) %>% 
  mutate(status = fct_collapse(status, 
                               "No Response" = c("No Response", "Not Needed", "Contacted"),
                               "Not Contacted" = c("Not Contacted", "Not Used")))

#reviewer_D percent of reviewers contacted by editor gender----
ed_contact <- rev_resp %>% 
  mutate(status = fct_collapse(status, 
                               "Contacted" = c("No Response", "Not Needed", "Contacted", "Accepted", "Declined"),
                               "Not Contacted" = c("Not Contacted", "Not Used"))) %>% 
  group_by(editor.gender, reviewer.gender, status) %>%
  summarise(n = n()) %>% 
  spread(key = status, value = n) %>% 
  mutate(percent_cont = get_percent(Contacted, (Contacted + `Not Contacted`)))

reviewer_D <- ed_contact %>% 
  ggplot()+
  geom_col(aes(x = editor.gender, y = percent_cont,
               fill = reviewer.gender), alpha = 0.65,
           position = "dodge")+
  scale_fill_manual(values = gen_colors)+
  gen_x_replace+
  labs(x = "Editor Gender", y = "\nPercent Contacted")+
  my_theme_horiz

#reviewer_E response of reviewers by gender
f_ed_resp <- rev_resp %>% 
  filter(status != "Not Contacted") %>% 
  filter(editor.gender == "female") %>% 
  group_by(status, reviewer.gender) %>% 
  summarise(n = n()) %>% 
  spread(key = status, value = n) %>% 
  mutate(No_Resp = get_percent(`No Response`, (Accepted + `No Response` + Declined)),
         Accept = get_percent(Accepted, (Accepted + `No Response` + Declined))) %>% 
  mutate(editor.gender = "female") %>% 
  select(-Accepted, -`No Response`, -Declined) %>% 
  gather(No_Resp:Accept, value = Percent, key = Rev.Resp)

m_ed_resp <- rev_resp %>% 
  filter(status != "Not Contacted") %>% 
  filter(editor.gender == "male") %>% 
  group_by(status, reviewer.gender) %>% 
  summarise(n = n()) %>% 
  spread(key = status, value = n) %>% 
  mutate(No_Resp = get_percent(`No Response`, (Accepted + `No Response` + Declined)),
         Accept = get_percent(Accepted, (Accepted + `No Response` + Declined))) %>% 
  mutate(editor.gender = "male") %>% 
  select(-Accepted, -`No Response`, -Declined) %>% 
  gather(No_Resp:Accept, value = Percent, key = Rev.Resp)

ed_resp <- rbind(f_ed_resp, m_ed_resp)

reviewer_E <- ed_resp %>% 
  ggplot()+
  geom_col(aes(x = editor.gender, y = Percent,
               fill = reviewer.gender), 
           position = "dodge", alpha = 0.65)+
  facet_wrap(~ if_else(Rev.Resp == "No_Resp", "No Response", "Accept"))+
  scale_fill_manual(values = gen_colors, 
                    labels = gen_labels)+
  labs(x = "Editor Gender", y = "\nPercent of Reviewers",
       fill = "Reviewer Gender")+
  gen_x_replace+
  my_theme_leg_horiz+
  theme(legend.position = c(0.8, 0.8))

