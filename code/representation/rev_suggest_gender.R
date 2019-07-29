#Does reviewer acceptance depend on gender of editor/reviewer? -- need potential reviewer dataset
people <- people_data %>% select(-role, -contains("auth"), 
                                 -grouped.random, -random.manu.num) %>%
  distinct() %>% 
  mutate(gender = fct_explicit_na(gender, na_level = "none"))

authors <- people_data %>% filter(role == "editor") %>% 
  select(random.manu.num, gender) %>%
  rename("editor.gender" = "gender") %>% distinct() 

rev_resp <- read_csv("data/2018_rev_resp_ready.csv") %>% 
  left_join(., people, by = "random.person.id") %>% 
  mutate(role = "reviewer") %>% 
  left_join(., editors, by = "random.manu.num") %>% 
  filter(!is.na(random.person.id)) %>% 
  filter(!is.na(editor.gender)) %>% 
  filter(status != "Withdrawn") %>% 
  mutate(status = fct_collapse(status, 
                               "No Response" = c("No Response", "Not Needed", "Contacted"),
                               "Not Contacted" = c("Not Contacted", "Not Used")))

rev_resp %>% 
  ggplot()+
  geom_bar(aes(x = gender, fill = gender), position = "dodge")+
  facet_wrap(status~editor.gender)

ggsave("resp_by_gender_raw.png")

f_ed_resp <- rev_resp %>% 
  filter(status != "Not Contacted") %>% 
  filter(editor.gender == "female") %>% 
  group_by(status, gender) %>% 
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
  group_by(status, gender) %>% 
  summarise(n = n()) %>% 
  spread(key = status, value = n) %>% 
  mutate(No_Resp = get_percent(`No Response`, (Accepted + `No Response` + Declined)),
         Accept = get_percent(Accepted, (Accepted + `No Response` + Declined))) %>% 
  mutate(editor.gender = "male") %>% 
  select(-Accepted, -`No Response`, -Declined) %>% 
  gather(No_Resp:Accept, value = Percent, key = Rev.Resp)

ed_resp <- rbind(f_ed_resp, m_ed_resp)

ed_resp %>% 
  ggplot()+
  geom_col(aes(x = editor.gender, y = Percent, fill = editor.gender), 
           position = "dodge")+
  facet_wrap(Rev.Resp~gender)+
  my_theme_horiz

#ggsave("acc_resp_rates_by_gender.png")