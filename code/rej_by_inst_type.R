#Does the institution of the corresponding author matter?

#cross ASM editorial rejection rate by insitution----
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

#editorial rejections by journal----
ASM_subs_j <- bias_data %>% 
  filter(US.inst == "yes") %>% 
  filter(!is.na(US.inst.type)) %>% 
  select(journal, gender, grouped.random, US.inst.type) %>% 
  distinct() %>% 
  group_by(journal, US.inst.type, gender) %>% summarise(total = n()) 

ed_rej_subs_j <- ed_rejs %>% 
  filter(US.inst == "yes") %>% 
  filter(!is.na(US.inst.type)) %>% 
  select(journal, gender, grouped.random, US.inst.type) %>% 
  distinct() %>% 
  group_by(journal, US.inst.type, gender) %>% 
  summarise(ed.rejections = n())

Supplementary_A <- left_join(ASM_subs_j, ed_rej_subs_j, 
          by = c("US.inst.type", "gender", "journal")) %>% 
  mutate(prop.ed.rej = get_percent(ed.rejections, total)) %>% 
  select(-total, -ed.rejections) %>% 
  spread(key = gender, value = prop.ed.rej) %>%
  mutate(performance = male - female) %>% 
  ggplot()+
  geom_col(aes(x = US.inst.type, y = performance, fill = performance))+
  coord_flip()+
  facet_wrap(~journal)+
  gen_gradient+
  labs(x = "Institution Type\n", 
       y = "\nDifference in Editorial Rejection\nby Author Gender (% Men - % Women)",
       fill = "Author\nGender")+
  my_theme_leg_horiz

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
  labs(x = "Institution Type\n", 
       y = "\nDifference by Author Gender\n(% Men - % Women)",
       fill = "Author\nGender")+
  my_theme_leg_horiz

#----  
acc_subs_j <- acc %>% 
  select(journal, gender, grouped.random, US.inst.type) %>% 
  distinct() %>% 
  group_by(journal, US.inst.type, gender) %>% 
  summarise(accepted = n())

Supplementary_B <- left_join(ASM_subs_j, acc_subs_j, 
          by = c("US.inst.type", "gender", "journal")) %>% 
  mutate(prop.accepted = get_percent(accepted, total)) %>% 
  select(-total, -accepted) %>% 
  spread(key = gender, value = prop.accepted) %>% 
  mutate(performance = male - female) %>% 
  ggplot()+
  geom_col(aes(x = journal, y = performance, fill = performance))+
  #scale_fill_manual(labels = gen_ed_labels, values = gen_ed_colors)+
  facet_wrap(~US.inst.type)+
  coord_flip()+
  gen_gradient+
  labs(x = "Journal", 
       y = "\nDifference by Author Gender\n(% Men - % Women)",
       fill = "Author\n Gender")+
  my_theme_leg_horiz
