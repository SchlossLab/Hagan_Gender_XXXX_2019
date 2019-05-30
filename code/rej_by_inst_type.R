
ed_rejs <- bias_data %>% 
  filter(published == "no") %>% 
  filter(EJP.decision == "Reject" & is.na(days.to.review)) %>% 
  select(-days.to.review, -contains("version")) %>% 
  distinct() 

#cross ASM editorial rejection rate by insitution----
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

left_join(ASM_subs, ed_rej_subs, 
                         by = c("US.inst.type", "gender")) %>% 
  mutate(prop.ed.rej = get_percent(ed.rejections, total)) %>% 
  ggplot()+
  geom_col(aes(x = gender, y = prop.ed.rej, fill = gender))+
  scale_fill_manual(labels = gen_ed_labels, values = gen_ed_colors)+
  facet_wrap(~US.inst.type, scales = "free_y")+
  labs(x = "Gender", y = "Percent Editorial Rejections")+
  my_theme_horiz

#editorial rejections by journal
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

left_join(ASM_subs_j, ed_rej_subs_j, 
          by = c("US.inst.type", "gender", "journal")) %>% 
  mutate(prop.ed.rej = get_percent(ed.rejections, total)) %>% 
  ggplot()+
  geom_col(aes(x = journal, y = prop.ed.rej, fill = gender), 
           position = "dodge")+
  scale_fill_manual(labels = gen_ed_labels, values = gen_ed_colors)+
  facet_wrap(~US.inst.type, scales = "free_y")+
  coord_flip()+
  labs(x = "Journal", y = "Percent Editorial Rejections",
       fill = "Gender")+
  my_theme_leg_horiz

#by rejection following review
rev_rejs <- bias_data %>% 
  filter(published == "no") %>% 
  filter(EJP.decision == "Reject") %>% 
  filter(US.inst == "yes") %>% 
  filter(!is.na(US.inst.type)) %>%
  filter(!is.na(days.to.review)) %>% #eliminate editorial rejections
  select(-days.to.review, contains("version")) %>% 
  distinct()

rev_rej_subs <- rev_rejs %>% 
  filter(US.inst == "yes") %>% 
  filter(!is.na(US.inst.type)) %>% 
  select(gender, grouped.random, US.inst.type) %>% 
  distinct() %>% 
  group_by(US.inst.type, gender) %>% summarise(rev.rejections = n())

left_join(ASM_subs, rev_rej_subs, 
          by = c("US.inst.type", "gender")) %>% 
  mutate(prop.rev.rej = get_percent(rev.rejections, total)) %>% 
  ggplot()+
  geom_col(aes(x = gender, y = prop.rev.rej, fill = gender))+
  scale_fill_manual(labels = gen_ed_labels, values = gen_ed_colors)+
  facet_wrap(~US.inst.type, scales = "free_y")+
  labs(x = "Gender", y = "Percent Rejections after Review")+
  my_theme_horiz

rev_rej_subs_j <- rev_rejs %>% 
  filter(US.inst == "yes") %>% 
  filter(!is.na(US.inst.type)) %>% 
  select(journal, gender, grouped.random, US.inst.type) %>% 
  distinct() %>% 
  group_by(journal, US.inst.type, gender) %>% 
  summarise(rev.rejections = n())

left_join(ASM_subs_j, rev_rej_subs_j, 
          by = c("US.inst.type", "gender", "journal")) %>% 
  mutate(prop.rev.rej = get_percent(rev.rejections, total)) %>% 
  ggplot()+
  geom_col(aes(x = journal, y = prop.rev.rej, fill = gender), 
           position = "dodge")+
  scale_fill_manual(labels = gen_ed_labels, values = gen_ed_colors)+
  facet_wrap(~US.inst.type, scales = "free_y")+
  coord_flip()+
  labs(x = "Journal", y = "Percent Rejections after Review",
       fill = "Gender")+
  my_theme_leg_horiz


#accepted
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

left_join(ASM_subs, acc_subs, 
          by = c("US.inst.type", "gender")) %>% 
  mutate(prop.accepted = get_percent(accepted, total)) %>% 
  ggplot()+
  geom_col(aes(x = gender, y = prop.accepted, fill = gender))+
  scale_fill_manual(labels = gen_ed_labels, values = gen_ed_colors)+
  facet_wrap(~US.inst.type, scales = "free_y")+
  labs(x = "Gender", y = "Percent Accepted")+
  my_theme_horiz

acc_subs_j <- acc %>% 
  select(journal, gender, grouped.random, US.inst.type) %>% 
  distinct() %>% 
  group_by(journal, US.inst.type, gender) %>% 
  summarise(accepted = n())

left_join(ASM_subs_j, acc_subs_j, 
          by = c("US.inst.type", "gender", "journal")) %>% 
  mutate(prop.accepted = get_percent(accepted, total)) %>% 
  ggplot()+
  geom_col(aes(x = journal, y = prop.accepted, fill = gender), 
           position = "dodge")+
  scale_fill_manual(labels = gen_ed_labels, values = gen_ed_colors)+
  facet_wrap(~US.inst.type, scales = "free_y")+
  coord_flip()+
  labs(x = "Journal", y = "Percent Accepted",
       fill = "Gender")+
  my_theme_leg_horiz

#calc overperformance

ASM_rej_rate %>% 
  ggplot()+
  geom_col(aes(x = US.inst.type, y = prop_rej))+
  facet_wrap(~gender)+
  coord_flip()

ggsave("rej_by_gender_inst.png")

journals <- data %>% pull(journal) %>% unique()

journ_rej_rates <- map_df(journals, function(x){
  
  corres_rej_data %>% 
    filter(journal == x) %>% 
    filter(US.inst == "yes") %>% 
    filter(!is.na(US.inst.type)) %>% 
    select(gender, EJP.decision, grouped.random, US.inst.type) %>%
    distinct() %>% 
    group_by(US.inst.type, gender, EJP.decision) %>% 
    summarise(n = n()) %>% 
    spread(key = EJP.decision, value = n) %>%
    mutate(prop_rej = round((Reject/(Reject + `Accept, no revision`))*100, digits = 2)) %>% 
    mutate(., journal = x)
})

journ_rej_rates %>% 
  ggplot()+
  geom_col(aes(x = US.inst.type, y = prop_rej, fill = gender), position = "dodge")+
  facet_wrap(~journal)+
  coord_flip()

ggsave("rej_by_journ_inst.png")