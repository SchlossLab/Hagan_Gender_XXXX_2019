#are papers authored by all gendered rejected at equivalent rates?

#setup----
acc_rej_data <- data %>% 
  filter(EJP.decision == "Accept, no revision" | EJP.decision == "Reject") 

auth_types <- c("first", "middle", "last", "corres")

#rejection rates by author type----
rej_by_auth <- map_df(auth_types, function(x){
  
  get_auth_type(x, acc_rej_data) %>% 
  filter(role == "author") %>% 
  select(gender, EJP.decision, grouped.random) %>% distinct() %>% 
  group_by(gender, EJP.decision) %>% summarise(n = n()) %>% 
  spread(key = EJP.decision, value = n) %>% 
  mutate(prop_rej = round((Reject/(Reject + `Accept, no revision`))*100, digits = 2)) %>% 
    mutate(., auth_type = x)
})

#cross ASM rejection rate
ASM_rej_rate <- acc_rej_data %>% 
  select(EJP.decision, grouped.random) %>% distinct() %>% 
  group_by(EJP.decision) %>% 
  summarise(n = n()) %>% 
  spread(key = EJP.decision, value = n) %>%
  mutate(prop_rej = round((Reject/(Reject + `Accept, no revision`))*100, digits = 2)) 

#theme_set(theme_cowplot(font_size=12))
auth_type_A <- rej_by_auth %>% 
  ggplot() + 
  geom_col(aes(x = gender, y = prop_rej, fill = gender)) +
  facet_wrap(~auth_type)+
  scale_fill_manual(values = gen_colors)+
  scale_x_discrete(breaks = gen_levels,
                    labels = gen_labels)+
  annotate(geom = "text", x = 2, 
           y = (ASM_rej_rate[[3]]+3), label = "ASM rejection rate")+
  geom_hline(data = ASM_rej_rate, aes(yintercept = prop_rej))+
  labs(x = "\nPredicted Gender", y = "Percent of Manuscripts Rejected\n")+
  my_theme_horiz

#ggsave("results/asm_rej_by_gender.jpg")

#rejection rates by gender and journal----
rej_by_journ <- map_df(auth_types, function(x){
  
  journ_prop <- get_auth_type(x, acc_rej_data) %>% 
    filter(role == "author") %>% 
    filter(gender != "none") %>% 
    select(journal, gender, EJP.decision, grouped.random) %>% 
    distinct() %>% 
    group_by(journal, gender, EJP.decision) %>% 
    summarise(n = n()) %>% 
    spread(key = EJP.decision, value = n) %>% 
    mutate(prop_rej = round((Reject/(Reject + `Accept, no revision`))*100, digits = 2)) %>% 
    select(-Reject, -`Accept, no revision`) %>% 
    spread(key = gender, value = prop_rej) %>% 
    mutate(difference = male - female) %>% 
    mutate(., auth.type = x)
})

journals <- acc_rej_data %>% pull(journal) %>% unique()

journ_rej_rates <- map_df(journals, function(x){
  
  acc_rej_data %>% 
  filter(journal == x) %>% 
  select(EJP.decision, grouped.random) %>% distinct() %>% 
  group_by(EJP.decision) %>% 
  summarise(n = n()) %>% 
  spread(key = EJP.decision, value = n) %>%
  mutate(prop_rej = round((Reject/(Reject + `Accept, no revision`))*100, digits = 2)) %>% 
  mutate(., journal = x)
})

auth_type_B <- rej_by_journ %>% 
  ggplot() + 
  geom_col(aes(x = journal, y = difference, fill = difference)) +
  coord_flip()+
  facet_wrap(~auth.type)+
  gen_gradient+
  labs(x = "Journal", y = "Difference in Percent Rejection",
       fill = "% Points\nDifference")+
  my_theme_leg_horiz

#ggsave("results/asm_rej_by_journal.jpg")

corres_total <- bias_data %>% 
  select(grouped.random, EJP.decision, gender) %>% 
  filter(!is.na(EJP.decision)) %>% 
  distinct() %>% 
  group_by(gender) %>% summarise(n = n())

auth_type_C <- bias_data %>% 
  select(grouped.random, EJP.decision, gender) %>% 
  filter(!is.na(EJP.decision)) %>% 
  distinct() %>% 
  group_by(EJP.decision, gender) %>% 
  summarise(n = n()) %>% 
  spread(key = gender, value = n) %>% 
  mutate(men = get_percent(male, corres_total[2,2]),
         women = get_percent(female, corres_total[1,2])) %>% 
  select(-male, -female) %>% 
  mutate(performance = men - women) %>% 
  ggplot()+
  geom_col(aes(x = EJP.decision, y = performance, fill = performance))+
  coord_flip()+
  gen_gradient +
  labs(x = "Decision", y = "Difference in Submitted Manuscripts")+
  my_theme_horiz

#by journal
journ_corres_total <- bias_data %>% 
  select(grouped.random, journal, EJP.decision, gender) %>% 
  filter(!is.na(EJP.decision)) %>% 
  distinct() %>% 
  group_by(journal, gender) %>% summarise(n = n())
  
corres_rej_by_journ <- map_df(journals, function(x){
  
  corres_total <- journ_corres_total %>% 
    filter(journal == x)
  
  journ_data <- bias_data %>% 
    filter(journal == x) %>% 
    filter(!is.na(EJP.decision)) %>%
    select(grouped.random, gender, EJP.decision) %>% distinct() %>% 
    group_by(EJP.decision, gender) %>% summarise(n = n()) %>% 
    spread(key = gender, value = n) %>% 
    mutate(men = get_percent(male, corres_total[2,3]),
           women = get_percent(female, corres_total[1,3])) %>% 
    select(-female, -male) %>% 
    mutate(performance = men - women) %>% 
    mutate(., journal = x) 
  
  return(journ_data)
})

auth_type_D <- corres_rej_by_journ %>% 
  filter(EJP.decision != "Withdrawn") %>% 
  filter(EJP.decision != "Revise and re-review") %>% 
  ggplot()+
  geom_col(aes(x = EJP.decision, y = performance, fill = performance))+
  facet_wrap(~journal)+
  coord_flip()+
  gen_gradient+
  labs(x = "Decision", y = "Difference in Submitted Manuscripts")+
  my_theme_horiz

sm_val <- c("Withdrawn", "Revise and re-review")

auth_type_E <- corres_rej_by_journ %>%
  filter(EJP.decision %in% c("Withdrawn", "Revise and re-review")) %>% 
  ggplot()+
  geom_col(aes(x = EJP.decision, y = performance, fill = performance))+
  facet_wrap(~journal)+
  coord_flip()+
  gen_gradient+
  labs(x = "Gender", y = "Submitted Manuscripts")+
  my_theme_leg
