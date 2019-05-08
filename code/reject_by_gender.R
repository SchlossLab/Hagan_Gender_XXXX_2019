#are papers authored by all gendered rejected at equivalent rates?

#setup----
acc_rej_data <- data %>% 
  filter(EJP.decision == "Accept, no revision" | EJP.decision == "Reject") %>% 
  mutate(gender.y = fct_explicit_na(gender.y, na_level = "none"))

auth_types <- c("first", "middle", "last", "corres")

#rejection rates by author type----
rej_by_auth <- map_df(auth_types, function(x){
  
  get_auth_type(x, acc_rej_data) %>% 
  filter(role.y == "author") %>% 
  select(gender.y, EJP.decision, grouped.random) %>% distinct() %>% 
  group_by(gender.y, EJP.decision) %>% summarise(n = n()) %>% 
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


rej_by_auth %>% 
  ggplot() + 
  geom_col(aes(x = gender.y, y = prop_rej, fill = gender.y)) +
  facet_wrap(~auth_type)+
  scale_fill_manual(values = gen_colors)+
  annotate(geom = "text", x = 1, y = (ASM_rej_rate[[3]]+3), label = "ASM rejection rate")+
  geom_hline(data = ASM_rej_rate, aes(yintercept = prop_rej))+
  labs(x = "Predicted Gender", y = "Percent of Manuscripts Rejected")+
  my_theme_horiz

ggsave("results/asm_rej_by_gender.jpg")

#rejection rates by gender and journal----
rej_by_journ <- map_df(auth_types, function(x){
  
  get_auth_type(x, acc_rej_data) %>% 
    filter(role.y == "author") %>% 
    select(journal, gender.y, EJP.decision, grouped.random) %>% distinct() %>% 
    group_by(journal, gender.y, EJP.decision) %>% summarise(n = n()) %>% 
    spread(key = EJP.decision, value = n) %>% 
    mutate(prop_rej = round((Reject/(Reject + `Accept, no revision`))*100, digits = 2)) %>% 
    mutate(., auth_type = x)
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

rej_by_journ %>% 
  ggplot() + 
  geom_col(aes(x = auth_type, y = prop_rej, fill = gender.y), 
           position = "dodge") +
  facet_wrap(~journal)+
  scale_fill_manual(values = gen_colors)+
  geom_hline(data = journ_rej_rates, aes(yintercept = prop_rej))+
  labs(x = "Predicted Gender", y = "Percent of Manuscripts Rejected")+
  my_theme_horiz

ggsave("results/asm_rej_by_journal.jpg")