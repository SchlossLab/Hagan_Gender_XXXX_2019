
corres_rej_data <- data %>% 
  filter(EJP.decision == "Accept, no revision" | EJP.decision == "Reject") %>% 
    filter(author.corres == TRUE) %>% 
    select(gender, EJP.decision, grouped.random, US.inst, US.inst.type, journal) %>% distinct() 

#cross ASM rejection rate
ASM_rej_rate <- corres_rej_data %>% 
  filter(US.inst == "yes") %>% 
  filter(!is.na(US.inst.type)) %>% 
  select(gender, EJP.decision, grouped.random, US.inst.type) %>% 
  distinct() %>% 
  group_by(US.inst.type, gender, EJP.decision) %>% summarise(n = n()) %>% 
  spread(key = EJP.decision, value = n) %>% 
  mutate(prop_rej = round((Reject/(Reject + `Accept, no revision`))*100, digits = 2)) 

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