
corres_rej_data <- data %>% 
  filter(EJP.decision == "Accept, no revision" | EJP.decision == "Reject") %>% 
    filter(author.corres == TRUE) %>% 
    select(gender, EJP.decision, grouped.random, US.inst, US.inst.type) %>% distinct() 

#cross ASM rejection rate
ASM_rej_rate <- corres_rej_data %>% 
  select(gender, EJP.decision, grouped.random, US.inst) %>% distinct() %>% 
  group_by(US.inst, gender, EJP.decision) %>% summarise(n = n()) %>% 
  spread(key = EJP.decision, value = n) %>% 
  mutate(prop_rej = round((Reject/(Reject + `Accept, no revision`))*100, digits = 2)) %>% 
  filter(!is.na(US.inst)) 

journals <- data %>% pull(journal) %>% unique()

journ_rej_rates <- map_df(journals, function(x){
  
  corres_rej_data %>% 
    filter(journal == x) %>% 
    select(EJP.decision, grouped.random) %>% distinct() %>% 
    group_by(EJP.decision) %>% 
    summarise(n = n()) %>% 
    spread(key = EJP.decision, value = n) %>%
    mutate(prop_rej = round((Reject/(Reject + `Accept, no revision`))*100, digits = 2)) %>% 
    mutate(., journal = x)
})
