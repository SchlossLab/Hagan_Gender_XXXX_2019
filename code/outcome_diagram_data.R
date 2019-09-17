outcomes <- data %>% 
  select(grouped.random, published, grouped.vers, num.authors, num.versions) %>% 
  distinct() 

manus <- outcomes %>% pull(grouped.random) %>% unique()

final_outcome <- map_df(manus, function(x){
  
  outcomes %>% filter(grouped.random == x) %>% 
    arrange(desc(grouped.vers)) %>% 
    head(n = 1) 
})

pub_summary <- final_outcome %>% group_by(published) %>% summarise(n = n())

vers_auth_summary <- final_outcome %>% 
  summarise(med.vers = median(num.versions), 
            med.auth = median(num.authors), 
            iqr.vers = IQR(num.versions), iqr.auth = IQR(num.authors))

first_descision <- data %>% 
  filter(grouped.vers == 1) %>% 
  select(grouped.random, EJP.decision, role, random.person.id, gender) %>% 
  filter(EJP.decision %in% c("Accept", "Revise", "Reject")) %>% 
  distinct()

genders <- first_descision %>% group_by(EJP.decision, gender) %>% 
  summarise(n = n())

decisions <- first_descision %>% 
  select(EJP.decision, grouped.random) %>% 
  distinct() %>% 
  group_by(EJP.decision) %>% summarise(n = n())
