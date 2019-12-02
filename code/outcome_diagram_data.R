outcomes <- data %>% 
  select(grouped.random, published, grouped.vers, num.authors, num.versions, review.start, EJP.decision) %>% 
  filter(EJP.decision %in% c("Accept", "Reject", "Revise")) %>% 
  filter(grouped.vers == 1) %>% 
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

#outcome of manuscripts after editor's first review
editor_descision <- data %>%  
  filter(grouped.vers == 1) %>% 
  filter(grouped.random %in% manus) %>% 
  filter(is.na(review.start)) %>% 
  select(grouped.random, EJP.decision, role, 
         random.person.id, gender) %>% 
  filter(EJP.decision %in% c("Accept", "Revise", "Reject")) %>% 
  distinct()

#first decision database
first_descision <- data %>%  
  filter(grouped.vers == 1) %>% 
  filter(grouped.random %in% manus) %>%
  filter(!is.na(review.start)) %>% 
  select(grouped.random, EJP.decision, role, 
         random.person.id, gender) %>% 
  filter(EJP.decision %in% c("Accept", "Revise", "Reject")) %>% 
  distinct()

#identify duplicate
editor_should <- anti_join(final_outcome, first_descision, 
                           by = "grouped.random") %>% 
  select(grouped.random) %>% distinct()

extra_ed <- anti_join(editor_descision, editor_should, 
                      by = "grouped.random") %>% 
  select(grouped.random) %>% 
  distinct() %>% pull()

#update editor decisions to remove duplicated manuscripts
editor_descision <- editor_descision %>% 
  filter(grouped.random %not_in% extra_ed)

#outcome of manuscripts after first review by editor
ed_des_genders <- editor_descision %>% 
  group_by(EJP.decision, gender) %>% 
  summarise(n = n())

ed_decisions <- editor_descision %>% 
  select(EJP.decision, grouped.random) %>% 
  distinct() %>% 
  group_by(EJP.decision) %>% summarise(n = n())

#outcome of manuscripts after first round of review
genders <- first_descision %>% 
  group_by(EJP.decision, gender) %>% 
  summarise(n = n())

decisions <- first_descision %>% 
  select(EJP.decision, grouped.random) %>% 
  distinct() %>% 
  group_by(EJP.decision) %>% summarise(n = n())
