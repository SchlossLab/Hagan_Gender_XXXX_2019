#requires source("code/load_data.R")
#impact of variables on whether or not an article is published: editor gender, reviewer gender, first author gender, last author gender, corresponding author gender 

#other interesting variables not included here: # reviewers, # versions, days.pending, 
#setup----
genders <- c("male", "female")

gender_cols <- c("first.auth", "corres.auth", "last.auth")

editor_cols <- c("editor", "sen.editor")

#select data----
reg_data <- data %>% 
  select(role, published, doi, journal, num.versions, num.authors, contains("days"), 
         author.seq, author.corres, gender, reviewer.gender,  
         reviewer.random.id, grouped.random, random.manu.num, random.person.id, EJP.decision, 
         institution, US.inst) %>% 
  distinct() %>%
  mutate(corres.auth = if_else(author.corres == "TRUE", paste(gender), paste("NA")),
         editor = if_else(role == "editor", paste(gender), paste("NA")), #all ending up as NAs!!!
         sen.editor = if_else(role == "senior.editor", paste(gender), paste("NA"))
         ) %>% 
  select(-author.seq, -author.corres, -days.to.review) %>%
  distinct() 

corres_auth <- reg_data %>% 
  select(published, corres.auth, grouped.random, random.manu.num, US.inst) %>% 
  filter(corres.auth %in% genders) %>% distinct()
  
editor <- reg_data %>% 
  select(published, editor, grouped.random, random.manu.num) %>% 
  filter(editor %in% genders) %>% distinct() 

sen_editor <- reg_data %>% 
  select(published, sen.editor, grouped.random, random.manu.num) %>% 
  filter(sen.editor %in% genders) %>% distinct()

reviewers <- reg_data %>% 
  select(reviewer.gender, reviewer.random.id, grouped.random, random.manu.num) %>% 
  filter(!is.na(reviewer.random.id)) %>% distinct()

uniq.rev.manu <- reviewers %>% pull(random.manu.num) %>% unique()

men_rev_data <- map_dfr(uniq.rev.manu, function(x){
  
  summary <- reviewers %>% filter(random.manu.num == x) %>% 
    distinct() %>% summary()
  
  num_rev <- paste(c(summary[1,1], summary[2,1], summary[3,1]), collapse = "") %>% 
    str_extract_all(., "\\d") %>% unlist() %>% as.numeric(.)
  
  df <- reviewers %>% filter(random.manu.num == x) %>% 
    select(random.manu.num) %>% distinct() %>% 
    cbind(., prop.men.rev = num_rev[2]/sum(num_rev),
          num.rev = sum(num_rev))
  
  return(df)

})

auth_data <- reg_data %>% 
  filter(role == "author") %>% 
  select(random.manu.num, num.authors, random.person.id, gender)

uniq.manu <- auth_data %>% pull(random.manu.num) %>% unique()

author_ratio <- map_dfr(uniq.manu, function(x){
  auth_data %>% 
  filter(random.manu.num == x) %>% distinct() %>% 
  mutate(if.female = if_else(gender == "female", 1, 0),
         prop.fem.auth = sum(if.female)/num.authors) %>% 
  select(random.manu.num, prop.fem.auth, num.authors) %>% distinct()
})

ed_reject <- data %>% 
  filter(published == "no") %>% 
  filter(EJP.decision == "Reject" & is.na(days.to.review)) %>% 
  select(random.manu.num) %>% 
  mutate(editorial.reject = "yes")

rev_1_progress <- data %>% 
  filter(version.reviewed == 0) %>% 
  filter(version == 0) %>% 
  select(random.manu.num, EJP.decision) %>% 
  distinct() %>% 
  mutate(pass.first.review = if_else(EJP.decision == "Reject", "no", "yes")) %>% select(-EJP.decision)

na_val <- c("NA.female", "NA.male", "male.NA", "female.NA")
  
reg2_data <- full_join(corres_auth, editor, 
                       by = c("published", "random.manu.num", "grouped.random")) %>% 
  distinct() %>% 
  filter(corres.auth %in% c("male", "female")) %>% 
  full_join(., sen_editor, 
            by = c("published", "random.manu.num", "grouped.random")) %>% 
  distinct() %>% 
  left_join(., author_ratio, by = "random.manu.num") %>% distinct() %>% 
  left_join(., men_rev_data, by = "random.manu.num") %>% distinct() %>% 
  left_join(., ed_reject, by = "random.manu.num") %>% distinct() %>% 
  left_join(., rev_1_progress, by = "random.manu.num") %>% 
  distinct() %>% 
  mutate(sen.editor.x.edreject = paste0(sen.editor, ".", editorial.reject),
         editor.x.revprog = paste0(editor, ".", pass.first.review)) %>% 
  select(-random.manu.num, -grouped.random) %>% 
  mutate_all(as.factor)

write_csv(reg2_data, path = "data/gender_predict.csv")
