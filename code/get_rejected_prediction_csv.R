#requires source("code/load_data.R")
#impact of variables on whether or not an article is published: editor gender, reviewer gender, first author gender, last author gender, corresponding author gender, US.instxgender, inst.typexgender

#other interesting variables not included here: # reviewers, # versions, days.pending, 
#setup----
genders <- c("male", "female")

gender_cols <- c("first.auth", "corres.auth", "last.auth")

editor_cols <- c("editor", "sen.editor")

#select data----
reg_data <- data %>% 
  filter(grouped.vers == 1) %>% 
  select(role, journal, num.versions, num.authors, 
         author.seq, author.corres, gender, days.to.review,
         reviewer.gender, reviewer.random.id,  
         random.manu.num, random.person.id, EJP.decision, 
         US.inst, US.inst.type) %>% 
  distinct() %>%
  mutate(corres.auth = if_else(author.corres == "TRUE", 
                               paste(gender), paste("NA")),
         editor = if_else(role == "editor", 
                          paste(gender), paste("NA")), #all ending up as NAs!!!
         sen.editor = if_else(role == "senior.editor", 
                              paste(gender), paste("NA"))
         ) %>% 
  select(-author.seq, -author.corres) %>%
  distinct() 

#restrict data to authors/reviewers w. predicted gender & clean up na's----
corres_auth <- reg_data %>% 
  select(corres.auth, random.manu.num, 
         US.inst, US.inst.type, journal) %>% 
  filter(corres.auth %in% genders) %>% distinct()
  
editor <- reg_data %>% 
  select(editor, random.manu.num) %>% 
  filter(editor %in% genders) %>% distinct() 

sen_editor <- reg_data %>% 
  select(sen.editor, random.manu.num) %>% 
  filter(sen.editor %in% genders) %>% distinct()

reviewers <- reg_data %>% 
  select(reviewer.gender, reviewer.random.id, random.manu.num) %>% 
  filter(!is.na(reviewer.random.id)) %>% distinct()

#list of reviewed manuscripts--- 
#uniq.rev.manu <- reviewers %>% pull(random.manu.num) %>% unique()
#
##calculate the proportion of reviewers predicted to be men by looping through each manuscript (includes #unknown in total number of reviewers)----
#men_rev_data <- map_dfr(uniq.rev.manu, function(x){
#  
#  summary <- reviewers %>% filter(random.manu.num == x) %>% 
#    distinct() %>% summary()#restrict to single manuscript & get reviewer count
#  
#  num_rev <- paste(c(summary[1,1], summary[2,1], 
#                     summary[3,1]), collapse = "") %>% #pull reviewer count from summary
#    str_extract_all(., "\\d") %>% #get values (drop gender)
#    unlist() %>% as.numeric(.) #collapse to single vector
#  
#  df <- reviewers %>% filter(random.manu.num == x) %>% #restrict to single manu
#    select(random.manu.num) %>% distinct() %>% #drop to single entry
#    cbind(., prop.men.rev = num_rev[2]/sum(num_rev), #calculate proportion of reviewers that were men
#          num.rev = sum(num_rev))#calculate total number of reviewers
#  
#  return(df)
#
#})

#dataset of author genders
auth_data <- reg_data %>% 
  filter(role == "author") %>% 
  select(random.manu.num, num.authors, random.person.id, gender)

#manuscripts in the dataset---
uniq.manu <- auth_data %>% pull(random.manu.num) %>% unique()

#loop through each manuscript to calculate the proportion of authors that are women on each paper----
author_ratio <- map_dfr(uniq.manu, function(x){
  auth_data %>% 
    filter(random.manu.num == x) %>% distinct() %>% #restrict to one manu
    mutate(if.female = if_else(gender == "female", 1, 0), #convert gender to binary for easy math
           prop.fem.auth = sum(if.female)/num.authors) %>% #calculate prop of women authors
    select(random.manu.num, prop.fem.auth, num.authors) %>% distinct()
})

#identify final status of manuscript first submissions----
ed_reject <- reg_data %>% 
  filter(EJP.decision == "Reject" & is.na(days.to.review)) %>% 
  select(random.manu.num) %>% 
  mutate(editorial.reject = "yes")

#rev_1_progress <- reg_data %>% 
#  filter(!is.na(days.to.review)) %>% 
#  select(random.manu.num, EJP.decision) %>% 
#  distinct() %>% 
#  mutate(pass.first.review = 
#           if_else(EJP.decision == "Reject", "no", "yes")) %>% 
#  select(-EJP.decision)

#join final dataset----
reg2_data <- full_join(corres_auth, editor, 
                       by = "random.manu.num") %>% 
  distinct() %>% 
  filter(corres.auth %in% c("male", "female")) %>% 
  full_join(., sen_editor, 
            by = "random.manu.num") %>% 
  left_join(., author_ratio, by = "random.manu.num") %>% 
  #left_join(., men_rev_data, by = "random.manu.num") %>% 
  left_join(., ed_reject, by = "random.manu.num") %>%  
  #left_join(., rev_1_progress, by = "random.manu.num") %>% 
  distinct() %>% 
  mutate(#reviewed = if_else(is.na(num.rev), 0, 1),
         inst.gender = paste0(US.inst.type, ".", corres.auth),
         US.gender = paste0(US.inst, ".", corres.auth),
         editorial.reject = replace_na(editorial.reject, "no"),
         sen.editor.x.edreject = paste0(sen.editor, ".", editorial.reject),
         #editor.x.revprog = paste0(editor, ".", pass.first.review),
         edreject.gender = paste0(editorial.reject, ".", corres.auth)
         #revprog.gender = paste0(pass.first.review, ".", corres.auth)
         ) %>% 
  select(-random.manu.num) %>% 
  mutate_all(as.factor)

write_csv(reg2_data, path = "data/reject_predict.csv")
