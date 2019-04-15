#impact of variables on whether or not an article is published: editor gender, reviewer gender, first author gender, last author gender, corresponding author gender 

#other interesting variables not included here: # reviewers, # versions, days.pending, 

genders <- c("0", "1", "2")

reg_data <- data %>% 
  select(role.y, published, author.seq, author.corres, author.last, gender.y, reviewer.gender, reviewer.random.id, random.manu.num, random.person.id.y) %>% 
  distinct() %>% 
  mutate(gender.y = fct_explicit_na(gender.y, na_level = "none"),
         reviewer.gender = fct_explicit_na(gender.y, na_level = "none")) %>% 
  mutate(published = if_else(published == "yes", 1, 0),
         first.auth = if_else(author.seq == 1, paste(gender.y), paste("NA")),
         corres.auth = if_else(author.corres == "TRUE", paste(gender.y), paste("NA")),
         last.auth = if_else(author.last == "TRUE", paste(gender.y), paste("NA")),
         editor = if_else(role.y == "editor", paste(gender.y), paste("NA")),
         sen.editor = if_else(role.y == "senior.editor", paste(gender.y), paste("NA"))
         ) %>% 
  select(-role.y, -author.seq, -author.corres, -author.last, -gender.y, -random.person.id.y) %>%
  distinct() 

first_auth <- reg_data %>% 
  select(published, first.auth, random.manu.num) %>% 
  filter(first.auth %in% genders) %>% View()
  mutate(first.auth = fct_lump(first.auth, n = 3)) %>% distinct() 

corres_auth <- reg_data %>% 
  select(published, corres.auth, random.manu.num) %>% 
  filter(corres.auth %in% genders) %>% distinct()
  
last_auth <- reg_data %>% 
  select(published, last.auth, random.manu.num) %>% 
  filter(last.auth %in% genders) %>% distinct()

editor <- reg_data %>% 
  select(published, editor, random.manu.num) %>% 
  filter(editor %in% genders) %>% distinct()

sen_editor <- reg_data %>% 
  select(published, sen.editor, random.manu.num) %>% 
  filter(sen.editor %in% genders) %>% distinct()

reviewers <- data %>% 
  select(reviewer.gender, reviewer.random.id, grouped.random, random.manu.num) %>% 
  filter(!is.na(reviewer.random.id)) %>% distinct()

uniq.manu <- reviewers %>% pull(random.manu.num) %>% unique()

men_rev_data <- map_dfr(uniq.manu, function(x){
  reviewers %>% filter(random.manu.num == x) %>% 
  group_by(random.manu.num, reviewer.gender) %>% 
  summarise(n = n()) %>% 
  mutate(prop = n/sum(n)) %>% 
  mutate(men.rev = case_when(
    prop >= 0.75 & prop <= 1 ~ 0,
    prop >= 0.5 & prop <= 0.74 ~ 1,
    prop >= 0.1 & prop <= 0.49 ~ 2,
    prop == 0 ~ 3
  ))
}) %>% select(random.manu.num, men.rev) %>% distinct()

gender_cols <- c("first.auth", "corres.auth", "last.auth")

editor_cols <- c("editor", "sen.editor")

reg2_data <- full_join(first_auth, corres_auth, by = c("published", "random.manu.num")) %>% 
  distinct() %>% 
  full_join(.,  last_auth, by = c("published", "random.manu.num")) %>% 
  distinct() %>% 
  full_join(., editor, by = c("published", "random.manu.num")) %>% 
  distinct() %>% 
  full_join(., sen_editor, by = c("published", "random.manu.num")) %>% distinct() %>% 
  full_join(., men_rev_data, by = "random.manu.num") %>% distinct() %>% 
  #mutate_at(gender_cols, fct_recode(., 0 = "0", 1 = "1", 2 = "2")) %>% 
  mutate_if(is.character, as.factor)
