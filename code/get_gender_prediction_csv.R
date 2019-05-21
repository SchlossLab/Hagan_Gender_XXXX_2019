#requires source("code/load_data.R")
#impact of variables on whether or not an article is published: editor gender, reviewer gender, first author gender, last author gender, corresponding author gender 

#other interesting variables not included here: # reviewers, # versions, days.pending, 
#setup----
cites <- read_csv("data/cites.csv") %>% 
  select(`Published Months`, `Article DOI`, Cites, `Citation Date`) %>%
  group_by(`Published Months`, `Article DOI`) %>% summarise(total.cites = sum(Cites))

usage <- read_csv("data/usage.csv") %>% 
  select(`Article DOI (article_metadata)`, contains("Total"))

genders <- c("male", "female", "none")

gender_cols <- c("first.auth", "corres.auth", "last.auth")

editor_cols <- c("editor", "sen.editor")

#select data----
reg_data <- data %>% 
  select(role, published, doi, journal, num.versions, num.authors, contains("days"), author.seq, author.corres, gender, reviewer.gender, review.score, reviewer.random.id, grouped.random, random.manu.num, random.person.id, EJP.decision) %>% 
  distinct() %>%
  mutate(gender = fct_explicit_na(gender, na_level = "none"),
         reviewer.gender = fct_explicit_na(gender, na_level = "none")) %>% 
  mutate(corres.auth = if_else(author.corres == "TRUE", paste(gender), paste("NA")),
         editor = if_else(role == "editor", paste(gender), paste("NA")),
         sen.editor = if_else(role == "senior.editor", paste(gender), paste("NA"))
         ) %>% 
  select(-author.seq, -author.corres) %>%
  distinct() 

cites_usage <- reg_data %>% select(doi, grouped.random, random.manu.num, num.versions, num.authors, contains("days"), journal) %>% 
  mutate(doi = str_to_lower(doi)) %>% 
  left_join(., cites, by = c("doi" = "Article DOI")) %>% 
  left_join(., usage, by = c("doi" = "Article DOI (article_metadata)")) %>% 
  distinct() %>% 
  mutate(cites.month = total.cites/`Published Months`,
         abstract.views.month = `Total Abstract`/`Published Months`,
         html.views.month = `Total HTML`/`Published Months`,
         pdf.views.month = `Total PDF`/`Published Months`) %>% 
  select(-contains("Total"), -`Published Months`)

corres_auth <- reg_data %>% 
  select(published, corres.auth, grouped.random, random.manu.num) %>% 
  filter(corres.auth %in% genders) %>% distinct()
  
editor <- reg_data %>% 
  select(published, editor, grouped.random, random.manu.num) %>% 
  filter(editor %in% genders) %>% distinct()

sen_editor <- reg_data %>% 
  select(published, sen.editor, grouped.random, random.manu.num) %>% 
  filter(sen.editor %in% genders) %>% distinct()

reviewers <- reg_data %>% 
  select(review.score, reviewer.gender, reviewer.random.id, grouped.random, random.manu.num) %>% 
  filter(!is.na(reviewer.random.id)) %>% distinct()

uniq.manu <- reviewers %>% pull(random.manu.num) %>% unique()

men_rev_data <- map_dfr(uniq.manu, function(x){
  reviewers %>% filter(random.manu.num == x) %>% distinct() %>% 
  group_by(random.manu.num, reviewer.gender, review.score) %>% 
  summarise(n = n()) %>% 
  mutate(avg.rev = (review.score*n)/sum(n),
    prop.men.rev = n/sum(n),
    num.rev = sum(n)) #%>% 
#  mutate(men.rev = case_when(
#    prop >= 0.75 & prop <= 1 ~ 0,
#    prop >= 0.5 & prop <= 0.74 ~ 1,
#    prop >= 0.1 & prop <= 0.49 ~ 2,
#    prop == 0 ~ 3
#  ))
}) %>% select(random.manu.num, prop.men.rev, num.rev, avg.rev) %>% distinct()

auth_data <- reg_data %>% 
  filter(role == "author") %>% 
  select(random.manu.num, num.authors, random.person.id, gender)

author_ratio <- map_dfr(uniq.manu, function(x){
  auth_data %>% 
  filter(random.manu.num == x) %>% distinct() %>% 
  mutate(if.female = if_else(gender == "female", 1, 0),
         prop.fem.auth = sum(if.female)/num.authors) %>% 
  select(random.manu.num, prop.fem.auth, num.authors) %>% distinct()
})

reg2_data <- full_join(corres_auth, editor, by = c("published", "random.manu.num", "grouped.random")) %>% 
  distinct() %>% 
  full_join(., sen_editor, by = c("published", "random.manu.num", "grouped.random")) %>% distinct() %>% 
  full_join(., author_ratio, by = "random.manu.num") %>% distinct() %>% 
  full_join(., men_rev_data, by = "random.manu.num") %>% distinct() %>% 
  full_join(., cites_usage, by = c("random.manu.num", "grouped.random")) %>% 
  distinct() %>% 
  mutate(reviewed = if_else(is.na(men.rev), 0, 1)) %>% 
  mutate_all(as.factor)

#write_csv(reg2_data, path = "data/gender_log_reg.csv")
