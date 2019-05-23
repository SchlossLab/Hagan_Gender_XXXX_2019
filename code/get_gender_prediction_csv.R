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
  select(role, published, doi, journal, num.versions, num.authors, contains("days"), author.seq, author.corres, gender, reviewer.gender, review.score, reviewer.random.id, grouped.random, random.manu.num, random.person.id, EJP.decision, institution,
         US.inst, US.inst.type) %>% 
  distinct() %>%
  mutate(corres.auth = if_else(author.corres == "TRUE", paste(gender), paste("NA")),
         editor = if_else(role == "editor", paste(gender), paste("NA")),
         sen.editor = if_else(role == "senior.editor", paste(gender), paste("NA"))
         ) %>% 
  select(-author.seq, -author.corres, -days.to.review) %>%
  distinct() 

cites_usage <- reg_data %>% 
  filter(corres.auth != "NA") %>% 
  select(doi, grouped.random, random.manu.num, 
                                   num.versions, contains("days"), journal,
                                   EJP.decision, US.inst, US.inst.type) %>% 
  mutate(doi = str_to_lower(doi)) %>% 
  left_join(., cites, by = c("doi" = "Article DOI")) %>% 
  left_join(., usage, by = c("doi" = "Article DOI (article_metadata)")) %>% 
  distinct() %>% 
  mutate(cites.month = total.cites/`Published Months`,
         abstract.views.month = `Total Abstract`/`Published Months`,
         html.views.month = `Total HTML`/`Published Months`,
         pdf.views.month = `Total PDF`/`Published Months`) %>% 
  select(-contains("Total"), -`Published Months`)

manu_list <- cites_usage %>% pull(random.manu.num) %>% unique()

single_entry_cites <- map_dfr(manu_list, function(x){
  cites_usage %>% 
    filter(random.manu.num == x) %>% 
    arrange(desc(days.pending)) %>% 
    head(n = 1)
})

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

uniq.rev.manu <- reviewers %>% pull(random.manu.num) %>% unique()

men_rev_data <- map_dfr(uniq.rev.manu, function(x){
  
  summary <- reviewers %>% filter(random.manu.num == x) %>% 
    distinct() %>% summary()
  
  num_rev <- paste(c(summary[1,2], summary[2,2], summary[3,2]), collapse = "") %>% 
    str_extract_all(., "\\d") %>% unlist() %>% as.numeric(.)
  
  df <- reviewers %>% filter(random.manu.num == x) %>% 
    select(random.manu.num) %>% distinct() %>% 
    cbind(., avg.rev = summary[4,1] %>% 
            str_replace(., "Mean   :", "") %>% as.numeric(),
          prop.men.rev = num_rev[2]/sum(num_rev),
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

reg2_data <- full_join(corres_auth, editor, 
                       by = c("published", "random.manu.num", "grouped.random")) %>% 
  distinct() %>% 
  full_join(., sen_editor, 
            by = c("published", "random.manu.num", "grouped.random")) %>% 
  distinct() %>% 
  left_join(., author_ratio, by = "random.manu.num") %>% distinct() %>% 
  left_join(., men_rev_data, by = "random.manu.num") %>% distinct() %>% 
  left_join(., single_entry_cites, 
            by = c("random.manu.num", "grouped.random")) %>% 
  distinct() %>% 
  mutate(reviewed = if_else(is.na(prop.men.rev), 0, 1)) %>% 
  select(-doi, -random.manu.num, -grouped.random) %>% 
  mutate_all(as.factor)

write_csv(reg2_data, path = "data/gender_log_reg.csv")
