#aggregate numbers for Amy Chang

#EICs
eics <- eic_data <- read_csv("data/eic_genders.csv") %>% 
  group_by(EiC.gender) %>% summarise(EiC = n()) %>% 
  rename(gender = EiC.gender)

#Editors 
editors <- data %>% filter(str_detect(role, "editor")==TRUE) %>% 
  select(random.person.id, gender) %>% distinct() %>% 
  group_by(gender) %>% summarise(Editors = n()) %>% 
  rename(gender = gender)

#Reviewers 
reviewers <- data %>% 
  select(reviewer.gender, reviewer.random.id) %>% distinct() %>% 
  group_by(reviewer.gender) %>% summarise(Reviewers = n()) %>% 
  rename(gender = reviewer.gender)

#Authors
authors <- data %>% filter(role == "author") %>% 
  select(random.person.id, gender) %>% distinct() %>% 
  group_by(gender) %>% summarise(Authors = n()) %>% 
  rename(gender = gender)

#1st authors 
first.auth <- data %>% filter(role == "author" & author.seq == "1") %>% 
  select(random.person.id, gender) %>% distinct() %>% 
  group_by(gender) %>% summarise(First.Authors = n()) %>% 
  rename(gender = gender)

#Corresponding authors 
corres.auth <- data %>% filter(role == "author" & author.corres == "TRUE") %>% 
  select(random.person.id, gender) %>% distinct() %>% 
  group_by(gender) %>% summarise(Corres.Authors = n()) %>% 
  rename(gender = gender)

df_list <- as.list(eics, editors, reviewers, authors, first.auth, corres.auth)

#compile into single summary table 
gender_summary <- plyr::join_all(list(eics, editors, reviewers, authors,
                                      first.auth, corres.auth), 
                                 by = "gender", type = "full") %>% 
  mutate(gender = case_when(
    str_detect(gender, "female") ~ "Women",
    str_detect(gender, "male") ~ "Men",
    TRUE ~ "Unclear"
  )) %>% 
  rename(Journals.Role = gender) %>%
  t() %>% as.data.frame() %>% 
  rownames_to_column()
  
write_csv(gender_summary, "data/gender_summary.csv", col_names = FALSE)
