
cites <- read_csv("data/cites.csv") %>% 
  select(`Article DOI (article_metadata)`, `Published Months`, 
         `Article Date of Publication (article_metadata)`, `Citation Date`, Cites, `Mendeley Saves`) %>% 
  group_by(`Article DOI (article_metadata)`,`Published Months`, 
           `Article Date of Publication (article_metadata)`, `Mendeley Saves`) %>% 
  summarise(Cites = sum(Cites))

usage <- read_csv("data/usage.csv") %>% 
  select(`Article DOI (article_metadata)`, `Total Abstract`, `Total HTML`, `Total PDF`)

c_u_data <- full_join(cites, usage, by = "Article DOI (article_metadata)") %>% distinct()

impact_data <- data %>% filter(author.corres == TRUE) %>% 
  filter(!is.na(doi)) %>% 
  select(gender.y, journal, random.manu.num, doi) %>% 
  distinct() %>% 
  mutate(doi = str_to_lower(doi)) %>% 
  left_join(., c_u_data, by = c("doi" = "Article DOI (article_metadata)")) %>% 
  distinct() %>% 
  filter(!is.na(`Article Date of Publication (article_metadata)`))
  