#setup----
cites <- read_csv("../data/cites_1.csv") %>% 
  select(`Article DOI (article_metadata)`, `Published Months`, 
         `Article Date of Publication (article_metadata)`, `Citation Date`, Cites, `Mendeley Saves`) %>% 
  group_by(`Article DOI (article_metadata)`,`Published Months`, 
           `Article Date of Publication (article_metadata)`, `Mendeley Saves`) %>% 
  summarise(Cites = sum(Cites))

usage <- read_csv("../data/usage_1.csv") %>% 
  select(`Article DOI (article_metadata)`, `Total Abstract`, `Total HTML`, `Total PDF`)

c_u_data <- full_join(cites, usage, by = "Article DOI (article_metadata)") %>% distinct()

hist_jif <- read_csv("../data/jif_2010_17.csv") %>% 
  separate("Journal;Impact Factor;Year", 
           into = c("Journal", "JIF", "year"), sep = ";") %>% 
  filter(JIF != "n/a") %>% 
  group_by(Journal) %>% 
  summarise(n = n(), sum.JIF = sum(as.numeric(JIF)))

max_jif <- read_csv("../data/max_jif.csv") %>% 
  separate("Journal;max.JIF;year.founded", 
           into = c("Journal", "max.JIF", "year.founded"), sep = ";") %>%   left_join(., hist_jif, by = "Journal") %>% 
  mutate(years.old = 2019 - as.numeric(year.founded),
         avg.JIF = (sum.JIF/n),
         percieved.JIF = avg.JIF + as.numeric(max.JIF) + years.old) %>% 
  select(Journal, avg.JIF, percieved.JIF)

#dataset----
impact_data <- bias_data %>%  
  filter(!is.na(doi)) %>% 
  select(gender, journal, random.manu.num, doi) %>% 
  distinct() %>% 
  mutate(doi = str_to_lower(doi)) %>% 
  left_join(., c_u_data, 
            by = c("doi" = "Article DOI (article_metadata)")) %>% 
  distinct() %>% 
  filter(!is.na(`Article Date of Publication (article_metadata)`)) %>% 
  left_join(., max_jif, by = c("journal" = "Journal")) %>%
  mutate(`Total Reads` = `Total HTML` + `Total PDF`,
         log.avg.JIF = log10(avg.JIF),
         `Cites/log.JIF` = Cites-log.avg.JIF) %>% 
  gather(`Mendeley Saves`:`Cites/log.JIF`, 
         key = measure.name, value = measure.value) %>% 
  mutate(value.per.month = measure.value/`Published Months`)

#plots----
factors_C <- plot_impact_data("Cites", 3)

factors_D <- plot_impact_data("Total Reads", 300)

#plot_impact_data("avg.JIF", "NULL")
