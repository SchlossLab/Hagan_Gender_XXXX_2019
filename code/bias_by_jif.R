hist_jif <- read_csv("data/jif_2010_17.csv") %>% 
  separate("Journal;Impact Factor;Year", 
           into = c("Journal", "JIF", "year"), sep = ";") %>% 
  filter(JIF != "n/a") %>% 
  group_by(Journal) %>% 
  summarise(n = n(), sum.JIF = sum(as.numeric(JIF)))

max_jif <- read_csv("data/max_jif.csv") %>% 
  separate("Journal;max.JIF;year.founded", 
           into = c("Journal", "max.JIF", "year.founded"), sep = ";") %>% 
  left_join(., hist_jif, by = "Journal") %>% 
  mutate(years.old = 2019 - as.numeric(year.founded),
         avg.JIF = (sum.JIF/n),
         percieved.JIF = avg.JIF + as.numeric(max.JIF) + years.old)