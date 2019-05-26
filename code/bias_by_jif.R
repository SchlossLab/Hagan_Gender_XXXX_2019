acc_data <- data %>% filter(published == "yes") %>% 
  filter(EJP.decision != "NA") %>% 
  mutate(gender = fct_explicit_na(gender, na_level = "none")) %>% 
  filter(role == "author" & author.corres == "TRUE") %>% 
  select(version, grouped.random, random.manu.num, gender, 
         EJP.decision, journal, contains("days"),
         num.versions) %>% distinct()

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
         percieved.JIF = avg.JIF + as.numeric(max.JIF) + years.old) %>% 
  select(Journal, avg.JIF, percieved.JIF)

jif_acc_data <- left_join(acc_data, max_jif, 
                          by = c("journal" = "Journal"))

jif_acc_data %>% 
  filter(gender != "none") %>% 
  ggplot()+
  geom_density(aes(x = percieved.JIF, fill = gender), 
               alpha = 0.5)+
  facet_wrap(~EJP.decision)+
  scale_fill_manual(values = gen_colors)+
  my_theme_leg_horiz

ggsave("percieved_jif_density.png")