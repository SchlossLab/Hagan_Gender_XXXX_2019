acc_data <- bias_data %>% 
  select(grouped.random, gender, journal) %>% distinct()

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
  filter(journal %in% c("mBio", "mSphere")) %>% 
  ggplot(aes(x = avg.JIF, fill = gender))+
  geom_histogram(aes(y=0.5*..density..), 
               alpha=0.5, position='identity', binwidth=0.5)+
  scale_fill_manual(labels = gen_ed_labels, values = gen_ed_colors)+
  labs(x = "Average JIF", y = "Proportion of Submitted", fill = "Gender")+
  my_theme_leg_horiz
