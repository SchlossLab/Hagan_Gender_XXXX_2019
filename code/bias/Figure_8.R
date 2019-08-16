acc_data <- bias_data %>% 
  select(published, version, grouped.random, random.manu.num, gender, 
         EJP.decision, contains("days"), journal,
         num.versions, -days.to.review) %>% 
  distinct()

manus <- acc_data %>% pull(grouped.random) %>% unique()

accepted_data <- acc_data %>% 
  filter(published == "yes") %>% 
  filter(version == "0") %>% 
  select(-num.versions) %>% 
  distinct()

#days from submission to production
factors_A <- accepted_data %>% 
  ggplot(aes(x = days.pending, fill = gender))+
  geom_density(alpha = 0.5)+
  coord_cartesian(xlim = c(0, 200))+
  scale_fill_manual(values = gen_colors)+
  facet_wrap(~journal)+
  labs(x = "Days from 'Submission' to\n'Ready for Publication' Dates\n",
       y = "\nDensity")+
  my_theme


#Do papers authored by women take longer to get accepted than those authored by men?
acc_data <- bias_data %>% 
  select(version, grouped.random, random.manu.num, gender, 
         EJP.decision, contains("days"), journal,
         num.versions) %>% 
  distinct()

manus <- acc_data %>% pull(grouped.random) %>% unique()

manu_summary <- map_df(manus, function(x){
  acc_data %>% filter(grouped.random == x) %>% 
    select(version, random.manu.num, days.to.decision) %>% 
    distinct() %>% 
    group_by(random.manu.num) %>% 
    summarise(total.decision = sum(days.to.decision)) %>% 
    mutate(grouped.random = x)
})

acc_data <- acc_data %>% 
  left_join(., manu_summary, by = c("grouped.random", "random.manu.num"))

factors_B <- acc_data %>% 
  select(gender, grouped.random, total.decision, journal) %>% distinct() %>% 
  filter(total.decision >= 0 & total.decision <= 200) %>% 
  ggplot()+
  geom_density(aes(x = total.decision, fill = gender), alpha = 0.5)+
  facet_wrap(~journal)+
  scale_fill_manual(values = gen_colors)+
  labs(x = "Days in Peer Review System\n", 
       y = "\nDensity")+
  my_theme

#setup----
cites <- read_csv("data/cites.csv") %>% 
  select(`Article DOI (article_metadata)`, `Published Months`, 
         `Article Date of Publication (article_metadata)`, `Citation Date`, Cites, `Mendeley Saves`) %>% 
  group_by(`Article DOI (article_metadata)`,`Published Months`, 
           `Article Date of Publication (article_metadata)`, `Mendeley Saves`) %>% 
  summarise(Cites = sum(Cites))

usage <- read_csv("data/usage.csv") %>% 
  select(`Article DOI (article_metadata)`, `Total Abstract`, `Total HTML`, `Total PDF`)

c_u_data <- full_join(cites, usage, by = "Article DOI (article_metadata)") %>% distinct()

hist_jif <- read_csv("data/jif_2010_17.csv") %>% 
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

plot_grid(factors_A, factors_B, factors_C, factors_D, labels = c('A', 'B', 'C', 'D'), label_size = 18)

ggsave("Figure_8.png", device = 'png', 
       path = 'submission/', width = 12, height = 9)
