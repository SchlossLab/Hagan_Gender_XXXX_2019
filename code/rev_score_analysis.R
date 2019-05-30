#Are papers authored by women ranked differently by reviewers?
#Does ranking correlate to impact (via citations)?

cites <- read_csv("data/cites.csv") %>% 
  select(`Published Months`, `Article DOI`, Cites, `Citation Date`) %>%
  group_by(`Published Months`, `Article DOI`) %>% summarise(total.cites = sum(Cites))

usage <- read_csv("data/usage.csv") %>% 
  select(`Article DOI (article_metadata)`, contains("Total"))

no_score_journ <- c("AEM", "JVI", "IAI", "MCB", "JB")

rev_score_data <- bias_data %>% 
  select(gender, journal, published, review.score, reviewer.gender, 
         reviewer.random.id, random.manu.num, version.reviewed, 
         EJP.decision, doi, US.inst, US.inst.type) %>% distinct() %>% 
  filter(!is.na(review.score)) %>% 
  mutate(doi = str_to_lower(doi)) %>% 
  left_join(., cites, by = c("doi" = "Article DOI")) %>% 
  left_join(., usage, by = c("doi" = "Article DOI (article_metadata)"))

#graphs of review scores by journal & gender----
rev_score_data %>% 
  select(random.manu.num, journal, gender, review.score) %>%
  distinct() %>% 
  ggplot(aes(x = review.score, fill = gender))+
  geom_histogram(aes(y=0.5*..density..), 
                    alpha=0.5, position='identity', binwidth=0.5)+
  scale_fill_manual(labels = gen_ed_labels, values = gen_ed_colors)+
  facet_wrap(~journal)+
  labs(x = "Review Score", y = "Proportion")+
  my_theme_horiz

rev_score_data %>% 
  ggplot()+
  geom_boxplot(aes(x = gender, y = review.score))+
  facet_wrap(~journal)+
  my_theme_horiz

ggsave("results/rev_score_boxplot.jpg")

#us institutions by review score & gender
rev_score_us <- bias_data %>% 
  filter(!is.na(review.score)) %>% 
  filter(!is.na(US.inst)) %>% 
  select(random.manu.num, journal, US.inst, gender, review.score) %>% 
  distinct()

rev_score_us %>%
  ggplot(aes(x = review.score, fill = gender))+
  geom_histogram(aes(y=0.5*..density..), 
                 alpha=0.5, position='identity', binwidth=0.5)+
  scale_fill_manual(labels = gen_ed_labels, values = gen_ed_colors)+
  facet_wrap(journal~US.inst)+
  labs(x = "Review Score", y = "Proportion")+
  my_theme_horiz

rev_score_inst <- bias_data %>% 
  filter(!is.na(review.score)) %>% 
  filter(US.inst == "yes") %>% 
  filter(!is.na(US.inst.type)) 

rev_score_inst %>%
  filter(journal == "AAC") %>% 
  ggplot(aes(x = review.score, fill = gender))+
  geom_histogram(aes(y=0.5*..density..), 
                 alpha=0.5, position='identity', binwidth=0.5)+
  scale_fill_manual(labels = gen_ed_labels, values = gen_ed_colors)+
  facet_wrap(~US.inst.type)+
  labs(x = "Review Score", y = "Proportion")+
  my_theme_horiz

rev_score_inst %>%
  filter(journal == "CVI") %>% 
  ggplot(aes(x = review.score, fill = gender))+
  geom_histogram(aes(y=0.5*..density..), 
                 alpha=0.5, position='identity', binwidth=0.5)+
  scale_fill_manual(labels = gen_ed_labels, values = gen_ed_colors)+
  facet_wrap(~US.inst.type)+
  labs(x = "Review Score", y = "Proportion")+
  my_theme_horiz

rev_score_inst %>%
  filter(journal == "JCM") %>% 
  ggplot(aes(x = review.score, fill = gender))+
  geom_histogram(aes(y=0.5*..density..), 
                 alpha=0.5, position='identity', binwidth=0.5)+
  scale_fill_manual(labels = gen_ed_labels, values = gen_ed_colors)+
  facet_wrap(~US.inst.type)+
  labs(x = "Review Score", y = "Proportion")+
  my_theme_horiz

rev_score_inst %>%
  filter(journal == "mBio") %>% 
  ggplot(aes(x = review.score, fill = gender))+
  geom_histogram(aes(y=0.5*..density..), 
                 alpha=0.5, position='identity', binwidth=0.5)+
  scale_fill_manual(labels = gen_ed_labels, values = gen_ed_colors)+
  facet_wrap(~US.inst.type)+
  labs(x = "Review Score", y = "Proportion")+
  my_theme_horiz

rev_score_inst %>%
  filter(journal == "mSphere") %>% 
  ggplot(aes(x = review.score, fill = gender))+
  geom_histogram(aes(y=0.5*..density..), 
                 alpha=0.5, position='identity', binwidth=0.5)+
  scale_fill_manual(labels = gen_ed_labels, values = gen_ed_colors)+
  facet_wrap(~US.inst.type)+
  labs(x = "Review Score", y = "Proportion")+
  my_theme_horiz

rev_score_inst %>%
  filter(journal == "mSystems") %>% 
  ggplot(aes(x = review.score, fill = gender))+
  geom_histogram(aes(y=0.5*..density..), 
                 alpha=0.5, position='identity', binwidth=0.5)+
  scale_fill_manual(labels = gen_ed_labels, values = gen_ed_colors)+
  facet_wrap(~US.inst.type)+
  labs(x = "Review Score", y = "Proportion")+
  my_theme_horiz
