#Are papers authored by women ranked differently by reviewers?
#Does ranking correlate to impact (via citations)?

cites <- read_csv("data/cites.csv") %>% 
  select(`Published Months`, `Article DOI`, Cites, `Citation Date`) %>%
  group_by(`Published Months`, `Article DOI`) %>% summarise(total.cites = sum(Cites))

usage <- read_csv("data/usage.csv") %>% 
  select(`Article DOI (article_metadata)`, contains("Total"))

no_score_journ <- c("AEM", "JVI", "IAI", "MCB", "JB")

rev_score_data <- data %>% filter(author.corres == TRUE) %>% 
  select(gender.y, journal, published, review.score, reviewer.gender, 
         reviewer.random.id, random.manu.num, version.reviewed, EJP.decision, doi) %>% distinct() %>% 
  filter(!is.na(review.score)) %>% 
  mutate(doi = str_to_lower(doi)) %>% 
  left_join(., cites, by = c("doi" = "Article DOI")) %>% 
  left_join(., usage, by = c("doi" = "Article DOI (article_metadata)"))

#graphs of review scores by journal & gender----
rev_score_data %>% 
  filter(!is.na(review.score)) %>% 
  mutate(EJP.decision = fct_collapse(EJP.decision,
                                     Accept = c("Accept, no revision",
                                                "Revise only",
                                                "Revise and re-review"))) %>% 
  group_by(journal, EJP.decision, review.score) %>% 
  summarise(n = n()) %>% 
  ggplot()+
  geom_col(aes(x = EJP.decision, y = n, fill = review.score), position = "dodge")+
  facet_wrap(~journal, scales = "free_y")+
  my_theme_leg

rev_score_data %>% 
  filter(!is.na(review.score)) %>% 
  ggplot()+
  geom_violin(aes(x = gender.y, y = review.score), scale = "area")+
  facet_wrap(~journal)+
  my_theme_horiz

ggsave("results/rev_score_violin.jpg")

rev_score_data %>% 
  filter(!is.na(review.score)) %>% 
  ggplot()+
  geom_boxplot(aes(x = gender.y, y = review.score))+
  facet_wrap(~journal)+
  my_theme_horiz

ggsave("results/rev_score_boxplot.jpg")

rev_score_data %>% 
  filter(!is.na(review.score)) %>% 
  filter(!is.na(gender.y)) %>% 
  ggplot()+
  geom_density(aes(x = review.score, fill = gender.y), alpha = 0.25)+
  scale_fill_manual(values = gen_ed_colors)+
  facet_wrap(~journal, scales = "free_y")+
  my_theme_horiz

ggsave("results/rev_score_density.jpg")


#graphs of reviews scores & cites----
rev_score_data %>% 
  filter(!is.na(`Published Months`)) %>% 
  filter(between(`Published Months`, 24, 48)) %>% 
  mutate(cites.per.month = total.cites/`Published Months`) %>% 
  ggplot()+
  geom_boxplot(aes(x = review.score, y = cites.per.month, group = review.score))+
  facet_wrap(~journal)+
  my_theme_horiz

ggsave("results/rev_score_citespermon_box.jpg")

rev_score_data %>% 
  filter(!is.na(`Published Months`)) %>% 
  filter(between(`Published Months`, 24, 48)) %>% 
  mutate(cites.per.month = total.cites/`Published Months`) %>% 
  ggplot()+
  geom_jitter(aes(x = as.numeric(review.score), y = cites.per.month, group = review.score))+
  facet_wrap(~journal, scales = "free_y")+
  my_theme_horiz

ggsave("results/rev_score_citespermon_point.jpg")

rev_score_data %>% 
  filter(!is.na(`Published Months`)) %>% 
  filter(between(`Published Months`, 1, 12)) %>% 
  mutate(abs.views.per.month = `Total Abstract`/`Published Months`) %>% 
  ggplot()+
  geom_boxplot(aes(x = review.score, y = abs.views.per.month, group = review.score))+
  facet_wrap(~journal)+
  my_theme_horiz

ggsave("results/rev_score_absviewspermon_box.jpg")

rev_score_data %>% 
  filter(!is.na(`Published Months`)) %>% 
  filter(between(`Published Months`, 1, 12)) %>% 
  mutate(abs.views.per.month = `Total Abstract`/`Published Months`) %>% 
  ggplot()+
  geom_jitter(aes(x = as.numeric(review.score), y = abs.views.per.month, group = review.score))+
  facet_wrap(~journal)+
  my_theme_horiz

ggsave("results/rev_score_absviewspermon_jitter.jpg")

rev_score_data %>% 
  filter(!is.na(`Published Months`)) %>% 
  filter(between(`Published Months`, 1, 12)) %>% 
  mutate(html.views.per.month = `Total HTML`/`Published Months`) %>% 
  mutate(pdf.views.per.month = `Total PDF`/`Published Months`) %>% 
  mutate(full.views.per.month = html.views.per.month + pdf.views.per.month) %>% 
  ggplot()+
  geom_boxplot(aes(x = review.score, y = full.views.per.month, group = review.score))+
  facet_wrap(~journal, scales = "free_y")+
  my_theme_horiz

ggsave("results/rev_score_fullviewspermon_box.jpg")

rev_score_data %>% 
  filter(!is.na(`Published Months`)) %>% 
  filter(between(`Published Months`, 1, 12)) %>% 
mutate(html.views.per.month = `Total HTML`/`Published Months`) %>% 
  mutate(pdf.views.per.month = `Total PDF`/`Published Months`) %>% 
  mutate(full.views.per.month = html.views.per.month + pdf.views.per.month) %>% 
  ggplot()+
  geom_jitter(aes(x = as.numeric(review.score), y = full.views.per.month, group = review.score))+
  facet_wrap(~journal, scales = "free_y")+
  my_theme_horiz

ggsave("results/rev_score_fullviewspermon_jitter.jpg")
