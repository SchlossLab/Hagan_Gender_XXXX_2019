#Generate plots for final summary/takeaway figure

auth_types <- c("Middle", "Corresponding", "Last", "First")

comp_EJP <- c("Accept, no revision", "Reject")

summary_data <- data %>% 
  filter(gender != "none") %>% 
  filter(EJP.decision %in% comp_EJP) %>% 
  mutate(role = case_when(
    role == "author" & 
      author.corres == "FALSE" & author.last == "FALSE" &
      author.seq != 1 ~ "Middle",
    role == "author" & 
      (author.corres == "TRUE") ~ "Corresponding",
    role == "author" &
      author.last == "TRUE" ~ "Last",
    role == "author" &
      author.seq == 1 ~ "First",
    TRUE ~ role
  )) %>% mutate(year = year(submitted.date)) %>% 
  select(random.person.id, gender, role, journal, grouped.random, published, EJP.decision) %>% 
  filter(role %in% auth_types) %>% 
  mutate(EJP.decision = fct_recode(EJP.decision, Accept = "Accept, no revision")) %>% 
  distinct() 


#A. Relative acceptance rates by gender across all journals----
summary_A <- summary_data %>% 
  group_by(gender, role, EJP.decision) %>% 
  summarise(n = n()) %>% 
  spread(key = EJP.decision, value = n) %>% 
  mutate(rel_percent_rej = round(Reject/(Reject+Accept), digits = 2)*100) %>% 
  select(-Reject, -Accept) %>% 
  spread(key = gender, value = rel_percent_rej) %>% 
  mutate(dif_rel_rej = female - male) %>%
  ggplot()+
  geom_col(aes(x = role, y = dif_rel_rej, fill = dif_rel_rej))+
  scale_fill_gradient2(low = "#D55E00", mid='snow3', 
                       high = "#0072B2", space = "Lab")+
  labs(x = "Authorship Role", y = "Difference in Acceptance Rate")+
  my_theme_horiz

#B. Relative acceptance rates by gender and journal----
summary_B <- summary_data %>% 
    group_by(journal, gender, role, EJP.decision) %>% 
    summarise(n = n()) %>% 
    spread(key = EJP.decision, value = n) %>% 
    mutate(rel_percent_rej = round(Reject/(Reject+Accept), digits = 2)*100) %>% 
    select(-Reject, -Accept) %>% 
    spread(key = gender, value = rel_percent_rej) %>% 
    mutate(dif_rel_rej = female - male) %>% 
  #mutate(rel_chx = rel_chx - 50) %>% 
  ggplot()+
    geom_col(aes(x = journal, y = dif_rel_rej, fill = dif_rel_rej))+
    facet_wrap(~role)+
    coord_flip()+
  scale_fill_gradient2(low = "#D55E00", mid='snow3', high = "#0072B2", space = "Lab")+
  labs(x = "Journal", y = "Difference in Acceptance Rate",
       fill = "% Point\nDifference")+
  my_theme_leg
  
plot_grid(summary_A, summary_B, labels = c('A', 'B'), label_size = 18)

ggsave("Figure_4.png", device = 'png', 
       path = 'submission/', width = 12, height = 6)
#C. Retention of authors to leadership by gender----
#source("../code/gender/retention_alluvial.R")

#summary_C <- retention_plot #Figure_5.png
