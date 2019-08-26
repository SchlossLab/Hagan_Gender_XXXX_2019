#Calculate proportion of author types & workloads


#Calculate percentage point differences----
auth_types <- c("Middle", "Corresponding", "Last", "First")

comp_EJP <- c("Accept, no revision", "Reject")

auth_type_summary_data <- data %>% 
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


#C. Relative acceptance rates by gender across all journals----
acc_diff_auth <- auth_type_summary_data %>% 
  group_by(gender, role, EJP.decision) %>% 
  summarise(n = n()) %>% 
  spread(key = EJP.decision, value = n) %>% 
  mutate(rel_percent_rej = round(Reject/(Reject+Accept), digits = 2)*100) %>% 
  select(-Reject, -Accept) %>% 
  spread(key = gender, value = rel_percent_rej) %>% 
  mutate(dif_rel_rej = female - male)

figure4_C <- acc_diff_auth %>% 
  ggplot()+
  geom_col(aes(x = role, y = dif_rel_rej, fill = dif_rel_rej))+
  gen_gradient+
  labs(x = "Authorship Role", y = "\nDifference in Acceptance Rate")+
  my_theme_horiz

#D. Relative acceptance rates by gender and journal----
acc_diff_auth_j <- auth_type_summary_data %>% 
    group_by(journal, gender, role, EJP.decision) %>% 
    summarise(n = n()) %>% 
    spread(key = EJP.decision, value = n) %>% 
    mutate(rel_percent_rej = round(Reject/(Reject+Accept), digits = 2)*100) %>% 
    select(-Reject, -Accept) %>% 
    spread(key = gender, value = rel_percent_rej) %>% 
    mutate(dif_rel_rej = female - male)
  
figure4_D <- acc_diff_auth_j %>% 
  ggplot()+
    geom_col(aes(x = journal, y = dif_rel_rej, fill = dif_rel_rej))+
    facet_wrap(~role)+
    coord_flip()+
  gen_gradient+
  labs(x = "Journal", y = "Difference in Acceptance Rate",
       fill = "% Point\nDifference")+
  my_theme_leg
  
col_1 <- plot_grid(figure4_A, figure4_B,
                   labels = c('A', 'B'), 
                   label_size = 18, nrow = 2)

col_2 <- plot_grid(figure4_C, figure4_D,
                   labels = c('C', 'D'), 
                   label_size = 18, nrow = 1)

plot_grid(col_1, col_2, nrow = 2, rel_heights = c(1.5, 1))

ggsave("Figure_4.png", device = 'png', 
       path = '../submission/', width = 12, height = 12)
