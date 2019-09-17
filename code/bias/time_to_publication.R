acc_data <- bias_data %>% 
  select(published, version, grouped.random, random.manu.num, gender, 
         EJP.decision, contains("days"), journal,
         num.versions, -days.to.review, grouped.vers) %>% 
  distinct()

manus <- acc_data %>% pull(grouped.random) %>% unique()

accepted_data <- acc_data %>% 
  filter(published == "yes") %>% 
  filter(version == 0) %>% 
  select(-num.versions) %>% 
  distinct()

#days from submission to production
factors_A <- accepted_data %>% 
  ggplot(aes(x = days.pending, fill = gender))+
  geom_density(alpha = 0.5)+
  coord_cartesian(xlim = c(0, 200))+
  scale_fill_manual(values = gen_colors)+
  facet_wrap(~journal)+
  labs(x = "\nDays from 'Submission' to 'Ready for Publication' Dates",
       y = "Density\n", fill = "Author Gender")+
  my_theme_leg


#number of revisions
final_decision <- map_df(manus, function(x){
  acc_data %>% filter(grouped.random == x) %>% 
    distinct() %>% 
    arrange(desc(grouped.vers)) %>% head(n = 1)
})

vers_data <- final_decision %>% 
  select(grouped.random, num.versions) %>% 
  distinct() %>% 
  left_join(accepted_data, ., by = "grouped.random") %>% 
  select(gender, num.versions, grouped.random, journal) %>% 
  distinct()

Supplementary_C <- vers_data %>% 
  ggplot(aes(x = gender, y = num.versions, fill = gender))+
  geom_boxplot()+
  facet_wrap(~journal)+
  scale_fill_manual(values = gen_colors)+
  labs(x = "\nGender", y = "Total Number of Versions\n")+
  my_theme_horiz
