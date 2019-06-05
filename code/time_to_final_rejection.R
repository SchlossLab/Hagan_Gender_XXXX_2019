#time at journal for non-published (aka rejected) outcomes
rejected_data <- bias_data %>% 
  filter(published == "no") %>% 
  filter(EJP.decision == "Reject") %>% 
  filter(!is.na(days.to.review)) %>% #eliminate editorial rejections
  select(version, grouped.random, random.manu.num, gender, 
         EJP.decision, days.final, days.to.decision, journal,
         num.versions, days.to.review) %>% 
  distinct()

rejected_manus <- rejected_data %>% pull(grouped.random) %>% unique()

rejected_versions <- map_dfr(rejected_manus, function(x){
    rejected_data %>%  
    filter(grouped.random == x) %>% 
    arrange(desc(num.versions)) %>% head(n = 1)
})

#days from submission to rejection decision  
Supplementary_D <- rejected_versions %>% 
  ggplot(aes(x = gender, y = days.final, fill = gender))+
  geom_boxplot()+
  coord_cartesian(ylim = c(0, 50))+
  scale_fill_manual(values = gen_colors)+
  facet_wrap(~journal)+
  labs(x = "\nGender", y = "Days to Rejection\n")+
  my_theme_horiz

#number of revisions
Supplementary_E <- rejected_versions %>% 
  ggplot(aes(x = gender, y = num.versions, fill = gender))+
  geom_boxplot()+
  facet_wrap(~journal)+
  scale_fill_manual(values = gen_colors)+
  labs(x = "\nGender", y = "Number of Versions Prior to Rejection\n")+
  my_theme_horiz
