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
  gen_x_replace +
  labs(x = "Gender", y = "Days to Rejection")+
  my_theme

#number of revisions
Supplementary_E <- rejected_versions %>% 
  ggplot(aes(x = gender, y = num.versions, fill = gender))+
  geom_boxplot()+
  facet_wrap(~journal)+
  scale_fill_manual(values = gen_colors)+
  gen_x_replace +
  labs(x = "Gender", y = "\nNumber of Versions Prior to Rejection")+
  my_theme

plot_grid(Supplementary_D, Supplementary_E,
          labels = c('A', 'B'), label_size = 18)

ggsave("supp_time.png", device = 'png', 
       path = 'submission/', width = 9, height = 8)
