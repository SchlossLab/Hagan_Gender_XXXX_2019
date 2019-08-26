acc_data <- bias_data %>% 
  select(published, version, grouped.random, random.manu.num, gender, 
         EJP.decision, contains("days"), journal,
         num.versions, days.to.review) %>% 
  filter(published == "yes") %>% 
  filter(version == 0) %>% 
  distinct()

manus <- acc_data %>% pull(grouped.random) %>% unique()

accepted_data <- acc_data %>% 
  select(-num.versions, -days.to.review) %>% 
  distinct()

#days from submission to production
figure_S2A <- accepted_data %>% 
  ggplot(aes(x = days.pending, fill = gender))+
  geom_density(alpha = 0.5)+
  coord_cartesian(xlim = c(0, 200))+
  scale_fill_manual(values = gen_colors)+
  facet_wrap(~journal)+
  labs(x = "Days from 'Submission' to\n'Ready for Publication' Dates\n",
       y = "\nDensity")+
  my_theme

#Do papers authored by women take longer to get accepted than those authored by men?----
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

figure_S2B <- acc_data %>% 
  select(gender, grouped.random, total.decision, journal) %>% distinct() %>% 
  filter(total.decision >= 0 & total.decision <= 200) %>% 
  ggplot()+
  geom_density(aes(x = total.decision, fill = gender), alpha = 0.5)+
  facet_wrap(~journal)+
  scale_fill_manual(values = gen_colors)+
  labs(x = "Days in Peer Review System\n", 
       y = "\nDensity")+
  my_theme

#versions for accepted outcomes----

accepted_versions <- map_dfr(manus, function(x){
  acc_data %>%  
    filter(grouped.random == x) %>% 
    arrange(desc(num.versions)) %>% head(n = 1)
})

#number of revisions before acceptance
figure_S2C <- accepted_versions %>% 
  ggplot(aes(x = gender, y = num.versions, fill = gender))+
  geom_boxplot()+
  facet_wrap(~journal)+
  scale_fill_manual(values = gen_colors)+
  gen_x_replace +
  labs(x = "Gender", y = "\nNumber of Versions")+
  my_theme

plot_grid(figure_S2A, figure_S2B, figure_S2C,
          labels = c('A', 'B', 'C'), label_size = 18)

ggsave("Figure_S3.png", device = 'png', 
       path = '../submission/', width = 12, height = 12)
