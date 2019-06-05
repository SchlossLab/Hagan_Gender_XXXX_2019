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
  labs(x = "\nDays in Peer Review System", 
       y = "Density\n", fill = "Gender")+
  my_theme_leg

#days from initial submission to ready for production date

