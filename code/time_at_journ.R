#Do papers authored by women take longer to get accepted than those authored by men?

acc_data <- data %>% filter(published == "yes") %>% 
  mutate(gender.y = fct_explicit_na(gender.y, na_level = "none")) %>% 
  mutate(days.pending = as.duration(ymd_hms(submitted.date) %--% ymd_hms(ready.for.production.date))/ddays(1)) %>% 
  filter(role.y == "author" & author.corres == "TRUE") %>% 
  select(version, grouped.random, random.manu.num, gender.y, 
         EJP.decision, days.to.decision, journal, days.pending,
         num.versions) %>% distinct()

manus <- acc_data %>% pull(grouped.random) %>% unique()

manu_summary <- map_df(manus, function(x){
  acc_data %>% filter(grouped.random == x) %>% 
  group_by(gender.y, random.manu.num, journal, num.versions) %>% 
  summarise(total.decision = sum(days.to.decision))
})

manu_summary %>% 
  filter(gender.y != "none") %>% 
  filter(total.decision >= 0 & total.decision <= 200) %>% 
  ggplot()+
  geom_density(aes(x = total.decision, fill = gender.y))+
  scale_fill_manual(values = gen_colors)

manu_summary %>% 
  filter(gender.y != "none") %>% 
  filter(total.decision >= 0 & total.decision <= 200) %>% 
  ggplot()+
  geom_density(aes(x = total.decision, fill = gender.y))+
  facet_wrap(~journal)+
  scale_fill_manual(values = gen_colors)

manu_summary %>% 
  filter(total.decision >= 0 & total.decision <= 200) %>% 
  ggplot()+
  geom_boxplot(aes(x = gender.y, y = total.decision, fill = gender.y))+
  scale_fill_manual(values = gen_colors)

manu_summary %>% 
  filter(gender.y != "none") %>% 
  ggplot()+
  geom_density(aes(x = num.versions, fill = gender.y))+
  scale_fill_manual(values = gen_colors)

manu_summary %>% 
  ggplot()+
  geom_boxplot(aes(x = gender.y, y = num.versions, fill = gender.y))+
  scale_fill_manual(values = gen_colors)


manu_summary %>% 
  ggplot()+
  geom_boxplot(aes(x = gender.y, y = num.versions, fill = gender.y))+
  facet_wrap(~journal)+
  scale_fill_manual(values = gen_colors)

#days from initial submission to ready for production date
manu_pending <- map_df(manus, function(x){
  acc_data %>% filter(version == 0 & grouped.random == x) %>% 
    select(-random.manu.num, -days.to.decision) %>% distinct() %>% 
    arrange(desc(days.pending)) %>% head(n = 1)
})

manu_pending %>% 
  filter(gender.y != "none") %>% 
  filter(days.pending <= 300) %>% 
  ggplot()+
  geom_density(aes(x = days.pending, fill = gender.y))+
  scale_fill_manual(values = gen_colors)

manu_pending %>% 
  #filter(days.pending <= 300) %>% 
  ggplot()+
  geom_boxplot(aes(x = gender.y, y = days.pending, fill = gender.y))+
  #scale_y_log10()+
  coord_cartesian(ylim = c(0,200))+
  scale_fill_manual(values = gen_colors)

manu_pending %>% 
  #filter(days.pending <= 300) %>% 
  ggplot()+
  geom_boxplot(aes(x = gender.y, y = days.pending, fill = gender.y))+
  facet_wrap(~journal)+
  #scale_y_log10()+
  coord_cartesian(ylim = c(0,200))+
  scale_fill_manual(values = gen_colors)
