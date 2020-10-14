acc_data <- bias_data %>% 
  select(published, version, grouped.random, random.manu.num, gender, 
         EJP.decision, contains("days"), journal, grouped.vers,
         num.versions, days.to.review) %>% 
  filter(published == "yes") %>% 
  filter(grouped.vers == 1) %>% 
  distinct()

manus <- acc_data %>% pull(grouped.random) %>% unique()

accepted_data <- acc_data %>% 
  select(-num.versions, -days.to.review) %>% 
  distinct()

accepted_summary <-accepted_data %>% 
  group_by(journal, gender) %>% 
  summarise(avg.pending = mean(days.pending, na.rm = TRUE), 
            med.pending = median(days.pending, na.rm = TRUE), min.pending = min(days.pending, na.rm = TRUE),
            max.pending = max(days.pending, na.rm = TRUE)) 

accepted_avg <- accepted_summary %>% 
  select(journal, gender, avg.pending) %>% 
  spread(key = gender, value = avg.pending) %>% 
  mutate(avg.diff = round(female - male)) %>% 
  select(-female, -male) %>% 
  arrange(desc(avg.diff))

#days from submission to production
figure_S6A <- accepted_data %>% 
  ggplot(aes(x = days.pending, fill = gender))+
  geom_density(alpha = 0.5)+
  coord_cartesian(xlim = c(0, 200))+
  scale_fill_manual(values = gen_colors, 
                    labels = gen_labels)+
  facet_wrap(~journal)+
  labs(x = "Days from 'Submission' to\n'Ready for Publication' Dates\n",
       y = "\nDensity", fill = "Gender")+
  my_theme_leg+
  theme(legend.position = "top")

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

decision_summary <- acc_data %>% 
  group_by(journal, gender) %>% 
  summarise(avg.decision = mean(total.decision, na.rm = TRUE), 
            med.decision = median(total.decision, na.rm = TRUE), min.decision = min(total.decision, na.rm = TRUE),
            max.decision = max(total.decision, na.rm = TRUE)) 

avg_decision <- decision_summary %>% 
  select(journal, gender, avg.decision) %>% 
  spread(key = gender, value = avg.decision) %>% 
  mutate(diff.decision = round(female - male)) %>% 
  select(-male, -female) %>% 
  arrange(desc(diff.decision))

figure_S6B <- acc_data %>% 
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
    arrange(desc(grouped.vers)) %>% head(n = 1)
})

version_sum <- accepted_versions %>%
  group_by(gender) %>% 
  summarise(med_vers = median(num.versions),
            IQR_vers = IQR(num.versions))

#number of revisions before acceptance -- data described in text
#figure_S6C <- accepted_versions %>% 
#  ggplot(aes(x = gender, y = num.versions, fill = gender))+
#  geom_boxplot()+
#  facet_wrap(~journal)+
#  scale_fill_manual(values = gen_colors)+
#  gen_x_replace +
#  labs(x = "Gender", y = "\nNumber of Versions")+
#  my_theme

#plot figure-----
plot_grid(figure_S6A, figure_S6B, ncol = 1,
          labels = c('A', 'B'), label_size = 18)

ggsave("Figure_S6.tiff", device = 'tiff', units = "in", scale = 1,
       path = 'submission', width = 6, height = 9, compression = "lzw")
