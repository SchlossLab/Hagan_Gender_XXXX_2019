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

acc_data %>% 
  select(gender, grouped.random, total.decision) %>% distinct() %>% 
  filter(total.decision >= 0 & total.decision <= 200) %>% 
  ggplot()+
  geom_density(aes(x = total.decision, fill = gender), alpha = 0.5)+
  scale_fill_manual(values = gen_colors)+
  my_theme_leg_horiz

ggsave("results/total_decision_density.jpg")

acc_data %>% 
  select(gender, grouped.random, total.decision, journal) %>% distinct() %>% 
  filter(total.decision >= 0 & total.decision <= 200) %>% 
  ggplot()+
  geom_density(aes(x = total.decision, fill = gender), alpha = 0.5)+
  facet_wrap(~journal)+
  scale_fill_manual(values = gen_colors)+
  my_theme_leg_horiz

ggsave("results/total_decision_journ_density.jpg")

acc_data %>% 
  select(gender, grouped.random, total.decision) %>% distinct() %>% 
  filter(total.decision >= 0 & total.decision <= 200) %>% 
  ggplot()+
  geom_boxplot(aes(x = gender, y = total.decision, fill = gender))+
  scale_fill_manual(values = gen_colors)+
  my_theme_horiz

ggsave("results/total_decision_box.jpg")

#days from initial submission to ready for production date
final_decision <- map_df(manus, function(x){
  acc_data %>% filter(grouped.random == x) %>% 
    distinct() %>% 
    arrange(desc(num.versions)) %>% head(n = 1)
})

final_decision %>% #looks at published papers
  filter(EJP.decision != "Reject") %>% 
  filter(!is.na(EJP.decision)) %>% 
  filter(days.pending <= 300) %>% 
  ggplot()+
  geom_density(aes(x = days.pending, fill = gender))+
  facet_wrap(~EJP.decision)+
  scale_fill_manual(values = gen_colors)

ggsave("results/dayspend_density.jpg")

final_decision %>% 
  filter(EJP.decision != "Reject") %>% 
  filter(!is.na(EJP.decision)) %>% 
  ggplot()+
  geom_boxplot(aes(x = gender, y = days.pending, fill = gender))+
  coord_cartesian(ylim = c(0,300))+
  facet_wrap(~EJP.decision)+
  scale_fill_manual(values = gen_colors)

ggsave("results/dayspend_box.jpg")

final_decision %>% 
  ggplot()+
  geom_density(aes(x = days.pending, fill = gender), alpha = 0.5)+
  facet_wrap(~journal)+
  coord_cartesian(xlim = c(0,200))+
  scale_fill_manual(values = gen_colors)

ggsave("results/dayspend_journ_density.jpg")