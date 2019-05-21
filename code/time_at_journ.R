#Do papers authored by women take longer to get accepted than those authored by men?

acc_data <- data %>% filter(published == "yes") %>% 
  mutate(gender = fct_explicit_na(gender, na_level = "none")) %>% 
  filter(role == "author" & author.corres == "TRUE") %>% 
  select(version, grouped.random, random.manu.num, gender, 
         EJP.decision, days.to.decision, journal, days.pending,
         num.versions) %>% distinct()

manus <- acc_data %>% pull(grouped.random) %>% unique()

manu_summary <- map_df(manus, function(x){
  acc_data %>% filter(grouped.random == x) %>% 
  group_by(gender, random.manu.num, journal, num.versions) %>% 
  summarise(total.decision = sum(days.to.decision))
})

manu_summary %>% 
  filter(gender != "none") %>% 
  filter(total.decision >= 0 & total.decision <= 200) %>% 
  ggplot()+
  geom_density(aes(x = total.decision, fill = gender), alpha = 0.5)+
  scale_fill_manual(values = gen_colors)+
  my_theme_leg_horiz

ggsave("results/total_decision_density.jpg")

manu_summary %>% 
  filter(gender != "none") %>% 
  filter(total.decision >= 0 & total.decision <= 200) %>% 
  ggplot()+
  geom_density(aes(x = total.decision, fill = gender), alpha = 0.5)+
  facet_wrap(~journal)+
  scale_fill_manual(values = gen_colors)+
  my_theme_leg_horiz

ggsave("results/total_decision_journ_density.jpg")

manu_summary %>% 
  filter(total.decision >= 0 & total.decision <= 200) %>% 
  ggplot()+
  geom_boxplot(aes(x = gender, y = total.decision, fill = gender))+
  scale_fill_manual(values = gen_colors)+
  my_theme_horiz

ggsave("results/total_decision_box.jpg")

manu_summary %>% 
  filter(gender != "none") %>% 
  ggplot()+
  geom_density(aes(x = as.numeric(num.versions), fill = gender), 
               alpha = 0.5)+
  scale_fill_manual(values = gen_colors)+
  my_theme_leg_horiz

ggsave("results/numversion_density.jpg")

manu_summary %>% 
  filter(gender != "none") %>% 
  ggplot()+
  geom_density(aes(x = as.numeric(num.versions), fill = gender), 
               alpha = 0.5)+
  facet_wrap(~journal)+
  scale_fill_manual(values = gen_colors)+
  my_theme_leg_horiz

ggsave("results/numvers_journ_density.jpg")

manu_summary %>% 
  ggplot()+
  geom_boxplot(aes(x = gender, y = as.numeric(num.versions), 
                   fill = gender))+
  scale_fill_manual(values = gen_colors)+
  my_theme_horiz

ggsave("results/numversion_box.jpg")

manu_summary %>% 
  ggplot()+
  geom_boxplot(aes(x = gender, y = num.versions, fill = gender))+
  facet_wrap(~journal)+
  scale_fill_manual(values = gen_colors)+
  my_theme_horiz

ggsave("results/numvers_journ_box.jpg")

#days from initial submission to ready for production date
manu_pending <- map_df(manus, function(x){
  acc_data %>% filter(version == 0 & grouped.random == x) %>% 
    select(-random.manu.num, -days.to.decision) %>% distinct() %>% 
    arrange(desc(days.pending)) %>% head(n = 1)
})

manu_pending %>% 
  filter(gender != "none") %>% 
  filter(days.pending <= 300) %>% 
  ggplot()+
  geom_density(aes(x = days.pending, fill = gender))+
  scale_fill_manual(values = gen_colors)

ggsave("results/dayspend_journ_box.jpg")

manu_pending %>% 
  ggplot()+
  geom_boxplot(aes(x = gender, y = days.pending, fill = gender))+
  coord_cartesian(ylim = c(0,200))+
  scale_fill_manual(values = gen_colors)

ggsave("results/dayspend_journ_box.jpg")

manu_pending %>% 
  ggplot()+
  geom_boxplot(aes(x = gender, y = days.pending, fill = gender))+
  facet_wrap(~journal)+
  coord_cartesian(ylim = c(0,200))+
  scale_fill_manual(values = gen_colors)

ggsave("results/dayspend_journ_box.jpg")
