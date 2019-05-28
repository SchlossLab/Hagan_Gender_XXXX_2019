#Do papers authored by women take longer to get accepted than those authored by men?

acc_data <- bias_data %>% filter(published == "yes") %>% 
  select(version, grouped.random, random.manu.num, gender, 
         EJP.decision, contains("days"), journal,
         num.versions) %>% distinct()

manus <- acc_data %>% pull(grouped.random) %>% unique()

manu_summary <- map_df(manus, function(x){
  acc_data %>% filter(grouped.random == x) %>% 
  group_by(gender, random.manu.num, journal, num.versions) %>% 
  summarise(total.decision = sum(days.to.decision))
})

manu_summary %>% 
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
  ggplot()+
  geom_density(aes(x = as.numeric(num.versions), fill = gender), 
               alpha = 0.5)+
  scale_fill_manual(values = gen_colors)+
  my_theme_leg_horiz

ggsave("results/numversion_density.jpg")

manu_summary %>% 
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
    select(-random.manu.num, -days.to.decision, -days.final, 
           -days.to.production, -days.to.review) %>% 
    distinct() %>% 
    arrange(desc(days.pending)) %>% head(n = 1)
})

manu_pending %>% 
  filter(days.pending <= 300) %>% 
  ggplot()+
  geom_density(aes(x = days.pending, fill = gender))+
  facet_wrap(~EJP.decision)+
  scale_fill_manual(values = gen_colors)

ggsave("results/dayspend_density.jpg")

manu_pending %>% 
  ggplot()+
  geom_boxplot(aes(x = gender, y = days.pending, fill = gender))+
  coord_cartesian(ylim = c(0,200))+
  facet_wrap(~EJP.decision)+
  scale_fill_manual(values = gen_colors)

ggsave("results/dayspend_box.jpg")

manu_pending %>% 
  ggplot()+
  geom_boxplot(aes(x = gender, y = days.pending, fill = gender))+
  facet_wrap(~journal)+
  coord_cartesian(ylim = c(0,200))+
  scale_fill_manual(values = gen_colors)

ggsave("results/dayspend_journ_box.jpg")

#withdrawn papers
manu_pending %>% 
  filter(EJP.decision == "Withdrawn") %>% 
  filter(days.pending <= 300) %>% 
  ggplot()+
  geom_density(aes(x = days.pending, fill = gender), 
               alpha = 0.5)+
  scale_fill_manual(values = gen_colors)

ggsave("results/dayspend_withd_density.jpg")

manu_pending %>% 
  filter(EJP.decision == "Withdrawn") %>% 
  ggplot()+
  geom_boxplot(aes(x = gender, y = days.pending, fill = gender))+
  scale_fill_manual(values = gen_colors)

ggsave("results/dayspend_withd_box.jpg")

manu_pending %>% 
  filter(EJP.decision == "Withdrawn") %>% 
  ggplot()+
  geom_boxplot(aes(x = gender, y = days.pending, fill = gender))+
  facet_wrap(~journal)+
  #coord_cartesian(ylim = c(0,200))+
  scale_fill_manual(values = gen_colors)

ggsave("results/dayspend_withd_journ_box.jpg")

jb_withdrawn <- manu_pending %>% 
  filter(EJP.decision == "Withdrawn") %>% 
  filter(journal == "JB") %>% distinct()

medians <- jb_withdrawn %>% group_by(gender) %>% 
  summarise(med = median(days.pending))

jb_withdrawn %>% 
  ggplot(aes(x = gender, y = days.pending))+
  geom_boxplot()+
  geom_dotplot(aes(fill = gender), binaxis = "y", 
               stackdir = "center",
               method = "histodot", binwidth = 1,
               dotsize = 15)+
  scale_fill_manual(values = gen_colors)+
  my_theme_horiz
  #geom_hline(aes(yintercept = medians[]))#add line for medians

#days from submission to final decision
manu_final <- map_df(manus, function(x){
  acc_data %>% filter(version == 0 & grouped.random == x) %>% 
    select(-random.manu.num, -days.to.decision, -days.pending,
           -days.to.production, -days.to.review) %>% 
    distinct() %>% 
    arrange(desc(days.final)) %>% head(n = 1)
})

manu_final %>% 
  filter(EJP.decision != "NA") %>% 
  filter(days.final >= 0 & days.final <=300) %>% 
  ggplot()+
  facet_wrap(~EJP.decision)+
  geom_density(aes(x = days.final, fill = gender))+
  scale_fill_manual(values = gen_colors)

ggsave("results/daysfinal_density.jpg")

manu_final %>% 
  filter(EJP.decision != "NA") %>% 
    ggplot()+
  geom_boxplot(aes(x = gender, y = days.final, fill = gender))+
  facet_wrap(~EJP.decision)+
  coord_cartesian(ylim = c(0,200))+
  scale_fill_manual(values = gen_colors)

ggsave("results/daysfinal_box.jpg")

manu_final %>%
  filter(EJP.decision != "NA") %>% 
  ggplot()+
  geom_boxplot(aes(x = gender, y = days.final, fill = gender))+
  facet_wrap(~journal)+
  coord_cartesian(ylim = c(0,200))+
  scale_fill_manual(values = gen_colors)

ggsave("results/daysfinal_journ_box.jpg")

manu_final %>% 
  filter(EJP.decision == "Withdrawn") %>% 
  ggplot()+
  geom_boxplot(aes(x = gender, y = days.final, fill = gender))+
  scale_fill_manual(values = gen_colors)

manu_final %>% 
  filter(EJP.decision == "Withdrawn") %>% 
  ggplot()+
  geom_boxplot(aes(x = gender, y = days.final, fill = gender))+
  facet_wrap(~journal)+
  #coord_cartesian(ylim = c(0,200))+
  scale_fill_manual(values = gen_colors)

jb_withdrawn_final <- manu_final %>% 
  filter(EJP.decision == "Withdrawn") %>% 
  filter(journal == "JB") %>% distinct()

medians <- jb_withdrawn_final %>% group_by(gender) %>% 
  summarise(med = median(days.final))

jb_withdrawn_final %>% 
  ggplot(aes(x = gender, y = days.final))+
  geom_boxplot()+
  geom_dotplot(aes(fill = gender), binaxis = "y", 
               stackdir = "center",
               method = "histodot", binwidth = 1,
               dotsize = 15)+
  scale_fill_manual(values = gen_colors)+
  my_theme_horiz
