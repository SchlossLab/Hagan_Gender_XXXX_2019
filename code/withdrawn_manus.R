#withdrawn papers
final_decision %>% 
  filter(EJP.decision == "Withdrawn") %>% 
  filter(days.pending <= 300) %>% 
  ggplot()+
  geom_density(aes(x = days.pending, fill = gender), 
               alpha = 0.5)+
  scale_fill_manual(values = gen_colors)

ggsave("results/dayspend_withd_density.jpg")

final_decision %>% 
  filter(EJP.decision == "Withdrawn") %>% 
  ggplot()+
  geom_boxplot(aes(x = gender, y = days.pending, fill = gender))+
  scale_fill_manual(values = gen_colors)

ggsave("results/dayspend_withd_box.jpg")

final_decision %>% 
  filter(EJP.decision == "Withdrawn") %>% 
  ggplot()+
  geom_boxplot(aes(x = gender, y = days.pending, fill = gender))+
  facet_wrap(~journal)+
  #coord_cartesian(ylim = c(0,200))+
  scale_fill_manual(values = gen_colors)

ggsave("results/dayspend_withd_journ_box.jpg")

withdrawn <- final_decision %>% 
  filter(EJP.decision == "Withdrawn") %>% 
  filter(journal %in% c("JB", "MCB")) %>% distinct()

withdrawn %>% 
  ggplot(aes(x = gender, y = days.pending))+
  geom_boxplot()+
  geom_dotplot(aes(fill = gender), binaxis = "y", 
               stackdir = "center",
               method = "histodot", binwidth = 1,
               dotsize = 15)+
  facet_wrap(~journal)+
  scale_fill_manual(values = gen_colors)+
  my_theme_horiz
#geom_hline(aes(yintercept = medians[]))#add line for medians

#days from submission to final decision
final_decision %>% 
  filter(EJP.decision != "NA") %>% 
  filter(days.final >= 0 & days.final <=300) %>% 
  ggplot()+
  facet_wrap(~EJP.decision)+
  geom_density(aes(x = days.final, fill = gender))+
  scale_fill_manual(values = gen_colors)

ggsave("results/daysfinal_density.jpg")

final_decision %>% 
  filter(EJP.decision != "NA") %>% 
  ggplot()+
  geom_boxplot(aes(x = gender, y = days.final, fill = gender))+
  facet_wrap(~EJP.decision)+
  coord_cartesian(ylim = c(0,200))+
  scale_fill_manual(values = gen_colors)

ggsave("results/daysfinal_box.jpg")

final_decision %>%
  filter(EJP.decision != "NA") %>% 
  ggplot()+
  geom_boxplot(aes(x = gender, y = days.final, fill = gender))+
  facet_wrap(~journal)+
  coord_cartesian(ylim = c(0,200))+
  scale_fill_manual(values = gen_colors)

ggsave("results/daysfinal_journ_box.jpg")

final_decision %>% 
  filter(EJP.decision == "Withdrawn") %>% 
  ggplot()+
  geom_boxplot(aes(x = gender, y = days.final, fill = gender))+
  scale_fill_manual(values = gen_colors)

final_decision %>% 
  filter(EJP.decision == "Withdrawn") %>% 
  ggplot()+
  geom_boxplot(aes(x = gender, y = days.final, fill = gender))+
  facet_wrap(~journal)+
  #coord_cartesian(ylim = c(0,200))+
  scale_fill_manual(values = gen_colors)

final_decision %>% 
  filter(EJP.decision == "Withdrawn") %>% 
  ggplot(aes(x = gender, y = days.final))+
  geom_boxplot()+
  geom_dotplot(aes(fill = gender), binaxis = "y", 
               stackdir = "center",
               method = "histodot", binwidth = 1,
               dotsize = 15)+
  scale_fill_manual(values = gen_colors)+
  facet_wrap(~journal)+
  my_theme_horiz