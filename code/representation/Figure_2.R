#generate figures to summarize reviewer data

#A. US reviewers by institutions & gender----
Figure_2A <- summ_US_stats %>% 
  filter(role == "reviewer") %>% 
  ggplot()+
  geom_col(aes(fill = gender, y = percent, x = US.inst.type),
           position = "dodge")+
  coord_flip()+
  scale_fill_manual(values = gen_ed_colors)+
  labs(x = "\nU.S. Institution Type", y = "Percent of Reviewer Gender\n")+
  my_theme_horiz

#B. Proportion of Reviewers suggested each Year----
Figure_2B <- plot_rev_time("reviewer_data")

#C. Number of papers reviewed by Gender----
reviewer_C <- reviewer_data %>% 
  distinct() %>% #doesn't have the manuscript ids
  group_by(random.person.id, gender) %>% 
  summarise(n = n()) %>%
  ggplot()+
  geom_boxplot(aes(x = gender, group = gender, y = n, fill = gender))+
  scale_y_log10()+
  #coord_cartesian(ylim = c(0, 8))+
  coord_flip()+
  scale_x_discrete(labels = gen_labels)+
  scale_fill_manual(values = gen_colors)+
  labs(x = "\nReviewer Gender", y = "Number of Papers Reviewed")+
  my_theme_horiz  #figure out how to add n of individuals

source("../code/representation/rev_suggest_gender.R") #reviewer_D, reviewer_E

plot_AB <- plot_grid(Figure_2A, Figure_2B,
          labels = c('A', 'B'), label_size = 18)

plot_CDE <- plot_grid(reviewer_C, reviewer_D, reviewer_E,
          labels = c('C', 'D', 'E'), label_size = 18, nrow = 1)

plot_grid(plot_AB, plot_CDE, nrow = 2)

ggsave("Figure_2.png", device = 'png', 
       path = '../submission/', width = 12, height = 9)
