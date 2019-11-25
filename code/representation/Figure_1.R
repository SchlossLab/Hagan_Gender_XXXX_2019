#Generate components of the figure summarizing editor stats

#A. proportion of editors from US institutions by gender----
Fig_1A <- summ_US_stats %>% 
  filter(role == "editor") %>% 
  ggplot(aes(fill = gender, y = percent, x = US.inst.type, label = n))+
  geom_col(position = "dodge")+
  coord_flip(ylim = c(0, 60))+
  scale_fill_manual(labels = gen_labels, values = gen_colors)+
  labs(x = "\n", y = "Percent of Editor Gender\n",
       fill = "Gender")+
  geom_text(aes(y = percent + 1.5), 
            position = position_dodge(width = 0.9), 
            vjust = 0.5)+
  my_theme_leg_horiz+
  theme(legend.position = c(0.8, 0.4))

#B. Proportion of editors (editors + senior.editors) at ASM over time by gender & manuscripts handled----

#proportion of individuals each year
ed_w_prop <- map_dfr(years, function(x){
  get_prop_by_yr(x, editor_data, "gender", "All")
}) 

ed_prop_text <- get_gen_prop_text(ed_w_prop, 2, "gender") #calc label placement

Fig_1B <- ggplot(ed_w_prop) + 
  geom_line(aes(x = year, y = proportion, color = gender))+
  coord_cartesian(ylim = c(0, 100))+
  scale_color_manual(breaks = gen_levels, labels = NULL, values = gen_colors)+
  annotate(geom = "text", x = 2017, y = ed_prop_text[1,2]+2, label = "Women")+
  annotate(geom = "text", x = 2017, y = ed_prop_text[2,2]+4, label = "Men")+
  labs(x = "Year", y = "\nProportion of Editors")+
  my_theme_horiz

#C. US reviewers by institutions & gender----
Fig_1C <- summ_US_stats %>% 
  filter(role == "reviewer") %>% 
  ggplot(aes(fill = gender, y = percent, x = US.inst.type, label = n))+
  geom_col(position = "dodge")+
  coord_flip()+
  scale_fill_manual(labels = gen_labels, values = gen_colors)+
  labs(x = "\n", y = "Percent of Reviewer Gender", fill = "Gender")+
  geom_text(aes(y = percent + 1.5), 
            position = position_dodge(width = 0.9), 
            vjust = 0.5)+
  my_theme_leg_horiz+
  theme(legend.position = c(0.8, 0.4))

#D. Proportion of Reviewers suggested each Year----
Fig_1D <- plot_rev_time("reviewer_data") #add color?

#generate full figures----
plot_grid(Fig_1A, Fig_1B, Fig_1C, Fig_1D,
          labels = c('A', 'B', 'C', 'D'), label_size = 18,
          nrow = 2)
#
ggsave("Figure_1.png", device = 'png', 
       path = 'submission', width = 12, height = 6)
