#Reviewer supplemental

#A. Proportion of Potential Reviewers suggested each Year----
Fig_S2A <- plot_rev_time("pot_rev_data")+
  my_theme_leg_horiz

#B. Proportion of reviewer genders by journal-----
j_rev_w_prop <- map_dfr(years, function(x){
  get_prop_by_yr(x, reviewer_data, "gender", "Each")})

max_journ_value <- get_ymax(j_rev_w_prop)

Fig_S2B <- j_rev_w_prop %>% 
  filter(gender != "NA") %>% 
  j_gen_line_plot(., max_journ_value) + 
  labs(x = "Year", y = "\nProportion of Reviewers")+
  my_theme

#make figure----
plot_grid(Fig_S2A, Fig_S2B,
          labels = c('A', 'B'), label_size = 18,
          rel_heights = c(1, 1.5), 
          ncol = 1)

ggsave("Figure_S2.tiff", device = 'tiff', units = "in", scale = 1.25,
       path = 'submission', width = 6.8, height = 5)
