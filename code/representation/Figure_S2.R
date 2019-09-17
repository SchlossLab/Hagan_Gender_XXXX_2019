#Reviewer supplemental

#A. Proportion of Potential Reviewers suggested each Year----
Fig_S2A <- plot_rev_time("pot_rev_data")

#B. Proportion of reviewer genders by journal-----
j_rev_w_prop <- map_dfr(years, function(x){
  get_prop_by_yr(x, reviewer_data, "gender", "Each")})

max_journ_value <- get_ymax(j_rev_w_prop)

Fig_S2B <- j_rev_w_prop %>% 
  filter(gender != "NA") %>% 
  j_gen_line_plot(., max_journ_value) + 
  labs(x = "Year", y = "\nProportion of Reviewers",
       linetype = "Gender")

plot_grid(Fig_S2A, Fig_S2B,
          labels = c('A', 'B'), label_size = 18,
          rel_heights = c(1, 1.5), 
          ncol = 1)

ggsave("Figure_S2.png", device = 'png', 
       path = '../submission', width = 8, height = 8)
