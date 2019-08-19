
#A. Proportion of Potential Reviewers suggested each Year----
sup_A <- plot_rev_time("pot_rev_data")

#B. Proportion of reviewer genders by journal-----
j_rev_w_prop <- map_dfr(years, function(x){
  get_prop_by_yr(x, reviewer_data, "gender", "Each")})

max_journ_value <- get_ymax(j_rev_w_prop)

sup_B <- j_rev_w_prop %>% 
  filter(gender != "NA") %>% 
  j_gen_line_plot(., max_journ_value) + 
  labs(x = "Year", y = "\nProportion of Reviewers",
       linetype = "Gender")

#C. Proportion of middle authors over time: submitted & published----
sup_C <- plot_sub_v_pub_time("sub_mid_auth", "pub_mid_auth")

#D. Proportion of last authors over time: submitted & published----
sup_D <- plot_sub_v_pub_time("sub_last_auth", "pub_last_auth")

plot_grid(sup_A, sup_B, sup_C, sup_D,
          labels = c('A', 'B', 'C', 'D'), nrow = 4, rel_heights = c(1, 2, 1, 1))

ggsave("Figure_S1.png", device = 'png', 
       path = '../submission/', width = 8, height = 15)
