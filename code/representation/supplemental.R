
#A. Proportion of Potential Reviewers suggested each Year----
pot_rev_w_prop <- map_dfr(years, function(x){
  get_prop_by_yr(x, pot_rev_data, "gender", "All")})

#figure out which year is the last & isolate the proportion values
text_values <- get_gen_prop_text(pot_rev_w_prop, 3, "gender")

max_value <- get_ymax(pot_rev_w_prop) 

#line plot of all journals combined by year
sup_A <- gender_line_plot(pot_rev_w_prop, max_value, 
                               text_values[1,2], text_values[2,2], text_values[3,2]) + 
  labs(x = "Year\n", y = "Proportion of Potential Reviewers")