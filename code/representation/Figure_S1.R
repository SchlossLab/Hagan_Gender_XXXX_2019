
#A. Proportion of Potential Reviewers suggested each Year----
pot_rev_w_prop <- map_dfr(years, function(x){
  get_prop_by_yr(x, pot_rev_data, "gender", "All")})

#figure out which year is the last & isolate the proportion values
text_values <- get_gen_prop_text(pot_rev_w_prop, 3, "gender")

max_value <- get_ymax(pot_rev_w_prop) 

#line plot of all journals combined by year
sup_A <- gender_line_plot(pot_rev_w_prop, max_value, 
                               text_values[1,2], text_values[2,2], text_values[3,2]) + 
  labs(x = "Year\n", y = "\nProportion of\nPotential Reviewers")

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
sub_m_authors_w_prop <- map_dfr(years, function(x){
  get_prop_by_yr(x, sub_mid_auth, "gender", "All") %>% 
    mutate(manu.type = "submitted")}) 

pub_m_authors_w_prop <- map_dfr(years, function(x){
  get_prop_by_yr(x, pub_mid_auth, "gender", "All") %>% 
    mutate(manu.type = "published")}) 

m_authors_w_prop <- rbind(sub_m_authors_w_prop, pub_m_authors_w_prop) %>% 
  filter(gender != "none")

max_value <- get_ymax(m_authors_w_prop) 

#line plot
sup_C <- m_authors_w_prop %>%  
  ggplot() + 
  geom_line(aes(x = year, y = proportion, 
                linetype = manu.type, color = gender), size = 0.75)+
  coord_cartesian(ylim = c(0, max_value))+
  scale_color_manual(values = gen_ed_colors, 
                     breaks = gen_ed_labels)+
  my_theme_leg_horiz + 
  labs(x = "Year",
       y = "\nProportion of\nMiddle Authors",
       linetype = "Manuscript Status",
       color = "Gender")

#D. Proportion of last authors over time: submitted & published----
sub_l_authors_w_prop <- map_dfr(years, function(x){
  get_prop_by_yr(x, sub_last_auth, "gender", "All") %>% 
    mutate(manu.type = "submitted")}) 

pub_l_authors_w_prop <- map_dfr(years, function(x){
  get_prop_by_yr(x, pub_last_auth, "gender", "All") %>% 
    mutate(manu.type = "published")}) 

l_authors_w_prop <- rbind(sub_l_authors_w_prop, pub_l_authors_w_prop) %>% 
  filter(gender != "none")

#figure out which year is the last & isolate the proportion values
m_text_values <- l_authors_w_prop %>% 
  filter(year == "2017") %>% filter(gender == "male")

f_text_values <- l_authors_w_prop %>% 
  filter(year == "2017") %>% filter(gender == "female")

max_value <- get_ymax(l_authors_w_prop) 

#line plot
sup_D <- l_authors_w_prop %>%  
  ggplot() + 
  geom_line(aes(x = year, y = proportion, linetype = manu.type,
                color = gender), size = 0.75)+
  coord_cartesian(ylim = c(0, max_value))+
  scale_color_manual(values = gen_ed_colors, 
                     breaks = gen_ed_labels)+
  annotate(geom = "text", x = 2017, y = m_text_values[2,5]+2, label = "Men")+
  annotate(geom = "text", x = 2017, y = f_text_values[2,5]+2, label = "Women")+
  my_theme_leg_horiz + 
  labs(x = "Year",
       y = "\nProportion of\nLast Authors",
       linetype = "Manuscript Status")

plot_grid(sup_A, sup_B, sup_C, sup_D,
          labels = c('A', 'B', 'C', 'D'), nrow = 4, rel_heights = c(1, 2, 1, 1))

ggsave("Figure_S1.png", device = 'png', 
       path = '../submission/', width = 8, height = 15)
