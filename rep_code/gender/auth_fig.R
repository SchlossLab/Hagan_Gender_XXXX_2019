#generate component graphs of the "author figure"

#A. Proportion of unique authors each year: unclear/men/women----
all_authors_w_prop <- map_dfr(years, function(x){
  get_prop_by_yr(x, uniq_author_data, "gender", "All")})

#figure out which year is the last & isolate the proportion values
text_values <- get_gen_prop_text(all_authors_w_prop, 3, "gender")

max_value <- get_ymax(all_authors_w_prop) 

#line plot of all journals combined by year
author_A <- gender_line_plot(all_authors_w_prop, max_value, 
                 text_values[1,2], text_values[2,2], text_values[3,2]) + 
  labs(x = "Year", y = "Proportion of Unique Authors",
    caption = "Proportion of unique authors on manuscripts each year. 
    Each person is counted once per year")


#B. Proportion of men/women first authors over time: submitted & published----
sub_f_authors_w_prop <- map_dfr(years, function(x){
  get_prop_by_yr(x, sub_first_auth, "gender", "All") %>% 
    mutate(manu.type = "submitted")}) 

pub_f_authors_w_prop <- map_dfr(years, function(x){
  get_prop_by_yr(x, pub_first_auth, "gender", "All") %>% 
    mutate(manu.type = "published")}) 

f_authors_w_prop <- rbind(sub_f_authors_w_prop, pub_f_authors_w_prop) %>% 
  filter(gender != "none")

max_value <- get_ymax(f_authors_w_prop) 

#line plot
author_B <- f_authors_w_prop %>%  
    ggplot() + 
    geom_line(aes(x = year, y = proportion, 
                  linetype = manu.type, color = gender), size = 0.75)+
    coord_cartesian(ylim = c(0, max_value))+
    scale_color_manual(values = gen_ed_colors, 
                       breaks = gen_ed_labels)+
    my_theme_leg_horiz + 
    labs(x = "Year",
         y = "Proportion of First Authors",
         linetype = "Manuscript Status",
         color = "Gender", #not showing for some reason...
         caption = "Proportion of first authors on manuscripts each year.")

#C. Proportion of men/women corresponding authors over time: submitted & published----
sub_c_authors_w_prop <- map_dfr(years, function(x){
  get_prop_by_yr(x, sub_corres_auth, "gender", "All") %>% 
    mutate(manu.type = "submitted")}) 

pub_c_authors_w_prop <- map_dfr(years, function(x){
  get_prop_by_yr(x, pub_corres_auth, "gender", "All") %>% 
    mutate(manu.type = "published")}) 

c_authors_w_prop <- rbind(sub_c_authors_w_prop, pub_c_authors_w_prop) %>% 
  filter(gender != "none")

#figure out which year is the last & isolate the proportion values
m_text_values <- c_authors_w_prop %>% 
  filter(year == "2017") %>% filter(gender == "male")

f_text_values <- c_authors_w_prop %>% 
  filter(year == "2017") %>% filter(gender == "female")

max_value <- get_ymax(c_authors_w_prop) 

#line plot
author_C <- c_authors_w_prop %>%  
  ggplot() + 
  geom_line(aes(x = year, y = proportion, linetype = manu.type,
                color = gender), size = 0.75)+
  coord_cartesian(ylim = c(0, max_value))+
  scale_color_manual(values = gen_ed_colors, 
    breaks = gen_ed_labels)+
  annotate(geom = "text", x = 2017, y = m_text_values[2,5]+1.5, label = "Men")+
  annotate(geom = "text", x = 2017, y = f_text_values[2,5]+1.5, label = "Women")+
  my_theme_leg_horiz + 
  labs(x = "Year",
       y = "Proportion of Corresponding Authors",
       linetype = "Manuscript Status",
       caption = "Proportion of corresponding authors on manuscripts each year. 
       Counted by unique manuscripts each year.")
