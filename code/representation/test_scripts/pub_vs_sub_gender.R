#compare submissions by gender

#unique manuscripts
sub_author_data %>% 
  select(-random.manu.num) %>% 
  distinct() %>% 
  gender_bar_plot(.)

pub_author_data %>% 
  select(-random.manu.num) %>% 
  distinct() %>% 
  gender_bar_plot(.)+
  facet_wrap(~journal, scales = "free_y")

#
pub_authors_w_prop <- map_dfr(journals, function(x){
  map_dfr(years, function(y){
  get_prop_by_yr(y, pub_author_data, gender, x)}
  )}) 

label_pos <- pub_authors_w_prop %>% filter(year == "2018") %>%
  pull(proportion)

gender_line_plot(pub_authors_w_prop, 50, label_pos[1], label_pos[2], label_pos[3]) + 
  labs(x = "Year Submitted", y = "Proportion of All Authors",
       caption = "Proportion of unique individuals that submitted manuscripts each year. Each person is counted once per year")

