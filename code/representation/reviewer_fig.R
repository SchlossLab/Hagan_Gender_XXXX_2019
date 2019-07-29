#generate figures to summarize reviewer data

#A. Proportion of Potential Reviewers suggested each Year---
pot_rev_w_prop <- map_dfr(years, function(x){
  get_prop_by_yr(x, pot_rev_data, "gender", "All")})

#figure out which year is the last & isolate the proportion values
text_values <- get_gen_prop_text(pot_rev_w_prop, 3, "gender")

max_value <- get_ymax(pot_rev_w_prop) 

#line plot of all journals combined by year
reviewer_A <- gender_line_plot(pot_rev_w_prop, max_value, 
                 text_values[1,2], text_values[2,2], text_values[3,2]) + 
  labs(x = "Year\n", y = "Proportion of Potential Reviewers")

#B. Number of papers reviewed by Gender----
reviewer_B <- data %>% filter(role == "reviewer") %>% 
  mutate(year = year(submitted.date)) %>% 
  select(year, random.person.id, grouped.random, gender) %>% 
  filter(!is.na(year)) %>% 
  mutate(gender = fct_explicit_na(gender, na_level = "none")) %>% 
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
  labs(x = "\nReviewer Gender", y = "Number of Papers Reviewed\n")+
  my_theme_horiz  #figure out how to add n of individuals

source("code/representation/rev_suggest_gender.R")

plot_grid(reviewer_A, reviewer_B, reviewer_C, reviewer_D,
          labels = c('A', 'B', 'C', 'D'), label_size = 18)

ggsave("Figure_2.png", device = 'png', 
       path = 'submission/', width = 12, height = 9)
