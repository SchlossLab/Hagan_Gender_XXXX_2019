#generate component graphs of the "author figure"

#A. Proportion of unique authors each year: unclear/men/women----
all_authors_w_prop <- map_dfr(years, function(x){
  get_prop_by_yr(x, uniq_author_data, "gender", "All")})

#figure out which year is the last & isolate the proportion values
text_values <- get_gen_prop_text(all_authors_w_prop, 3, "gender")

max_value <- get_ymax(all_authors_w_prop) 

#line plot of all journals combined by year
Figure_3B <- gender_line_plot(all_authors_w_prop, max_value, 
                 text_values[1,2], text_values[2,2], text_values[3,2]) + 
  labs(x = "Year\n", y = "\nProportion of Authors")

#B. Author proportion from US inst types
Figure_3A <- summ_US_stats %>% 
  filter(role == "author") %>% 
  ggplot()+
  geom_col(aes(fill = gender, y = percent, x = US.inst.type),
           position = "dodge")+
  coord_flip()+
  scale_fill_manual(values = gen_ed_colors)+
  labs(x = "\nU.S. Institution Type", y = "Percent of Gender\n")+
  my_theme_horiz

source("../code/representation/prop_women_coauth.R")#author C & D

row_1 <- plot_grid(Figure_3A, labels = c('A'), label_size = 18)

row_2 <- plot_grid(Figure_3B, labels = 'B', label_size = 18)

row_3 <- plot_grid(author_C, author_D,
                   labels = c('C', 'D'), 
                   label_size = 18, nrow = 1)

plot_grid(row_1, row_2, row_3, nrow = 3)

ggsave("Figure_3.png", device = 'png', 
       path = '../submission/', width = 9, height = 12)
