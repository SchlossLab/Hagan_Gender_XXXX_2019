#generate component graphs of the "author figure"

#A. Author proportion from US inst types
Fig_4A_data <- n2_US_stats %>% 
  filter(role == "author") %>% 
  left_join(., sum_inst_role, by = c("role", "US.inst.type")) %>% 
  mutate(US.inst.type = paste0(US.inst.type, " (N=", total, ")"))

Fig_4A <- Fig_4A_data %>% 
  ggplot(aes(fill = gender, y = percent, 
             x = fct_reorder(US.inst.type, desc(total))))+
  geom_col()+
  coord_flip()+
  scale_fill_manual(labels = gen_labels, values = gen_colors)+
  labs(x = "\n", y = "Percent of Gender\n",
       fill = "Gender")+
  my_theme_leg_horiz

#B. Proportion of unique authors each year: unclear/men/women----
all_authors_w_prop <- map_dfr(years, function(x){
  get_prop_by_yr(x, uniq_author_data, "gender", "All")})

#figure out which year is the last & isolate the proportion values
text_values <- get_gen_prop_text(all_authors_w_prop, 3, "gender")

max_value <- get_ymax(all_authors_w_prop) 

#line plot of all journals combined by year
Fig_4B <- gender_line_plot(all_authors_w_prop, max_value) + 
  labs(x = "Year\n", y = "\nProportion of Authors", color = "Gender") +
  my_theme_horiz

#C. Proportion of men/women first authors over time: submitted & published----
Fig_4C <- plot_sub_v_pub_time("sub_first_auth", 
                              "pub_first_auth", FALSE)+
  my_theme_leg_horiz +
  theme(legend.position = c(0.8, 0.3))

#D. Proportion of men/women corresponding authors over time: submitted & published----
Fig_4D <- plot_sub_v_pub_time("sub_corres_auth", 
                              "pub_corres_auth", FALSE)+
  my_theme_horiz

#make figure----
row_1 <- plot_grid(Fig_4A, labels = 'A', label_size = 18)

row_2 <- plot_grid(Fig_4B, Fig_4C, Fig_4D,
                   labels = c('B', 'C', 'D'), 
                   label_size = 18, nrow = 3)

plot_grid(row_1, row_2, nrow = 2, rel_heights = c(1, 2))

ggsave("Figure_4.png", device = 'png', 
       path = 'submission', width = 9, height = 12)
