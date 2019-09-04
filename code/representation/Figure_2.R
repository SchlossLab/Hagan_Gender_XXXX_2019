#generate figures to summarize reviewer data

#A. Number of papers handled by gender -- a weighted proportion
ed_manu_prop <- map_df(years, function(x){
  
  editor_data %>% filter(year == x) %>% #restrict to single year
    select(gender, grouped.random, year, random.person.id) %>% 
    distinct() %>% 
    group_by(gender, random.person.id, grouped.random) %>% summarise(n = n()) %>% #calculate number of each gender in that year
    group_by(gender, random.person.id) %>% summarise(n = sum(n)) %>% 
    group_by(gender) %>% summarise(weighted_n = sum(n)) %>%
    mutate(proportion = get_percent(weighted_n, sum(weighted_n))) %>% #add column calculating the proportions for the year, requires analysis_functions.R
    cbind(year = x, .) #add year 
})

ed_manu_text <- get_gen_prop_text(ed_manu_prop, 2, "gender") #calc label placement

Fig_2A <- ggplot(ed_manu_prop) + 
  geom_line(aes(x = year, y = proportion, color = gender))+
  coord_cartesian(ylim = c(0, 100))+
  scale_color_manual(breaks = gen_levels, labels = NULL, values = gen_colors)+
  annotate(geom = "text", x = 2017, y = ed_manu_text[1,2]+2, label = "Women")+
  annotate(geom = "text", x = 2017, y = ed_manu_text[2,2]+5, label = "Men")+
  labs(x = "Year\n", y = "\nProportion of\nEditor Workload")+
  my_theme_horiz

#B. Number of papers reviewed by Gender----
Fig_2B <- reviewer_data %>% 
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
  labs(x = "\nReviewer Gender", y = "Number of Manuscripts Reviewed\n")+
  my_theme_horiz  #figure out how to add n of individuals

source("../code/representation/rev_suggest_gender.R") #reviewer_D, reviewer_E -- increase facet label size or space between facets in E

plot_AB <- plot_grid(Fig_2A, Fig_2B,
          labels = c('A', 'B'), label_size = 18)

plot_CDE <- plot_grid(reviewer_E, reviewer_D,
          labels = c('C', 'D'), label_size = 18)

plot_grid(plot_AB, plot_CDE, nrow = 2, 
          rel_heights = c(1, 2))

ggsave("Figure_2.png", device = 'png', 
     path = '../submission', width = 12, height = 9)
