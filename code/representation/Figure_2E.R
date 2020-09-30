
#C. Proportion of editors at each journal over time by gender---- 
j_ed_prop <-  map_dfr(years, function(x){
  get_prop_by_yr(x, editor_data, "gender", "Each")})

# Weighted editor proportion
ed_manu_prop_j <- map_df(years, function(x){
  
  map_dfr(journals, function(j){#map through the following function for each journal
    
    tryCatch(
      editor_data %>% filter(journal == j) %>% #select journal
        filter(year == x) %>% #restrict to single year
        select(gender, grouped.random, year, random.person.id) %>% distinct() %>% 
        group_by(gender, random.person.id, grouped.random) %>% summarise(n = n()) %>% #calculate number of each gender in that year
        group_by(gender, random.person.id) %>% summarise(n = sum(n)) %>%
        group_by(gender) %>% summarise(weighted_n = sum(n)) %>% 
        mutate(weighted_proportion = get_percent(weighted_n, sum(weighted_n))) %>% #add column calculating the proportions for the year, requires analysis_functions.R
        cbind(year = x, journal = j, .), #add column specifying the year
      error = function(e) {bind_cols(year = x, journal = j, gender = "NA", weighted_n = as.numeric("0"), weighted_proportion = as.numeric("0"))}) #if nothing present, return NA value in a dataframe 
  })
})

#merge into single dataset & tidy
ed_prop_j <- full_join(j_ed_prop, ed_manu_prop_j, 
                       by = c("year", "gender", "journal")) %>%
  distinct() %>% 
  filter(gender != "NA") %>% 
  gather(proportion, weighted_proportion, key = type, value = proportion)

Fig_2E <- ggplot(ed_prop_j) + 
  geom_line(aes(x = year, y = proportion, linetype = type, color = gender))+
  facet_wrap(~journal, nrow = 2)+
  coord_cartesian(ylim = c(0, 100))+
  scale_color_manual(breaks = gen_ed_levels, 
                     labels = gen_ed_labels, 
                     values = gen_ed_colors)+
  scale_linetype_manual(breaks = c("proportion", "weighted_proportion"), labels = c("Individuals", "Manuscripts Handled"), values = c("solid", "dashed"))+
  labs(x = "Year", y = "\nPercent of Editors", 
       linetype = "Type:", color = "Gender:")+
  my_theme_leg+
  theme(legend.position="top")

#generate figure 2----
Fig_2ab <- plot_grid(Figure_2A, Fig_2B, 
          labels = c('A', 'B'), label_size = 18,
          nrow = 1)

Fig_2de <- plot_grid(Figure_2C, Fig_2D,
                       labels = c('D', 'E'), label_size = 18,
                       nrow = 1)

plot_grid(Fig_2ab, Fig_2E, Fig_2de, labels = c('', 'C', ''), label_size = 18,
          nrow = 3, rel_heights = c(1, 1.5, 1))

ggsave("Figure_2.tiff", device = 'tiff', units = "in", scale = 1.75,
       path = 'submission', width = 6.8, height = 6.8)


