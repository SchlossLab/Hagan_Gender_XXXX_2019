#Generate components of the figure summarizing editor stats

#A. Proportion of editors (editors + senior.editors) at ASM over time by gender & manuscripts handled----

#proportion of individuals each year
ed_w_prop <- map_dfr(years, function(x){
  get_prop_by_yr(x, editor_data, "gender", "All")
})

#proportions of individuals weighted by manuscripts
ed_manu_prop <- map_df(years, function(x){
  
  editor_data %>% filter(year == x) %>% #restrict to single year
    select(gender, grouped.random, year, random.person.id) %>% 
    distinct() %>% 
    group_by(gender, random.person.id, grouped.random) %>% summarise(n = n()) %>% #calculate number of each gender in that year
    group_by(gender, random.person.id) %>% summarise(n = sum(n)) %>% 
    group_by(gender) %>% summarise(weighted_n = sum(n)) %>%
    mutate(weighted_proportion = get_percent(weighted_n, sum(weighted_n))) %>% #add column calculating the proportions for the year, requires analysis_functions.R
    cbind(year = x, .) #add year 
})

#merge proportions & weighted proportions into single table & tidy
ed_prop_ASM <- full_join(ed_w_prop, ed_manu_prop, by = c("year", "gender")) %>% 
  distinct() %>% 
  gather(proportion, weighted_proportion, key = type, value = proportion)

ed_prop_text <- get_gen_prop_text(ed_w_prop, 2, "gender") #calc label placement

Figure_1B <- ggplot(ed_prop_ASM) + 
  geom_line(aes(x = year, y = proportion, linetype = type, color = gender))+
  coord_cartesian(ylim = c(0, 100))+
  scale_color_manual(breaks = gen_levels, labels = NULL, values = gen_colors)+
  scale_linetype_manual(breaks = c("proportion", "weighted_proportion"), labels = c("Individuals", "Manuscripts Handled"), values = c("solid", "dashed"))+
  annotate(geom = "text", x = 2017, y = ed_prop_text[1,2]+2, label = "Women")+
  annotate(geom = "text", x = 2017, y = ed_prop_text[2,2]+8, label = "Men")+
  labs(x = "Year", y = "\nProportion of Editors", linetype = "Type")+
  my_theme_horiz

#A. proportion of editors from US institutions by gender----
Figure_1A <- summ_US_stats %>% 
  filter(role == "editor") %>% 
  ggplot()+
  geom_col(aes(fill = gender, y = percent, x = US.inst.type),
           position = "dodge")+
  coord_flip()+
  scale_fill_manual(values = gen_ed_colors)+
  labs(x = "\nU.S. Institution Type", y = "Percent of Editor Gender")+
  my_theme_horiz

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

editor_C <- ggplot(ed_prop_j) + 
  geom_line(aes(x = year, y = proportion, linetype = type, color = gender))+
  facet_wrap(~journal, nrow = 2)+
  coord_cartesian(ylim = c(0, 100))+
  scale_color_manual(breaks = gen_levels, labels = gen_labels, values = gen_colors)+
  scale_linetype_manual(breaks = c("proportion", "weighted_proportion"), labels = c("Individuals", "Manuscripts Handled"), values = c("solid", "dashed"))+
  labs(x = "Year", y = "\nProportion of Editors", 
       linetype = "Type", color = "Gender")+
  my_theme_leg

#generate full figures----
row1 <- plot_grid(Figure_1A, Figure_1B, 
          labels = c('A', 'B'), label_size = 18)

plot_grid(row1, editor_C,
          labels = c('', 'C'), label_size = 18, nrow = 2)

ggsave("Figure_1.png", device = 'png', 
       path = '../submission/', width = 12, height = 6)
