#Calculate proportion of author types & workloads

#A. Proportion of men/women first authors over time: submitted & published----
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
figure4_A <- f_authors_w_prop %>%  
  ggplot() + 
  geom_line(aes(x = year, y = proportion, 
                linetype = manu.type, color = gender), size = 0.75)+
  coord_cartesian(ylim = c(0, max_value))+
  scale_color_manual(values = gen_ed_colors, 
                     breaks = gen_ed_labels)+
  my_theme_leg_horiz + 
  labs(x = "Year",
       y = "\nProportion of\nFirst Authors",
       linetype = "Manuscript Status",
       color = "Gender")

#B. Proportion of men/women corresponding authors over time: submitted & published----
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
figure4_B <- c_authors_w_prop %>%  
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
       y = "\nProportion of\nCorresponding Authors",
       linetype = "Manuscript Status")


#Calculate percentage point differences----
auth_types <- c("Middle", "Corresponding", "Last", "First")

comp_EJP <- c("Accept, no revision", "Reject")

summary_data <- data %>% 
  filter(gender != "none") %>% 
  filter(EJP.decision %in% comp_EJP) %>% 
  mutate(role = case_when(
    role == "author" & 
      author.corres == "FALSE" & author.last == "FALSE" &
      author.seq != 1 ~ "Middle",
    role == "author" & 
      (author.corres == "TRUE") ~ "Corresponding",
    role == "author" &
      author.last == "TRUE" ~ "Last",
    role == "author" &
      author.seq == 1 ~ "First",
    TRUE ~ role
  )) %>% mutate(year = year(submitted.date)) %>% 
  select(random.person.id, gender, role, journal, grouped.random, published, EJP.decision) %>% 
  filter(role %in% auth_types) %>% 
  mutate(EJP.decision = fct_recode(EJP.decision, Accept = "Accept, no revision")) %>% 
  distinct() 


#C. Relative acceptance rates by gender across all journals----
figure4_C <- summary_data %>% 
  group_by(gender, role, EJP.decision) %>% 
  summarise(n = n()) %>% 
  spread(key = EJP.decision, value = n) %>% 
  mutate(rel_percent_rej = round(Reject/(Reject+Accept), digits = 2)*100) %>% 
  select(-Reject, -Accept) %>% 
  spread(key = gender, value = rel_percent_rej) %>% 
  mutate(dif_rel_rej = female - male) %>%
  ggplot()+
  geom_col(aes(x = role, y = dif_rel_rej, fill = dif_rel_rej))+
  scale_fill_gradient2(low = "#D55E00", mid='snow3', 
                       high = "#0072B2", space = "Lab")+
  labs(x = "Authorship Role", y = "\nDifference in Acceptance Rate")+
  my_theme_horiz

#D. Relative acceptance rates by gender and journal----
figure4_D <- summary_data %>% 
    group_by(journal, gender, role, EJP.decision) %>% 
    summarise(n = n()) %>% 
    spread(key = EJP.decision, value = n) %>% 
    mutate(rel_percent_rej = round(Reject/(Reject+Accept), digits = 2)*100) %>% 
    select(-Reject, -Accept) %>% 
    spread(key = gender, value = rel_percent_rej) %>% 
    mutate(dif_rel_rej = female - male) %>% 
  #mutate(rel_chx = rel_chx - 50) %>% 
  ggplot()+
    geom_col(aes(x = journal, y = dif_rel_rej, fill = dif_rel_rej))+
    facet_wrap(~role)+
    coord_flip()+
  scale_fill_gradient2(low = "#D55E00", mid='snow3', high = "#0072B2", space = "Lab")+
  labs(x = "Journal", y = "Difference in Acceptance Rate",
       fill = "% Point\nDifference")+
  my_theme_leg
  
col_1 <- plot_grid(figure4_A, figure4_B,
                   labels = c('A', 'B'), 
                   label_size = 18, nrow = 2)

col_2 <- plot_grid(figure4_C, figure4_D,
                   labels = c('C', 'D'), 
                   label_size = 18, nrow = 1)

plot_grid(col_1, col_2, nrow = 2, rel_heights = c(1.5, 1))

ggsave("Figure_4.png", device = 'png', 
       path = 'submission/', width = 12, height = 12)
