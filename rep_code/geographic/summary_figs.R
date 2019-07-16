#Generate plots for final summary/takeaway figure

subtract <- function(x, y){
  dif <- as.numeric(x) - as.numeric(y)
}

comp_EJP <- c("Accept, no revision", "Reject")

summary_data <- data %>% 
  filter(author.corres == TRUE) %>% 
  filter(GDP %in% gdp_list) %>% 
  filter(EJP.decision %in% comp_EJP) %>% 
  mutate(year = year(submitted.date)) %>% 
  select(random.person.id, region, GDP, journal, grouped.random, published, EJP.decision) %>% 
  mutate(EJP.decision = fct_recode(EJP.decision, Accept = "Accept, no revision")) %>% 
  distinct() 

#A. Relative acceptance rates by gender across all journals----
summary_A <- summary_data %>% 
  group_by(region, EJP.decision) %>% 
  summarise(n = n()) %>% 
  spread(key = EJP.decision, value = n) %>% 
  mutate(rel_percent_rej = round(Reject/(Reject+Accept), digits = 2)*100) %>% 
  select(-Reject, -Accept) %>% 
  mutate(dif_rel_rej = rel_percent_rej - as.numeric(.[7,2])) %>% 
  as.tibble() %>% 
  mutate(region = fct_relevel(region, region_list)) %>%
  ggplot()+
  geom_col(aes(x = region, y = dif_rel_rej, fill = dif_rel_rej))+
  coord_flip()+
  scale_fill_gradient2(low = "#D55E00", mid='snow3', high = "#0072B2", space = "Lab")+
  labs(x = "Region", y = "Acceptance Rate Disparity",
       caption = "Relative to submissions by gender. Positive (blue) is better performance by men, negative(orange) is better performance by women, lack of a bar is no measurable difference.")+
  my_theme_horiz

#B. Relative acceptance rates by region and journal----
summary_B <- summary_data %>% 
  group_by(journal, region, EJP.decision) %>% 
  summarise(n = n()) %>% 
  spread(key = EJP.decision, value = n) %>% 
  mutate(rel_percent_rej = round(Reject/(Reject+Accept), digits = 2)*100) %>% 
  select(-Reject, -Accept) %>% 
  spread(key = region, value = rel_percent_rej) %>% 
  mutate(`South Asia` = `South Asia` - `North America`,
         `Latin America & Caribbean` = `Latin America & Caribbean` - `North America`,
         `Sub-Saharan Africa` = `Sub-Saharan Africa` - `North America`,
         `Middle East & North Africa` = `Middle East & North Africa` - `North America`,
         `Europe & Central Asia` = `Europe & Central Asia` - `North America`,
         `East Asia & Pacific` = `East Asia & Pacific` - `North America`,
         `North America` = `North America` - `North America`) %>% 
  gather(`South Asia`:`North America`, key = region, value = dif_rel_rej) %>% 
  ggplot()+
  geom_col(aes(x = region, y = dif_rel_rej, fill = dif_rel_rej))+
  facet_wrap(~journal)+
  coord_flip()+
  scale_fill_gradient2(low = "#D55E00", mid='snow3', high = "#0072B2", space = "Lab")+
  labs(x = "Region", y = "Acceptance Rate Disparity",
       fill = "Overperforming Gender", 
       caption = "Relative to submissions by each region. Positive (blue) is better performance by North America, negative(orange) is better performance by that region, lack of a bar is no measurable difference.")+
  my_theme_leg

#C. Relative acceptance rates by gender across all journals----
summary_C <- summary_data %>% 
  group_by(GDP, EJP.decision) %>% 
  summarise(n = n()) %>% 
  spread(key = EJP.decision, value = n) %>% 
  mutate(rel_percent_rej = round(Reject/(Reject+Accept), digits = 2)*100) %>% 
  select(-Reject, -Accept) %>% 
  mutate(dif_rel_rej = rel_percent_rej - as.numeric(.[3,2])) %>% 
  as.tibble() %>% 
  mutate(GDP = fct_relevel(GDP, gdp_list)) %>%
  ggplot()+
  geom_col(aes(x = GDP, y = dif_rel_rej, fill = dif_rel_rej))+
  coord_flip()+
  scale_fill_gradient2(low = "#D55E00", mid='snow3', high = "#0072B2", space = "Lab")+
  labs(x = "Economy", y = "Acceptance Rate Disparity",
       caption = "Relative to submissions by economy type. Positive (blue) is better performance by submissions from high economy, negative(orange) is better performance by that region, lack of a bar is no measurable difference.")+
  my_theme_horiz

#D. Relative acceptance rates by region and journal----
summary_D <- summary_data %>% 
  group_by(journal, GDP, EJP.decision) %>% 
  summarise(n = n()) %>% 
  spread(key = EJP.decision, value = n) %>% 
  mutate(rel_percent_rej = round(Reject/(Reject+Accept), 
                                 digits = 2)*100) %>% 
  select(-Reject, -Accept) %>% 
  spread(key = GDP, value = rel_percent_rej) %>% 
  mutate(Low = Low - High,
         `Lower Middle` = `Lower Middle` - High,
         `Upper Middle` = `Upper Middle` - High,
         High = High - High) %>% 
  gather(Low:High, key = GDP, value = dif_rel_rej) %>% 
  ggplot()+
  geom_col(aes(x = journal, y = dif_rel_rej, fill = dif_rel_rej))+
  facet_wrap(~GDP)+
  coord_flip()+
  scale_fill_gradient2(low = "#D55E00", mid='snow3', high = "#0072B2", space = "Lab")+
  labs(x = "Economy", y = "Acceptance Rate Disparity",
       fill = "Overperforming Economy", 
       caption = "Relative to submissions by each economy. Positive (blue) is better performance by High economies, negative(orange) is better performance by that economy, lack of a bar is no measurable difference.")+
  my_theme_leg