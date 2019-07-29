

auth_data <- data %>% 
  filter(role == "author") %>% 
  select(random.manu.num, num.authors, random.person.id, gender)

uniq.manu <- auth_data %>% pull(random.manu.num) %>% unique()

author_ratio <- map_dfr(uniq.manu, function(x){
  auth_data %>% 
    filter(random.manu.num == x) %>% distinct() %>% 
    mutate(if.female = if_else(gender == "female", 1, 0),
           prop.fem.auth = sum(if.female)/num.authors) %>% 
    select(random.manu.num, prop.fem.auth, num.authors) %>% distinct()
})

collab_data <- left_join(bias_data, author_ratio, by = c("random.manu.num", "num.authors")) %>% 
  select(random.manu.num, num.authors, prop.fem.auth, gender, journal,
         region, GDP) %>% 
  distinct() %>% 
  mutate(norm.fem = prop.fem.auth/num.authors)

author_D <- ggplot(collab_data)+
  geom_freqpoly(aes(x = prop.fem.auth, color = gender), size = 1)+
  scale_color_manual(labels = gen_ed_labels, values = gen_ed_colors)+
  labs(x = "Proportion of Women Authors", 
       y = "Number of Papers",
       color = "Corres Auth\nGender")+
  my_theme_leg_horiz+
  theme(legend.position = c(0.8, 0.8))

author_E <- ggplot(collab_data, aes(x = as.numeric(num.authors), 
                        y = as.numeric(prop.fem.auth)))+
  stat_density_2d(aes(fill = ..level..), 
                  geom = "polygon")+
  coord_cartesian(xlim = c(0, 15))+
  labs(x = "Number of Authors",
       y = "Proportion of\nWomen Authors")+
  my_theme_horiz
