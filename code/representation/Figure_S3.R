

#A. Proportion of middle authors over time: submitted & published----
Fig_S3A <- plot_sub_v_pub_time("sub_mid_auth", "pub_mid_auth")

#B. Proportion of last authors over time: submitted & published----
Fig_S3B <- plot_sub_v_pub_time("sub_last_auth", "pub_last_auth")

#C & D.  do men collaborate with women authors?----
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
  select(random.manu.num, num.authors, prop.fem.auth, gender, journal) %>% 
  distinct() %>% 
  mutate(norm.fem = prop.fem.auth/num.authors)

Fig_S3C <- ggplot(collab_data)+
  geom_freqpoly(aes(x = prop.fem.auth, color = gender), size = 1)+
  scale_color_manual(labels = gen_ed_labels, values = gen_ed_colors)+
  labs(x = "Proportion of Women Authors", 
       y = "Number of Papers",
       color = "Corres Auth\nGender")+
  my_theme_leg_horiz+
  theme(legend.position = c(0.8, 0.8))

Fig_S3D <- ggplot(collab_data, aes(x = as.numeric(num.authors), 
                        y = as.numeric(prop.fem.auth)))+
  stat_density_2d(aes(fill = ..level..), 
                  geom = "polygon")+
  coord_cartesian(xlim = c(0, 15))+
  labs(x = "Number of Authors",
       y = "Proportion of\nWomen Authors",
       fill = "Density")+
  my_theme_leg_horiz+
  theme(legend.position = c(0.8, 0.8))

#make figure----
row1 <- plot_grid(Fig_S3A, Fig_S3B, labels = c('A', 'B'), 
                  label_size = 18, nrow = 1)

row3 <- plot_grid(Fig_S3C, Fig_S3D, labels = c('C', 'D'), 
                  label_size = 18, nrow = 2)

plot_grid(row1, row3, nrow = 1, rel_heights = c(2, 1))

ggsave("Figure_S2.png", device = 'png', 
       path = '../submission/', width = 9, height = 12)
