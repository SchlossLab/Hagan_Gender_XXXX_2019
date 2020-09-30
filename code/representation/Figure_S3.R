

#A. Proportion of middle authors over time: submitted & published----
Fig_S3A <- plot_sub_v_pub_time("sub_mid_auth", "pub_mid_auth", TRUE)+
  my_theme_leg_horiz+
  theme(legend.position = c(0.9, 0.2))

#B. Proportion of last authors over time: submitted & published----
Fig_S3B <- plot_sub_v_pub_time("sub_last_auth", "pub_last_auth", TRUE)+
  my_theme_horiz

#make figure----
plot_grid(Fig_S3A, Fig_S3B, labels = c('A', 'B'), 
                  label_size = 18, nrow = 2)

ggsave("Figure_S3.tiff", device = 'tiff', units = "in", scale = 1.5,
       path = 'submission', width = 6, height = 4)
