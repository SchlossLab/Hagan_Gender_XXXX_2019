#plot the proportion of published & submitted papers at each journal by gender

#first authors
figure_S2A <- plot_sub_v_pub_j_time("sub_first_auth", "pub_first_auth")

#corresponding authors
figure_S2B <- plot_sub_v_pub_j_time("sub_corres_auth", "pub_corres_auth")

plot_grid(figure_S2A, figure_S2B,
          labels = c('A', 'B'), label_size = 18)

ggsave("Figure_S2.png", device = 'png', 
       path = '../submission/', width = 12, height = 9)
