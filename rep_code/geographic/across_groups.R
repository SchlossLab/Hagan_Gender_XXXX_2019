#plot with editor, reviewer, corresponding author pub/sub
#need pub_authors_w_prop, sub_authors_w_prop, ed_w_prop, rev_w_prop
#plotting proportions of each group

#merge dataframes
prop_df <- merge(pub_authors_w_prop, sub_authors_w_prop) %>%
  merge(prop_df, ed_w_prop) %>%
  merge(prop_df, rev_w_prop)

#plot
ggplot(prop_df) +
  geom_line(aes(x = year, y = proportion, linetype = region))+
  coord_cartesian(ylim = c(0, 25))+
  labs(x = "Year", y = "Proportion")+
  scale_x_continuous(limits = c(2012, 2018))+ 
  scale_y_continuous(limits = c(10, 75))+
  my_theme_leg_horiz 