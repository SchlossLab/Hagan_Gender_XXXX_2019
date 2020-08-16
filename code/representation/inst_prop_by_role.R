
#compare inst composition by role
inst_stats_data %>% 
  #filter(role == "editor" & gender != "none") %>% 
  ggplot()+
  geom_bar(aes(x = role, fill = US.inst.type), position = "fill")+
  my_theme_leg_horiz

#what is the percent of women at each institution type vs senior authors -- how does that compare to reviewers/editors
role_inst_levels <- c("author", "reviewer", "editor")

fem_inst_role <- n2_US_stats %>% 
  filter(gender == "female") %>% 
  mutate(role = factor(role, role_inst_levels, labels = role_inst_levels)) %>% 
  ggplot()+
  geom_col(aes(y= percent, x = fct_reorder(US.inst.type, percent), fill = role), position = "dodge")+
  coord_flip()+
  scale_fill_discrete(breaks = c("author", "reviewer", "editor"), 
                      labels = c("senior author", "reviewer", "editor"))+
  labs(y = "Percent Women", x = "")+
  my_theme_leg_horiz


