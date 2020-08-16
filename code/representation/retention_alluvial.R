#library(ggalluvial)

role_levels <- c("junior author", "senior author", "potential.reviewer", "reviewer", "editor")

gender_role <- data %>% filter(gender != "none") %>% 
  mutate(role = case_when(
    role == "author" & 
      author.corres == "FALSE" & author.last == "FALSE" ~ "junior author",
    role == "author" & 
      (author.corres == "TRUE" | author.last == "TRUE") ~ "senior author",
    role == "senior.editor" ~ "editor",
    TRUE ~ role
  )) %>% 
  select(random.person.id, gender, role) %>% 
  distinct() %>% 
  group_by(random.person.id, gender, role) %>% 
  summarise(n =n()) %>% 
  spread(., key = role, value = n) %>% 
  mutate_if(is.integer, recode_role) %>%
  filter(`junior author` == "yes") %>% 
  gather(., key = role, value = status, -c(random.person.id, gender)) %>% 
  mutate(role = factor(role, levels = role_levels)) %>% 
  filter(!is.na(role))

alluv_df <- map_df(role_levels, function(x){
  gender_role %>% filter(role == x)})

percent_retent <- alluv_df %>% 
  group_by(gender, role, status) %>%
  tally() %>%
  mutate(p = get_percent(n, sum(n))) %>%
  ungroup() %>% 
  filter(status == "yes")

#dropped plot----
#alluv_test$role <- factor(alluv_test$role, levels = role_levels) 
#retention_plot <- alluv_df %>% group_by(gender) %>% 
#ggplot(.,
#       aes(x = role, stratum = status, alluvium = random.person.id, 
#           fill = status)) +
#  facet_wrap(~gen_ed_facet(gender), ncol = 1)+
#  scale_fill_brewer(type = "qual", palette = "Accent") +
#  geom_flow(stat = "alluvium", lode.guidance = "rightleft") +
#  geom_stratum()+
#  scale_x_discrete(breaks = c("junior author", "senior author", 
#                              "potential.reviewer", "reviewer", "editor"),
#                   labels = c("Junior Author", "Senior Author",
#                              "Potential Reviewer", "Reviewer", "Editor"))+
#  labs(x = "Role in Publishing", fill = "Filled Role")+
#  my_theme_leg
#
#ggsave("Figure_8.png", device = 'png', 
#       path = '../submission/', width = 12, height = 9)