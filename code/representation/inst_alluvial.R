library(ggalluvial)

inst_role_levels <- c("author", "reviewer", "editor")

inst_role <- inst_stats_data %>%  
  distinct() %>% 
  group_by(random.person.id, US.inst.type, gender, role) %>% 
  summarise(n =n()) %>% 
  spread(., key = role, value = n) %>% 
  mutate_if(is.integer, recode_role) %>%
  #filter(author == "yes") %>%
  gather(., key = role, value = status, -c(random.person.id, gender, US.inst.type)) %>% 
  mutate(role = factor(role, levels = inst_role_levels))

inst_alluv_df <- map_df(inst_role_levels, function(x){
  inst_role %>% filter(role == x)})

#inst_alluv_df$role <- factor(inst_alluv_df$role, levels = role_levels) 
inst_alluv_plot <- inst_alluv_df %>% group_by(gender) %>% 
  ggplot(.,
       aes(x = role, stratum = status, alluvium = random.person.id, 
           fill = US.inst.type)) +
  facet_wrap(~gender, ncol = 1)+
  scale_fill_brewer(type = "qual", palette = "Accent") +
  geom_flow(stat = "alluvium", lode.guidance = "rightleft") +
  geom_stratum()+
  scale_x_discrete(breaks = c("author", "reviewer", "editor"),
                   labels = c("Author", "Reviewer", "Editor"))+
  labs(x = "Role in Publishing", fill = "Filled Role")+
  my_theme_leg

ggsave("inst_alluv.png", device = 'png', 
       path = 'submission/', width = 12, height = 9)

group_test <- inst_stats_data %>% group_by(US.inst.type, role, gender) %>% 
  count() %>% ungroup() %>% 
  spread(., key = role, value = n)

group_test$role <- factor(group_test$role, 
                          levels = c("author", "reviewer", "editor"))

plot <- ggplot(inst_alluv_df,
       aes(y = US.inst.type, x = role, 
           stratum = status, alluvium = random.person.id, )) +
  geom_alluvium(aes(fill = US.inst.type))+
  facet_wrap(~gender, ncol = 1)+
#  scale_fill_brewer(type = "qual", palette = "Accent") +
  geom_flow(stat = "alluvium", lode.guidance = "rightleft") +
  geom_stratum()
#  scale_x_discrete(breaks = c("junior author", "senior author", 
#                              "potential.reviewer", "reviewer", "editor"),
#                   labels = c("Junior Author", "Senior Author",
#                              "Potential Reviewer", "Reviewer", "Editor"))+
#  labs(x = "Role in Publishing", fill = "Filled Role")+
#  my_theme_leg

ggsave("inst_alluv.png", device = 'png', 
       path = 'submission/', width = 12, height = 9)


test <- inst_stats_data %>% 
  mutate(id = row_number()) %>% 
  select(-random.person.id)

test_auth <- group_test %>% 
  filter(role == "author") %>% 
  spread(key = role, value = n)
  
  mutate(author = "Yes") %>%
  rename(author_n = "n") %>% 
  select(-role)

test_rev <- group_test %>% 
  filter(role == "reviewer") %>% 
  mutate(reviewer = "Yes") %>% 
  rename(reviewer_n = "n") %>% 
  select(-role)

test_ed <- group_test %>% 
  filter(role == "editor") %>% 
  mutate(editor = "Yes") %>%
  rename(editor_n = "n") %>% 
  select(-role)

test2 <- left_join(test_auth, test_rev,
                   by = c("gender", "US.inst.type")) %>% 
  left_join(., test_ed,
            by = c("gender", "US.inst.type"))

ggplot(group_test,
       aes(y = n, axis1 = US.inst.type, axis2 = role)) +
  geom_alluvium(aes(fill = gender))+
  #facet_wrap(~US.inst.type, ncol = 1)+
  geom_stratum()+
  #geom_text(stat = "stratum", 
  #          label.strata = TRUE)+
  scale_x_discrete(limits = c("Institution", "Role"))+
  labs(y = "N")
