#generate figures to summarize reviewer data

#A. Number of papers handled by gender -- a weighted proportion
ed_manu_prop <- map_df(years, function(x){
  
  editor_data %>% filter(year == x) %>% #restrict to single year
    select(gender, grouped.random, year, random.person.id) %>% 
    distinct() %>% 
    group_by(gender, random.person.id, grouped.random) %>% summarise(n = n()) %>% #calculate number of each gender in that year
    group_by(gender, random.person.id) %>% summarise(n = sum(n)) %>% 
    group_by(gender) %>% summarise(weighted_n = sum(n)) %>%
    mutate(proportion = get_percent(weighted_n, sum(weighted_n))) %>% #add column calculating the proportions for the year, requires analysis_functions.R
    cbind(year = x, .) #add year 
})

ed_manu_text <- get_gen_prop_text(ed_manu_prop, 2, "gender") #calc label placement

Fig_2A <- ggplot(ed_manu_prop) + 
  geom_line(aes(x = year, y = proportion, color = gender))+
  coord_cartesian(ylim = c(0, 100))+
  scale_color_manual(breaks = gen_ed_levels, 
                     labels = gen_ed_labels, 
                     values = gen_ed_colors)+
  #annotate(geom = "text", x = 2017, y = ed_manu_text[1,2]+2, label = "Women")+
  #annotate(geom = "text", x = 2017, y = ed_manu_text[2,2]+5, label = "Men")+
  labs(x = "Year\n", y = "\nProportion of\nEditor Workload")+
  my_theme_horiz

#B. Number of papers reviewed by Gender----
Fig_2B <- reviewer_data %>% 
  distinct() %>% #doesn't have the manuscript ids
  group_by(random.person.id, gender) %>% 
  summarise(n = n()) %>% 
  ggplot()+
  geom_boxplot(aes(x = gender, group = gender, y = n, fill = gender))+
  scale_y_log10()+
  #coord_cartesian(ylim = c(0, 8))+
  coord_flip()+
  scale_x_discrete(labels = gen_labels)+
  scale_fill_manual(values = gen_colors)+
  labs(x = "\nReviewer Gender", y = "Number of Manuscripts Reviewed\n")+
  my_theme_horiz  #figure out how to add n of individuals

#Does reviewer acceptance depend on gender of editor/reviewer? -- need potential reviewer dataset
people <- people_data %>% select(-role, -contains("auth"), 
                                 -grouped.random, -random.manu.num) %>%
  distinct() %>% 
  mutate(gender = fct_explicit_na(gender, na_level = "none"))

editor_dat <- data %>% filter(role == "editor") %>% 
  mutate(year = year(submitted.date)) %>% 
  select(year, random.manu.num, gender, journal) %>% 
  filter(!is.na(year)) %>% 
  rename("editor.gender" = "gender") %>% 
  distinct()

authors <- people_data %>% filter(role == "editor") %>% 
  select(random.manu.num, gender) %>%
  rename("editor.gender" = "gender") %>% distinct() 

rev_resp <- read_csv("data/2018_rev_resp_ready.csv") %>% 
  left_join(., people, by = "random.person.id") %>% 
  rename("reviewer.gender" = "gender") %>% 
  left_join(., editor_dat, by = "random.manu.num") %>% 
  filter(!is.na(random.person.id)) %>% 
  filter(editor.gender != "none") %>% 
  filter(status != "Withdrawn") %>% 
  select(-suggested.include, -suggested.exclude, -title) %>% 
  mutate(status = fct_collapse(status, 
                               "No Response" = c("No Response", "Not Needed", "Contacted"),
                               "Not Contacted" = c("Not Contacted", "Not Used")))

#reviewer_D percent of reviewers contacted by editor gender----
ed_contact <- rev_resp %>% 
  mutate(status = fct_collapse(status, 
                               "Contacted" = c("No Response", "Not Needed", "Contacted", "Accepted", "Declined"),
                               "Not Contacted" = c("Not Contacted", "Not Used"))) %>% 
  group_by(editor.gender, reviewer.gender, status) %>%
  summarise(n = n()) %>% 
  spread(key = status, value = n) %>% 
  mutate(percent_cont = get_percent(Contacted, (Contacted + `Not Contacted`)))

#reviewer_D <- ed_contact %>% 
#  ggplot()+
#  geom_col(aes(x = editor.gender, y = percent_cont,
#               fill = reviewer.gender), alpha = 0.65,
#           position = "dodge")+
#  scale_fill_manual(values = gen_colors)+
#  gen_x_replace+
#  labs(x = "Editor Gender", y = "\nPercent Contacted")+
#  my_theme_horiz

#reviewer_E response of reviewers by gender
f_ed_resp <- rev_resp %>% 
  filter(status != "Not Contacted") %>% 
  filter(editor.gender == "female") %>% 
  group_by(status, reviewer.gender) %>% 
  summarise(n = n()) %>% 
  spread(key = status, value = n) %>% 
  mutate(No_Resp = get_percent(`No Response`, (Accepted + `No Response` + Declined)),
         Accept = get_percent(Accepted, (Accepted + `No Response` + Declined))) %>% 
  mutate(editor.gender = "female") %>% 
  select(-Accepted, -`No Response`, -Declined) %>% 
  gather(No_Resp:Accept, value = Percent, key = Rev.Resp)

m_ed_resp <- rev_resp %>% 
  filter(status != "Not Contacted") %>% 
  filter(editor.gender == "male") %>% 
  group_by(status, reviewer.gender) %>% 
  summarise(n = n()) %>% 
  spread(key = status, value = n) %>% 
  mutate(No_Resp = get_percent(`No Response`, (Accepted + `No Response` + Declined)),
         Accept = get_percent(Accepted, (Accepted + `No Response` + Declined))) %>% 
  mutate(editor.gender = "male") %>% 
  select(-Accepted, -`No Response`, -Declined) %>% 
  gather(No_Resp:Accept, value = Percent, key = Rev.Resp)

ed_resp <- rbind(f_ed_resp, m_ed_resp)

Fig_2C <- ed_resp %>% 
  filter(Rev.Resp == "Accept") %>% 
  ggplot()+
  geom_col(aes(x = editor.gender, y = Percent,
               fill = reviewer.gender), 
           position = "dodge")+
  coord_flip(ylim = c(0, 60))+
  #facet_wrap(~ if_else(Rev.Resp == "No_Resp", "No Response", "Accept"))+
  scale_fill_manual(values = gen_colors, 
                    labels = gen_labels)+
  labs(x = "\nEditor", y = "\nPercent of Reviewers",
       fill = "Reviewer Gender")+
  gen_x_replace+
  my_theme_leg_horiz+
  theme(legend.position = c(0.91,0.25))

#generate full figure----
blank <- ggplot()

plot_AB <- plot_grid(Fig_2A, Fig_2B, Fig_2C, nrow = 3,
          labels = c('A', 'B', 'C'), label_size = 18)

ggsave("Figure_2.png", device = 'png', 
     path = 'submission', width = 9, height = 9)
