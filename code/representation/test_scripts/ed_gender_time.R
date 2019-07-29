#generate & save barplots of total unique editors in ea. position + line plots of each editor type (except EIC) by year & by year faceted by journal

#eics
eic_data %>% 
  gender_bar_plot()+
  labs(x = "Presenting Gender", y ="Number of Editors-in-Chief")
ggsave(filename = "eic_gender_prop.png", 
       path = "results/gender/figures")

#senior_editors----
senior_ed %>% 
  select(random.person.id, gender) %>% 
  distinct() %>% 
  gender_bar_plot(.)+
labs(x = "Presenting Gender", y ="Number of Senior Editors")
ggsave(filename = "sen_ed_gender_prop.png", 
       path = "results/gender/figures")

sen_ed_w_prop <- map_dfr(years, function(x){
  get_prop_by_yr(x, senior_ed, "gender", "All")
}) 

gen_prop_value <- get_gen_prop_text(sen_ed_w_prop, 2, "gender")

gender_line_plot(sen_ed_w_prop, 100, gen_prop_value[1,2], 
                 gen_prop_value[2,2], "N") + 
  labs(x = "Year", y = "Proportion of Senior Editors")

ggsave(filename = "sen_ed_gender_line.png", 
       path = "results/gender/figures")

j_sen_ed_prop <- map_dfr(years, function(x){
  get_prop_by_yr(x, senior_ed, "gender", "Each")})

max_journ_value <- get_ymax(j_sen_ed_prop)

j_sen_ed_prop %>% 
  filter(gender != "NA") %>% 
  j_gen_line_plot(., max_journ_value)+
  scale_y_continuous(breaks = c(0, 25, 50, 75, 100))+
  labs(x = "Years", y = "Proportion of Senior Editors", linetype = "Gender")

ggsave(filename = "sen_ed_j_gen_line.png", 
       path = "results/gender/figures")

#editors----
  
editors %>% 
  select(random.person.id, gender) %>% 
  distinct() %>% 
  gender_bar_plot(.)+
  labs(x = "Presenting Gender", y ="Number of Editors")

ggsave(filename = "editor_gender_prop.png", 
       path = "results/gender/figures")

ed_w_prop <- map_dfr(years, function(x){
  get_prop_by_yr(x, editors, "gender", "All")
}) #%>% filter(gender == "female") %>% select(-gender) %>% arrange(year)

ed_prop_text <- get_gen_prop_text(ed_w_prop, 2, "gender")

gender_line_plot(ed_w_prop, 100, ed_prop_text[1,2], 
                 ed_prop_text[2,2], "N")+ 
  labs(x = "Year", y = "Proportion of Editors")

ggsave(filename = "editor_gender_line.png", 
       path = "results/gender/figures")

j_ed_prop <- map_dfr(years, function(x){
  get_prop_by_yr(x, editors, "gender", "Each")})

max_journ_value <- get_ymax(j_ed_prop)

j_ed_prop %>% 
  filter(gender != "NA") %>% 
  j_gen_line_plot(., max_journ_value)+
  scale_y_continuous(breaks = c(0, 25, 50, 75, 100))+
  labs(x = "Year", y = "Proportion of Editors", linetype = "Gender")

ggsave(filename = "editor_j_gen_line.png", 
       path = "results/gender/figures")
