#generate plots of all editor GDP over time
# 1) load_data.R sourced data & library(lubridate) 
# 2) get_plot_options.R 
# 3) analysis_functions.R

#----------------------------------------------------------

#senior_editors----
senior_ed %>% 
  filter(journal %in% mjournals) %>%
  select(random.person.id, GDP) %>% 
  distinct() %>% 
  geographic_bar_plot(.)+
  coord_flip() +
  labs(x = "GDP", y ="Number of Senior Editors")

ggsave(filename = "sen_ed_gdp_prop.png", 
       path = "results/geographic/figures")

sen_ed_w_prop_geo <- map_dfr(years, function(x){
  get_prop_by_yr(x, senior_ed, "GDP", "All")
}) 

gen_prop_value_geo <- get_gen_prop_text(sen_ed_w_prop_geo, 4, "GDP")

geographic_line_plot(sen_ed_w_prop_geo, 100) + 
  labs(x = "Year", y = "Proportion of Senior Editors")

ggsave(filename = "sen_ed_gdp_line.png", 
       path = "results/geographic/figures")

j_sen_ed_prop_geo <- map_dfr(years, function(x){
  get_prop_by_yr(x, senior_ed, "GDP", "Each")})

max_journ_value_geo <- get_ymax(j_sen_ed_prop_geo)

j_sen_ed_prop_geo %>% 
  filter(GDP != "NA") %>% 
  geographic_line_plot(., max_journ_value)+
  scale_y_continuous(breaks = c(0, 25, 50, 75, 100))+
  labs(x = "Years", y = "Proportion of Senior Editors", linetype = "GDP")

ggsave(filename = "sen_ed_j_gdp_line.png", 
       path = "results/geographic/figures")

#editors----

editors %>% 
  select(random.person.id, GDP) %>% 
  distinct() %>% 
  geographic_bar_plot(editors, editors$GDP)+
  coord_flip() +
  labs(x = "GDP", y ="Number of Editors")

ggsave(filename = "editor_gdp_prop.png", 
       path = "results/geographic/figures")

ed_w_prop <- map_dfr(years, function(x){
  get_prop_by_yr(x, editors, "GDP", "All")
}) 

geographic_line_plot(ed_w_prop, 100)+ 
  labs(x = "Year", y = "Proportion of Editors")

ggsave(filename = "editor_gdp_geographic_line.png", 
       path = "results/geographic/figures")

j_ed_prop <- map_dfr(years, function(x){
  get_prop_by_yr(x, editors, "GDP", "Each")})

max_journ_value <- get_ymax(j_ed_prop)

j_ed_prop %>% 
  filter(GDP != "NA") %>% 
  geographic_line_plot(j_ed_prop, max_journ_value)+
  scale_y_continuous(breaks = c(0, 25, 50, 75, 100))+
  labs(x = "Year", y = "Proportion of Editors", linetype = "GDP")

colnames(j_ed_prop)
ggsave(filename = "editor_gdp_j_geo_line.png", 
       path = "results/geographic/figures")