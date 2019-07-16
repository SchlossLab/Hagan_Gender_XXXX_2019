#generate plots of all editor origin over time
# 1) load_data.R sourced data & library(lubridate) 
# 2) get_plot_options.R 
# 3) analysis_functions.R

#----------------------------------------------------------
  
  #generate & save barplots of total unique editors in ea. position + line plots of each editor type (except EIC) by year & by year faceted by journal
  
  
#   #eics
#   eic_data %>% 
#   gender_bar_plot()+
#   labs(x = "Origin", y ="Number of Editors-in-Chief")
# ggsave(filename = "eic_geo_prop.png", 
#        path = "results/geographic/figures")

#senior_editors----

senior_ed <- senior_ed %>% filter(GDP %in% gdp_list)

senior_ed %>% 
  filter(journal %in% mjournals) %>%
  select(random.person.id, region) %>% 
  distinct() %>% 
  geographic_bar_plot(., "region")+
  coord_flip() +
  labs(x = "Origin", y ="Number of Senior Editors")

ggsave(filename = "sen_ed_geographic_prop.png", 
       path = "results/geographic/figures")

sen_ed_w_prop_geo <- map_dfr(years, function(x){
  get_prop_by_yr(x, senior_ed, "region", "All")
}) 

gen_prop_value_geo <- get_gen_prop_text(sen_ed_w_prop_geo, 7, "region")

geographic_line_plot(sen_ed_w_prop_geo, 100, sen_ed_w_prop_geo$region) + 
  labs(x = "Year", y = "Proportion of Senior Editors")+
  theme(legend.position = "none")+ #hide legend
  annotate(geom = "text", x = 2018, y = 90, label = "North America")+
  annotate(geom = "text", x = 2018, y = 7, label = "Europe & Central Asia")
  
ggsave(filename = "sen_ed_geo_line.png", 
       path = "results/geographic/figures")

j_sen_ed_prop_geo <- map_dfr(years, function(x){
  get_prop_by_yr(x, senior_ed, "region", "Each")})

max_journ_value_geo <- get_ymax(j_sen_ed_prop_geo)

line_prop <- j_sen_ed_prop_geo %>% 
  filter(journal %in% mjournals) %>%
  filter(region != "NA")

  geographic_line_plot(line_prop, max_journ_value_geo, line_prop$region)+
  facet_wrap(~journal)+
  scale_y_continuous(breaks = c(0, 25, 50, 75, 100))+
  labs(x = "Years", y = "Proportion of Senior Editors", linetype = "region") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(filename = "sen_ed_j_geo_line.png", 
       path = "results/geographic/figures")

#editors----

editors <- editors %>% filter(GDP %in% gdp_list)

editors %>% 
  select(random.person.id, region) %>% 
  distinct() %>% 
  geographic_bar_plot(editors, editors$region)+
  coord_flip() +
  labs(x = "Origin", y ="Number of Editors")
  

ggsave(filename = "editor_geographic_prop.png", 
       path = "results/geographic/figures")

ed_w_prop <- map_dfr(years, function(x){
  get_prop_by_yr(x, editors, "region", "All")
}) 

geographic_line_plot(ed_w_prop, 100, "region")+ 
  labs(x = "Year", y = "Proportion of Editors")

ggsave(filename = "editor_geographic_line.png", 
       path = "results/geographic/figures")

j_ed_prop <- map_dfr(years, function(x){
  get_prop_by_yr(x, editors, "region", "Each")})

max_journ_value <- get_ymax(j_ed_prop)

j_ed_prop %>% 
  filter(region != "NA") %>% 
  geographic_line_plot(j_ed_prop, max_journ_value, j_ed_prop$region)+
  facet_wrap(~journal)+
  scale_y_continuous(breaks = c(0, 25, 50, 75, 100))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(x = "Year", y = "Proportion of Editors", linetype = "Region")

ggsave(filename = "editor_j_geo_line.png", 
       path = "results/geographic/figures")

