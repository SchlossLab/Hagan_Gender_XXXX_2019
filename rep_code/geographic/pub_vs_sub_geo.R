#compare submissions by region

#unique manuscripts
sub_author_data %>% 
  mutate(region = fct_explicit_na("none")) %>% #removing implicit NA
  select(-random.manu.num) %>% 
  distinct() %>%
  group_by(region) %>%
  geographic_bar_plot(., "region")

ggsave(device = png, filename = paste0(name, "pub_vs_sub_geo.png"),
       path = "results/geographic/figures")

pub_author_data %>% 
  select(-random.manu.num) %>% 
  distinct() %>% 
  geographic_bar_plot(., "region")+
  facet_wrap(~journal, scales = "free_y")

ggsave(device = png, filename = paste0(name, "pub_vs_sub_geo.png"),
       path = "results/geographic/figures")

#
pub_authors_w_prop <- map_dfr(journals, function(x){
  map_dfr(years, function(y){
    get_prop_by_yr(y, pub_author_data, region, x)}
  )}) 

label_pos <- pub_authors_w_prop %>% filter(year == "2018") %>%
  pull(proportion)

geographic_line_plot(pub_authors_w_prop, 50, pub_authors_w_prop$region) + 
  labs(x = "Year Submitted", y = "Proportion of All Authors",
       caption = "Proportion of unique individuals that submitted manuscripts each year. Each person is counted once per year")

ggsave(device = png, filename = paste0(name, "pub_vs_sub_geo.png"),
       path = "results/geographic/figures")
