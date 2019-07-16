#plotting the break down of all reviewers by origin:
# 1. generates a bar plot with the total count of unique reviewers over the full time period
# 2. generates a df of the unique individuals per year in that reviewer role
# 3. Plots 2 as a line plot
# 4. generates a df of the unique individuals per year in that reviewer role by journal 
# 5. plots 4 as a grid of line plots, faceted by journal
# 6. saves each of the figures with a name that includes plot type and df

#generate dataset of all reviwer origin over time - requires data & lubridate from load_data.R
#setup - pull from this_df to get unique reviewers

#setup ----
rev_data <- get(each_rev_type) #pull this df from the global environment

name <- paste0(each_rev_type) #isolate the name of the df as a string

print(name)

rev_type <- case_when( #identify reviewers being examined
  str_detect(name, "reviewer") ~ "Reviewers",
  str_detect(name, "pot_rev") ~ "Potential Reviewers"
)

#manu_type <- case_when( #identify type of manuscript (if any)
#  str_detect(name, "sub") ~ "Submitting",
#  str_detect(name, "pub") ~ "Publishing",
#  TRUE ~ "Unique"
#)

#print(manu_type)
#total unique reviewers across dataset ----
fil_rev_data <- rev_data %>% 
  filter(GDP %in% gdp_list) %>% 
  select(year, random.person.id, region) %>% distinct() %>% 
  group_by(region)

  geographic_bar_plot(fil_rev_data, fil_rev_data$region)+
  coord_flip()+
  labs(x = "Origin", 
       y = paste("Number of", rev_type))

ggsave(filename = paste0(name, "unique_reviewers_total.png"), path = "results/geographic/figures")

#print("barplot")
#break out reviewer type by region and year for all journals combined ----
all_rev_w_prop <- map_dfr(years, function(x){
  rev_data %>% filter(GDP %in% gdp_list) %>% 
  get_prop_by_yr(x, ., "region", "All")}) %>%
  mutate(region =
           fct_explicit_na(region,
           na_level = "None"))

#try following code on all_rev_w_prop
#df %>% mutate_if(is.factor,
#fct_explicit_na,
#na_level = "none")

all_rev_w_prop <- all_rev_w_prop %>% mutate_if(is.factor,
                             fct_explicit_na,
                             na_level = "none")

#figure out which year is the last & isolate the proportion values
text_values <- get_gen_prop_text(all_rev_w_prop, 7, "region")

max_value <- get_ymax(all_rev_w_prop) 

#line plot of all journals combined by year
geographic_line_plot(all_rev_w_prop, max_value, "region") + 
  labs(x = "Year", y = paste("Proportion of", rev_type),
       caption = paste("Proportion of", str_to_lower(rev_type), "for submitted manuscripts each year. Each person is counted once per year"))+
  theme(legend.position = "none")+
  annotate(geom = "text", x = 2018, y = 65, label = "North America")+
  annotate(geom = "text", x = 2018, y = 25, label = "Europe & Central Asia")+
  annotate(geom = "text", x = 2018, y = 10, label = "East Asia & Pacific")+
  scale_x_continuous(limits = c(2012, 2018))+ 
  scale_y_continuous(limits = c(10, 75))

#plot should only contain observations for N. America, Europe and C. Asia, and E. Asia and Pacific
ggsave(filename = paste0(name, "_geographic_time_line.png"), 
       path = "results/geographic/figures")
