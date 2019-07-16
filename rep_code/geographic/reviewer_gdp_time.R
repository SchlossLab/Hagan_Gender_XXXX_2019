#plotting the break down of all reviewers by GDP:
# 1. generates a bar plot with the total count of unique reviewers over the full time period
# 2. generates a df of the unique individuals per year in that reviewer role
# 3. Plots 2 as a line plot
# 4. generates a df of the unique individuals per year in that reviewer role by journal 
# 5. plots 4 as a grid of line plots, faceted by journal
# 6. saves each of the figures with a name that includes plot type and df

#generate dataset of all reviwer origin over time - requires data & lubridate from load_data.R
#setup - pull from this_df to get unique reviewers

#setup ----
data <- get(each_rev_type) #pull this df from the global environment

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
gdp_data <- data %>% 
  gdp_data %>%
  select(year, random.person.id, GDP) %>% distinct() %>%
  group_by(GDP) %>%
  fct_explicit_na(gdp_data$GDP, na_level = "NA") %>%
  geographic_bar_plot(gdp_data, gdp_data$GDP)+
  coord_flip()+
  labs(x = "Origin", 
       y = paste("Number of", rev_type))

ggsave(filename = paste0(name, "_gdp_unique_reviewers_total.png"), path = "results/geographic/figures")

#print("barplot")
#break out reviewer type by GDP and year for all journals combined ----
all_rev_w_prop <- map_dfr(years, function(x){
  get_prop_by_yr(x, data, "GDP", "All")}) 

#figure out which year is the last & isolate the proportion values
text_values <- get_gen_prop_text(all_rev_w_prop, 4, "GDP")

max_value <- get_ymax(all_rev_w_prop) 

#line plot of all journals combined by year
geographic_line_plot(all_rev_w_prop, max_value) + 
  labs(x = "Year", y = paste("Proportion of", rev_type),
       caption = paste("Proportion of", str_to_lower(rev_type), "for submitted manuscripts each year. Each person is counted once per year"))

ggsave(filename = paste0(name, "_gdp_time_line.png"), 
       path = "results/geographic/figures")
