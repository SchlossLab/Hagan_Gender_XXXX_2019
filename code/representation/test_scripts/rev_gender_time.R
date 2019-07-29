#plotting the break down of all reviewers by gender:
# 1. generates a bar plot with the total count of unique reviewers over the full time period
# 2. generates a df of the unique individuals per year in that reviewer role
# 3. Plots 2 as a line plot
# 4. generates a df of the unique individuals per year in that reviewer role by journal 
# 5. plots 4 as a grid of line plots, faceted by journal
# 6. saves each of the figures with a name that includes plot type and df

#setup ----
this_df <- get(each_rev_type) #pull this df from the global environment

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
this_df %>% 
  select(-year) %>% distinct() %>% 
  gender_bar_plot(.)+
  labs(x = "Presenting Gender", 
       y = paste("Number of", rev_type))

ggsave(filename = paste0(name, "_total.png"), path = "results/gender/figures")

#print("barplot")
#break out reviewer type by gender and year for all journals combined ----
all_rev_w_prop <- map_dfr(years, function(x){
  get_prop_by_yr(x, this_df, "gender", "All")}) #%>% filter(gender == "female") %>% arrange(year)

#figure out which year is the last & isolate the proportion values
text_values <- get_gen_prop_text(all_rev_w_prop, 3, "gender")

max_value <- get_ymax(all_rev_w_prop) 

#line plot of all journals combined by year
gender_line_plot(all_rev_w_prop, max_value, 
                 text_values[1,2], text_values[2,2], text_values[3,2]) + 
  labs(x = "Year", y = paste("Proportion of", rev_type),
       caption = paste("Proportion of", str_to_lower(rev_type), "for submitted manuscripts each year. Each person is counted once per year"))

ggsave(filename = paste0(name, "_gender_time_line.png"), 
       path = "results/gender/figures")

#print("line plot by yr")
#
j_rev_w_prop <- map_dfr(years, function(x){
    get_prop_by_yr(x, this_df, "gender", "Each")})

max_journ_value <- get_ymax(j_rev_w_prop)

j_rev_w_prop %>% 
  filter(gender != "NA") %>% 
  j_gen_line_plot(., max_journ_value) + 
  labs(x = "Year", y = paste("Proportion of", rev_type),
       linetype = "Gender",
       caption = paste("Proportion of", str_to_lower(rev_type), 
"for submitted manuscripts each year. Each person is counted once per year"))

ggsave(filename = paste0(name, "_j_gen_time_line.png"), 
       path = "results/gender/figures")
#print("line plot by journal")