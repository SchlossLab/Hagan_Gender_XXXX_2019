#plotting the break down of all submitting authors by gender:
# 1. generates a bar plot with the total count of unique authors over the full time period
# 2. generates a df of the unique individuals per year in that author role
# 3. Plots 2 as a line plot
# 4. generates a df of the unique individuals per year in that author role by journal 
# 5. plots 4 as a grid of line plots, faceted by journal
# 6. saves each of the figures with a name that includes plot type and df

#setup ----
this_df <- get(each_auth_type) #pull this df from the global environment

name <- paste0(each_auth_type) #isolate the name of the df as a string

print(name)

auth_type <- case_when( #identify authors being examined
  str_detect(name, "first") ~ "First",
  str_detect(name, "last") ~ "Last",
  str_detect(name, "corres") ~ "Corresponding",
  str_detect(name, "mid") ~ "Middle",
  TRUE ~ "All"
)

manu_type <- case_when( #identify type of manuscript (if any)
  str_detect(name, "sub") ~ "Submitting",
  str_detect(name, "pub") ~ "Publishing",
  TRUE ~ "Unique"
)

#print(manu_type)
#total unique authors across dataset ----
this_df %>% 
  select(gender, random.person.id) %>% distinct() %>% 
  gender_bar_plot(.)+
  labs(x = "Presenting Gender", 
       y = if(auth_type == "All"){
         paste("Number of", manu_type, "Authors")
         }else{paste("Number of", manu_type, auth_type, "Authors")})

ggsave(filename = paste0(name, "_total_auth.png"), path = "results/gender/figures")

#print("barplot")
#break out author type by gender and year for all journals combined ----
all_authors_w_prop <- map_dfr(years, function(x){
  get_prop_by_yr(x, this_df, "gender", "All")}) #%>% filter(gender == "female") %>% arrange(year)

#figure out which year is the last & isolate the proportion values
text_values <- get_gen_prop_text(all_authors_w_prop, 3, "gender")

max_value <- get_ymax(all_authors_w_prop) 

#line plot of all journals combined by year
gender_line_plot(all_authors_w_prop, max_value, 
                 text_values[1,2], text_values[2,2], text_values[3,2]) + 
  labs(x = if(auth_type == "All"){
    paste("Year")}else{paste(manu_type, "Year")},
       y = if(auth_type == "All"){
         paste("Proportion of", manu_type, "Authors")
         }else{paste("Proportion of", manu_type, auth_type, "Authors")},
       caption = if(auth_type == "All"){
         paste("Proportion of", str_to_lower(auth_type), "authors on manuscripts each year. Each person is counted once per year")
         }else{paste("Proportion of", str_to_lower(auth_type), "authors on", str_to_lower(manu_type), "manuscripts each year.")})

ggsave(filename = paste0(name, "_gender_time_line.png"), 
       path = "results/gender/figures")

#print("line plot by yr")
#
j_authors_w_prop <- map_dfr(years, function(x){
    get_prop_by_yr(x, this_df, "gender", "Each")})

max_journ_value <- get_ymax(j_authors_w_prop)

j_authors_w_prop %>% 
  filter(gender != "NA") %>% 
  j_gen_line_plot(., max_journ_value) + 
  labs(x = if(auth_type == "All"){
    paste("Year")}else{paste(manu_type, "Year")},
       y = if(auth_type == "All"){
         paste("Proportion of", manu_type, "Authors")}else{
         paste("Proportion of", manu_type, auth_type, "Authors")},
       linetype = "Gender",
       caption = if(auth_type == "All"){
         paste("Proportion of", str_to_lower(auth_type), 
"authors on manuscripts each year. Each person 
is counted once per year")}else{
  paste("Proportion of", str_to_lower(auth_type), 
        "authors on", str_to_lower(manu_type), "manuscripts each year.")})

ggsave(filename = paste0(name, "_j_gen_time_line.png"), 
       path = "results/gender/figures")
#print("line plot by journal")