#setup ----
#create general dataframe to loop each author type through
this_df <- get(each_auth_type) #pull this df from the global environment

#name is the author type, i.e. uniq_author_data
name <- paste0(each_auth_type) #isolate the name of the df as a string

print(name)

auth_type <- case_when( #identify authors being examined, i.e. first, last, corresponding, middle
  str_detect(name, "first") ~ "First",
  str_detect(name, "last") ~ "Last",
  str_detect(name, "corres") ~ "Corresponding",
  str_detect(name, "mid") ~ "Middle",
  TRUE ~ "All"
)

#type of manuscript i.e. submitting or publishing
manu_type <- case_when( #identify type of manuscript (if any)
  str_detect(name, "sub") ~ "Submitting",
  str_detect(name, "pub") ~ "Publishing",
  TRUE ~ "Unique"
)

this_df <- this_df %>% 
  filter(GDP %in% gdp_list)

print(manu_type)



#break out author type by GDP and year for all journals combined ----
all_authors_w_prop <- map_dfr(years, function(x){
  get_prop_by_yr(x, this_df, "GDP", "All")}) #%>% arrange(year)

max_value <- get_ymax(all_authors_w_prop) 

#line plot of all journals combined by year
geographic_line_plot(all_authors_w_prop, max_value, all_authors_w_prop$GDP) + 
  labs(x = if(auth_type == "All"){
    paste("Year")}else{paste(manu_type, "Year")},
    y = if(auth_type == "All"){
      paste("Proportion of", manu_type, "Authors")
    }else{paste("Proportion of", manu_type, auth_type, "Authors")},
    caption = if(auth_type == "All"){
      paste("Proportion of", str_to_lower(auth_type), "authors on manuscripts each year. Each person is counted once per year")
    }else{paste("Proportion of", str_to_lower(auth_type), "authors on", str_to_lower(manu_type), "manuscripts each year.")})

ggsave(filename = paste0(name, "_GDP_time_line.png"), 
       path = "results/geographic/figures")

#print("line plot by yr")
#vectorizing years
years <- this_df %>% pull(year) %>% unique()

j_authors_w_prop <- map_dfr(years, function(x){
  get_prop_by_yr(x, this_df, "GDP", "Each")})

max_journ_value <- get_ymax(j_authors_w_prop)

j_authors_w_prop %>%
  #filter(GDP != "NA") %>%
  geographic_line_plot(., max_journ_value, j_authors_w_prop$GDP) +
  labs(x = if(auth_type == "All"){
    paste("Year")}else{paste(manu_type, "Year")},
    y = if(auth_type == "All"){
      paste("Proportion of", manu_type, "Authors")}else{
        paste("Proportion of", manu_type, auth_type, "Authors")},
    linetype = "GDP",
    caption = if(auth_type == "All"){
      paste("Proportion of", str_to_lower(auth_type),
            "authors on manuscripts each year. Each person
            is counted once per year")}else{
              paste("Proportion of", str_to_lower(auth_type),
                    "authors on", str_to_lower(manu_type), "manuscripts each year.")})

ggsave(filename = paste0(name, "_j_GDP_time_line.png"),
       path = "results/geographic/figures")
#print("line plot by journal")


#generate dataset of all author origin over time - requires data & lubridate from load_data.R
author_gdp_data <- data %>%
  filter(role != "reviewer", role != "editor", role != "senior.editor") %>%
  mutate(year = year(submitted.date)) %>% #new column with the year of submission
  select(year, random.person.id, country, role, GDP, contains("author")) %>% #restrict to year, person, reviewer status, region, and country of origin
  filter(!is.na(year)) %>% #drop data w/o sub year
  mutate(role = fct_explicit_na(role, na_level = "none")) %>% #refactor na values so that they can be plotted correctly
  distinct() #single entry for each year/editor combo

#plot all authors by GDP & year
author_gdp_data %>%
  ggplot() +
  stat_count(aes(x = GDP), geom = "bar") +
  geom_bar(aes(x = GDP, fill = GDP)) +
  facet_grid( ~ year) +
  #scale_fill_manual(values = cbPalette) + #set fill colors
  #scale_x_discrete(labels = gen_labels)+ 
  labs(x = "Author Origin GDP", y = "Number of Authors") +
  my_theme_leg

years <- author_gdp_data %>% pull(year) %>% unique() #vector of all years in dataset

#generate dataframe summarising the proportion of each author country of origin for each year
author_gdp_prop <- map_dfr(years, function(x){ #map through the vector of years to generate a dataset
  
  author_gdp_df <- author_origin_data %>%
    filter(year == x) %>% #restrict to single year
    group_by(GDP) %>% summarise(n = n()) %>% #calculate number of editors per origin in that year
    mutate(proportion = get_percent(n, sum(n))) %>% #add column calculating the proportions for the year, requires analysis_functions.R
    cbind(year = x, .) #add column specifying the year
  
  return(author_gdp_df)
})

#plot summaries by year
ggplot(author_gdp_prop) +
  geom_line(aes(x = year, y = proportion, linetype = GDP))+
  coord_cartesian(ylim = c(0, 100))+
  labs(x = "Year", y = "Proportion of Authors per GDP")
#my_theme

ggsave(device = png, filename = paste0(uniq_author_data, "author_GDP_time.png"),
       path = "results/geographic/figures")
