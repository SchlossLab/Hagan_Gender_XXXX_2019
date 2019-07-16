#generate plots of all author origin over time
# 1) load_data.R sourced data & library(lubridate) 
# 2) get_plot_options.R 
# 3) analysis_functions.R

#----

#plotting the break down of all submitting authors by geography:
# 1. generates a bar plot with the total count of unique authors over the full time period
# 2. generates a df of the unique individuals per year in that author role
# 3. Plots 2 as a line plot
# 4. generates a df of the unique individuals per year in that author role by journal 
# 5. plots 4 as a grid of line plots, faceted by journal
# 6. saves each of the figures with a name that includes plot type and df


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

#Fig 1. total unique authors across dataset ----
uniq_bar <- this_df %>% 
  select(region, random.person.id) %>% 
  distinct() 
  geographic_bar_plot(uniq_bar, uniq_bar$region)+
  coord_flip() +
  labs(x = "Region", 
       y = if(auth_type == "All"){
         paste("Number of", manu_type, "Authors")
       }else{paste("Number of", manu_type, auth_type, "Authors")})

ggsave(filename = paste0(name, "_total_auth.png"), path = "results/geographic/figures")

#print("barplot")
#break out author type by region and year for all journals combined ----
all_authors_w_prop <- map_dfr(years, function(x){
  get_prop_by_yr(x, this_df, "region", "All")}) #%>% arrange(year)

max_value <- get_ymax(all_authors_w_prop) 

#Fig 2. line plot of all journals combined by year----
geographic_line_plot(all_authors_w_prop, max_value, "region") + 
  labs(x = if(auth_type == "All"){
    paste("Year")}else{paste(manu_type, "Year")},
    y = if(auth_type == "All"){
      paste("Proportion of", manu_type, "Authors")
    }else{paste("Proportion of", manu_type, auth_type, "Authors")},
    caption = if(auth_type == "All"){
      paste("Proportion of", str_to_lower(auth_type), "authors on manuscripts each year. Each person is counted once per year")
    }else{paste("Proportion of", str_to_lower(auth_type), "authors on", str_to_lower(manu_type), "manuscripts each year.")})+
  scale_x_continuous(limits = c(2012, 2018))+ 
  scale_y_continuous(limits = c(1, 100))

ggsave(filename = paste0(name, "_geographic_time_line.png"), 
       path = "results/geographic/figures")

#print("line plot by yr")
#Fig 3. vectorizing years----
years <- this_df %>% pull(year) %>% unique()

j_authors_w_prop <- map_dfr(years, function(x){
  get_prop_by_yr(x, this_df, "region", "Each")})

max_journ_value <- get_ymax(j_authors_w_prop)

j_authors_w_prop %>%
  geographic_line_plot(., max_journ_value, j_authors_w_prop$region) +
  facet_wrap(~journal)+
  labs(x = if(auth_type == "All"){
    paste("Year")}else{paste(manu_type, "Year")},
    y = if(auth_type == "All"){
      paste("Proportion of", manu_type, "Authors")}else{
        paste("Proportion of", manu_type, auth_type, "Authors")},
    linetype = "Region",
    caption = if(auth_type == "All"){
      paste("Proportion of", str_to_lower(auth_type),
            "authors on manuscripts each year. Each person
            is counted once per year")}else{
              paste("Proportion of", str_to_lower(auth_type),
                    "authors on", str_to_lower(manu_type), "manuscripts each year.")})+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_x_continuous(limits = c(2012, 2018))+ 
  scale_y_continuous(limits = c(10, 75))

#plot should only contain observations for top 3 countries

ggsave(filename = paste0(name, "_j_geo_time_line.png"),
       path = "results/geographic/figures")
#print("line plot by journal")
print("save 3")

#generate dataset of all author origin over time - requires data & lubridate from load_data.R
author_origin_data <- this_df %>%
  select(year, random.person.id, region, contains("author")) %>% #restrict to year, person, reviewer status, region, and country of origin
  filter(!is.na(year)) %>% #drop data w/o sub year
  distinct() #single entry for each year/editor combo

print("origin data")
#select(reviewer_origin_data$reviewer.institution, reviewer.gender, reviewer.country, reviewer.random.id)<- NULL

#Fig 4. plot all authors by region & year----
author_origin_data %>%
  ggplot() +
  stat_count(aes(x = region), geom = "bar") +
  geom_bar(aes(x = region, fill = region)) +
  facet_grid( ~ year) +
  #scale_fill_manual(values = cbPalette) + #set fill colors
  #scale_x_discrete(labels = gen_labels)+ #set gender labels
  labs(x = "Author Origin", y = "Number of Authors") +
  my_theme_leg+
  theme(axis.text.x = element_blank(), axis.ticks = element_blank())

#removed x-axis labels for regions and maintained legend to simplify plot

ggsave(device = png, filename = paste0(name, "author_origins.png"),
       path = "results/geographic/figures")

years <- author_origin_data %>% pull(year) %>% unique() #vector of all years in dataset

print("origin years")
#Fig 5. generate dataframe summarising the proportion of each author country of origin for each year----
author_origin_prop <- map_dfr(years, function(x){ #map through the vector of years to generate a dataset

  author_origin_df <- author_origin_data %>%
    filter(year == x) %>% #restrict to single year
    group_by(region) %>% summarise(n = n()) %>% #calculate number of editors per origin in that year
    mutate(proportion = get_percent(n, sum(n))) %>% #add column calculating the proportions for the year, requires analysis_functions.R
    cbind(year = x, .) #add column specifying the year

  return(author_origin_df)
})

print("origin prop")

#plot summaries by year
ggplot(author_origin_prop) +
  geom_line(aes(x = year, y = proportion, linetype = region))+
  coord_cartesian(ylim = c(0, 100))+
  labs(x = "Year", y = "Proportion of Authors per Region")+
  scale_x_continuous(limits = c(2012, 2018))+ 
  scale_y_continuous(limits = c(10, 75))+
  my_theme_leg_horiz

#plot should contain only top 3 regions

ggsave(device = png, filename = paste0(name, "author_origin_time.png"),
       path = "results/geographic/figures")
# 
