#plotting the break down of all submitting authors by gender:
# 1. generates a bar plot with the total count of unique authors over the full time period
# 2. generates a df of the unique individuals per year in that author role
# 3. Plots 2 as a line plot
# 4. generates a df of the unique individuals per year in that author role by journal 
# 5. plots 4 as a grid of line plots, faceted by journal
# 6. saves each of the figures with a name that includes plot type and df

#setup ----
sub_df <- get(temp_sub) #pull this df from the global environment

pub_df <- get(temp_pub)

name <- paste0(temp_sub) #isolate the name of the df as a string

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
unq_sub <- sub_df %>% 
  select(gender, random.person.id) %>% distinct() %>% 
  mutate(manu.type = "submitted")

unq_pub <- pub_df %>% 
  select(gender, random.person.id) %>% distinct() %>% 
  mutate(manu.type = "published")

rbind(unq_pub, unq_sub) %>% 
  ggplot()+
  geom_bar(aes(x = gender, fill = manu.type), position= "dodge")+
  #scale_fill_manual(values = gen_colors)+
  scale_x_discrete(labels = gen_labels)+
  my_theme_leg_horiz+
  labs(x = "Presenting Gender", 
       y = paste0("Number of", auth_type, "Authors"),
       fill = "Manuscript Type")

ggsave(filename = paste0(auth_type, "_total_subvpub_auth.png"), path = "results/gender/figures")

#print("barplot")
#break out author type by gender and year for all journals combined ----
sub_authors_w_prop <- map_dfr(years, function(x){
  get_prop_by_yr(x, sub_df, "gender", "All") %>% 
    mutate(., manu.type = "submitted")}) 
  
pub_authors_w_prop <- map_dfr(years, function(x){
  get_prop_by_yr(x, pub_df, "gender", "All") %>% 
    mutate(manu.type = "published")}) 

all_authors_w_prop <- rbind(sub_authors_w_prop, pub_authors_w_prop)

#figure out which year is the last & isolate the proportion values
text_values <- map_df(gen_levels, function(x){
  all_authors_w_prop %>% 
    filter(gender == x) %>% 
  get_gen_prop_text(., 2, "manu.type") %>% 
    mutate(gender = paste(x))})

max_value <- get_ymax(all_authors_w_prop) 

#line plot of all journals combined by year

map(gen_levels, function(x){
  
sub_y <- text_values %>% filter(gender == x) %>% .[2,2]
pub_y <- text_values %>% filter(gender == x) %>% .[1,2]

gender <- case_when(
  x == "female" ~ "Women",
  x == "male" ~ "Men",
  x == "none" ~ "Unclear"
)

all_authors_w_prop %>% 
  filter(gender == x) %>% 
ggplot() + 
  geom_line(aes(x = year, y = proportion, linetype = manu.type))+
  coord_cartesian(ylim = c(0, max_value))+
  #scale_linetype_manual(breaks = gen_levels, labels = gen_labels, values = gen_linetype)+
  annotate(geom = "text", x = 2018, y = pub_y+1.5, label = "Published")+
  annotate(geom = "text", x = 2018, y = sub_y+1.5, label = "Submitted")+
  my_theme_horiz + 
  labs(x = "Year",
       y = paste("Proportion of", gender, auth_type, "Authors"),
       caption = paste("Proportion of", str_to_lower(gender), 
                       str_to_lower(auth_type), 
                       "authors on manuscripts each year."))

ggsave(filename = paste0(gender, "_", auth_type, "_pub_v_sub_time.png"), 
       path = "results/gender/figures")

})
#print("line plot by yr")
#
sub_j_authors_w_prop <- map_dfr(years, function(x){
    get_prop_by_yr(x, sub_df, "gender", "Each") %>% 
    mutate(., manu.type = "submitted")})

pub_j_authors_w_prop <- map_dfr(years, function(x){
  get_prop_by_yr(x, pub_df, "gender", "Each") %>% 
    mutate(., manu.type = "published")})

j_authors_w_prop <- rbind(sub_j_authors_w_prop, pub_j_authors_w_prop)

max_journ_value <- get_ymax(j_authors_w_prop)

map(gen_levels, function(x){

  gender <- case_when(
    x == "female" ~ "Women",
    x == "male" ~ "Men",
    x == "none" ~ "Unclear"
  )
  
j_authors_w_prop %>% 
  filter(gender == x) %>% 
  ggplot() + 
  geom_line(aes(x = year, y = proportion, linetype = manu.type))+
  #scale_y_continuous(breaks = c(0, 15, 30, 45))+
  #scale_linetype_manual(values = gen_linetype, breaks = gen_levels, labels = gen_labels)+
  coord_cartesian(ylim = c(0, max_journ_value)) +
  facet_wrap(~ journal)+ 
  my_theme_leg +
  labs(x ="Year",
       y = paste("Proportion of", gender, auth_type, "Authors"),
       linetype = "Manuscript Type",
       caption = paste("Proportion of", str_to_lower(gender),
                       str_to_lower(auth_type),
                       "authors on manuscripts each year"))

ggsave(filename = paste0(gender,"_", auth_type, "_j_gen_time.png"), 
       path = "results/gender/figures")
})
#print("line plot by journal")