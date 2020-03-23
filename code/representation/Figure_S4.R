auth_data <- data %>% 
  filter(role == "author") %>% 
  select(random.manu.num, random.person.id, gender, num.authors) %>%
  distinct()

uniq.manu <- auth_data %>% pull(random.manu.num) %>% unique()

author_ratio <- map_dfr(uniq.manu, function(x){
  auth_data %>% 
    filter(random.manu.num == x) %>% distinct() %>% 
    mutate(if.female = if_else(gender == "female", 1, 0),
           prop.fem.auth = sum(if.female)/num.authors) %>% 
    select(random.manu.num, prop.fem.auth, num.authors) %>% distinct()
})

collab_data <- left_join(bias_data, author_ratio, by = c("random.manu.num", "num.authors")) %>% 
  select(grouped.random, num.authors, prop.fem.auth, gender, journal) %>% 
  distinct() %>% 
  mutate(norm.fem = prop.fem.auth/num.authors)

sing_auth_num <- collab_data %>% 
  select(gender, grouped.random, num.authors) %>% 
  distinct() %>% 
  filter(num.authors == 1) %>% 
  group_by(gender) %>% 
  summarise(n = n())

fem_by_auth_num <- collab_data %>% 
  select(gender, grouped.random, num.authors, prop.fem.auth) %>% 
  distinct() %>% 
  #group_by(num.authors, gender) %>% 
  #summarise(n = n(), med.fem = median(prop.fem.auth)) %>% 
  filter(num.authors != 1) %>% 
  mutate(auth.bin = case_when(
    num.authors <= 5 ~ "2-5 Authors",
    num.authors >= 6 & num.authors <= 10 ~ "6-10 Authors",
    num.authors >= 11 & num.authors <= 15 ~ "11-15 Authors",
    num.authors >= 16 & num.authors <= 20 ~ "16-20 Authors",
    num.authors >= 21 & num.authors <= 30 ~ "21-30 Authors",
    num.authors >= 31 & num.authors <= 40 ~ "31-40 Authors",
    TRUE ~ "> 40 Authors"
  ),
  prop.fem.auth.bin = case_when(
    prop.fem.auth <= 0.01 ~ "0",
    prop.fem.auth >= 0.01 & prop.fem.auth <= .24 ~ "<25%",
    prop.fem.auth >= .25 & prop.fem.auth <= .50  ~ "25-50%",
    prop.fem.auth >= .51 & prop.fem.auth <= .75  ~ "51-75%",
    TRUE ~ ">75%"
  )) 

auth_bin_lev <- c("2-5 Authors", "6-10 Authors", "11-15 Authors",
                  "16-20 Authors", "21-30 Authors",
                  "31-40 Authors", "> 40 Authors")

fem_bin_lev <- c("0", "<25%", "25-50%", "51-75%", ">75%")

fem_by_auth_num$auth.bin <- fct_relevel(fem_by_auth_num$auth.bin, auth_bin_lev)  
fem_by_auth_num$prop.fem.auth.bin <- fct_relevel(fem_by_auth_num$prop.fem.auth.bin, fem_bin_lev)  

#create df to fill in missing values in plots -- manual approach to "complete" function
missing_bins <- tibble(
  gender = c("female"),
  grouped.random = c(0),
  num.authors = c(0),
  prop.fem.auth = c(0),
  auth.bin = c("2-5 Authors", "6-10 Authors", "11-15 Authors", "16-20 Authors", "21-30 Authors"),
  prop.fem.auth.bin = c("0")
)

fem_by_auth_num <- fem_by_auth_num %>% 
  rbind(., missing_bins)

plot_fem_by_auth <- fem_by_auth_num %>% 
  ggplot()+
  geom_bar(aes(fill = gender, x = prop.fem.auth.bin), position = "dodge")+
  scale_y_log10()+
  scale_fill_manual(values = gen_ed_colors, labels = gen_ed_labels)+
  facet_wrap(~auth.bin, scales = "free_y")+
  labs(x = "Proportion of Women Authors", 
       y = "Number of Manuscripts (log10 scale)",
       fill = "Corresponding\nAuthor Gender")+
  my_theme_leg+
  theme(legend.position = c(0.8, 0.1))

ggsave("Figure_S4.png", device = 'png', 
       path = 'submission', width = 8, height = 8)
