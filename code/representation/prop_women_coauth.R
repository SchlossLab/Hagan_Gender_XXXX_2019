

auth_data <- data %>% 
  filter(role == "author") %>% 
  select(random.manu.num, num.authors, random.person.id, gender)

uniq.manu <- auth_data %>% pull(random.manu.num) %>% unique()

author_ratio <- map_dfr(uniq.manu, function(x){
  auth_data %>% 
    filter(random.manu.num == x) %>% distinct() %>% 
    mutate(if.female = if_else(gender == "female", 1, 0),
           prop.fem.auth = sum(if.female)/num.authors) %>% 
    select(random.manu.num, prop.fem.auth, num.authors) %>% distinct()
})

collab_data <- left_join(bias_data, author_ratio, by = c("random.manu.num", "num.authors")) %>% distinct()