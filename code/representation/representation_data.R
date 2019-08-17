#Calculate editor stats for manuscript

#EIC----
prop_eic <- eic_data %>% group_by(gender) %>% 
  summarise(n = n()) %>% 
  mutate(prop = get_percent(n, sum(n)))

num_eic <- sum(prop_eic$n)

#total editors----
prop_ed <- editor_data %>% 
  select(gender, random.person.id) %>% 
  distinct() %>% 
  group_by(gender) %>% 
  summarise(n = n()) %>% 
  mutate(prop = get_percent(n, sum(n)))

num_ed <- sum(prop_ed$n)

#reviewers----
prop_rev <- reviewer_data %>% 
  select(gender, random.person.id) %>% 
  distinct() %>% 
  group_by(gender) %>% 
  summarise(n = n()) %>% 
  mutate(prop = get_percent(n, sum(n)))

num_rev <- sum(prop_rev$n)

single_reviewer <- reviewer_data %>% 
  distinct() %>% #doesn't have the manuscript ids
  group_by(random.person.id, gender) %>% 
  summarise(n = n()) %>% 
  group_by(gender, n) %>% 
  summarise(num.rev = n()) %>% 
  arrange(n) %>% 
  head(n = 3)

#authors----

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

m_corres_prop_fem <- left_join(bias_data, author_ratio, by = c("random.manu.num", "num.authors")) %>% 
  select(random.manu.num, num.authors, prop.fem.auth, gender, journal) %>% 
  filter(gender == "male") %>% 
  distinct() %>% 
  group_by(prop.fem.auth) %>% 
  summarise(n = n())

sum_m_corres <- sum(m_corres_prop_fem$n)
