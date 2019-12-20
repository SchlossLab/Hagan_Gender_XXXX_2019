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

med_rev <- reviewer_data %>% 
  distinct() %>% #doesn't have the manuscript ids
  group_by(random.person.id, gender) %>% 
  summarise(n = n()) %>% 
  group_by(gender) %>% 
  summarise(med.rev = median(n),
            iqr.rev = IQR(n))

single_reviewer <- reviewer_data %>% 
  distinct() %>% #doesn't have the manuscript ids
  group_by(random.person.id, gender) %>% 
  summarise(n = n()) %>% 
  group_by(gender, n) %>% 
  summarise(num.rev = n()) %>% 
  arrange(n) %>% 
  head(n = 3) %>% as_tibble() %>% 
  mutate(prop = get_percent(num.rev, prop_rev$n))

med_contacted <- ed_contact %>% 
  group_by(editor.gender) %>% 
  summarise(med.cont = median(percent_cont))

avg_f_ed_resp <- f_ed_resp %>% 
  group_by(Rev.Resp) %>% 
  summarise(avg.resp = mean(Percent))
  
avg_m_ed_resp <- m_ed_resp %>% 
  group_by(Rev.Resp) %>% 
  summarise(avg.resp = mean(Percent))

#authors: proportion of women authors----

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

m_corres_prop_fem <- left_join(bias_data, author_ratio, 
                               by = c("random.manu.num", "num.authors")) %>% 
  select(random.manu.num, num.authors, prop.fem.auth, gender, journal) %>% 
  filter(num.authors != 1) %>% 
  filter(gender == "male") %>% 
  distinct() %>% 
  group_by(prop.fem.auth) %>% 
  summarise(n = n())

sum_m_corres <- sum(m_corres_prop_fem$n)

most_w_auth_m_corres <- m_corres_prop_fem %>% 
  filter(prop.fem.auth >= 0.51) %>% 
  summarise(total = sum(n))

f_corres_prop_fem <- left_join(bias_data, author_ratio, 
                               by = c("random.manu.num", "num.authors")) %>% 
  select(random.manu.num, num.authors, prop.fem.auth, gender, journal) %>% 
  filter(num.authors != 1) %>% 
  filter(gender == "female") %>% 
  distinct() %>% 
  group_by(prop.fem.auth) %>% 
  summarise(n = n())

sum_f_corres <- sum(f_corres_prop_fem$n)

most_w_auth_f_corres <- f_corres_prop_fem %>% 
  filter(prop.fem.auth >= 0.51) %>% summarise(total = sum(n))

#authors: first author proportions----
f_authors_avg_prop <- get_sub_pub_prop("sub_first_auth", 
                                       "pub_first_auth", "All") %>% 
  filter(gender != "none") %>% 
  group_by(manu.type, gender) %>% 
  summarise(avg = round(mean(proportion), digits = 2))

#authors: corres author proportions----
c_authors_avg_prop <- get_sub_pub_prop("sub_corres_auth", 
                                       "pub_corres_auth", "All") %>% 
  filter(gender != "none")%>% 
  group_by(manu.type, gender) %>% 
  summarise(avg = round(mean(proportion), digits = 2))

#Distribution of manuscripts submitted by gender & inst type

#percent women in US vs Non-US
num_US_v_Non <- bias_data %>% 
  select(gender, random.person.id, grouped.random, US.inst) %>% 
  distinct() %>% 
  group_by(US.inst, gender) %>% summarise(n = n()) %>% as_tibble()

percent_gen_US_v_Non <- num_US_v_Non %>% 
  spread(key = gender, value = n) %>% 
  mutate(percent.W.sub = get_percent(female, (female+male)))

#percent of submitted manuscripts by inst type
num_inst_type <- bias_data %>% 
  select(gender, random.person.id, grouped.random, US.inst.type) %>% 
  distinct() %>% 
  group_by(US.inst.type, gender) %>% summarise(n = n()) %>% as_tibble()

percent_gen_inst_type <- num_inst_type %>% 
  spread(key = gender, value = n) %>% 
  mutate(percent.W.sub = get_percent(female, (female+male)))

#authors:
#min_perform <- acc_diff_auth_j %>% 
#  arrange(.$dif_rel_rej) %>% 
#  pull(dif_rel_rej) %>% head(n = 1)
#