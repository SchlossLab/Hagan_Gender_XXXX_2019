#testing for category bias figures----

get_cat_bias_data <- function(cat_journ){
  
  cat_data <- bias_data %>% 
    filter(journal == cat_journ) %>% 
    #filter(journal %in% c("JVI", "JCM", "AAC", "AEM", "IAI")) %>% 
    filter(!is.na(category))
  
  #percent of manus in each category submitted by women
  percent_W <- cat_data %>% 
    select(grouped.random, category, gender) %>% distinct() %>% 
    group_by(category, gender) %>% summarise(n = n()) %>% 
    spread(key = gender, value = n) %>% 
    mutate(percent.W = get_percent(female, (male+female))) %>% 
    select(-male, -female) %>% 
    mutate(percent.W = round(percent.W))
  
  #number of editors per category by gender
  cat_editors <- data %>% 
    filter(role == "editor") %>% 
    filter(journal == cat_journ) %>% 
    select(category, gender, random.person.id) %>% distinct() %>% 
    group_by(category, gender) %>% 
    summarise(n_editor = n()) %>% 
    spread(key = gender, value = n_editor) %>% 
    mutate(percent.W.editor = round(get_percent(female, 
                                                (male+female)))) %>% 
    select(-male, -female)
  
  #number of submissions by category & gender
  cat_summary <- cat_data %>% 
    select(category, gender, grouped.random) %>% distinct() %>% 
    group_by(category, gender) %>% summarise(total = n())
  
  #number of submissions by category
  cat_total <- cat_summary %>% 
    group_by(category) %>% summarise(cat_N = sum(total))
  
  #number of submissions by gender
  gend_total <- cat_data %>% 
    select(gender, grouped.random) %>% distinct() %>% 
    group_by(gender) %>% summarise(n = n())
  
  #editorial rejections----
  ed_rej_data <- cat_data %>% 
    filter(EJP.decision == "Reject" & is.na(days.to.review)) %>% 
    select(grouped.random, gender, 
           EJP.decision, contains("days"), category,
           num.versions, -days.to.review) %>% 
    distinct()
  
  #% manus that are editorial rejections by gender & category
  percent_ed_reject <- ed_rej_data %>% 
    group_by(category, gender) %>% 
    summarise(n = n()) %>% 
    left_join(., cat_summary, by = c("category", "gender")) %>% 
    distinct() %>% 
    mutate(prop_rej = get_percent(n, total)) 
  
  #editor decisions----
  review_data <- cat_data %>% 
    filter(version.reviewed == 0) %>% 
    filter(grouped.vers == 1) %>% 
    select(gender, category, grouped.random, EJP.decision, version) %>% 
    filter(EJP.decision %in% c("Accept", "Reject", "Revise")) %>% 
    distinct()
  
  #total decisions after review across categories
  review_summary <- cat_data %>% 
    filter(version.reviewed == 0) %>% 
    filter(grouped.vers == 1) %>% 
    select(gender, grouped.random, EJP.decision) %>% distinct() %>% 
    group_by(gender) %>% summarise(total = n())
  
  #performance of ea gender in each review decision
  perf_review <- review_data %>% 
    group_by(gender, EJP.decision) %>% 
    summarise(n = n()) %>% 
    left_join(., review_summary, by = "gender") %>% 
    mutate(prop_dec = get_percent(n, total)) %>%
    select(-n, -total) %>% distinct() %>% 
    spread(key = gender, value = prop_dec) %>% 
    mutate(performance = male - female)
  
  #number of manus by each gender
  cat_gend_num <- review_data %>% 
    group_by(category, gender) %>% summarise(total = n())

  review_perf_data <- review_data %>% 
    group_by(category, gender, EJP.decision) %>% 
    summarise(n_review = n()) %>% 
    left_join(., cat_gend_num, 
              by = c("category", "gender")) %>% 
    distinct() %>% 
    mutate(prop_rej = get_percent(n_review, total)) %>%
    select(-n_review, -total) %>% 
    spread(key = gender, value = prop_rej) %>% 
    mutate(performance = male - female) %>% 
    filter(EJP.decision == "Reject") %>% 
    select(-male, -female) %>% 
    mutate(EJP.decision = if_else(EJP.decision == "Reject", "Reject after Review", "NA"))
  
  #plot----
  plot_data <- percent_ed_reject %>% select(-n, -total) %>% 
    spread(key = gender, value = prop_rej) %>% 
    mutate(performance = male - female) %>% 
    select(-male, -female) %>% 
    mutate(EJP.decision = "Editorial Rejection") %>% 
    full_join(., review_perf_data, by = c("category", "performance", "EJP.decision")) %>% 
    left_join(., cat_total, by = "category") %>%
    left_join(., percent_W, by = "category") %>% 
    left_join(., cat_editors, by = "category") %>% 
    as_tibble(.) %>% 
    mutate(journal = cat_journ)
  
  return(plot_data)
}

plot_cat_bias <- function(data, cat_journ){
  
  plot <- data %>% 
    filter(journal == cat_journ) %>% 
    mutate(category = paste0(category, 
                             " (N=", cat_N, "; %WA=", percent.W, 
                             "; %WE=", percent.W.editor, ")")) %>% 
    ggplot() +
    geom_col(aes(x = fct_reorder(category, performance), 
                 y = performance, fill = performance)) + 
    facet_wrap(~EJP.decision, #scales = "free_y", 
               ncol = 2)+
    coord_flip()+
    gen_gradient+
    labs(x = "\n", 
         y = paste("Difference at ", cat_journ, "\n"))+
    my_theme_horiz
  
  return(plot)
}

cat_journ_list <- c("AAC", "AEM", "IAI", "JCM", "JVI")

cat_bias_data <- map_dfr(cat_journ_list, get_cat_bias_data)

S7_ed_rej <- cat_bias_data %>% 
  filter(EJP.decision == "Editorial Rejection")

S7_rev_rej <- cat_bias_data %>% 
  filter(EJP.decision == "Reject after Review")

#does percentage of women authors correlate with diff in perform
stats_WA_ed_rej <- summary(lm(S7_ed_rej$performance~S7_ed_rej$percent.W))

stats_WA_rev_rej <- summary(lm(S7_rev_rej$performance~S7_rev_rej$percent.W))

#does percent of women editors correlate with diff in performance
stats_WE_ed_rej <- summary(lm(S7_ed_rej$performance~S7_ed_rej$percent.W.editor))

stats_WE_rev_rej <- summary(lm(S7_rev_rej$performance~S7_rev_rej$percent.W.editor))

#do percent of w authors correlate with w editors
stats_WEvWA <- summary(lm(cat_bias_data$percent.W.editor~Fig_S7_data$percent.W))

#plot Fig S7----
Fig_S7_list <- map(cat_journ_list, function(x, y){
  plot_cat_bias(cat_bias_data, x)
  })

plot_grid(plotlist = Fig_S7_list, 
                   labels = c('A', 'B', 'C', 'D', 'E'), 
                   label_size = 18, ncol = 1)

ggsave("Figure_S7.png", device = 'png', 
       path = 'submission', width = 12, height = 15)
