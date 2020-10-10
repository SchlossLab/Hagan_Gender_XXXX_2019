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
    group_by(category) %>% summarise(cat.N = sum(total))
  
  #acceptance rate by category
  cat_accept <- cat_data %>% 
    filter(EJP.decision == "Accept") %>% 
    select(category, grouped.random) %>% distinct() %>% 
    group_by(category) %>% 
    summarise(n.acc = n()) %>% 
    left_join(., cat_total, by = "category") %>% 
    mutate(cat.acc.rate = get_percent(n.acc, cat.N)) %>% 
    select(-cat.N)
  
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
    left_join(., cat_accept, by = "category") %>% 
    as_tibble(.) %>% 
    mutate(journal = cat_journ)
  
  return(plot_data)
}

plot_cat_bias <- function(data, cat_journ){
  
  plot_breaks <- pretty(data$performance, n = 16)
  
  plot <- data %>% 
    filter(journal == cat_journ) %>% 
    mutate(category = paste0(category, 
                             " (N=", cat.N, ")")) %>% 
    ggplot() +
    geom_col(aes(x = fct_reorder(category, desc(cat.N)), 
                 y = performance, fill = performance)) + 
    facet_wrap(~EJP.decision, #scales = "free_y", 
               ncol = 2)+
    coord_flip()+
    gen_gradient_40+
    scale_y_continuous(breaks = plot_breaks,
                       labels = abs(plot_breaks))+
    labs(x = "\n", 
         y = paste("Difference at ", cat_journ),
         caption = "Men <--- Favored Gender ---> Women\n")+
    my_theme_horiz
  
  return(plot)
}

cat_journ_list <- c("AAC", "AEM", "IAI", "JCM", "JVI")

cat_bias_data <- map_dfr(cat_journ_list, get_cat_bias_data)

S8_ed_rej <- cat_bias_data %>% 
  filter(EJP.decision == "Editorial Rejection")

S8_rev_rej <- cat_bias_data %>% 
  filter(EJP.decision == "Reject after Review")

acc_range <- cat_bias_data %>% select(category, journal, cat.acc.rate) %>% 
  distinct() %>%  arrange(desc(cat.acc.rate))#range in acceptance rates by category

cat_N_list <- cat_bias_data %>% select(category, journal, cat.N) %>% distinct() %>% arrange(desc(cat.N))

avg_W_sub <- mean(S8_ed_rej$percent.W, na.rm = TRUE) %>% round(., digits = 1)

#does percentage of women authors correlate with diff in perform
stats_WA_ed_rej <- summary(lm(S8_ed_rej$performance~S8_ed_rej$percent.W))

stats_WA_rev_rej <- summary(lm(S8_rev_rej$performance~S8_rev_rej$percent.W))

#does percentage of women authors correlate with acceptance rates
stats_WA_acc <- summary(lm(S8_ed_rej$cat.acc.rate~S8_ed_rej$percent.W))

#does percent of women editors correlate with diff in performance
stats_WE_ed_rej <- summary(lm(S8_ed_rej$performance~S8_ed_rej$percent.W.editor))

stats_WE_rev_rej <- summary(lm(S8_rev_rej$performance~S8_rev_rej$percent.W.editor))

#do percent of w authors correlate with w editors
stats_WEvWA <- summary(lm(S8_ed_rej$percent.W.editor~S8_ed_rej$percent.W))

#do the # of manuscripts in the category correlate with diff in performance
num_ed_rej <- summary(lm(S8_ed_rej$performance~S8_ed_rej$cat.N))

num_rev_rej <- summary(lm(S8_rev_rej$performance~S8_rev_rej$cat.N))

#do the # of manuscripts in the category correlate with acceptance rates
num_v_acc <- summary(lm(S8_ed_rej$cat.acc.rate~S8_ed_rej$cat.N))

#do percent of W authors correlate with cat_N
stats_numvWA <- summary(lm(S8_ed_rej$cat.N~S8_ed_rej$percent.W))

#do percent of W editors correlate with cat_N
stats_numvWE <- summary(lm(S8_ed_rej$cat.N~S8_ed_rej$percent.W.editor))

#plot Fig S8----
Fig_S8_breaks <- pretty(Fig_S6B_data$performance, n = 9)

Fig_S8_list <- map(cat_journ_list, function(x, y){
  plot_cat_bias(cat_bias_data, x)
  })

Fig_S8_legend_plot <- Fig_S6B_data %>% 
  ggplot(aes(x = journal, y = performance, fill = performance))+
  geom_col()+
  facet_wrap(~US.inst.type, scales = "free", ncol = 2)+
  coord_flip()+
  gen_gradient_40+
  labs(x = "\n", 
       y = "Difference in Acceptance Rates\n", 
       fill = "% Points\nDifference")+
  my_theme_horiz+
  theme(legend.position = "top")

Fig_S8_legend <- get_legend(Fig_S8_legend_plot)

Fig_S8_plots <- plot_grid(plotlist = Fig_S8_list, 
                   labels = c('A', 'B', 'C', 'D', 'E'), 
                  rel_heights = c(1.25, 1.25, 1, 1, 1),
                   label_size = 18, ncol = 1)

plot_grid(Fig_S8_legend, Fig_S8_plots,
          rel_heights = c(0.05, 1),
          nrow = 2)

ggsave("Figure_S8.tiff", device = 'tiff', units = "in", scale = 2,
       path = 'submission', width = 6.8, height = 8)
