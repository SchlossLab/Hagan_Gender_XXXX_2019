#testing for category bias figures----

plot_cat_bias <- function(cat_journ){
  
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
  
  #number of submissions by category
  cat_summary <- cat_data %>% 
    select(category, grouped.random) %>% distinct() %>% 
    group_by(category) %>% summarise(total = n())
  
  #editorial rejections----
  ed_rej_data <- cat_data %>% 
    filter(EJP.decision == "Reject" & is.na(days.to.review)) %>% 
    select(grouped.random, gender, 
           EJP.decision, contains("days"), category,
           num.versions, -days.to.review) %>% 
    distinct()
  
  #% manus that are editorial rejections by gender & category
  percent_ed_reject <- ed_rej_data %>% 
    group_by(category) %>% 
    summarise(n = n()) %>% 
    left_join(., cat_summary, by = c("category")) %>% 
    distinct() %>% 
    mutate(prop.rej = get_percent(n, total)) 
  
  #editor decisions----
  review_data <- cat_data %>% 
    filter(version.reviewed == 0) %>% 
    filter(grouped.vers == 1) %>% 
    select(category, grouped.random, EJP.decision, version) %>% 
    filter(EJP.decision %in% c("Accept", "Reject", "Revise")) %>% 
    distinct()
  
  #total decisions after review across categories
  review_summary <- cat_data %>% 
    filter(version.reviewed == 0) %>% 
    filter(grouped.vers == 1) %>% 
    select(grouped.random, EJP.decision, category) %>%
    distinct() %>% 
    group_by(category) %>% summarise(total = n())
  
  review_perf_data <- review_data %>% 
    group_by(category, EJP.decision) %>% 
    summarise(n_review = n()) %>% 
    left_join(., review_summary, 
              by = c("category")) %>% 
    distinct() %>% 
    mutate(prop.rej = get_percent(n_review, total)) %>%
    select(-n_review, -total) %>% 
    filter(EJP.decision == "Reject") %>% 
    mutate(EJP.decision = if_else(EJP.decision == "Reject", 
                                  "Reject after Review", "NA"))
  
  #plot----
  plot_data <- percent_ed_reject %>% select(-n, -total) %>% 
    mutate(EJP.decision = "Editorial Rejection") %>% 
    full_join(., review_perf_data, by = c("category", "prop.rej", "EJP.decision")) %>% 
    left_join(., cat_total, by = "category") %>%
    left_join(., percent_W, by = "category") %>% 
    left_join(., cat_editors, by = "category") %>% 
    as_tibble(.) %>% 
    mutate(category = paste0(category, 
                             " (N=", cat_N, "; %WA=", percent.W, 
                             "; %WE=", percent.W.editor, ")"))
  
  plot <- plot_data %>% 
    ggplot() +
    geom_col(aes(x = fct_reorder(category, prop.rej), 
                 y = prop.rej, fill = percent.W)) + 
    facet_wrap(~EJP.decision, scales = "free_x")+
    coord_flip()+
    labs(x = "\n", 
         y = paste("Rejection Rates at", cat_journ, "\n"))+
    my_theme_leg_horiz
  
  return(plot)
}

Fig_S7A <- plot_cat_bias("AAC")

Fig_S7B <- plot_cat_bias("AEM")

Fig_S7C <- plot_cat_bias("IAI")

Fig_S7D <- plot_cat_bias("JCM")

Fig_S7E <- plot_cat_bias("JVI")

plot_grid(Fig_S7A, Fig_S7B, Fig_S7C, Fig_S7D, Fig_S7E,
                   labels = c('A', 'B', 'C', 'D', 'E'), 
                   label_size = 18, nrow = 5)

ggsave("Alt_Figure_S7.png", device = 'png', 
       path = 'submission', width = 12, height = 15)
