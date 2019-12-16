#testing for category bias figures----

plot_cat_bias <- function(cat_journ){
  
  cat_data <- bias_data %>% 
    filter(journal == cat_journ) %>% 
    #filter(journal %in% c("JVI", "JCM", "AAC", "AEM", "IAI")) %>% 
    filter(!is.na(category))
  
  #editorial rejections----
  cat_ed_rejs <- cat_data %>% 
    filter(EJP.decision == "Reject" & is.na(days.to.review)) %>% 
    select(grouped.random, gender, 
           EJP.decision, contains("days"), category,
           num.versions, -days.to.review) %>% 
    distinct()
  
  #percent of submissions that are editorial rejections by gender
  num_sub_summary <- cat_data %>% 
    select(gender, grouped.random) %>% distinct() %>% 
    group_by(gender) %>% summarise(n = n())
  
  num_ed_rej <- cat_ed_rejs %>% 
    group_by(gender) %>% 
    summarise(n = n()) %>% 
    mutate(prop_rej = get_percent(n, num_sub_summary$n)) %>% 
    select(-n) %>% 
    spread(key = gender, value = prop_rej) %>% 
    mutate(performance = male - female) %>% 
    cbind(category = "All Combined", .) %>% as_tibble()
  
  #percent of submissions that are editorial rejections by gender & category
  cat_summary <- cat_data %>% 
    select(category, gender, grouped.random) %>% distinct() %>% 
    group_by(category, gender) %>% summarise(total = n())
  
  cat_total <- cat_summary %>% 
    group_by(category) %>% summarise(cat_N = sum(total))
  
  cat_ed_rejections <- cat_ed_rejs %>% 
    group_by(category, gender) %>% 
    summarise(n = n()) %>% 
    left_join(., cat_summary, by = c("category", "gender")) %>% 
    distinct() %>% 
    mutate(prop_rej = get_percent(n, total)) 
  
  cat_ed_reject_n <- cat_ed_rejections %>% 
    select(-total, -prop_rej) %>% 
    spread(key = gender, value = n) %>% 
    mutate(n = male + female) %>% 
    select(-male, -female)
  
  plot_data <- cat_ed_rejections %>% select(-n, -total) %>% 
    spread(key = gender, value = prop_rej) %>% 
    mutate(performance = male - female) %>% 
    left_join(., cat_ed_reject_n, by = "category") %>% 
    left_join(., cat_total, by = "category") %>%
    as_tibble(.) %>% 
    mutate(category = paste0(category, " (N=", cat_N, ")"))
  
  plot <- plot_data %>% 
    ggplot() +
    geom_col(aes(x = fct_reorder(category, performance), 
                 y = performance, fill = performance)) + 
    coord_flip()+
    gen_gradient+
    geom_hline(data = num_ed_rej, aes(yintercept = performance))+
    #annotate(geom = "text", x = 12, y = -2.5, label = "All Journals")+
    #geom_text(aes(x = journal, y = 0.75, label = n))+
    labs(x = "\n", 
         y = paste("Difference in", cat_journ, "Editorial Rejections\n"))+
    my_theme_horiz
  
  return(plot)
}

Fig_SA <- plot_cat_bias("AEM")

Fig_SB <- plot_cat_bias("JCM")

Fig_SC <- plot_cat_bias("JVI")

plot_grid(Fig_SA, Fig_SB, Fig_SC,
                   labels = c('A', 'B', 'C'), 
                   label_size = 18, nrow = 3)

ggsave("Figure_S_cat.png", device = 'png', 
       path = 'submission', width = 12, height = 12)