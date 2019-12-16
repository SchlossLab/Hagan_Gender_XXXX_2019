create_feature_rankings <- function(data){

  splits <- nrow(data)
  
  data$split <- rownames(data)
  
  ranked_data <- map_dfr(1:splits, function(x){
    
    weights <- data %>%
      filter(split == x) %>% 
      select(-Bias, -model, -split) %>%
      gather(factor_key=TRUE) %>%
      mutate(sign = case_when(value<0 ~ "negative",
                              value>0 ~ "positive",
                              value==0 ~ "zero"))
  
    weights$value <- abs(weights$value)
  
    ranks <- weights %>% 
      arrange(desc(value)) %>%
      mutate(rank = 1:nrow(weights)) %>%
      mutate(value = case_when(sign=="negative" ~ value*-1,
                               sign=="positive"~ value,
                               sign=="zero" ~ value)) %>%
      select(key, value, rank, sign)
  
    return(ranks)
  })
  
  imp_first_10 <- ranked_data %>%
    # 2. Group by the OTU name and compute median rank for each OTU
    group_by(key) %>%
    summarise(median_rank = median(rank)) %>%
    # 3. Arrange from highest ranked 1, descending
    arrange(median_rank) %>%
    # 4. Grab only the highest ranked 20
    head(n=10) %>%
    select(key, median_rank)
  
  # Here we want to only grab the data (rank info from 100 datasplits) of only the top 20 median ranked OTUs
  # The imp data will be returned for Figure 3 where we plot each rank info for each data-split of the 20 top OTUs
  imp <- ranked_data %>%
    filter(key %in% imp_first_10$key)
  
  return(imp)
}
# This function:
#     1. Top 5 ranked (1-5 lowest rank) OTUs (ranks of the OTU for 100 splits)
#     2. Returns a plot. Each datapoint is the rank of the OTU at one datasplit.

plot_feature_ranks <- function(data){
  # Plot from highest median ranked OTU to least (only top 5) and thir ranks that lay between 1-100
  # Rank 1 is the highest rank
  plot <- ggplot(data, aes(reorder(data$key, -data$rank, FUN = median), data$rank)) +
    geom_point(aes(colour= factor(data$sign)), size=1.5) + # datapoints lighter color
    scale_color_manual(values=c("#56B4E9","red3", "#999999")) +
    stat_summary(fun.y = function(x) median(x), colour = 'black', geom = "point", size = 3) + # Median darker
    coord_flip(ylim=c(0,15)) +
    my_theme_leg_horiz
  return(plot)
}

rank_test <- create_feature_rankings(feat_weights) %>% 
  mutate(key = key %>% str_replace("\\.{3}", " & ") %>% 
           str_replace("\\.female", "\\.women") %>% 
           str_replace("\\.male", "\\.men") %>% 
           str_replace_all("X.inst.gender.|X.US.inst.type.", "") %>% 
           str_replace_all("US.gender.no.|US.inst.no", "Non-US ") %>% 
           str_replace_all("(?<=US.)gender.yes|inst.yes", "") %>% 
           str_replace_all("inst.gender.Other.", "Other US Inst ") %>% 
           str_replace_all("\\.", " ") %>% 
           str_to_title() %>% 
           str_replace_all("Us", "US") %>% 
           trimws())
