library(tidyverse)
library(caret) #package for generating confusion matrices
library(knitr)
library(kableExtra)

#function to convert male/female into binary (1/0)
convert_gender <- function(x){
  case_when(
    x == "male" ~ "0",
    x == "female" ~ "1"
  )
}

arrange_data <- function(x){
  x %>% 
    select(contains("gender"), probability, count) %>% 
    filter(!is.na(actual.gender) & !is.na(genderize.gender)) %>% 
    mutate(actual.gender = map(actual.gender, convert_gender) %>% unlist() %>% as.factor()) %>% 
    mutate(genderize.gender = map(genderize.gender, convert_gender) %>% unlist() %>% as.factor())
}

#function to compare genderize outcomes to reference data & return summary stats----
get_summary_stats <- function(input_df){
  
  df_name <- input_df
  
  df <- get(input_df) %>% mutate(pmod = (probability*count+2)/(count+4))
  
  p85 <- df %>% filter(probability >= 0.85)
  
  pmod85 <- df %>% filter(pmod >= 0.85)
  
  all_dfs <- list(df, p85, pmod85)
  
  #function to loop through each dataframe & create a df of the specified summary stats
  get_stats_df <- function(df){
    
    df <- arrange_data(df)

    x <- confusionMatrix(df[[2]], df[[1]]) #create confusion matrix & summary stats
    
    stats <- c(round(x[[4]][[1]], digits = 4), #pull sensitivity
               round(x[[4]][[2]], digits = 4), #pull specificity
               round(x[[3]][[1]], digits = 4)) %>% #pull accuracy
      enframe(name = NULL)#convert to df
    
    return(stats)
  }
  
  stats_df <- map_dfc(all_dfs, get_stats_df) %>% 
    rename(., "v1" = "value", "v2" = "value1", "v3" = "value2") #create df of all summary stats
  
  #colnames(stats_df) <- c("1", "2", "3")
  
  prop_na_df <- c(
    get_percent(
      nrow(filter(df, is.na(genderize.gender))),
      nrow(df)),
    get_percent((nrow(df) - nrow(p85)), nrow(df)),
    get_percent((nrow(df) - nrow(pmod85)), nrow(df))
  ) %>% tibble() %>% t() %>% unname() %>% as_tibble()
  
  colnames(prop_na_df) <- c("v1", "v2", "v3")
  
  rows <- c("Sensitivity", "Specificity", "Accuracy", "Percent Unknown") %>% 
    as_tibble() #stats measured
  
  stats_summary <-  bind_rows(stats_df, prop_na_df) %>% 
    cbind(rows, .) #bind measure names with summary stats
  
  colnames(stats_summary) <- c("Measure", paste0(df_name, "_all"), 
                               paste0(df_name, "_p85"), 
                               paste0(df_name, "_pmod85"))
  
  return(stats_summary)
}

#compare outcomes after having converted to ASCII, w. no country data----
b_c_data_ascii <- read_csv("../data/B_C_auth_genderize_join_nosp.csv")

total_names_ascii <- nrow(b_c_data_ascii)

unknown_names_ascii <- b_c_data_ascii %>% filter(is.na(actual.gender)) %>% nrow() 

compared_names_ascii <- b_c_data_ascii %>% filter(!is.na(actual.gender) & !is.na(genderize.gender)) %>% nrow()

na_names_ascii <- b_c_data_ascii %>% filter(is.na(genderize.gender)) %>% nrow()

b_c_ascii_summary <- get_summary_stats("b_c_data_ascii")

#compare outcomes after having converted to ASCII, with country data----
b_c_country_data_ascii <- read_csv("../data/B_C_auth_country_genderize_join_nosp.csv")

country_ascii_compared_names <- b_c_country_data_ascii %>% 
  filter(!is.na(actual.gender) & !is.na(genderize.gender)) %>% nrow()

b_c_country_ascii_summary <- get_summary_stats("b_c_country_data_ascii")

#summary table using ascii data -- slightly decreased accuracy but increased the proportion of names genderized
stats_summary <- left_join(b_c_ascii_summary, b_c_country_ascii_summary, by = "Measure")



