library(tidyverse)
library(caret) #package for generating confusion matrices

#function to convert male/female into binary (1/0)
convert_gender <- function(x){
  case_when(
    x == "male" ~ "0",
    x == "female" ~ "1"
  )
}

#function to compare genderize outcomes to reference data & return summary stats----
get_summary_stats <- function(df){
  
  data <- df %>% 
    select(contains("gender"), probability, count) %>% 
    mutate(pmod = (probability*count+2)/(count+4)) %>% 
    filter(!is.na(actual.gender) & !is.na(genderize.gender)) %>% 
    mutate(actual.gender = map(actual.gender, convert_gender) %>% unlist() %>% as.factor()) %>% 
    mutate(genderize.gender = map(genderize.gender, convert_gender) %>% unlist() %>% as.factor())
  
  p85 <- data %>% filter(probability >= 0.85)
  
  pmod85 <- data %>% filter(pmod >= 0.85)
  
  all_dfs <- list(data, p85, pmod85)
  
  #function to loop through each dataframe & create a df of the specified summary stats
  get_stats_df <- function(df){
    
    #print(df)
    
    x <- confusionMatrix(df[[2]], df[[1]]) #create confusion matrix & summary stats
    
    stats <- c(round(x[[4]][[1]], digits = 4), #pull sensitivity
               round(x[[4]][[2]], digits = 4), #pull specificity
               round(x[[3]][[1]], digits = 4)) %>% as.tibble() #pull accuracy and convert to df
    
    return(stats)
  }
  
  stats_df <- map_dfc(all_dfs, get_stats_df) #create df of all summary stats
  
  rows <- c("Sensitivity", "Specificity", "Accuracy") #stats measured
  
  stats_summary <- as.tibble(rows) %>% cbind(., stats_df) #bind measure names with summary stats
  
  colnames(stats_summary) <- c("Measure", paste0(deparse(substitute(df)), "_all"), 
                               paste0(deparse(substitute(df)), "_p85"), 
                               paste0(deparse(substitute(df)), "_pmod85"))
  
  return(stats_summary)
}

#summary stats for dataset, no country added----
b_c_data <- read_csv("../data/B_C_auth_genderize_join.csv") 

total_names <- nrow(b_c_data)

unknown_names <- b_c_data %>% filter(is.na(actual.gender)) %>% nrow() 

compared_names <- b_c_data %>% filter(!is.na(actual.gender) & !is.na(genderize.gender)) %>% nrow()

na_names <- b_c_data %>% filter(is.na(genderize.gender)) %>% nrow()

b_c_summary <- get_summary_stats(b_c_data)

#compare genderize outcomes using country codes to nichole's data----
country_gender_data <- read_csv("../data/B_C_auth_country_genderize_join.csv") 

country_compared_names <- country_gender_data %>% 
  filter(!is.na(actual.gender) & !is.na(genderize.gender)) %>% nrow()

country_summary <- get_summary_stats(b_c_data)

#compare outcomes after having converted to ASCII, w. no country data----
b_c_data_ascii <- read_csv("../data/B_C_auth_genderize_join_nosp.csv")

total_names_ascii <- nrow(b_c_data_ascii)

unknown_names_ascii <- b_c_data_ascii %>% filter(is.na(actual.gender)) %>% nrow() 

compared_names_ascii <- b_c_data_ascii %>% filter(!is.na(actual.gender) & !is.na(genderize.gender)) %>% nrow()

na_names_ascii <- b_c_data_ascii %>% filter(is.na(genderize.gender)) %>% nrow()

b_c_ascii_summary <- get_summary_stats(b_c_data_ascii)

#compare outcomes after having converted to ASCII, with country data----
b_c_country_data_ascii <- read_csv("../data/B_C_auth_country_genderize_join_nosp.csv")

country_ascii_compared_names <- b_c_country_data_ascii %>% 
  filter(!is.na(actual.gender) & !is.na(genderize.gender)) %>% nrow()

b_c_country_ascii_summary <- get_summary_stats(b_c_country_data_ascii)

#summary table using ascii data -- slightly decreased accuracy but increased the proportion of names genderized
stats_summary <- left_join(b_c_ascii_summary, b_c_country_ascii_summary, by = "Measure")
