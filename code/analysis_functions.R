#library(tidyverse)

get_percent <- function(x, y){
  z <- (as.numeric(x)/as.numeric(y))*100
  percent <- round(z, digits = 2)
  return(percent)
}


is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

#function to filter an author type from a df
get_auth_type <- function(x, df){
  
  #select filtering criteria & parse for filter()
  auth_type <- case_when(
    x == "middle" ~ paste("author.last != 'TRUE' & author.seq != '1'"),
    x == "first" ~ paste("author.seq == '1'"),
    x == "last" ~ paste("author.last == 'TRUE'"),
    x == "corres" ~ paste("author.corres == 'TRUE'")
  ) %>% rlang::parse_expr()
  
  #filter dataset
  data <- df %>% 
    filter(!!auth_type)
  
  return(data)  
}
