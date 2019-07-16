library(rlang) #required for list_to_sent()
library(tidyverse)

#calculate two decimal percentage given num & denom
get_percent <- function(x, y){
  z <- (as.numeric(x)/as.numeric(y))*100
  percent <- round(z, digits = 2)
  return(percent)
}

#developed for alluvial plots - turn Na values to 'no' & counts to "yes"
recode_role <- function(x){
  if_else(is.na(x), "no", "yes")
}

is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

#return a list of ranked variables prepared for a sentence. e.g., Ireland, UK, and China
list_to_sent <- function(df, sort, n, pull){ 
  
  sort_quo <- parse_quo(sort, env = caller_env()) #parse sorting col 
  
  df %>% 
    arrange(desc(!!sort_quo)) %>% #arrange df by sorting col
    when(
      n >= 0 ~ head(., n = n), #if n is +, get first n rows
      ~ tail(., n = -n) #if n is -, get last n rows
      ) %>% 
    pull(pull) %>% #convert "pull" column to a vector
    paste(., sep = "", collapse = ", ") %>% #collapse into a string
    str_replace(., ",\\s(?=[:alpha:]+$)", ", and ")#add oxford comma, &
}

#function to filter an author type from a df
get_auth_type <- function(x, df){
  
  #select filtering criteria & parse for filter()
  auth_type <- case_when(
    x == "middle" ~ paste("author.last != 'true' & author.seq != '1'"),
    x == "first" ~ paste("author.seq == '1'"),
    x == "last" ~ paste("author.last == 'true'"),
    x == "corres" ~ paste("author.corres == 'true'")
  ) %>% rlang::parse_expr()
  
  #filter dataset
  data <- df %>% 
    filter(!!auth_type)
  
  return(data)  
}

#function to use with map & a vector of years to generate n & prop of a specified grouping (e.g., gender, region) by yr
get_prop_by_yr <- function(x, df, group, journ){
  
  #quo_group <- enquo(group) #allows group_by to interpret the variable
  #print(paste(x, journ, "start"))
  
  journals <- df %>% pull(journal) %>% unique()
  
  t_data <- if(journ == "All"){#generate a df combining data from all journals for the given year
    df %>% 
      filter(year == x) %>% #restrict to single year
      select(!!sym(group), year, random.person.id) %>% distinct() %>% 
      group_by(!!sym(group)) %>% summarise(n = n()) %>% #calculate number of each gender in that year
      mutate(proportion = get_percent(n, sum(n))) %>% #add column calculating the proportions for the year
      cbind(year = x, journal = journ, .) #create df of data & specify year & journal variables used
  }else if(journ == "Each"){#generate a df of data for every journal for the specified yr(s)
    
    test <- map_dfr(journals, function(j){#map through the following function for each journal
      
      quo_journ <- enquo(j)#allow journal name to be used w/in filter
      
    tryCatch(
      df %>% filter(journal == !!j) %>% #select journal
      filter(year == x) %>% #restrict to single year
        select(!!sym(group), year, random.person.id) %>% distinct() %>% 
        group_by(!!sym(group)) %>% summarise(n = n()) %>% #calculate number of each gender in that year
      mutate(proportion = get_percent(n, sum(n))) %>% #add column calculating the proportions for the year, requires analysis_functions.R
      cbind(year = x, journal = j, .), #add column specifying the year
      error = function(e) {tibble(year = x, journal = j, quo_group := "NA", n = as.numeric("0"), proportion = as.numeric("0"))}) #if nothing present, return NA value in a dataframe 
    })
    return(test)
    }else if(journ != "All"|journ != "Each"){
    tryCatch(df %>% filter(journal == journ) %>% 
      filter(year == x) %>% #restrict to single year
        select(!!sym(group), year, random.person.id) %>% distinct() %>% 
        group_by(!!sym(group)) %>% summarise(n = n()) %>% #calculate number of each gender in that year
      mutate(proportion = get_percent(n, sum(n))) %>% #add column calculating the proportions for the year, requires analysis_functions.R
      cbind(year = x, journal = journ, .),#add column specifying the year
      error = function(e) {tibble(year = x, journal = journ, !!quo_group := "NA", n = as.numeric("0"), proportion = as.numeric("0"))}) #if nothing present, return NA value in a dataframe 
    
  }else{print("error")}
  
  #print(paste(x, journ, "done"))
  
  return(t_data)
}

