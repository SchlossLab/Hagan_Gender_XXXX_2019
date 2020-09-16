library(rlang) #required for list_to_sent()
#library(tidyverse)

#calculate two decimal percentage given num & denom
get_percent <- function(x, y){
  z <- (as.numeric(x)/as.numeric(y))*100
  percent <- round(z, digits = 1)
  return(percent)
}

`%not_in%` <- Negate(`%in%`)

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
    x == "corresponding" ~ paste("author.corres == 'TRUE'")
  ) %>% rlang::parse_expr()
  
  #filter dataset
  data <- df %>% 
    filter(!!auth_type)
  
  return(data)  
}

#developed for alluvial plots - turn Na values to 'no' & counts to "yes"
recode_role <- function(x){
  if_else(is.na(x), "no", "yes")
}

#return a list of ranked variables prepared for a sentence. e.g., Ireland, UK, and China
list_to_sent <- function(df, sort, n, pull){ 
  
  sort_quo <- parse_quo(sort, env = caller_env()) #parse sorting col 
  
  collapse <- df %>% 
    arrange(desc(!!sort_quo)) %>% #arrange df by sorting col
    when(
      n >= 0 ~ head(., n = n), #if n is +, get first n rows
      ~ tail(., n = -n) #if n is -, get last n rows
    ) %>% 
    pull(pull) %>% #convert "pull" column to a vector
    paste(., sep = "", collapse = ", ")#collapse into a string
  
  sent <- collapse %>% 
    str_replace("fixed((?<=,\\s.,\\s.),[[:space:]])", ", and ")#add oxford comma, &
  
  return(sent)
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

get_sub_pub_prop <- function(sub_df, pub_df, prop){
  
  temp_sub <- as.character(sub_df) 
  
  temp_pub <- as.character(pub_df)
  
  sub_df <- get(temp_sub) #pull this df from the global environment
  
  pub_df <- get(temp_pub)
  
  sub_c_authors_w_prop <- map_dfr(years, function(x){
    get_prop_by_yr(x, sub_df, "gender", prop) %>% 
      mutate(manu.type = "submitted")}) 

  pub_c_authors_w_prop <- map_dfr(years, function(x){
    get_prop_by_yr(x, pub_df, "gender", prop) %>% 
      mutate(manu.type = "published")}) 

  c_authors_w_prop <- rbind(sub_c_authors_w_prop, pub_c_authors_w_prop) 
  
  return(c_authors_w_prop)
}

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