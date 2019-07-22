library(tidyverse)
library(NbClust)

descriptors <- read_csv("data/valued_descriptors.csv")

journ_desc <- descriptors %>% 
  filter(type == "journal") %>% 
  select(-word, -type) %>% distinct() %>% 
  rename("journal.val" = "value")

cat_desc <- descriptors %>% 
  filter(type == "category") %>% 
  select(-word, -type) %>% distinct() %>% 
  rename("category.val" = "value")

max_desc <- descriptors %>% group_by(grouped.random) %>% summarise(n = n()) %>% arrange(desc(n))
#limit to papers with at least 8 unique descriptors, restrict to 8

keyword_desc <- descriptors %>% 
  filter(is.na(type)) %>% 
  select(-type, -word) %>% distinct() %>% 
  rename("keyword.val" = value)

manus <- descriptors %>% pull(grouped.random) %>% unique()

eight_desc <- map_dfr(manus, function(x){
  
  this_manu <- keyword_desc %>% filter(grouped.random == x) %>% distinct()
  
  df <- this_manu %>% head(n = 8) %>% select(-grouped.random) %>% 
      t() %>% as.tibble() %>% cbind(grouped.random = x, .)
  
  return(df)
}) %>% filter(!is.na(V8))

#set.seed(1234)
#nc <- NbClust(df, min.nc=2, max.nc=15, method="kmeans")
#table(nc$Best.n[1,])