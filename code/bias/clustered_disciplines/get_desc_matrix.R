library(tidyverse)
library(NbClust)

descriptors <- read_csv("data/valued_descriptors.csv")

journ_desc <- descriptors %>% 
  filter(type == "journal") %>% 
  select(-word, -type) %>% distinct() %>% 
  rename("journal.val" = "value")

keyword_desc <- descriptors %>% 
  filter(is.na(type)) %>% 
  select(-type, -word) %>% distinct() %>% 
  rename("keyword.val" = value)

manus <- descriptors %>% pull(grouped.random) %>% unique()

#limit to papers with at least 8 unique descriptors, restrict to 8
eight_desc <- map_dfr(manus, function(x){
  
  this_manu <- keyword_desc %>% filter(grouped.random == x) %>% distinct()
  
  df <- this_manu %>% head(n = 8) %>% select(-grouped.random) %>% 
      t() %>% as.tibble() %>% cbind(grouped.random = x, .)
  
  return(df)
}) %>% filter(!is.na(V8))

print("eight desc done")

cat_desc <- descriptors %>% 
  filter(type == "category") %>% 
  select(-word, -type) %>% distinct() %>% 
  rename("category.val" = "value")

one_cat <- map_dfr(manus, function(x){
  
  cat_desc %>% filter(grouped.random == x) %>% 
    distinct() %>% head(n = 1)

})

print("one cat done")

all_desc <- eight_desc %>% 
  left_join(., journ_desc, by = "grouped.random") %>% 
  left_join(., one_cat, by = "grouped.random") %>% 
  distinct()

write_csv(all_desc, "data/desc_matrix.csv")

print("desc_matrix saved")

