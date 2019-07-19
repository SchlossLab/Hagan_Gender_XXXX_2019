library(stringdist)
library(tidyverse)
library(tidytext)

data("stop_words")

clean_title_data <- read_csv("data/2018_title_keyword_data.csv")

descript_test <- clean_title_data %>% 
  select(-random.manu.num) %>% distinct() %>% head(n=500)

tidy_journals <- descript_test %>% 
  select(journal, category, grouped.random) %>% 
  distinct() %>% gather(journal, category, key = "type", value = "word") %>% 
  distinct()

tidy_descriptors <- descript_test %>% 
  unnest_tokens(word, title) %>% 
  unnest_tokens(word, keywords) %>% 
  distinct() %>% select(-journal, -category) %>% 
  anti_join(stop_words) %>% 
  full_join(., tidy_journals, by = c("grouped.random", "word")) %>% 
  distinct() %>% 
  filter(!is.na(word))
