library(stringdist)
library(tidyverse)
library(tidytext)

data("stop_words")

clean_title_data <- read_csv("data/2018_title_keyword_data.csv")

descript_test <- clean_title_data %>% 
  select(-random.manu.num) %>% distinct()

#convert journal & category values to "words"
tidy_journals <- descript_test %>% 
  select(journal, category, grouped.random) %>% 
  distinct() %>% gather(journal, category, key = "type", value = "word") %>% 
  distinct()

tidy_descriptors <- descript_test %>% 
  unnest_tokens(word, title) %>% #split words in title
  unnest_tokens(word, keywords) %>% #split keywords
  distinct() %>% select(-journal, -category) %>% 
  anti_join(stop_words) %>% #drop prepositions
  full_join(., tidy_journals, 
            by = c("grouped.random", "word")) %>% #rejoin journal/cat values but in the "word" column
  distinct() %>% 
  filter(!is.na(word)) #filter out instances where there wasn't a category/journal

#unique list of descriptors
unique_dist <- tidy_descriptors %>% select(word) %>% distinct()

#assign each descriptor a random value
random_value <- sample(1:nrow(unique_dist), nrow(unique_dist), replace = FALSE) %>% #generate unique random number for each
  as_tibble() %>% cbind(., unique_dist) #bind to list of manu nums

#distribute random values to tidied dataset
value_assigned <- tidy_descriptors %>% 
  left_join(., random_value, by = "word") %>% 
  distinct()

write_csv(value_assigned, "data/valued_descriptors.csv")