library(stringdist)
library(tidyverse)

descript_test <- clean_title_data %>% 
  select(-random.manu.num) %>% distinct()

uniquemodels <- unique(as.character(descript_test$descriptors))
distancemodels <- stringdistmatrix(uniquemodels, uniquemodels, method = "jw")
rownames(distancemodels) <- as.character(descript_test$grouped.random)
hc <- hclust(as.dist(distancemodels))
plot(hc)
rect.hclust(hc,k=20)