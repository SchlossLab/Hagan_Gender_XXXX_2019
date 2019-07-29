library(stringdist)
library(tidyverse)

clean_title_data <- read_csv("data/2018_title_keyword_data.csv")

descript_test <- clean_title_data %>% 
  select(-random.manu.num) %>% distinct() %>% head(n=500)

uniquemodels <- unique(as.character(descript_test$descriptors))
distancemodels <- stringdistmatrix(uniquemodels, uniquemodels, method = "jw")
rownames(distancemodels) <- as.character(descript_test$grouped.random)
hc <- hclust(as.dist(distancemodels))
plot(hc)
rect.hclust(hc,k=20)

kpm <- stringdistmatrix(uniquemodels,useNames="strings",method="lv")
rownames(kpm) <- as.character(descript_test$grouped.random)
kpm_hc <- hclust(as.dist(kpm))
plot(kpm_hc)
plot(hclust(kpm,method = "ward"))

d  <- adist(uniquemodels)
rownames(d) <- descript_test$grouped.random
hc <- hclust(as.dist(d))
plot(hc)
rect.hclust(hc,k=5)
df <- data.frame(uniquemodels,cutree(hc,k=5))
