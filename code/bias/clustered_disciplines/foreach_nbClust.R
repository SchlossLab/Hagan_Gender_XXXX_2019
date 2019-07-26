library(tidyverse)
library(NbClust)
library(biganalytics)

desc_matrix <- read_csv("data/desc_matrix.csv")

test_matrix <- desc_matrix %>% select(-grouped.random) %>% data.matrix()

set.seed(1234)

r <- foreach(i=15, j=100) %dopar% {
  nc <- NbClust(test_matrix, min.nc=i, max.nc=j, method="kmeans")
  
  clust_test <- table(nc$Best.n[1,])
}

print("NbClust complete")

save.image()

write_csv(r, "data/clust_test_2019_26_07.csv")

print("clust output saved")

results_tbl <- r[[1]] %>% as.tibble() %>% arrange(desc(n))

num_clus <- results_tbl[1,1] %>% pull()

kmeans <- bigkmeans(test_matrix, centers = num_clus, iter.max = 100000, nstart = 1, dist = "euclid")

save.image()

print("kmeans complete")

clusters <- kmeans[[1]] %>% as.tibble()

clustered_manus <- desc_matrix %>% 
  select(grouped.random) %>% 
  cbind(., clusters)

write_csv(clustered_manus, "data/clust_manus_2019_26_07.csv")

print("clust manus saved")
