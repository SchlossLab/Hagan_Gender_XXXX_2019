library(tidyverse)
library(NbClust)
library(biganalytics)

desc_matrix <- read_csv("data/desc_matrix.csv")

test_matrix <- desc_matrix %>% select(-grouped.random, -journal.val) %>% data.matrix() %>% head(n = 1000)

set.seed(1234)

library(doMC)
registerDoMC(2)

r <- foreach(i=15, j=100) %dopar% {
  nc <- NbClust(test_matrix, min.nc=i, max.nc=j, method="kmeans")
  
  clust_test <- table(nc$Best.n[1,])
}

nc <- NbClust(test_matrix, min.nc=10, max.nc=99, method="kmeans")

print("NbClust complete")

save.image()

clust_test <- table(nc$Best.n[1,])

save.image()

write_csv(clust_test, "data/clust_test_2019_25_07.csv")

print("clust output saved")