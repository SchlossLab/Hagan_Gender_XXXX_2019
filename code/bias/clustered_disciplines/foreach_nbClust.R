library(tidyverse)
library(NbClust)
library(biganalytics)

desc_matrix <- read_csv("data/desc_matrix.csv")

test_matrix <- desc_matrix %>% select(-grouped.random) %>% data.matrix()

set.seed(1234)

library(doMC)
registerDoMC(2)

r <- foreach(i=15, j=100) %dopar% {
  nc <- NbClust(test_matrix, min.nc=i, max.nc=j, method="kmeans")
  
  clust_test <- table(nc$Best.n[1,])
}

print("NbClust complete")

save.image()

write_csv(r, "data/clust_test_2019_26_07.csv")

print("clust output saved")