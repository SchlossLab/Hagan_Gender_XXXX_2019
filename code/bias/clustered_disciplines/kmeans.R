library(tidyverse)
library(NbClust)

desc_matrix <- read_csv("data/desc_matrix.csv")

test_matrix <- desc_matrix %>% select(-grouped.random, -journal.val) %>% data.matrix() %>% head(n=1000)

set.seed(1234)

nc <- NbClust(test_matrix, min.nc=10, max.nc=99, method="kmeans")

big_test <- bigkmeans(test_matrix, 15, iter.max = 100, nstart = 1, dist = "euclid")

print("NbClust complete")

save.image()

clust_test <- table(nc$Best.n[1,])

save.image()

write_csv(clust_test, "data/clust_test_2019_22_07.csv")

print("clust output saved")