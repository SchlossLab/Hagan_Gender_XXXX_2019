library(tidyverse)
library(NbClust)

desc_matrix <- read_csv("data/desc_matrix.csv")

set.seed(1234)

nc <- NbClust(desc_matrix, min.nc=15, max.nc=100, method="kmeans")

print("NbClust complete")

save.image()

clust_test <- table(nc$Best.n[1,])

save.image()

write_csv(clust_test, "data/clust_test_2019_22_07.csv")

print("clust output saved")