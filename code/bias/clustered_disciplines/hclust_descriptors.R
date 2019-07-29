source("code/bias/clustered_disciplines/tidy_descriptors.R")

unique_dist <- unique(as.character(tidy_descriptors$word))

dist  <- adist(unique_dist)
rownames(dist) <- tidy_descriptors$grouped.random
hc <- hclust(as.dist(dist))
plot(hc)

rect.hclust(hc,k=1000)
df <- data.frame(uniquemodels,cutree(hc,k=1000))