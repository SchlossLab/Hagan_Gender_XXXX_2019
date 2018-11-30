#library(tidyverse)

get_percent <- function(x, y){
  z <- (as.numeric(x)/as.numeric(y))*100
  percent <- round(z, digits = 2)
  return(percent)
}


is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}