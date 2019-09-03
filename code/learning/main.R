######################################################################
# Author: Begum Topcuoglu
# Date: 2018-04-23
# Title: Main pipeline learning and testing classifier for gender bias
######################################################################

######################################################################
# Description:

# This script will read in data from Ada's ASM dataset

# It will run the following machine learning pipelines:
#     - L2 Logistic Regression

######################################################################

######################################################################
# Dependencies and Outputs:

# Be in the project directory.

# The outputs are:
#   (1) AUC values for cross-validation and testing for each data-split
#   (2) meanAUC values for each hyper-parameter tested during each split.
######################################################################


################### IMPORT LIBRARIES and FUNCTIONS ###################
# The dependinces for this script are consolidated in the first part
deps = c("dummies", "dplyr", "tictoc", "caret" ,"rpart", "kernlab","LiblineaR", "pROC", "tidyverse");
for (dep in deps){
  if (dep %in% installed.packages()[,"Package"] == FALSE){
    install.packages(as.character(dep), quiet=TRUE, repos = "http://cran.us.r-project.org", dependencies=TRUE);
  }
  library(dep, verbose=FALSE, character.only=TRUE)
}
# Load in needed functions and libraries
source('code/learning/model_selection.R')
source('code/learning/model_pipeline.R')
source('code/learning/generateAUCs.R')
######################################################################

######################## DATA PREPARATION #############################
# Read in the gender data
data <- read.csv("code/learning/reject_predict.csv")

data <- data %>%
  select(-journal) %>% 
  drop_na() %>% 
  droplevels()

## Converting to factors
for (i in c( "corres.auth", "editor", "sen.editor", "editorial.reject", "US.inst", "US.inst.type", "inst.gender", "US.gender" )){
  data[,i]=as.factor(data[,i])
}
# Create dummy variables
new_data <- dummy.data.frame(data, names=c("editor", "sen.editor", "corres.auth", "US.inst", "US.inst.type", "inst.gender", "US.gender" ), sep=".")


# Convert the label to a factor
new_data$editorial.reject <- as.factor(new_data$editorial.reject)

###################################################################

######################## RUN PIPELINE #############################
# Choose which classification methods we want to run on command line
#                "L2_Logistic_Regression",
#                "L1_Linear_SVM",
#                "L2_Linear_SVM",

# We will run main.R from command line with arguments
#  - These arguments will be saved into variable "input"
#  - First argument is the seed number which is the array index
#  - Second argument is the model name (one of the list above)

input <- commandArgs(trailingOnly=TRUE)
seed <- as.numeric(input[1])
model <- input[2]

set.seed(4)
new_data <- new_data[sample(1:nrow(new_data)), ]

# Then arguments 1 and 2 will be placed respectively into the functions:
#   1. set.seed() : creates reproducibility and variability
#   2. get_results(): self-defined function that
#                     - runs the modeling pipeline
#                     - saves performance and hyper-parameters and imp features
set.seed(seed)
# Start walltime for running model
tic("model")
# Run the model
get_results(new_data, model, seed)
# Stop walltime for running model
secs <- toc()
# Save elapsed time
walltime <- secs$toc-secs$tic
# Save wall-time
write.csv(walltime, file=paste0("code/learning/data/temp/walltime_", model, "_", seed, ".csv"), row.names=F)
###################################################################
