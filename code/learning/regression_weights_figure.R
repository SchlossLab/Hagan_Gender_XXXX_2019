# Author: Begum Topcuoglu
# Date: 2018-05-24
#
######################################################################
# This script looks at the model interpretation
######################################################################


######################################################################
#----------------- Read in necessary libraries -------------------#
######################################################################
deps = c("cowplot","reshape2", "cowplot", "ggplot2","knitr","rmarkdown","vegan","gtools", "tidyverse");
for (dep in deps){
  if (dep %in% installed.packages()[,"Package"] == FALSE){
    install.packages(as.character(dep), quiet=TRUE);
  }
  library(dep, verbose=FALSE, character.only=TRUE)
}
######################################################################
#----------------- Define the functions we will use -----------------#
######################################################################


# -------------------- Read files ------------------------------------>
# This function:
#     1. takes a list of files(with their path)
#     2. reads them as delim files with comma seperator
#     3. returns the dataframe
read_files <- function(filenames){
  for(file in filenames){
    # Read the files generated in main.R 
    # These files have cvAUCs and testAUCs for 100 data-splits
    data <- read.delim(file, header=T, sep=',')
  }
  return(data)
}
# -------------------------------------------------------------------->


# ------------------- Re-organize feature importance  ----------------->
# This function:
#     1. Takes in a dataframe (different data for each model) and the model name
#     2. If the models are linear, returns the median and sd weights of highest weight 10 features
#     3. If the models are not linear, returns the permutation importance results for:
#         - Correlated and non-correlated OTUs:
#         - Top 10 features or feature groups will be listed
#         - median percent AUROC change from original AUROC after permutation
get_interp_info <- function(data, model_name){ 
  if(model_name=="L2_Logistic_Regression"){
    # If the models are linear, we saved the weights of every OTU for each datasplit
    # 1. Get dataframe transformed into long form
    #         The OTU names are in 1 column(repeated for 100 datasplits)
    #         The weight value are in 1 column(for each of the datasplits)
    weights <- data %>% 
      select(-Bias, -model) %>% 
      gather(factor_key=TRUE) %>% 
      # 2. Group by the OTU name and compute median and sd for each OTU
      group_by(key) %>% 
      summarise(median_weights = median(value), sd_weights = sd(value)) %>% 
      # 2. We now want to save to a new column the sign of the weights
      mutate(sign = case_when(median_weights<0 ~ "negative",
                              median_weights>0 ~ "positive",
                              median_weights==0 ~ "zero")) 
    # 3. We change all the weights to their absolute value
    #       Because we want to see which weights are the largest 
    weights$median_weights <- abs(weights$median_weights)
    # 4.  a) Order the dataframe from largest weights to smallest.
    #     b) Select the largest 10 
    #     c) Put the signs back to weights
    #     d) select the OTU names, median weights with their signs and the sd
    imp <- weights %>% 
      arrange(desc(median_weights)) %>% 
      head(n=10) %>% 
      mutate(median_weights = case_when(sign=="negative" ~ median_weights*-1,
                                      sign=="positive"~ median_weights)) %>% 
      select(key, median_weights, sd_weights)
    
  }
  else{print("Something is wrong")}
  return(imp)
}
# -------------------------------------------------------------------->



######################################################################
#--------------Run the functions and plot importance ----------#
######################################################################

# ----------- Read in saved combined feature importances ---------->
# List the important features files by defining a pattern in the path
# The weights are saved in non-corellated files
non_cor_files <-  list.files(path= "code/learning/data/process/", pattern='combined_all_imp_features_non_cor_.*', full.names = TRUE)
# -------------------------------------------------------------------->

# ----------- Loops to re-organize feature importance info ---------->
# This loop will:
#   1. Read the model files saved in 'interp_files' list
#   2. Get the model name from the file
#   3. Use te get_interp_info() for each model. 
#   4. Save the top 10 features and their median, sd importance value in a .tsv file

for(file_name in non_cor_files){
  importance_data <- read_files(file_name)
  model_name <- as.character(importance_data$model[1]) # get the model name from table
  get_interp_info(importance_data, model_name) %>% 
    as.data.frame() %>% 
    write_tsv(., paste0("code/learning/data/process/", model_name, "_non_cor_importance.tsv"))
}
# -------------------------------------------------------------------->


######################################################################
#-------------- Plot the weights of linear models ----------#
######################################################################

# -----------------------Base plot function -------------------------->
# We will plot the median feature weights for top 10 OTUs
# Define the base plot for the linear modeling methods
base_plot <-  function(data, x_axis, y_axis){
  plot <- ggplot(data, aes(fct_reorder(x_axis, -abs(y_axis)), y_axis)) +
    geom_point(colour = "brown2", size = 3) +
    theme_classic() +
    scale_x_discrete(name = "") +
    theme(legend.text=element_text(size=18),
          legend.title=element_text(size=22),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          text = element_text(size = 12),
          axis.text.x=element_text(size = 8, colour='black'),
          axis.text.y=element_text(size = 12, colour='black'),
          axis.title.y=element_text(size = 13),
          axis.title.x=element_text(size = 13), 
          panel.border = element_rect(colour = "black", fill=NA, size=1)) +
    geom_hline(yintercept=0, linetype="dashed", 
               color = "black")
  return(plot)
}
# ----------------------------------------------------------------------->

# ------------------- L2 logistic regression ---------------------------->
logit <- read.delim("code/learning/data/process/L2_Logistic_Regression_non_cor_importance.tsv", header=T, sep='\t') 
logit_plot <- base_plot(logit, x=logit$key, y=logit$median_weights) +
  scale_y_continuous(name="L2 logistic regression coefficients",
                     limits = c(-3, 3),
                     breaks = seq(-3, 3, 0.5)) +   
  geom_errorbar(aes(ymin=logit$median_weights-logit$sd_weights, 
                    ymax=logit$median_weights+logit$sd_weights), 
                width=.01)
# ----------------------------------------------------------------------->


######################################################################
#-----------------------Save figure as .pdf ------------------------ #
######################################################################
#combine with cowplot
linear <- plot_grid(logit_plot, labels = c("A"))

ggsave("weights_all_journals_published_or_not.png", plot = linear, device = 'png', width = 15, height = 5)




