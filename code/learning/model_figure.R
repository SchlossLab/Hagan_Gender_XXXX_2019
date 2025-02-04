# Author: Begum Topcuoglu
# Date: 2018-02-12
#
######################################################################
# This script plots Figure 1:
#   1. cvAUC (means of 100 repeats for the best hp) of 100 datasplits
#   2. testAUC of 100 datasplits
######################################################################

######################################################################
# Load in needed functions and libraries
source('code/learning/functions.R')
# detach("package:randomForest", unload=TRUE) to run
######################################################################


######################################################################
# Load .csv data generated with modeling pipeline
######################################################################

# Read in the cvAUCs, testAUCs for 100 splits.
best_files <- list.files(path= 'code/learning/data/process', pattern='combined_best.*', full.names = TRUE)


logit <- read_files(best_files[1])




best_performance <- logit %>%
  melt_data()

######################################################################
#Plot the AUC values for cross validation and testing for each model #
######################################################################


performance <- ggplot(best_performance, aes(x = fct_reorder(model, AUC), y = AUC, fill = Performance)) +
  geom_boxplot(alpha=0.7) +
  scale_fill_manual(values=c("#999999", "#E69F00")) +
  scale_y_continuous(name = "AUROC",
                     breaks = seq(0.5, 1, 0.1),
                     limits=c(0.5, 1),
                     expand=c(0,0)) +
  scale_x_discrete(name = "") +
  #                 labels=c("L2 Linear SVM",
   #                         "RBF SVM",
    #                        "L2 Logistic Regression",
     #                       "Decision Tree",
      #                      "L1 Linear SVM",
       #                     "XGBoost",
        #                    "Random Forest")) +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.background = element_rect(size=0.5, linetype="solid", color="black"),
        legend.justification=c(0,1),
        legend.position=c(0,1),
        legend.box.margin=margin(c(10,10,10,10)),
        legend.text=element_text(size=20),
        #legend.title=element_text(size=22),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        text = element_text(size = 12),
        axis.text.x=element_text(size = 15, colour='black'),
        axis.text.y=element_text(size = 12, colour='black'),
        axis.title.y=element_text(size = 30),
        axis.title.x=element_text(size = 20),
        panel.border = element_rect(colour = "black", fill=NA, size=1))

######################################################################
#-----------------------Save figure as .pdf ------------------------ #
######################################################################

ggsave("model_performance.pdf", plot = performance, device = 'pdf', width = 5, height = 8)
