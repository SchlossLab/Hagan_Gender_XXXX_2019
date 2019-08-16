
# Author: Begum Topcuoglu
# Date: 2019-04-23
######################################################################
# Description:
# This script trains and tests the model according to proper pipeline
######################################################################

######################################################################
# Dependencies and Outputs:
#    Model to put to function:
#       1. "L2_Logistic_Regression"
#       2. "L1_Linear_SVM" and "L2_Linear_SVM"

#    Dataset to put to function:
#         Features: First author gender
#                   Last author gender
#                   Corresponding author gender
#                   Editor gender
#                   Senior editor gender
#                   Reviewever gender proportion
#         Labels: Published or not
#
# Usage:
# Call as source when using the function. The function is:
#   pipeline(data, model)

# Output:
#  A results list of:
#     1. cvAUC and testAUC for 1 data-split
#     2. cvAUC for all hyper-parameters during tuning for 1 datasplit
#     3. feature importance info on first 10 features for 1 datasplit
#     4. trained model as a caret object
######################################################################

######################################################################
#------------------------- DEFINE FUNCTION -------------------#
######################################################################


pipeline <- function(dataset, model){
  
  # ------------------Pre-process the full Dataset------------------------->    
  # We are doing the pre-processing to the full dataset and then splitting 80-20
  # Scale all features between 0-1
  preProcValues <- preProcess(dataset, method = "range")
  dataset <- predict(preProcValues, dataset)
  # ----------------------------------------------------------------------->  

  # ------------------80-20 Datasplit for each seed------------------------->
  # Do the 80-20 data-split
  # Stratified data partitioning %80 training - %20 testing
  inTraining <- createDataPartition(dataset$reviewed, p = .80, list = FALSE)
  training <- dataset[ inTraining,]
  testing  <- dataset[-inTraining,]
  # ----------------------------------------------------------------------->

  # -------------Define hyper-parameter and cv settings-------------------->
  # Define hyper-parameter tuning grid and the training method
  grid <- tuning_grid(training, model)[[1]]
  method <- tuning_grid(training, model)[[2]]
  cv <- tuning_grid(training, model)[[3]]
  # ----------------------------------------------------------------------->

  # ---------------------------Train the model ---------------------------->
  # ------------------------------- 1. -------------------------------------
  # - We train on the 80% of the full dataset.
  # - We use the cross-validation and hyper-parameter settings defined above to train
  # ------------------------------- 2. -------------------------------------
  # We use ROC metric for all the models
  # To do that I had to make changes to the caret package functions.
  # The files 'data/caret_models/svmLinear3.R and svmLinear5.R are my functions.
  # I added 1 line to get Decision Values for linear SVMs:
  #
  #           prob = function(modelFit, newdata, submodels = NULL){
  #             predict(modelFit, newdata, decisionValues = TRUE)$decisionValues
  #           },
  #
  # This line gives decision values instead of probabilities and computes ROC in:
  #   1. train function with the cross-validataion
  #   2. final trained model
  # using decision values and saves them in the variable "prob"
  # ------------------------------- 3. --------------------------------------
  # - If the model is logistic regression, we need to add a family=binomial parameter.
  # - If the model is random forest, we need to add a ntree=1000 parameter.
  #         We chose ntree=1000 empirically.
  # ----------------------------------------------------------------------->
  if(model=="L2_Logistic_Regression"){
  print(model)
  trained_model <-  train(reviewed ~ ., # label
                          data=training, #total data
                          method = method,
                          trControl = cv,
                          metric = "ROC",
                          tuneGrid = grid,
                          family = "binomial")
  }
  else{
    print("Something is wrong")
  }

  # ------------- Output the cvAUC and testAUC for 1 datasplit ---------------------->
  # Mean cv AUC value over repeats of the best cost parameter during training
  cv_auc <- getTrainPerf(trained_model)$TrainROC
  # Save all results of hyper-parameters and their corresponding meanAUCs over 100 internal repeats
  results_individual <- trained_model$results
  # ---------------------------------------------------------------------------------->

  # -------------------------- Feature importances ----------------------------------->
  #   Output the weights of features of linear models
  #   Output the feature importances based on random permutation for non-linear models
  # Here we look at the top 10 important features
    # Predict on the test set and get predicted probabilities or decision values
    rpartProbs <- predict(trained_model, testing, type="prob")
    # Calculate the ROC for each model
    test_roc <- roc(ifelse(testing$reviewed == "yes", 1, 0), rpartProbs[[1]])
    # Get the AUROC value for test set
    test_auc <- test_roc$auc
    # Get feature weights
    feature_importance_non_cor <- trained_model$finalModel$W
    feature_importance_cor <- trained_model$finalModel$W
  # ---------------------------------------------------------------------------------->

  # ----------------------------Save metrics as vector ------------------------------->
  # Return all the metrics
  results <- list(cv_auc, test_auc, results_individual, feature_importance_non_cor, feature_importance_cor, trained_model)
  return(results)
}
