#plot logistic regression data outputs for prediction of corresponding author gender by the proportion of women authors

feat_weights <- read_csv("../data/combined_all_imp_features_pred_rej_gend.csv") 

auc <- read_csv("../data/combined_best_hp_results_pred_rej_gend.csv") 

published_weights <- feat_weights %>% select(-Bias, -model) %>% 
  gather(., key = "features", value = "weights") %>% 
  filter(!str_detect(features, "x")) %>% 
  mutate(weights = as.numeric(weights)) %>% 
  group_by(features) %>% 
  summarise(med.weight = median(weights)) %>% 
  arrange(desc(med.weight))

med_auc_US <- round(median(auc$test_aucs), digits = 2)
