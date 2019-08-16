#plot logistic regression data outputs for prediction of corresponding author gender by the proportion of women authors

feat_weights <- read_csv("data/combined_all_imp_features_published.csv") 

auc <- read_csv("data/combined_best_hp_results_published.csv") 

prop_fem_median <- feat_weights %>% 
  pull(prop.fem.auth) %>% 
  median(abs(.)) %>% round(digits = 2)

second_median <- feat_weights %>% select(-Bias, -model) %>% 
  gather(., key = "features", value = "weights") %>% 
  filter(!str_detect(features, "x")) %>% 
  mutate(weights = as.numeric(weights)) %>% 
  group_by(features) %>% 
  summarise(med.weight = median(weights)) %>% 
  arrange(desc(med.weight))
