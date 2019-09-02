#logistic regression data outputs for prediction of whether or not a paper was reviewed based on gender-data


#calculate auc
auc <- read_csv("../data/combined_best_hp_results_gender_review.csv") 

med_auc <- median(auc$test_aucs) %>% round(., digits = 2)

#compare median feature weights----

#feat_weights <- read_csv("data/combined_all_imp_features_gender_review.csv") 

#second_median <- feat_weights %>% select(-Bias, -model) %>% 
#  gather(., key = "features", value = "weights") %>% 
#  filter(!str_detect(features, "x")) %>% 
#  mutate(weights = as.numeric(weights)) %>% 
#  group_by(features) %>% 
#  summarise(med.weight = median(weights)) %>% 
#  arrange(desc(med.weight))


