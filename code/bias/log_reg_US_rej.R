#plot logistic regression data outputs for prediction of editorial rejections using all meta-data
library(tidyverse)

feat_weights <- read_csv("data/combined_all_imp_features_US_rej.csv") 

auc <- read_csv("data/combined_best_hp_results_US_rej.csv") 

published_weights <- feat_weights %>% select(-Bias, -model) %>% 
  gather(., key = "features", value = "weights") %>% 
  filter(!str_detect(features, "x")) %>% 
  mutate(weights = as.numeric(weights)) %>% 
  group_by(features) %>% 
  summarise(avg.weight = mean(weights), sd.weight = sd(weights)) %>% 
  mutate(clean_feat = features %>% str_replace("\\.{3}", " & ") %>% 
           str_replace("\\.female", "\\.women") %>% 
           str_replace("\\.male", "\\.men") %>% 
           str_replace_all("X.inst.gender.|X.US.inst.type.", "") %>% 
           str_replace_all("US.gender.no.|US.inst.no", "Non-US ") %>% 
           str_replace_all("(?<=US.)gender.yes|inst.yes", "") %>% 
           str_replace_all("inst.gender.Other.", "Other US Inst ") %>% 
           str_replace_all("\\.", " ") %>% 
           str_to_title() %>% 
           str_replace_all("Us", "US") %>% 
           trimws())

med_auc_US <- round(median(auc$test_aucs), digits = 2)

ranked_weights <- create_feature_rankings(feat_weights) %>% 
  mutate(key = key %>% str_replace("\\.{3}", " & ") %>% 
           str_replace("\\.female", "\\.women") %>% 
           str_replace("\\.male", "\\.men") %>% 
           str_replace_all("X.inst.gender.|X.US.inst.type.", "") %>% 
           str_replace_all("US.gender.no.|US.inst.no", "Non-US ") %>% 
           str_replace_all("(?<=US.)gender.yes|inst.yes", "") %>% 
           str_replace_all("inst.gender.Other.", "Other US Inst ") %>% 
           str_replace_all("\\.", " ") %>% 
           str_to_title() %>% 
           str_replace_all("Us", "US") %>% 
           trimws())
