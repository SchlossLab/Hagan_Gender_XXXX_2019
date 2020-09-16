##gather auc values & plot----

prop_fem_ml <- read_csv("../data/combined_best_hp_results_pred_auth_gend.csv") %>% 
  select(test_aucs) %>% 
  rename(A = "test_aucs")

rej_rate_ml <- read_csv("../data/combined_best_hp_results_pred_rej_gend.csv") %>% 
  select(test_aucs) %>%
  rename(B = "test_aucs")

US_rej_ml <- read_csv("../data/combined_best_hp_results_US_rej.csv") %>% 
  select(test_aucs) %>%
  rename(C = "test_aucs")

all_aucs <- cbind(prop_fem_ml, rej_rate_ml, US_rej_ml) %>% 
  gather(key = test, value = auc)

plot_aucs <- all_aucs %>% 
  ggplot()+
  geom_boxplot(aes(y = auc))+
  facet_wrap(~test)+
  labs(y = "AUC")+
  scale_y_continuous(limits = c(0.5, 1.0))+
  my_theme+
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank())

#gather weights----

prop_fem_feat <- read_csv("../data/combined_all_imp_features_pred_auth_gen.csv") %>% 
  select(-Bias, -model) %>% 
  gather(key = feature, value = weight) %>% 
  mutate(test = "prop_fem")

rej_rate_feat <- read_csv("../data/combined_all_imp_features_pred_rej_gend.csv") %>% 
  select(-Bias, -model) %>% 
  gather(key = feature, value = weight) %>% 
  mutate(test = "ed_rej_all")

#US_rej_feat <- read_csv("../data/combined_all_imp_features_US_rej.csv") %>% 
#  select(-Bias, -model) %>% 
#  gather(key = feature, value = weight) %>% 
#  mutate(test = "US_rej")

all_feats <- rbind(prop_fem_feat, rej_rate_feat, US_rej_feat) %>% 
  mutate(weights = abs(weights), 
         clean_feat = feature %>% str_replace("\\.{3}", " & ") %>% 
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

fem_feat_plot <-all_feats %>% 
  filter(test == "prop_fem") %>% 
  feature_box_plot(.)

rej_rate_feat_plot <- all_feats %>% 
  filter(test == "ed_rej_all") %>% 
  feature_box_plot(.)

##this plot isn't needed b/c fig S6
#US_rej_feat_plot <- all_feats %>% 
#  filter(test == "US_rej") %>% 
#  feature_box_plot(.)

###plot figure----

Fig_AB <- plot_grid(plot_aucs, rej_rate_feat_plot, 
                    labels = c('A', 'B'), rel_widths = c(.8, 1),
                    label_size = 18, nrow = 1)

plot_grid(Fig_AB, fem_feat_plot, 
          labels = c('', 'C'), rel_heights = c(0.5, 1),
          label_size = 18, nrow = 2)

ggsave("AUC_supp_fig.png", device = 'png', 
       path = 'submission', width = 9, height = 9)
