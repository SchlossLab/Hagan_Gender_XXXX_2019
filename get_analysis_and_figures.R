#load all data, run all analyses, and generate figures for inclusion in Rmd

source("code/load_data.R")

#representation figures & data ----
source("code/representation/US_inst_type_stats.R") #data for figures by institution ype & gender

rep_figures <- list.files(path = "code/representation", 
                          pattern = "Figure_*", full.names = TRUE)

for(fig in rep_figures){
  source(fig)
}

source("code/representation/retention_alluvial.R") #alluvial summary data

source("code/representation/representation_data.R") #other calculations for representation data

#logistic regression plot
source("code/bias/log_reg_US_rej.R")

#bias figures----

bias_figures <- list.files(path = "code/bias", pattern = "Figure_*", full.names = TRUE)

for(fig in bias_figures){
  source(fig)
}

#genderize figure----
source("code/genderize/Figure_S8.R")

#save Rdata output----
save.image(file = "submission/gender_analysis.RData")