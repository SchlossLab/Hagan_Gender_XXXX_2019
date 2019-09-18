#load all data, run all analyses, and generate figures for inclusion in Rmd

source("code/load_data.R")

#representation figures----
source("code/representation/US_inst_type_stats.R") #data for figures by institution ype & gender

rep_figures <- list.files(path = "code/representation", 
                          pattern = "Figure_*", full.names = TRUE)

for(fig in rep_figures){
  source(fig)
}
#source("code/representation/Figure_1.R") #Figure 1
#
#source("code/representation/Figure_2.R") #Figure 2
#
#source("code/representation/Figure_3.R") # Figure 3
#
source("code/representation/retention_alluvial.R") #alluvial summary data

#source("code/representation/Figure_S1.R") #supplemental figures
#
#source("code/representation/Figure_S2.R") #supplemental
#
#source("code/representation/Figure_S3.R") #supplemental
#
#source("code/representation/Figure_S4.R") #supplemental

source("code/representation/representation_data.R") #other calculations for representation data

#bias figures----

bias_figures <- list.files(path = "code/bias", pattern = "Figure_*", full.names = TRUE)

for(fig in bias_figures){
  source(fig)
}

#source("code/bias/Figure_4.R")
#
#source("code/bias/Figure_5.R")
#
#source("code/bias/Figure_6.R")
#
#source("code/bias/Figure_S5.R") #supplemental
#
#source("code/bias/Figure_S6.R") #supplemental

#genderize figures----

source("code/genderize/Figure_S8.R")

save.image(file = "submission/gender_analysis.RData")