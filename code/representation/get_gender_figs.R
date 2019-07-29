#source figures for author/editor/reviewer gender data -- requires:
#1. load_data
#2. analysis_functions
#3. get_plot_options
#4. author_setup & gatekeeper_setup

unique_author_list <- c("uniq_author_data", "mid_auth", "corres_auth", "last_auth", "first_auth")

for (each_auth_type in unique_author_list){
  source('code/gender/auth_gender_time.R')
}

s_v_p <- data.frame(
  sub_auth_list = c("sub_author_data", "sub_corres_auth", "sub_first_auth", "sub_last_auth", "sub_mid_auth"), 
  pub_auth_list = c("pub_author_data", "pub_corres_auth", "pub_first_auth", "pub_last_auth", "pub_mid_auth"))

for (i in seq_len(nrow(s_v_p))){
  temp_sub <- as.character(s_v_p[i,1]) 
  temp_pub <- as.character(s_v_p[i,2])
  
  source('code/gender/sub_vs_pub_time.R')
}

for (i in seq_len(nrow(s_v_p))){
  temp_sub <- as.character(s_v_p[i,1]) 
  temp_pub <- as.character(s_v_p[i,2])
  
  source('code/gender/rel_chx_sub_v_pub.R')
}

source('code/gender/ed_gender_time.R')

rev_list <- c("reviewer_data", "pot_rev_data")

for (each_rev_type in rev_list){
  source('code/gender/test_scripts/rev_gender_time.R')
}

source("code/gender/reviewed_hist.R")

source("code/gender/assigned_hist.R")

source("code/gender/retention_alluvial.R")

