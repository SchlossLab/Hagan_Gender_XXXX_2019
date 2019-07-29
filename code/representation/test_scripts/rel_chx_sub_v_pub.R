#are the proportion of published to submitted manuscripts proportional by gender?

#need to know # of sub manu by gender & # of pub manu by gender (all & by journal)
# calculate % difference?
#relative chx = (pub - sub)/sub -- *100 to get %

sub_df <- get(temp_sub) #pull this df from the global environment

pub_df <- get(temp_pub)

name <- paste0(temp_pub) %>% str_extract(., "(?<=(s|p)ub_)[[:graph:]]+")

list_df <- c(sub_df, pub_df)

auth_type <- case_when( #identify authors being examined
  str_detect(name, "first") ~ "First",
  str_detect(name, "last") ~ "Last",
  str_detect(name, "corres") ~ "Corresponding",
  str_detect(name, "mid") ~ "Middle",
  TRUE ~ "All"
)

#relative chx for all journals combined
all_j_sub <- map_dfr(years, function(x){
    get_prop_by_yr(x, sub_df, "gender", "All") %>% 
    rename(., n_sub = n) %>% 
    select(-proportion)})

all_j_pub <- map_dfr(years, function(x){
  get_prop_by_yr(x, pub_df, "gender", "All") %>% 
    rename(., n_pub = n) %>% 
    select(-proportion)})

all_j_prop <- full_join(all_j_pub, all_j_sub, by = c("year", "journal", "gender")) %>% distinct() %>% 
  mutate(rel_chx = round((n_pub - n_sub)/n_sub, digits = 2)*100)

ggplot(all_j_prop)+
  geom_boxplot(aes(x = gender, y = rel_chx))+
  coord_flip()+
  scale_x_discrete(labels = gen_labels)+
  labs(x = paste(auth_type, "Author Gender"),
   y = paste("Relative Acceptace by", auth_type, "Author"),
   caption = paste("Proportion of submitted manuscripts accepted relative to the", str_to_lower(auth_type), "gender. Lower values indicate greater proportion of rejection. Each point represents one year."))+
  my_theme_horiz

ggsave(filename = paste0(name, "pub_v_sub.png"), 
       path = "results/gender/figures")

#relative chx for each journal
ea_j_sub <- map_dfr(years, function(x){
  get_prop_by_yr(x, sub_df, "gender", "Each") %>% 
    rename(., n_sub = n) %>% 
    select(-proportion)})

ea_j_pub <- map_dfr(years, function(x){
  get_prop_by_yr(x, pub_df, "gender", "Each") %>% 
    rename(., n_pub = n) %>% 
    select(-proportion)})

ea_j_prop <- full_join(ea_j_pub, ea_j_sub, by = c("year", "journal", "gender")) %>% distinct() %>% 
  mutate(rel_chx = round((n_pub - n_sub)/n_sub, digits = 2)*100) %>% 
  filter(gender != "NA")

ggplot(ea_j_prop)+
  geom_boxplot(aes(x = gender, y = rel_chx))+
  facet_wrap(~journal)+
  coord_flip()+
  scale_x_discrete(labels = gen_labels)+
  labs(x = paste(auth_type, "Author Gender"),
       y = paste("Relative Acceptace by", auth_type, "Author"),
       caption = paste("Proportion of submitted manuscripts accepted relative to the", str_to_lower(auth_type), "gender. Lower values indicate greater proportion of rejection. Each point represents one year."))+
  my_theme_horiz

ggsave(filename = paste0(name, "pub_v_sub_journ.png"), 
       path = "results/gender/figures")