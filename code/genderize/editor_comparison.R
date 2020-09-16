#library(caret) #package for generating confusion matrices

#function to convert male/female into binary (1/0)
convert_gender <- function(x){
  case_when(
    x == "male" ~ "0",
    x == "female" ~ "1"
  )
}

#summary stats for dataset
ed_data <- read_csv("../data/editor_comparison.csv") 

total_ed_names <- nrow(ed_data)

unknown_ed_names <- ed_data %>% filter(is.na(actual.gender)) %>% nrow() 

compared_ed_names <- ed_data %>% filter(!is.na(actual.gender) & !is.na(genderize.gender)) %>% nrow()

na_ed_names <- ed_data %>% filter(is.na(genderize.gender)) %>% nrow()


#compare genderize outcomes (no country data) to nichole's data----
ed_gender_data <- ed_data %>% 
  filter(!is.na(actual.gender) & !is.na(genderize.gender)) %>% 
  mutate(actual.gender = map(actual.gender, convert_gender) %>% unlist() %>% as.factor()) %>% 
  mutate(genderize.gender = map(genderize.gender, convert_gender) %>% unlist() %>% as.factor())

ed_gender_matrix <- suppressWarnings(confusionMatrix(ed_gender_data$genderize.gender, ed_gender_data$actual.gender))

#gender_matrix[[2]] -- confusion matrix
ed_sensitivity <- ed_gender_matrix[[4]][[1]]
ed_specificity <- ed_gender_matrix[[4]][[2]]
ed_accuracy <- ed_gender_matrix[[3]][[1]]
