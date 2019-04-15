reg_data <- data %>% 
  mutate(days.pending = as.duration(ymd_hms(submitted.date) %--% ymd_hms(ready.for.production.date))/ddays(1)) %>% 
  mutate(published = if_else("yes", 1, 0),
         gender.y = case_when(
           gender.y == "female" ~ 3,
           gender.y == "male" ~ 2,
           TRUE ~ 0),
         reviewer.gender = case_when(
           gender.y == "female" ~ 3,
           gender.y == "male" ~ 2,
           TRUE ~ 0))