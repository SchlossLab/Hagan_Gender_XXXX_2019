#source("code/load_data.R") #loads and cleans all data for use

num_subs <- data %>% select(random.manu.num) %>% distinct() %>% nrow()

num_manus <- data %>% select(grouped.random) %>% distinct() %>% nrow()

#first_sub <- min(manu_data$submitted.date) #have to figure this one out...

decisions <- data %>% group_by(EJP.decision) %>% summarise(n = n())

versions <- data %>% group_by(version) %>% summarise(n = n())

num_pub <- data %>% filter(published == "yes") %>% distinct(grouped.random) %>% nrow()

percent_pubbed <- round((num_pub/num_manus)*100, 2)

#data %>% select(manuscript.number, version) %>% distinct() %>% nrow()
