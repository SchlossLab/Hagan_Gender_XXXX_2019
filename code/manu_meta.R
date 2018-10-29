source("Code/load_data.R") #loads and cleans all data for use

num_subs <- research_only %>% select(manuscript.number) %>% distinct() %>% nrow()

num_manus <- research_only %>% select(grouped.manu.number) %>% distinct() %>% nrow()

first_sub <- min(manu_data$submitted.date) #have to figure this one out...

decisions <- research_only %>% group_by(EJP.decision) %>% summarise(n = n())

versions <- research_only %>% group_by(version) %>% summarise(n = n())

num_pub <- research_only %>% filter(!is.na(doi)) %>% distinct(grouped.manu.number) %>% nrow()

percent_pubbed <- round((num_pub/num_manus)*100, 2)

research_only %>% select(manuscript.number, version) %>% distinct() %>% nrow()
