#analyze people data

num_people <- research_only %>% select(person.id.y) %>% distinct() %>% nrow() #how many different people

num_authors <- research_only %>% filter(role == "author") %>% select(person.id.y) %>% distinct() %>% nrow() #how many authors

num_editors <- research_only %>% filter(role == "editor") %>% select(person.id.y) %>% distinct() %>% nrow() #how many in editor roles

num_sen_ed <- research_only %>% filter(role == "senior.editor") %>% select(person.id.y) %>% distinct() %>% nrow() #

num_reviewers <- research_only %>% filter(role == "reviewer") %>% select(person.id.y) %>% distinct() %>% nrow() #how many reviewers

gender_people <- research_only %>% group_by(gender) %>% distinct(person.id.y) %>% summarise(n = n()) #gender breakdown

gender_roles <- research_only %>% group_by(role, gender) %>% distinct(person.id.y) %>% summarise(n = n()) %>% as.tibble()

get_role_total <- function(x){
  case_when(
    x == "author" ~ num_authors,
    x == "editor" ~ num_editors,
    x == "senior.editor" ~ num_sen_ed,
    x == "reviewer" ~ num_reviewers
  )
}

gender_roles_prop <- gender_roles %>% mutate(total = get_role_total(role)) %>% 
  mutate(prop = round((n/total)*100, digits = 2))

gender_roles_prop %>% 
  ggplot(aes(x = gender, y = prop, fill = gender)) +
  geom_col() +
  facet_grid(~role)

ggsave("Results/Plots/gender_prop_subs.jpg")
