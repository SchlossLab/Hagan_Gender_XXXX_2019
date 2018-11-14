#analyze people data

num_people <- data %>% select(random.person.id.y) %>% distinct() %>% nrow() #how many different people

num_authors <- data %>% filter(role.y == "author") %>% select(random.person.id.y) %>% distinct() %>% nrow() #how many authors

num_editors <- data %>% filter(role.y == "editor") %>% select(random.person.id.y) %>% distinct() %>% nrow() #how many in editor roles

num_sen_ed <- data %>% filter(role.y == "senior.editor") %>% select(random.person.id.y) %>% distinct() %>% nrow() #

num_reviewers <- data %>% filter(role.y == "reviewer") %>% select(random.person.id.y) %>% distinct() %>% nrow() #how many reviewers

gender_people <- data %>% group_by(gender.y) %>% distinct(random.person.id.y) %>% summarise(n = n()) #gender breakdown

gender_roles <- data %>% group_by(role.y, gender.y) %>% distinct(random.person.id.y) %>% summarise(n = n()) %>% as.tibble()

get_role_total <- function(x){
  case_when(
    x == "author" ~ num_authors,
    x == "editor" ~ num_editors,
    x == "senior.editor" ~ num_sen_ed,
    x == "reviewer" ~ num_reviewers
  )
}

gender_roles_prop <- gender_roles %>% mutate(total = get_role_total(role.y)) %>% 
  mutate(prop = round((n/total)*100, digits = 2)) %>% 
  mutate(gender.y = fct_explicit_na(gender.y, na_level = "none"))

role_labels <- c(author = "Author", editor = "Editor", reviewer = "Reviewer", senior.editor = "Editor-in-Chief")

gender_roles_prop %>% 
  ggplot(aes(x = gender.y, y = prop, fill = gender.y)) +
  geom_col() +
  facet_grid(~role.y, labeller = labeller(role.y = role_labels))+
  scale_fill_manual(values = gen_colors)+
  scale_x_discrete(labels = gen_labels)+
  labs(x = "Predicted Gender", y = "Proportion in Role")+
  my_theme_horiz

ggsave("results/gender_prop_subs.jpg")
