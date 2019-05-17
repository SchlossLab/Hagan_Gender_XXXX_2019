library(tidyverse)
library(lubridate)

source("code/analysis_functions.R") #functions used during analysis
source("code/get_plot_options.R") #plotting preferences & variables

manu_data <- read_csv("data/2018_manu_ready.csv")

people_data <- read_csv("data/2018_people_ready.csv")

reviews_data <- read_csv("data/2018_reviews_ready.csv")

data <- left_join(manu_data, reviews_data, by = c("random.manu.num", "grouped.random")) %>% 
  left_join(., people_data, by = c("grouped.random", "random.manu.num")) %>% 
  rename("reviewer.country" = "country.x", "reviewer.institution" = "institution.x", "reviewer.gender" = "gender.x", "reviewer.random.id" = "random.person.id.x") %>% #rename.x person info to reviewer info
  select(-role.x) %>% #drop unneeded role.x column (b/c all reviewer)
  filter(year(submitted.date) >= "2011") #drop anything submitted in 2011

carnegie_class <- read_csv("data/carnegie_class.csv") %>% 
  select(name, city, state, Basic)

us_industries <- read_csv("data/us_industries.csv") %>% 
  pull(institution.name) %>% 
  str_to_lower(.) %>% unique()

fed_labs <- read_csv("data/fed_state_labs.csv") %>% 
  pull(institution.name) %>% 
  str_to_lower(.) %>% unique()

R1 <- carnegie_class %>% 
  filter(Basic == "Doctoral Universities: Very High Research Activity") %>% 
  pull(name) %>% str_to_lower(.)

R2 <- carnegie_class %>% 
  filter(Basic == "Doctoral Universities: High Research Activity") %>% 
  pull(name) %>% str_to_lower(.)

low_research <- carnegie_class %>% 
  filter(str_detect(Basic, "High Research") == FALSE) %>% 
  pull(name) %>% str_to_lower(.)

industry <- c("abbott", "abbvie", "accelerate", "achaogen", "achillion", "amgen",
              "allergan", "amplyx", "pharmaceuticals", "astrazeneca", "avidbiotics",
              "biopharma", "diagnostics", "bd", "beckman coulter", "bio rad", 
              "biorad", "biomerieux", "myers squibb", "chembio", "cellex",
              "therapeutics", "dupont", "genentech", " inc", "pharma", 
              "biomérieux", "new england biolabs", "corporation") %>% 
  paste0(., collapse = "|")

govt <- c("usda", "agricultural research service", "air force", "nih", "edgewood",
          "regional healthcare", "state health", "armed forces", "nasa", 
          "department of public health", "fda", "cdc", "national cancer institute", 
          "food and drug administration", "cleveland clinic", "health department",
          "homeland security", "veterans affairs", "environmental protection agency",
          "epa", "national lab", "centers for disease control", " va ", "army", "llc",
          "state public health laboratory", "state public health lab", "veterans",
          "rocky mountain lab") %>% 
  paste0(., collapse = "|")

r1 <- c("harvard", "binghamton", "boston children's hospital", "cornell",
        "george washington", "brigham and women's", "boston university", 
        "cornell university", "cuny graduate", "dana farber", "dartmouth",
        "broad institute", "case western", "children's hospital", "children's mercy",
        "university of michigan", "clemson", "columbia university", "duke",
        "emory university", "auburn", "georgetown university", "wisconsin madison",
        "wisconsin–madison", "university of minnesota", "ohio state", "drexel",
        "university of california", "illinois at urbana champaign", "emory",
        "university of pittsburgh", "washington university", "indiana university",
        "pennsylvania state university", "johns hopkins", "colorado state",
        "university of maryland", "north carolina state", "chapel hill", "yale",
        "massachusetts amherst", "texas at dallas", "texas at el paso", "knoxville",
        "texas at arlington", "texas at austin", "texas tech", "virginia tech",
        "university of virginia", "vanderbilt", "tufts", "oregon state uni", 
        "universisty of kentucky", "brown medical", "brown university", "alabama",
        "florida state", "georgia institute", "georgia state uni", "west virginia",
        "iowa state unviersity", "iowa state university", "wayne state", 
        "california institute of technology", "florida international", "temple",
        "louisiana state university", "michigan state", "u michigan", "rutgers",
        "michigan medicine", "mississippi state", "montana state", "rensselaer",
        "new york university", "northwestern university", "oklahoma state", 
        "university of arizona", "ity of arkansas", "central florida", 
        "ity of chicago", "cincinnati", "colorado boulder", "colorado denver",
        "connecticut", "delaware$", "university of florida", "university of georgia",
        "sity of hawaii", "illinois at chicago", "of iowa", "of kansas", "of kentucky",
        "louisville", "of miami", "missouri columbia", "nebraska", "nevada", 
        "ity of new hampshire", "of new mexico", "of oklahoma", "of pennsylvania",
        "pittsburgh", "of rochester", "of south carolina", "south florida$", 
        "ity of southern cali", "utah", "of virginia", "wisconsin milwaukee",
        "wisconsin–milwaukee", "virginia commonwealth", "washington state uni",
        "tennessee", "tenessee", "of mass", "^university of missouri") %>% 
  paste0(., collapse = "|")

r2 <- c("cuny city", "east tennessee state", "baylor", "eastern michigan",
        "university of california, merced", "indiana university – purdue",
        "baltimore county", "north carolina charlotte", "north carolina in wilmington",
        "massachusetts med", "massachusetts lowell", "texas at san antonio",
        "massachusetts boston", "texas aandm", "missouri kansas", "missouri-kansas",
        "texas rio grande valley", "texas aandm university corpus christi", 
        "indiana university purdue university indianapolis", "purdue univesrity",
        "indiana university - purdue university indianapolis", "arizona state",
        "^purdue university", "louisiana state university, new orleans", 
        "arkansas state", "bowling green state", "brigham young", "clark uni",
        "catholic university", "central michigan", "william and may", "east carolina",
        "florida atlantic", "hampton universtiy", "jefferson", "kent state", 
        "loyola univ", "marshall", "mercer", "miami uni", "missouri uni", "camden",
        "new mexico state", "carolina aand", "north dakota state", "northern arizona",
        "northern illinois", "ohio uni", "old dominion", "rowan uni", "newark",
        "saint louis", "san diego state", "carbondale", "university of montana",
        "university of texas health science center at san antonio", "akron",
        "merced", "of maine", "wilmington", "ity of north dakota", "puerto rico",
        "of rhode island", "uc san diego", "south alabama", "south dakota",
        "toledo", "vermont", "wake forest", "wright state", "des moines",
        "university of tennessee at chattanooga", "missouri–st louis") %>% 
  paste0(., collapse = "|")

low <- c("baptist health", "california state", "university of washington", 
         "indiana university northwest", "wisconsin–green bay", "wisconsin–la crosse",
         "wisconsin parkside", "wisconsin la crosse", "albert einstein", 
         "texas women's university", "st john's university", "edwardsville",
         "california, sf", "california san francisco", "detroit mercy", "hofstra",
         "pacific univeristy", "pace university", "long island univeristy", 
         "creighton university", "augusta university", "icahn school of medicine",
         "arizona state university west campus", "mayo clinic college of medicine",
         "louisana state university health sciences", "rosalind franklin university",
         "medical univerity of south carolina", "midwestern university",
         "medical college oif wisconsin", "medical college of wisconsin",
         "oregon health and science university", "rush university", 
         "suny downstate", "niversity of tennessee health science center",
         "texas health science", "utmb", "texas medical branch",
         "arkansas for med", "nebraska medical center", "oklahoma health sci",
         "oklahoma hsc", "oklahoma-health sci", "ut southwestern", "utsouthwestern",
         "texas southwestern", "suny upstate", "weill cornell", "massacussetts med",
         "maryland baltimore", "maryland school of medicine", 
         "maryland sch of medicine") %>% 
  paste0(., collapse = "|")

test <- data %>% 
  mutate(institution.y = str_to_lower(institution.y)) %>% 
  mutate(
    US.inst = if_else(country.y == "United States", "yes", "no"), #deal w. multiple inst?
    US.inst.type = case_when(
      institution.y %in% R1 | str_detect(institution.y, r1) ~ "R1",
      institution.y %in% R2 | str_detect(institution.y, r2) ~ "R2",
      institution.y %in% low_research | str_detect(institution.y, low) ~ "Low research",
      institution.y %in% fed_labs | str_detect(institution.y, govt) ~ "Federal",
      institution.y %in% us_industries | str_detect(institution.y, industry) ~ "Industry"
    ))

US_ungrouped <- test %>% filter(US.inst == "yes") %>% 
  filter(author.corres == TRUE) %>% 
  select(institution.y, US.inst.type) %>% 
  group_by(US.inst.type, institution.y) %>% 
  filter(is.na(US.inst.type)) %>% 
  summarise(n = n()) %>% arrange(desc(n))

#write_csv(US_ungrouped, path = "data/ungrouped_inst.csv")