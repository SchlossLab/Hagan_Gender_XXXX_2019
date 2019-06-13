#setup (libraries & data import)----
library(tidyverse)
library(lubridate)

source("../code/analysis_functions.R") #functions used during analysis
source("../code/get_plot_options.R") #plotting preferences & variables

manu_data <- read_csv("../data/2018_manu_ready.csv")

people_data <- read_csv("../data/2018_people_ready.csv") %>% 
  select(-number_authors)

reviews_data <- read_csv("../data/2018_reviews_ready.csv")

gender_reviews <- people_data %>% 
  select(-role, -contains("auth"), -random.manu.num, -grouped.random, -title) %>% 
  left_join(reviews_data, ., by = "random.person.id") %>% distinct() %>% 
  rename("reviewer.country" = "country", "reviewer.institution" = "institution", "reviewer.gender" = "gender", "reviewer.random.id" = "random.person.id") #rename.x person info to reviewer info

data <- left_join(manu_data, gender_reviews, 
                  by = c("random.manu.num", "grouped.random")) %>% 
  left_join(., people_data, by = c("grouped.random", "random.manu.num")) %>% 
  distinct() %>% 
  filter(year(submitted.date) >= "2011") #drop anything submitted in 2011

#carnegie classifications (R1, R2 research etc)----
carnegie_class <- read_csv("../data/carnegie_class.csv") %>% 
  select(name, city, state, Basic)

us_industries <- read_csv("../data/us_industries.csv") %>% 
  pull(institution.name) %>% 
  str_to_lower(.) %>% unique()

fed_labs <- read_csv("../data/fed_state_labs.csv") %>% 
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
              "therapeutics", "dupont", "genentech", " inc", "pharma", "merck",
              "biomérieux", "new england biolabs", "corporation", "consultant", 
              "jmi laboratories", "novartis", "the medicines company", "pfizer",
              "glaxosmithkline", "genesis biotechnology group", "sanofi",
              "laboratories", "profectus biosciences", "emergent biosolutions",
              "integral molecular", "jackson laboratory", "roche",
              "neomed", "bayer", "cerexa", "gsk") %>% 
  paste0(., collapse = "|")

govt <- c("usda", "agricultural research service", "air force", "nih", "edgewood",
          "regional healthcare", "state health", "armed forces", "nasa", 
          "department of public health", "fda", "cdc", "national cancer institute", 
          "food and drug administration", "cleveland clinic", "health department",
          "homeland security", "veterans affairs", "environmental protection agency",
          "epa", "national lab", "centers for disease control", " va ", "army", "llc",
          "state public health laboratory", "state public health lab", "veterans",
          "rocky mountain lab", "usamriid", "niaid", "national institute of",
          "va medical center", "uniformed services university", "nci ",
          "national animal disease center", "national institute of allergy and",
          "united states geological survey", "naval medical research",
          "wadsworth center, new york state dept health", "niddk",
          "national oceanic and atmospheric administration", "united states", 
          "va ann arbor") %>% 
  paste0(., collapse = "|")

medical <- c("mayo clinic", "scripps research", "research hospital", 
             "fred hutchinson", "massachusetts general hospital", 
             "beth israel deaconess medical center", "hospital", "children's mercy",
             "memorial sloan kettering", "school of medicine", "cedars sinai",
             "medical school", "medicine at ucla", "arkansas for med", 
             "medical center", "medical uni", "college of medicine",
             "louisana state university health sciences", "cancer center",
             "center for infectious disease research", "primate research center",
             "rosalind franklin university", "icahn school of medicine",
             "university of tennessee health science center", "research institute",
             "anschutz medical campus", "health system", "institute",
             "texas health science", "utmb", "texas medical branch", "ohsu",
             "sch of medicine", "utmb", "texas medical branch", "uthscsa",
             "oklahoma health sci", "medical college", "national jewish health",
             "oklahoma hsc", "oklahoma-health sci", "massacussetts med",
             "oregon health and science university", "texas health science",
             "university of tennessee health science center", "lsuhsc",
             "international aids vaccine initiative", "woods hole",
             "aaron diamond aids research center", "school of med",
             "oklahoma medical research foundation", "marine biological laboratory",
             "scripps institution", "university of medicine") %>% 
  paste0(., collapse = "|")

r1 <- c("harvard", "binghamton", "cornell", "ucsd", "uc davis", "tulane university",
        "george washington", "brigham and women's", "boston university", 
        "cornell university", "cuny graduate", "dana farber", "dartmouth",
        "broad institute", "case western", "mit", "university of mississippi",
        "lousiana state university, baton rouge", "stanford", "uc berkeley", 
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
        "tennessee", "tenessee", "of mass", "^university of missouri",
        "university at buffalo", "georgia tech") %>% 
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
        "toledo", "vermont", "wake forest", "wright state", "des moines", "etsu",
        "university of tennessee at chattanooga", "missouri–st louis") %>% 
  paste0(., collapse = "|")

low <- c("baptist health", "california state", "university of washington", 
         "indiana university northwest", "wisconsin–green bay", "wisconsin–la crosse",
         "wisconsin parkside", "wisconsin la crosse", "albert einstein", 
         "texas women's university", "st john's university", "edwardsville",
         "california, sf", "california san francisco", "detroit mercy", "hofstra",
         "pacific univeristy", "pace university", "long island univeristy", 
         "creighton university", "augusta university", "maryland baltimore",
         "arizona state university west campus", "midwestern university",
         "rush university", "suny downstate", "weill cornell", "texas southwestern",
         "arkansas for med", "ut southwestern", "utsouthwestern", "ucsf",
         "suny upstate", "new york at buffalo", "hunter college",
         "suny buffalo", "california polytechnic ") %>% 
  paste0(., collapse = "|")

#bin US institutions----
binned_inst <- data %>% #deal w. multiple inst?
  mutate(institution = str_to_lower(institution)) %>% 
  filter(country == "United States") %>% 
  select(institution) %>% distinct() %>% 
  mutate(US.inst.type = case_when(
      institution %in% us_industries | str_detect(institution, industry) ~ "Industry Research",
      institution %in% R1 | str_detect(institution, r1) ~ "R1 Institution",
      str_detect(institution, medical) ~ "Institute or Medical School",
      institution %in% R2 | str_detect(institution, r2) ~ "R2  Institution",
      institution %in% low_research | str_detect(institution, low) ~ "Low Research  Institution",
      institution %in% fed_labs | str_detect(institution, govt) ~ "Federal Research"))



decisions <- c("Withdrawn", "Reject", "Revise and re-review",
               "Revise only", "Accept, no revision")

#merge final dataset ----
data <- data %>% 
  mutate(institution = str_to_lower(institution),
    US.inst = if_else(country == "United States", "yes", "no")) %>% 
  left_join(., binned_inst, by = "institution") %>% distinct() %>% 
  mutate(gender = fct_explicit_na(gender, na_level = "none"),
         reviewer.gender = fct_explicit_na(reviewer.gender, na_level = "none"),
         EJP.decision = factor(EJP.decision, levels = decisions))

bias_data <- data %>% 
  filter(author.corres == TRUE) %>% 
  filter(gender != "none") %>% 
  filter(!is.na(gender)) %>% 
  filter(!is.na(journal)) %>% 
  distinct()
