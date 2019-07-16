#setup (libraries & data import)----
library(tidyverse)
library(lubridate)
library(rlang)

source("../code/analysis_functions.R") #functions used during analysis
source("../code/get_plot_options.R") #plotting preferences & variables

manu_data <- read_csv("../data/2018_manu_ready.csv")

people_data <- read_csv("../data/2018_people_ready.csv") #%>% 
  #select(-number_authors)

reviews_data <- read_csv("../data/2018_reviews_ready.csv")

eic_data <- read_csv("../data/eic_genders.csv")

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

#World bank data----
world_bank_data <- read_csv("../data/world_bank_economy.csv", skip = 4) %>% 
  select(Economy, Region, `Income group`, `Lending category`) %>% 
  rename(country = Economy, region = Region, GDP = `Income group`, lending = `Lending category`) %>% 
  filter(country != "x")

#categorizing by countries by region----
east_asia_and_pacific <- c("American Samoa", "Australia", "Brunei", "Cambodia", "Korea, Republic of", "China", "Fiji", "French Polynesia", "Guam", "Hong Kong", "Indonesia", "Japan", "Kiribati", "Macao", "Micronesia", "New Caledonia", "Lao", "Lao People's Democratic Republic", "Macau", "Macao", "Malaysia", "Marshall Islands", "Mongolia", "Myanmar", "Nauru", "New Zealand", "Palau", "Philippines", "Reunion", "Samoa", "Singapore", "Solomon Islands", "Taiwan, Province of China", "Thailand", "Papua New Guinea", "Tonga", "Tuvalu", "Vanuatu", "Vietnam", "Viet Nam", "Western Samoa")
europe_and_central_asia <- c("Albania", "Andorra", "Armenia", "Austria", "Azerbaijan", "Belarus", "Belgium", "Bosnia and Herzegovina", "Bulgaria", "Bouvert Island", "Channel Islands", "Croatia", "Cyprus", "Czech Republic", "Denmark", "Estonia", "Faroe Islands", "Finland", "France", "Georgia", "Germany", "Gibraltar", "Greece", "Greenland", "Hungary", "Iceland", "Ireland", "Isle of Man", "Italy", "Jan Mayen", "Jersey", "Kazakhstan", "Kosovo", "Kyrgyz Republic", "Kyrgyzstan", "Latvia", "Liechtenstein", "Lithuania", "Luxembourg", "Macedonia", "Moldova", "Monaco", "Montenegro", "Macedonia", "Macedonia, the Former Yugoslav Republic of", "Netherlands", "Norway", "Poland", "Portugal", "Romania", "Russian Federation", "San Marino", "Scotland", "Serbia", "Serbia and Montenegro", "Slovak Republic", "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland", "Tajikistan", "Turkey", "Turkmenistan", "Ukraine", "United Kingdom", "Uzbekistan", "Wales")
latin_america_and_caribbean <- c("Anguilla", "Antigua and Barbuda", "Argentina", "Aruba", "The Bahamas", "Barbados", "Belize", "Bolivia", "Brazil", "British Virgin Islands", "Cayman Islands", "Chile", "Colombia", "Costa Rica", "Cuba", "Curacao", "Dominica", "Dominican Republic", "East Timor", "Ecuador", "El Salvador", "French Guiana", "Grenada", "Guatemala", "Guadeloupe", "Guyana", "Haiti", "Honduras", "Jamaica", "Martinique", "Mexico", "Nicaragua", "Panama", "Paraguay", "Peru", "Puerto Rico", "Sint Maarten", "St. Kitts and Nevis", "Saint Kitt and Nevis", "St. Lucia", "St. Martin", "St. Vincent and the Grenadines", "Saint Vincent and the Grenadines", "Suriname", "Trinidad and Tobago", "Turks and Caicos Islands", "Uruguay", "Venezuela", "Venezuela, Bolivarian Republic of", "Virgin Islands")
middle_east_and_north_africa <- c("Algeria", "Bahrain", "Djibouti", "Egypt", "Gaza Strip", "Iran", "Iran, Islamic Republic of", "Iraq", "Israel", "Jordan", "Kuwait", "Lebanon", "Libya", "Malta", "Morocco", "Oman", "Qatar", "Saudi Arabia", "Syrian Arab Republic", "Tunisia", "United Arab Emirates", "West Bank and Gaza", "Yemen", "West Bank")
north_america <- c("Bermuda", "Canada", "United States", "Jarvis Island")
south_asia <- c("Afghanistan", "Bangladesh", "Bhutan", "India", "Maldives", "Nepal", "Pakistan", "Sri Lanka")
subsaharan_africa <- c("Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", "Cabo Verde", "Cape Verde", "Cameroon", "Central African Republic", "Chad", "Comoros", "Congo", "Congo, Democratic Republic of the", "Côte d'Ivoire", "Equatorial Guinea", "Eritrea", "Ethiopia", "Gabon", "Gambia", "Ghana", "Guinea", "Guinea-Bissau", "Kenya", "Lesotho", "Liberia", "Madagascar", "Malawi", "Mali", "Mauritania", "Mauritius","Mayotte","Mozambique", "Namibia", "Niger", "Nigeria", "Rwanda", "São Tomé and Principe", "Sao Tome and Principe","Senegal", "Seychelles", "Sierra Leone", "Somalia", "South Africa", "South Sudan", "Sudan", "Swaziland", "Tanzania", "Togo", "Tristan da Cunha", "Uganda", "Zambia", "Zimbabwe")
east_asia_and_pacific <- world_bank_data %>% filter(region == "East Asia & Pacific") %>% pull(country) %>% append(., c("American Samoa", "Australia", "Brunei", "Cambodia", "Korea, Republic of", "China", "Fiji", "French Polynesia", "Guam", "Hong Kong", "Indonesia", "Japan", "Kiribati", "Macao", "New Caledonia", "Lao", "Lao People's Democratic Republic", "Malaysia", "Macau", "Marshall Islands", "Mongolia", "Myanmar", "Nauru", "New Zealand", "Palau", "Philippines", "Reunion", "Samoa", "Singapore", "Solomon Islands", "Taiwan, Province of China", "Thailand", "Papua New Guinea", "Tonga", "Tuvalu", "Vanuatu", "Vietnam", "Viet Nam", "Korea (North), Democratic People's Republic of", "Western Samoa", "Mayotte", "Palmyra Atoll", "Micronesia"))
europe_and_central_asia <- world_bank_data %>% filter(region == "Europe & Central Asia") %>% pull(country) %>% append(., c("Albania", "Andorra", "Armenia", "Austria", "Azerbaijan", "Belarus", "Belgium", "Bosnia and Herzegovina", "Bulgaria", "Bouvert Island", "Channel Islands", "Croatia", "Cyprus", "Czech Republic", "Denmark", "Estonia", "Faroe Islands", "Finland", "France", "Georgia", "Germany", "Gibraltar", "Greece", "Greenland", "Hungary", "Iceland", "Ireland", "Isle of Man", "Italy", "Jan Mayen", "Kazakhstan", "Kosovo", "Kyrgyz Republic", "Latvia", "Liechtenstein", "Lithuania", "Luxembourg", "Macedonia", "Moldova", "Monaco", "Montenegro", "Netherlands", "Norway", "Poland", "Portugal", "Romania", "Russian Federation", "San Marino", "Scotland", "Serbia", "Serbia and Montenegro", "Slovak Republic", "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland", "Tajikistan", "Turkey", "Turkmenistan", "Ukraine", "United Kingdom", "Uzbekistan", "Wales", "Kyrgyzstan", "Macedonia, the Former Yugoslav Republic of", "Jersey", "UCD · School of Medicine and Medical Science"))
latin_america_and_caribbean <- world_bank_data %>% filter(region == "Latin America & Caribbean") %>% pull(country) %>% append(., c("Anguilla", "Antigua and Barbuda", "Argentina", "Aruba", "The Bahamas", "Barbados", "Belize", "Bolivia", "Brazil", "British Virgin Islands", "Cayman Islands", "Chile", "Colombia", "Costa Rica", "Cuba", "Curacao", "Dominica", "Dominican Republic", "East Timor", "Ecuador", "El Salvador", "French Guiana", "Grenada", "Guatemala", "Guadeloupe", "Guyana", "Haiti", "Honduras", "Jamaica", "Mexico", "Nicaragua", "Panama", "Paraguay", "Peru", "Puerto Rico", "Sint Maarten", "St. Kitts and Nevis", "Saint Kitt and Nevis", "St. Lucia", "St. Martin", "St. Vincent and the Grenadines", "Suriname", "Trinidad and Tobago", "Turks and Caicos Islands", "Uruguay", "Venezuela", "Venezuela, Bolivarian Republic of", "Virgin Islands", "Martinique", "Saint Vincent and the Grenadines", "Netherlands Antilles"))
middle_east_and_north_africa <- world_bank_data %>% filter(region == "Middle East & North Africa") %>% pull(country) %>% append(., c("Algeria", "Bahrain", "Djibouti", "Egypt", "Gaza Strip", "Iran", "Iran, Islamic Republic of", "Iraq", "Israel", "Jordan", "Kuwait", "Lebanon", "Libya", "Malta", "Morocco", "Oman", "Qatar", "Saudi Arabia", "Syrian Arab Republic", "Tunisia", "United Arab Emirates", "West Bank and Gaza", "Yemen", "West Bank", "Syria"))
north_america <- world_bank_data %>% filter(region == "North America") %>% pull(country) %>% append(., c("Bermuda", "Canada", "United States", "Jarvis Island"))
south_asia <- world_bank_data %>% filter(region == "South Asia") %>% pull(country) %>% append(., c("Afghanistan", "Bangladesh", "Bhutan", "India", "Maldives", "Nepal", "Pakistan", "Sri Lanka"))
subsaharan_africa <- world_bank_data %>% filter(region == "Sub-Saharan Africa") %>% pull(country) %>% append(., c("Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", "Cabo Verde", "Cape Verde", "Cameroon", "Central African Republic", "Chad", "Comoros", "Congo", "Congo, Democratic Republic of the", "Côte d'Ivoire", "Equatorial Guinea", "Eritrea", "Ethiopia", "Gabon", "Gambia", "Ghana", "Guinea", "Guinea-Bissau", "Kenya", "Lesotho", "Liberia", "Madagascar", "Malawi", "Mali", "Mauritania", "Mauritius", "Mozambique", "Namibia", "Niger", "Nigeria", "Rwanda", "São Tomé and Principe", "Senegal", "Seychelles", "Sierra Leone", "Somalia", "South Africa", "South Sudan", "Sudan", "Swaziland", "Tanzania", "Togo", "Uganda", "Zambia", "Zimbabwe", "Guinea Bissau", "Sao Tome and Principe", "Tristan da Cunha", "CÃƒÂ´te d'Ivoire"))

region_list <- c("North America", "Latin America & Caribbean", 
                 "Sub-Saharan Africa", "Middle East & North Africa", 
                 "Europe & Central Asia", "South Asia", "East Asia & Pacific")

#categorizing countries by GDP----
#including low-income, lower middle income, upper middle income, and high-income economies

low <- world_bank_data %>% filter(GDP == "Low income") %>% pull(country) %>% append(., c("Afghanistan", "Benin", "Burkina Faso", "Burundi", "Central African Republic", "Chad", "Comoros", "Congo", "Eritrea", "Ethiopia", "Gambia", "Guinea", "Guinea-Bissau", "Haiti", "Korea", "Liberia", "Madagascar", "Malawi", "Mali", "Mozambique", "Nepal", "Niger", "Rwanda", "Senegal", "Sierra Leone", "Somalia", "South Sudan", "Syrian Arab Republic", "Tajikistan", "Tanzania", "Togo", "Uganda", "Yemen", "Zimbabwe", "Syria", "Guinea Bissau"))
lower_middle <- world_bank_data %>% filter(GDP == "Lower middle income") %>% pull(country) %>% append(., c("Angola", "Bangladesh", "Bhutan", "Bolivia", "Cabo Verde", "Cambodia", "Congo", "Côte d'Ivoire", "Dijibouti", "Egypt", "El Salvador", "Georgia", "Ghana", "Honduras", "India", "Indonesia", "Kenya", "Kiribati", "Kosovo", "Kyrgyz Republic", "Lao", "Lesotho", "Mauritania", "Micronesia", "Moldova", "Mongolia", "Morocco", "Myanmar", "Nicaragua", "Nigeria", "Pakistan", "Papua New Guinea", "Philippines", "São Tomé and Principe", "Solomon Islands", "Sri Lanka", "Sudan", "Swaziland", "Timor-Leste", "Tunisia", "Ukraine", "Uzbekistan", "Vanuatu", "Vietnam", "West Bank and Gaza", "Zambia", "Kyrgyzstan", "Sao Tome and Principe", "CÃƒÂ´te d'Ivoire"))
upper_middle <- world_bank_data %>% filter(GDP == "Upper middle income") %>% pull(country) %>% append(., c("Albania", "Algeria", "American Samoa", "Armenia", "Azerbaijan", "Belarus", "Belize", "Bosnia and Herzegovina", "Botswana", "Brazil", "Bulgaria", "China", "Colombia", "Costa Rica", "Cuba", "Dominica", "Dominican Republic", "Equatorial Guinea", "Ecuador", "Fiji", "Gabon", "Grenada", "Guatemala", "Guyana", "Iran", "Iraq", "Jamaica", "Jordan", "Kazakhstan", "Lebanon", "Libya", "Macedonia", "Malaysia", "Maldives", "Marshal Islands", "Mauritius", "Mexico", "Montenegro", "Namibia", "Nauru", "Paraguay", "Peru", "Romania", "Russian Federation", "Samoa", "Serbia", "South Africa", "St. Lucia", "St. Vincent and the Grenadines", "Suriname", "Thailand", "Tonga", "Turkey", "Turkmenistan", "Tuvalu", "Venezuela", "Western Samoa", "Macedonia, the Former Yugoslav Republic of", "Saint Vincent and the Grenadines"))
high <- world_bank_data %>% filter(GDP == "High income") %>% pull(country) %>% append(., c("Andorra", "Antigua and Barbuda", "Argentina", "Aruba", "Australia", "Austria", "Bahamas", "Bahrain", "Barbados", "Belgium", "Bermuda", "British Virgin Islands", "Brunei Darussalam", "Canada", "Cayman Islands", "Channel Islands", "Chile", "Croatia", "Curaçao", "Cyprus", "Czech Republic", "Denmark", "Estonia", "Faroe Islands", "Finland", "France", "French Polynesia", "Germany", "Gibraltar", "Greece", "Greenland", "Guam", "Hong Kong", "Hungary", "Iceland", "Ireland", "Isle of Man", "Israel", "Italy", "Japan", "Korea","Korea, Republic of", "Kuwait", "Latvia", "Liechtenstein", "Lithuania", "Luxembourg", "Macao", "Macau", "Malta", "Monaco", "Netherlands", "New Caledonia", "New Zealand", "Northern Mariana Islands", "Norway", "Oman", "Palau", "Panama", "Poland", "Portugal", "Puerto Rico", "Qatar", "San Marino", "Saudi Arabia","Scotland", "Seychelles", "Singapore", "Sint Maarten", "Slovak Republic", "Slovenia", "St. Kitts and Nevis", "St. Martin", "Sweden", "Switzerland", "Taiwan","Taiwan, Province of China", "Trinidad and Tobago", "Turks and Caicos Islands", "United Arab Emirates", "United Kingdom", "United States", "Uruguay", "Virgin Islands", "Mayotte", "Martinique", "Netherlands Antilles", "UCD · School of Medicine and Medical Science"))
unclassified <- c("Tristan da Cunha", "Antarctica", "Palmyra Atoll", "Jersey")

gdp_list <- c("Low", "Lower Middle", "Upper Middle", "High", "Unclassified")


region_df <- data %>%
  select(country) %>% distinct() %>%
  mutate(region = fct_collapse(country,
                               "East Asia & Pacific" = east_asia_and_pacific,
                               "Europe & Central Asia" = europe_and_central_asia,
                               "Latin America & Caribbean" = latin_america_and_caribbean,
                               "Middle East & North Africa" = middle_east_and_north_africa,
                               "North America" = north_america,
                               "South Asia" = south_asia,
                               "Sub-Saharan Africa" = subsaharan_africa),
         GDP = fct_collapse(country,
                            "Low" = low,
                            "Lower Middle" = lower_middle,
                            "Upper Middle" = upper_middle,
                            "High" = high,
                            "Unclassified" = unclassified))


#merge final dataset ----

decisions <- c("Withdrawn", "Reject", "Revise and re-review",
               "Revise only", "Accept, no revision")

data <- data %>% 
  mutate(institution = str_to_lower(institution),
    US.inst = if_else(country == "United States", "yes", "no")) %>% 
  left_join(., binned_inst, by = "institution") %>% distinct() %>% 
  left_join(., region_df, by = "country") %>% distinct() %>% 
  mutate(gender = fct_explicit_na(gender, na_level = "none"),
         reviewer.gender = fct_explicit_na(reviewer.gender, na_level = "none"),
         EJP.decision = factor(EJP.decision, levels = decisions))

#ensure ordered levels  
data$GDP <- fct_relevel(data$GDP, gdp_list)
data$region <- fct_relevel(data$region, region_list)  

#bias analysis dataset  
bias_data <- data %>% 
  select(-number_authors) %>% 
  filter(author.corres == TRUE) %>% 
  filter(gender != "none") %>% 
  filter(!is.na(gender)) %>% 
  filter(!is.na(journal)) %>% 
  distinct()

#representation analysis datasets
source("../code/author_setup.R")
source("../code/gatekeeper_setup.R")

mjournals <- c("mBio", "mSphere", "mSystems")