library(tidyverse)
library(lubridate)
library(rlang)

#source("code/analysis_functions.R") #functions used during analysis
#source("code/get_plot_options.R") #plotting preferences & variables

manu_data <- read_csv("../data/2018_manu_ready.csv")

people_data <- read_csv("../data/2018_people_ready.csv") #%>% 
  #select(-number_authors)

reviews_data <- read_csv("../data/2018_reviews_ready.csv")

#eic_data <- read_csv("../data/eic_genders.csv")

world_bank_data <- read_csv("../data/world_bank_economy.csv", skip = 4) %>% 
  select(Economy, Region, `Income group`, `Lending category`) %>% 
  rename(country = Economy, region = Region, GDP = `Income group`, lending = `Lending category`) %>% 
  filter(country != "x")

gender_reviews <- people_data %>% 
  select(-role, -contains("auth"), -random.manu.num, -grouped.random) %>% 
  left_join(reviews_data, ., by = "random.person.id") %>% distinct() %>% 
  rename("reviewer.country" = "country", "reviewer.institution" = "institution", "reviewer.gender" = "gender", "reviewer.random.id" = "random.person.id") #rename.x person info to reviewer info

data <- left_join(manu_data, gender_reviews, 
                  by = c("random.manu.num", "grouped.random")) %>% 
  left_join(., people_data, by = c("grouped.random", "random.manu.num")) %>% 
  distinct() %>% 
  filter(year(submitted.date) >= "2011") #drop anything submitted in 2011
 

#Create vectors to categorize countries by region for plots as per categorization by World Bank----
#Include East Asia and Pacific, Europe and Central Asia, Latin America and Caribbean, Middle East and North Africa
#North America, South Asia, and Sub-Saharan Africa


east_asia_and_pacific <- c("American Samoa", "Australia", "Brunei", "Cambodia", "Korea, Republic of", "China", "Fiji", "French Polynesia", "Guam", "Hong Kong", "Indonesia", "Japan", "Kiribati", "Macao", "Micronesia", "New Caledonia", "Lao", "Lao People's Democratic Republic", "Macau", "Macao", "Malaysia", "Marshall Islands", "Mongolia", "Myanmar", "Nauru", "New Zealand", "Palau", "Philippines", "Reunion", "Samoa", "Singapore", "Solomon Islands", "Taiwan, Province of China", "Thailand", "Papua New Guinea", "Tonga", "Tuvalu", "Vanuatu", "Vietnam", "Viet Nam", "Western Samoa")
europe_and_central_asia <- c("Albania", "Andorra", "Armenia", "Austria", "Azerbaijan", "Belarus", "Belgium", "Bosnia and Herzegovina", "Bulgaria", "Bouvert Island", "Channel Islands", "Croatia", "Cyprus", "Czech Republic", "Denmark", "Estonia", "Faroe Islands", "Finland", "France", "Georgia", "Germany", "Gibraltar", "Greece", "Greenland", "Hungary", "Iceland", "Ireland", "Isle of Man", "Italy", "Jan Mayen", "Jersey", "Kazakhstan", "Kosovo", "Kyrgyz Republic", "Kyrgyzstan", "Latvia", "Liechtenstein", "Lithuania", "Luxembourg", "Macedonia", "Moldova", "Monaco", "Montenegro", "Macedonia", "Macedonia, the Former Yugoslav Republic of", "Netherlands", "Norway", "Poland", "Portugal", "Romania", "Russian Federation", "San Marino", "Scotland", "Serbia", "Serbia and Montenegro", "Slovak Republic", "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland", "Tajikistan", "Turkey", "Turkmenistan", "Ukraine", "United Kingdom", "Uzbekistan", "Wales")
latin_america_and_caribbean <- c("Anguilla", "Antigua and Barbuda", "Argentina", "Aruba", "The Bahamas", "Barbados", "Belize", "Bolivia", "Brazil", "British Virgin Islands", "Cayman Islands", "Chile", "Colombia", "Costa Rica", "Cuba", "Curacao", "Dominica", "Dominican Republic", "East Timor", "Ecuador", "El Salvador", "French Guiana", "Grenada", "Guatemala", "Guadeloupe", "Guyana", "Haiti", "Honduras", "Jamaica", "Martinique", "Mexico", "Nicaragua", "Panama", "Paraguay", "Peru", "Puerto Rico", "Sint Maarten", "St. Kitts and Nevis", "Saint Kitt and Nevis", "St. Lucia", "St. Martin", "St. Vincent and the Grenadines", "Saint Vincent and the Grenadines", "Suriname", "Trinidad and Tobago", "Turks and Caicos Islands", "Uruguay", "Venezuela", "Venezuela, Bolivarian Republic of", "Virgin Islands")
middle_east_and_north_africa <- c("Algeria", "Bahrain", "Djibouti", "Egypt", "Gaza Strip", "Iran", "Iran, Islamic Republic of", "Iraq", "Israel", "Jordan", "Kuwait", "Lebanon", "Libya", "Malta", "Morocco", "Oman", "Qatar", "Saudi Arabia", "Syrian Arab Republic", "Tunisia", "United Arab Emirates", "West Bank and Gaza", "Yemen", "West Bank")
north_america <- c("Bermuda", "Canada", "United States", "Jarvis Island")
south_asia <- c("Afghanistan", "Bangladesh", "Bhutan", "India", "Maldives", "Nepal", "Pakistan", "Sri Lanka")
subsaharan_africa <- c("Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", "Cabo Verde", "Cape Verde", "Cameroon", "Central African Republic", "Chad", "Comoros", "Congo", "Congo, Democratic Republic of the", "Côte d'Ivoire", "Equatorial Guinea", "Eritrea", "Ethiopia", "Gabon", "Gambia", "Ghana", "Guinea", "Guinea-Bissau", "Kenya", "Lesotho", "Liberia", "Madagascar", "Malawi", "Mali", "Mauritania", "Mauritius","Mayotte","Mozambique", "Namibia", "Niger", "Nigeria", "Rwanda", "São Tomé and Principe", "Sao Tome and Principe","Senegal", "Seychelles", "Sierra Leone", "Somalia", "South Africa", "South Sudan", "Sudan", "Swaziland", "Tanzania", "Togo", "Tristan da Cunha", "Uganda", "Zambia", "Zimbabwe")

#data <- data %>%
#  mutate(region = case_when(
#   country %in% east_asia_and_pacific ~ "East Asia & Pacific",
#   country %in% europe_and_central_asia ~ "Europe & Central Asia",
#   country %in% latin_america_and_caribbean ~ "Latin America",
#   country %in% middle_east_and_north_africa ~ "Middle East",
#   country %in% north_america ~ "North America",
#   country %in% south_asia ~ "South Asia",
#   country %in% subsaharan_africa ~ "Sub-Saharan Africa",
#   TRUE ~ country))

#  data <- data %>% filter(str_detect(region != "NA", "Crunchbase on Fac"))

#data <- data %>%
#  mutate(region = case_when(
#   country %in% east_asia_and_pacific ~ "East Asia & Pacific",
#   country %in% europe_and_central_asia ~ "Europe & Central Asia",
#   country %in% latin_america_and_caribbean ~ "Latin America",
#   country %in% middle_east_and_north_africa ~ "Middle East",
#   country %in% north_america ~ "North America",
#   country %in% south_asia ~ "South Asia",
#   country %in% subsaharan_africa ~ "Sub-Saharan Africa",
#   TRUE ~ country))


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

#categorizing countries by GDP
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
data <- data %>%
  left_join(., region_df, by = "country")

data$GDP <- fct_relevel(data$GDP, gdp_list)
data$region <- fct_relevel(data$region, region_list)

source("../code/author_setup.R")
source("../code/gatekeeper_setup.R")

mjournals <- c("mBio", "mSphere", "mSystems")
