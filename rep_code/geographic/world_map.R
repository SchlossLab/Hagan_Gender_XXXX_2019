# #World Map
# 
#Install packages
library(tidyverse)
library(rvest)
library(magrittr)
library(stringr)

#Figure showing proportions of authors in different regions
#Use author data and pull region
#df = all_authors_w_prop
#colnames of interest = region, proportion

#Get world map----
map.world <- map_data("world")

# 
# #INSPECT---- 
# # map.world %>%
# #   group_by(region) %>%
# #   summarise()
# 
# 
# # RECODE NAMES----
# #Make sure that the regions in our df correspond with the countries in map.world
# #so that df's can merge
# 
# # all_authors_w_prop$region <- recode(all_authors_w_prop$region
# #                                    ,'United States' = 'USA'
# #                                    ,'United Kingdom' = 'UK'
# # )
# # 
# # east_asia_and_pacific <- map.world %>% filter(region == "East Asia & Pacific") %>% pull(country) %>% append(., c("American Samoa", "Australia", "Brunei", "Cambodia", "Korea, Republic of", "China", "Fiji", "French Polynesia", "Guam", "Hong Kong", "Indonesia", "Japan", "Kiribati", "Macao", "New Caledonia", "Lao", "Lao People's Democratic Republic", "Malaysia", "Macau", "Marshall Islands", "Mongolia", "Myanmar", "Nauru", "New Zealand", "Palau", "Philippines", "Reunion", "Samoa", "Singapore", "Solomon Islands", "Taiwan, Province of China", "Thailand", "Papua New Guinea", "Tonga", "Tuvalu", "Vanuatu", "Vietnam", "Viet Nam", "Korea (North), Democratic People's Republic of", "Western Samoa", "Mayotte", "Palmyra Atoll", "Micronesia"))
# # europe_and_central_asia <- map.world %>% filter(region == "Europe & Central Asia") %>% pull(country) %>% append(., c("Albania", "Andorra", "Armenia", "Austria", "Azerbaijan", "Belarus", "Belgium", "Bosnia and Herzegovina", "Bulgaria", "Bouvert Island", "Channel Islands", "Croatia", "Cyprus", "Czech Republic", "Denmark", "Estonia", "Faroe Islands", "Finland", "France", "Georgia", "Germany", "Gibraltar", "Greece", "Greenland", "Hungary", "Iceland", "Ireland", "Isle of Man", "Italy", "Jan Mayen", "Kazakhstan", "Kosovo", "Kyrgyz Republic", "Latvia", "Liechtenstein", "Lithuania", "Luxembourg", "Macedonia", "Moldova", "Monaco", "Montenegro", "Netherlands", "Norway", "Poland", "Portugal", "Romania", "Russian Federation", "San Marino", "Scotland", "Serbia", "Serbia and Montenegro", "Slovak Republic", "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland", "Tajikistan", "Turkey", "Turkmenistan", "Ukraine", "United Kingdom", "Uzbekistan", "Wales", "Kyrgyzstan", "Macedonia, the Former Yugoslav Republic of", "Jersey", "UCD · School of Medicine and Medical Science"))
# # latin_america_and_caribbean <- map.world %>% filter(region == "Latin America & Caribbean") %>% pull(country) %>% append(., c("Anguilla", "Antigua and Barbuda", "Argentina", "Aruba", "The Bahamas", "Barbados", "Belize", "Bolivia", "Brazil", "British Virgin Islands", "Cayman Islands", "Chile", "Colombia", "Costa Rica", "Cuba", "Curacao", "Dominica", "Dominican Republic", "East Timor", "Ecuador", "El Salvador", "French Guiana", "Grenada", "Guatemala", "Guadeloupe", "Guyana", "Haiti", "Honduras", "Jamaica", "Mexico", "Nicaragua", "Panama", "Paraguay", "Peru", "Puerto Rico", "Sint Maarten", "St. Kitts and Nevis", "Saint Kitt and Nevis", "St. Lucia", "St. Martin", "St. Vincent and the Grenadines", "Suriname", "Trinidad and Tobago", "Turks and Caicos Islands", "Uruguay", "Venezuela", "Venezuela, Bolivarian Republic of", "Virgin Islands", "Martinique", "Saint Vincent and the Grenadines", "Netherlands Antilles"))
# # middle_east_and_north_africa <- map.world %>% filter(region == "Middle East & North Africa") %>% pull(country) %>% append(., c("Algeria", "Bahrain", "Djibouti", "Egypt", "Gaza Strip", "Iran", "Iran, Islamic Republic of", "Iraq", "Israel", "Jordan", "Kuwait", "Lebanon", "Libya", "Malta", "Morocco", "Oman", "Qatar", "Saudi Arabia", "Syrian Arab Republic", "Tunisia", "United Arab Emirates", "West Bank and Gaza", "Yemen", "West Bank", "Syria"))
# # north_america <- map.world %>% filter(region == "North America") %>% pull(country) %>% append(., c("Bermuda", "Canada", "United States", "Jarvis Island"))
# # south_asia <- map.world %>% filter(region == "South Asia") %>% pull(country) %>% append(., c("Afghanistan", "Bangladesh", "Bhutan", "India", "Maldives", "Nepal", "Pakistan", "Sri Lanka"))
# # subsaharan_africa <- map.world %>% filter(region == "Sub-Saharan Africa") %>% pull(country) %>% append(., c("Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", "Cabo Verde", "Cape Verde", "Cameroon", "Central African Republic", "Chad", "Comoros", "Congo", "Congo, Democratic Republic of the", "Côte d'Ivoire", "Equatorial Guinea", "Eritrea", "Ethiopia", "Gabon", "Gambia", "Ghana", "Guinea", "Guinea-Bissau", "Kenya", "Lesotho", "Liberia", "Madagascar", "Malawi", "Mali", "Mauritania", "Mauritius", "Mozambique", "Namibia", "Niger", "Nigeria", "Rwanda", "São Tomé and Principe", "Senegal", "Seychelles", "Sierra Leone", "Somalia", "South Africa", "South Sudan", "Sudan", "Swaziland", "Tanzania", "Togo", "Uganda", "Zambia", "Zimbabwe", "Guinea Bissau", "Sao Tome and Principe", "Tristan da Cunha", "CÃƒÂ´te d'Ivoire"))
# # 
# 
# 
# 
# #JOIN DATAFRAMES----
# # world_df <- left_join(map.world, all_authors_w_prop, by = c('region' = 'region'))
# 
# 
# #Plot Map
# #Fig. 1 Proportions of Authors Across the Globe:
# #Add legend to display gradient for proportions
# 
# ggplot() +
#   geom_polygon(data = world_df, aes(x = long, y = lat, group = group)) +
#   geom_point(data = world_df, aes(x = long, y = lat), color = "#e60000") +
#   scale_fill_manual(values = c("#CCCCCC","#e60000")) +
#   labs(title = 'Proportions of Authors across the Globe') +
#   theme(text = element_text(family = "Gill Sans", color = "#FFFFFF")
#         ,panel.grid = element_blank()
#         ,plot.title = element_text(size = 12)
#         ,plot.subtitle = element_text(size = 10)
#         ,axis.text = element_blank()
#         ,axis.title = element_blank()
#         ,axis.ticks = element_blank()
#         ,legend.position = "none"
#   )

#Fig. 2 Proportions of Reviewers Across the Globe:


#Fig. 3 Proportions of Editors Across the Globe:


#Test Map Projection Code ----
#Test map projection of author proportions globally
#Need author_origin_prop for author proportions; author_origin_time must be completed first to generate

# loading packages needed to plot maps
library(mapproj) # needed to maintain dimensions
library(viridis) # needed for color scaling
library(ggalt) # needed for coord_proj
library(tidyverse)

# reading in data ---------------------------------------------------------

  # creating a list of all countries with data available for plotting
  countries <- map_data("world") %>% # making list of countries with available mapping information
    pull(region) %>% # pulls out list of countries
    unique(.) # makes list of unique countries
  


# plotting functions ------------------------------------------------------

# creating function for plotting world map information for authors
plot_world_author_prop <- function(author_origin_prop) {
  
  # creating df of plotting coordinates for world map
  world_map <- map_data("world") %>% # getting data
    mutate(region = tolower(region)) # transforming all country names to lower case
  
  # main plotting function to make world map
  plot <- author_origin_prop %>% 
    ggplot(aes(map_id = region)) + # map_id is required aes for geom_map
    geom_map(aes(fill = region), map = world_map, colour = "black", size = 0.25) + # plots the world map
    expand_limits(x = c(-179,179), y = c(-75,89)) + # expands plot limits to encompass data, x = longitude, y = latitude
    scale_fill_viridis(na.value = "white", guide = guide_colorbar(ticks = FALSE, frame.colour = "black"),
                       limits = c(0,40), breaks = c(0, 20, 40), labels = c(0, 20, 40)) + # making NA values = white and scaling using cividis palette from viridis pkg
    labs(title = "Worldwide Proportions of Authors", # setting chart labels
         fill = "Proportion") +
    coord_proj(proj = "+proj=wintri") + # correcting for Earth curvature using Winkel tripel projection
    theme(text = element_text(family = "Helvetica"),
          legend.title = element_text(size = 9),
          legend.text = element_text(size = 9),
          plot.title = element_text(hjust = 0.5, size = 9, face = "bold"),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          panel.background = element_rect(fill = "white"))
  
  # returning the map
  return(plot)
}


# generating plots of degrees from world ----------------------------------

# generating world map with global author proportions
world_author_map <- plot_world_author_prop(author_origin_prop)

# saving world degree freq map
ggsave(filename = "results/geographic/figures/world_author_map.png", plot = world_author_map, width = 15, height = 10, dpi = 300)



# detaching conflicting packages ------------------------------------------

# these pacakges may interfere with other packages, specifically purrr
detach("package:mapproj", unload = TRUE)
detach("package:ggalt", unload = TRUE)
detach("package:maps", unload = TRUE)
