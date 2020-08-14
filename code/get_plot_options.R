library(scales)
library(RColorBrewer)
library(cowplot)
#library(patchwork)

#preferred themes----
my_theme <- theme_classic() + 
  theme(legend.position = "none", axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"), 
        plot.title = element_text(size=16,face="bold"), 
        axis.text.x = element_text(angle = 45, hjust = 1))

my_theme_horiz <- theme_classic() + 
  theme(legend.position = "none", axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"), 
        plot.title = element_text(size=16,face="bold"))

my_theme_leg <- theme_classic() + 
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"), 
        plot.title = element_text(size=16,face="bold"), 
        axis.text.x = element_text(angle = 45, hjust = 1))

my_theme_leg_left <- theme_classic() + 
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"), 
        plot.title = element_text(size=16,face="bold"), 
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "left")

my_theme_leg_horiz <- theme_classic() + 
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"), 
        plot.title = element_text(size=16,face="bold"))

#adding proportion/count labels to barchart
prop_lab_low <- geom_text(aes(label = scales::percent((..count..)/sum(..count..)), y= ((..count..)/sum(..count..))), stat = "count", vjust = 1)

count_lab_low <- geom_text(stat = "count", aes(label=..count..), vjust = 1)

prop_lab_high <- geom_text(aes(label = scales::percent((..count..)/sum(..count..)), y= ((..count..)/sum(..count..))), stat = "count", vjust = -1)

count_lab_high <- geom_text(stat = "count", aes(label=..count..), vjust = -1)

#colorblind palettes
#The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") #grey, goldenrod, light blue, dark green, gold, navy blue, orange rust, pink

# The palette with black:
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") #black

#gradient for comparing "performance"
gen_gradient <- scale_fill_gradient2(limits = c(-10,10), midpoint = 0, low = "#D55E00", mid='snow3', 
                                       high = "#0072B2", space = "Lab")

gen_gradient_40 <- scale_fill_gradient2(limits = c(-45,45), midpoint = 0, low = "#D55E00", mid='snow3', 
                                     high = "#0072B2", space = "Lab")

#settings----
gen_levels <- c("none", "male", "female")

gen_ed_levels <- c("female", "male")

gen_breaks <- c("NA", "male", "female")

gen_labels <- c("none" = "Unknown", "male" = "Men",
                "female" = "Women")

gen_linetype <- c("solid", "dashed", "dotted")

gen_ed_labels <- c("Women", "Men")

gen_colors <- c("none" = "#999999", "male" = "#0072B2",
                "female" = "#D55E00")

gen_ed_colors <- c("#D55E00", "#0072B2")

gen_x_replace <- scale_x_discrete(breaks=gen_levels,
                 labels=gen_labels)

gen_ed_facet <- function(x){
  ifelse(x == "female", "Women Editors", "Men Editors")
}

gen_rev_facet <- function(x){
  ifelse(x == "female", "Women Reviewers", "Men Reviewers")
}

#figure out which year is the last & isolate the proportion values
get_gen_prop_text <- function(df, n_row, group){
  
  quo_group <- enquo(group)
  
  df %>% 
    arrange(desc(year)) %>% #put most recent year at top
    head(n = n_row) %>%  #take top n_rows (f/m/unknown)
    arrange(!!sym(group)) %>% #ensure alphabetical arrangement
    select(!!sym(group), proportion)#drop unness cols
}

#identify ymax & add 5 to set yaxis
get_ymax <- function(df){
  df %>% 
    arrange(desc(proportion)) %>% #arrange so highest val is @ top
    head(n = 1) %>% #take top row
    pull(proportion) %>% #pull highest value
    sum(., 5) %>% round() #add to five and round to a whole digit
}

#plotting functions----

plot_impact_data <- function(measure, coord_max){
  
  data <- impact_data %>% 
    filter(measure.name == measure)
  
  data <- if(measure == "avg.JIF"){data %>% 
      select(gender, random.manu.num, measure.value) %>% 
      distinct()}else(data)
  
  plot <- if(coord_max == "NULL"){
    ggplot(data, aes(x = measure.value, fill = gender))+
      labs(x = "Average JIF")
  }else(
    ggplot(data, aes(x = value.per.month, fill = gender))+
      coord_cartesian(xlim = c(0, coord_max)) +
      labs(x = measure)
  )
  
  plot <- plot + 
    geom_histogram(aes(y=0.5*..density..), 
                   alpha=0.5, position='identity', binwidth=0.5)+
    scale_fill_manual(labels = gen_ed_labels, values = gen_ed_colors)+
    labs(y = "\nProportion of Published", fill = "Gender",
         x = paste0(measure, " per Month Published"))+
    my_theme_leg_horiz
  
  return(plot)
}  

#plot proportion of each gender in a role over time, assumes df output from get_prop_by_yr()
gender_line_plot <- function(df, ymax){
  plot <- ggplot(df) + 
    geom_line(aes(x = year, y = proportion, 
                  color = gender), size =0.75)+
    coord_cartesian(ylim = c(0, ymax))+
    scale_color_manual(breaks = gen_levels, 
                       labels = gen_labels, 
                       values = gen_colors)
  
  #plot <- plot + my_theme_horiz
  
  return(plot)
}

#bar plot of total number of each gender for the given time period
gender_bar_plot <- function(df){
  plot <- ggplot(df)+
    geom_bar(aes(x = gender, fill = gender))+
    scale_fill_manual(values = gen_colors)+
    scale_x_discrete(labels = gen_labels)+
    my_theme_horiz
  
  return(plot)
}

#line plot of the proportions of all journals, assumes df output from get_prop_j_by_yr
j_gen_line_plot <- function(df, ymax){
  plot <- ggplot(df) + 
    geom_line(aes(x = year, y = proportion, group = gender, 
                  color = gender))+
    #scale_y_continuous(breaks = c(0, 15, 30, 45))+
    scale_color_manual(values = gen_colors, 
                          breaks = gen_levels, 
                          labels = gen_labels)+
    coord_cartesian(ylim = c(0, ymax)) +
    facet_wrap(~ journal, nrow = 2)
  
  #plot <- plot
  
  return(plot)
}

#line plot of submissions versus publications for each gender by journal
plot_sub_v_pub_j_time <- function(temp_sub, temp_pub){
  
  name <- paste0(as.character(temp_sub)) #isolate the name of the df as a string
  
  print(name)
  
  auth_type <- case_when( #identify authors being examined
    str_detect(name, "first") ~ "First",
    str_detect(name, "last") ~ "Last",
    str_detect(name, "corres") ~ "Corresponding",
    str_detect(name, "mid") ~ "Middle",
    TRUE ~ "All"
  )
  
  j_authors_w_prop <- get_sub_pub_prop(temp_sub, temp_sub, "Each")
  
  max_journ_value <- get_ymax(j_authors_w_prop)
  
  plot <- j_authors_w_prop %>% 
    filter(gender == "male" | gender == "female") %>% 
    ggplot() + 
    geom_line(aes(x = year, y = proportion, 
                  linetype = manu.type, color = gender))+
    coord_cartesian(ylim = c(0, max_journ_value)) +
    scale_color_manual(values = gen_ed_colors, 
                       breaks = gen_ed_labels)+
    facet_wrap(~ journal)+ 
    my_theme_leg +
    labs(x ="Year",
         y = paste("\nProportion of", auth_type, "Authors"),
         linetype = "Manuscript Status")
  
  return(plot)
}

#line plot of submissions versus publications for each gender, all journals combined-
plot_sub_v_pub_time <- function(temp_sub, temp_pub, text){
  
  name <- paste0(as.character(temp_sub)) #isolate the name of the df as a string
  
  print(name)
  
  auth_type <- case_when( #identify authors being examined
    str_detect(name, "first") ~ "First",
    str_detect(name, "last") ~ "Last",
    str_detect(name, "corres") ~ "Corresponding",
    str_detect(name, "mid") ~ "Middle",
    TRUE ~ "All"
  )
  
  c_authors_w_prop <- get_sub_pub_prop(temp_sub, temp_pub, "All") %>% 
    filter(gender == "female" | gender == "male")
  
  #figure out which year is the last & isolate the proportion values
  m_text_values <- c_authors_w_prop %>% 
    filter(year == "2017") %>% filter(gender == "male")
  
  f_text_values <- c_authors_w_prop %>% 
    filter(year == "2017") %>% filter(gender == "female")
  
  max_value <- get_ymax(c_authors_w_prop) 
  
  #line plot
  plot <- c_authors_w_prop %>%  
    ggplot() + 
    geom_line(aes(x = year, y = proportion, linetype = manu.type,
                  color = gender), size = 0.75)+
    coord_cartesian(ylim = c(0, max_value))+
    scale_color_manual(values = gen_ed_colors, 
                       breaks = gen_ed_labels)
  
  plot <- if(text == "TRUE"){
    plot +
    annotate(geom = "text", x = 2017, 
             y = m_text_values[2,5]+2, label = "Men")+
    annotate(geom = "text", x = 2017, 
             y = f_text_values[2,5]+2, label = "Women")
    }else{plot}
  
  plot <- plot +
    labs(x = "Year",
         y = paste("\nProportion of\n", auth_type, "Authors"),
         linetype = "Manuscript Status")
  
  return(plot)
}

plot_rev_time <- function(rev_df){
  
  rev_df <- as.character(rev_df)
  
  this_df <- get(rev_df) #pull this df from the global environment
  
  name <- paste0(rev_df) #isolate the name of the df as a string
  
  print(name)
  
  rev_type <- case_when( #identify reviewers being examined
    str_detect(name, "reviewer") ~ "Reviewers",
    str_detect(name, "pot_rev") ~ "Potential Reviewers"
  )
  
  all_rev_w_prop <- map_dfr(years, function(x){
    get_prop_by_yr(x, this_df, "gender", "All")}) 
  
  #figure out which year is the last & isolate the proportion values
  #text_values <- get_gen_prop_text(all_rev_w_prop, 3, "gender")
  
  max_value <- get_ymax(all_rev_w_prop) 
  
  #line plot of all journals combined by year
  plot <- gender_line_plot(all_rev_w_prop, max_value) + 
    labs(x = "Year", y = paste("\nProportion of\n", rev_type),
         color = "Gender")
  
  return(plot)
}

plot_feature_ranks <- function(data){
  # Plot from highest median ranked OTU to least (only top 5) and thir ranks that lay between 1-100
  # Rank 1 is the highest rank
  plot <- ggplot(data, aes(reorder(data$key, -data$rank, FUN = median), data$rank)) +
    geom_point(aes(colour= factor(data$sign)), size=1.5) + # datapoints lighter color
    scale_color_manual(values=c("#56B4E9","red3", "#999999")) +
    stat_summary(fun = function(x) median(x), colour = 'black', geom = "point", size = 3) + # Median darker
    coord_flip(ylim=c(0,15)) +
    my_theme_leg_horiz
  return(plot)
}
