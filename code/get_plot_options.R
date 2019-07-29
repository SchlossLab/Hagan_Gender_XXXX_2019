library(scales)
library(RColorBrewer)
library(cowplot)
#library(patchwork)

#preferred themes
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
gen_gradient <- scale_fill_gradient2(low = "#D55E00", mid='snow3', 
                                       high = "#0072B2", space = "Lab")

#settings----
gen_levels <- c("female", "male", "none")

gen_breaks <- c("female", "male", "NA")

gen_labels <- c("Women", "Men", "Unclear")

gen_linetype <- c("solid", "dashed", "dotted")

gen_ed_labels <- c("Women", "Men")

gen_colors <- c("#D55E00", "#0072B2", "#999999")

gen_ed_colors <- c("#D55E00", "#0072B2")

gen_x_replace <- scale_x_discrete(breaks=gen_levels,
                 labels=gen_labels)

gen_ed_facet <- function(x){
  ifelse(x == "female", "Woman", "Man")
}

#figure out which year is the last & isolate the proportion values
get_gen_prop_text <- function(df, n_row, group){
  
  quo_group <- enquo(group)
  
  df %>% 
    arrange(desc(year)) %>% #put most recent year at top
    head(n = n_row) %>%  #take top n_rows (f/m/unclear)
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
gender_line_plot <- function(df, ymax, wo_y, me_y, un_y){
  plot <- ggplot(df) + 
    geom_line(aes(x = year, y = proportion, linetype = gender), size =0.75)+
    coord_cartesian(ylim = c(0, ymax))+
    scale_linetype_manual(breaks = gen_levels, labels = gen_labels, values = gen_linetype)+
    annotate(geom = "text", x = 2018, y = wo_y+1.5, label = "Women")+
    annotate(geom = "text", x = 2018, y = me_y+1.5, label = "Men")
  
  plot <- if(un_y != "N"){ #conditional if some genders are unknown
    plot + annotate(geom = "text", x = 2018, y = un_y+1.5, label = "Unclear")
  }else(plot)
  
  plot <- plot + my_theme_horiz
  
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
    geom_line(aes(x = year, y = proportion, group = gender, linetype = gender))+
    #scale_y_continuous(breaks = c(0, 15, 30, 45))+
    scale_linetype_manual(values = gen_linetype, breaks = gen_levels, labels = gen_labels)+
    coord_cartesian(ylim = c(0, ymax)) +
    facet_wrap(~ journal)
  
  plot <- plot + my_theme_leg
  
  return(plot)
}