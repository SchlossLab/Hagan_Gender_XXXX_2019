library(scales)
library(RColorBrewer)
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

gen_ed_labels <- c("Women", "Men")

gen_colors <- c("#D55E00", "#0072B2", "#999999")

gen_ed_colors <- c("#D55E00", "#0072B2")

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
    labs(y = "Proportion of Published", fill = "Gender")+
    my_theme_leg_horiz
  
  return(plot)
}  
