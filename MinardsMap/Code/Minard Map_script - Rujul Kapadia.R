############################Creating Minard's map##########################
#Reading cities data - The cities through which the army had  marnched
minard_city <- read.table("C:\\Trinity\\Data Visualisation\\Assignment 2\\cities.txt",  stringsAsFactors = FALSE, header = TRUE)

#Reading troop data
minard_troop <- read.table("C:\\Trinity\\Data Visualisation\\Assignment 2\\troops.txt", stringsAsFactors = FALSE, header = TRUE)

#Reading temperature data
minard_temp <- read.table("C:\\Trinity\\Data Visualisation\\Assignment 2\\temps.txt",header = TRUE, stringsAsFactors = FALSE)

minard_temp$date = dmy(minard_temp$date)

colnames(minard_temp)[1] <- "long"
colnames(minard_troop)[1] <- "long"
colnames(minard_city)[1] <- "long"

library(ggplot2)

 
#Reading troop data
minard_troop_mod <- read.table("C:\\Trinity\\Data Visualisation\\Assignment 2\\troops_mod.csv", stringsAsFactors = FALSE, header = TRUE, sep=',')

######################Minard's  March Plot############################
#Calling ggplot
minard_g_plot_4_1 <- ggplot()
# Defining path
minard_g_plot_4_2 <- geom_path(data = minard_troop, aes(x = long, y = lat, group = group, color = direction, size = survivors), lineend = "round")
#Defining points on the plot
minard_g_plot_4_3 <- geom_point(data = minard_city, aes(x = long, y = lat), color = "#d82c2c")
#Labelling cities
minard_g_plot_4_4 <- geom_text_repel(data = minard_city, aes(x = long, y = lat, label = city), color = "#d82c2c", family = "Open Sans Condensed Bold") 
#Labelling army  size as per coordinates
minard_g_plot_4_5 <- geom_text_repel(data = minard_troop_mod, aes(x = long, y = lat, label = survivors), color = "#252523", family = "Open Sans Condensed Bold", size=3.4, angle=90, vjust=1.8, segment.alpha = 0)
#Scaling
minard_g_plot_4_6 <- scale_size(range = c(0.5, 15)) 
#Selecting path  colors
minard_g_plot_4_7 <- scale_colour_manual(values = c("#fdadbc", "#000000"))
#Removing x and y coord titles
minard_g_plot_4_8 <- labs(x = NULL, y = NULL)
#Set respective color-size guides
minard_g_plot_4_9 <-guides(color = FALSE, size = FALSE)
#Removing background theme
minard_g_plot_4_10 <- theme_bw()
#Customizing non-data  elements of the graph
minard_g_plot_4_11<- theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), panel.border = element_blank())

#Defining Minard's_March Plot
minard_march_plot <-minard_g_plot_4_1+minard_g_plot_4_2+minard_g_plot_4_3+minard_g_plot_4_4+minard_g_plot_4_5+minard_g_plot_4_6+minard_g_plot_4_7+minard_g_plot_4_8+minard_g_plot_4_9+minard_g_plot_4_10+minard_g_plot_4_11

#####################Temperature plot################################
#Formating labels for plot
minard_temp$label_format <- paste0(minard_temp$temp, "°, ", minard_temp$month, ". ", minard_temp$day)

#Defining plot and aesthetics
temp_g_plot_5_1 <- ggplot(aes(x = long, y = temp), data = minard_temp )
#Line plot  
temp_g_plot_5_2 <- geom_line()
#Definig temperature Labels  
temp_g_plot_5_3 <- geom_text(aes(label = label_format), size = 3.5, vjust=1.2)
#Axes labels
temp_g_plot_5_4 <- labs(x = NULL, y = "° Celsius")
#Scaling x axis
temp_g_plot_5_5 <- scale_x_continuous(limits = ggplot_build(minard_march_plot)$layout$panel_ranges[[1]]$x.range) 
#Scaling y axis
temp_g_plot_5_6 <- scale_y_continuous(position = "right")
#Defining cart cord  
temp_g_plot_5_7 <- coord_cartesian(ylim = c(-34, 4))
#Removing background theme
temp_g_plot_5_8 <- theme_bw()
##Customizing non-data  elements of the graph
temp_g_plot_5_9 <- theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(), axis.text.x = element_blank(), axis.ticks = element_blank(), panel.border = element_blank())

minard_temp_plot <- temp_g_plot_5_1+temp_g_plot_5_2+temp_g_plot_5_3+temp_g_plot_5_4+temp_g_plot_5_5+temp_g_plot_5_6+temp_g_plot_5_7+temp_g_plot_5_8+temp_g_plot_5_9

########################Combining plots############################
library(gridExtra)

minard_plot_combined <- gtable_rbind(ggplotGrob(minard_march_plot),ggplotGrob(minard_temp_plot))

layout_name<-minard_plot_combined$layout$name
temp <- minard_plot_combined$layout$t
map_panels <- temp[grep("panel", layout_name)]
minard_plot_combined$heights[map_panels] <- unit(c(3.1,1.1), "null")

grid::grid.newpage()   
grid::grid.draw(minard_plot_combined)

