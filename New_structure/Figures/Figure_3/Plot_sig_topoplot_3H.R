#Elena Cesnaite 21.11.2022
#the code was adjusted from https://www.mattcraddock.com/blog/2017/02/25/erp-visualization-creating-topographical-scalp-maps-part-1/

rm(list = ls())

#Load libraries
#install.packages(c("tidyverse", "akima", "scales", "mgcv", "gridExtra", "png", "grid"))
library(ggplot2)
library(tidyverse)
library(akima)
library(scales)
library(mgcv)
library(gridExtra)
library(png)
library(grid)
library(dplyr)

# Load data
h1 <- read_csv('/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/git/EEGManyPipelines-personal/Matlab/Result forms/Data/res_topoplot_h1.csv')
h2 <- read.csv('/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/git/EEGManyPipelines-personal/Matlab/Result forms/Data/res_topoplot_h2a.csv')
h3 <-read_csv('/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/git/EEGManyPipelines-personal/Matlab/Result forms/Data/res_topoplot_h3a_19072023.csv')

all_H <- c("h1","h2","h3")

#We’ll make a function to draw a circle for a head, then a triangle for a nose, and add them to the plot.
theme_topo <- function(base_size = 12)
{
  theme_bw(base_size = base_size) %+replace%
    theme(
      rect             = element_blank(),
      line             = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank()
    )
}

circleFun <- function(center = c(0,0),diameter = 1, npoints = 100) {
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

#file names
out_names <- c("topoplot_h1_v2.png","topoplot_h2a_v2.png","topoplot_h3a_v2.png")
for (i in 1:3) {

myData <- get(all_H[i])
  
#add x and y locations
myData$radianTheta <- pi/180*myData$theta
myData <- myData %>%
  mutate(x = .$radius*sin(.$radianTheta),
         y = .$radius*cos(.$radianTheta))

headShape <- circleFun(c(0, 0), round(max(myData$x)), npoints = 100) # 0
nose <- data.frame(x = c(-0.075,0,.075),y=c(.495,.575,.495))

#set color
myColor <- colorRampPalette(viridis_pal()(20))

## TOPOPLOT FOR ALL TEAMS ##
png(paste0("/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/figures/",out_names[i]), units="in", width=4, height=3, res=300)

p <- ggplot(headShape,aes(x,y))+
  geom_path(size = 1.5)+
  geom_point(data = myData,aes(x,y,colour = value),size = 4)+
  scale_colour_gradientn(colours = myColor(10), guide = "colourbar",oob = squish, name = "Nr. teams")+ #note: oob = squish forces everything outside the colour limits to equal nearest colour boundary (i.e. below min colours = min colour)
  geom_line(data = nose,aes(x, y, z = NULL),size = 1.5)+
  theme_topo()+ theme(legend.text=element_text(size=15), legend.title = element_text(size=15))+
  coord_equal() 

ggsave(paste0("/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/figures/",out_names[i]), plot = p,
       width = 4, height = 3, dpi = 300)
}

