# plot confidence in team's findings
rm(list = ls())

#load libraries
library(ggplot2)
library(tidyr)
library(purrr)
library(patchwork)
library(svglite)

posterior <- read.csv('/Beliefs paper/Data/Prior_and_posterior_beliefs/manypipelines_table_posterior_questionnaire.csv')
pairs <- c(5,8,17) # which columns to take

for (i in 1:3) {
  # split based on whether the effect was found or not
  data_to_plot <- posterior[,c(pairs[i],pairs[i]-1)]
  
  #add a column for confirmation 
  data_to_plot$confirm <- factor(data_to_plot[,2], levels = c(0, 1), labels = c("Not Confirmed", "Confirmed"))
  
  # plot
  svglite(paste0("/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/figures/confidence_h", i, ".svg"), 
          width = 4.4, height = 3)
  
  p <- ggplot(data_to_plot, aes(x = data_to_plot[,1], fill = data_to_plot[,3], color = data_to_plot[,3])) +
    geom_density(alpha = 0.7, linewidth = 0.7) +
    scale_fill_manual(values = c("Not Confirmed" = "#154360","Confirmed" = "#A4BED5FF")) +
    scale_color_manual(values = c("Not Confirmed" = "#154360","Confirmed" = "#A4BED5FF")) +
    labs(
      x = "Confidence rating",
      y = "Density",
      fill = "",
      color = "") +
    coord_cartesian(ylim = c(0, 0.04))+
    theme_minimal() +
    theme(legend.position = "top",text = element_text(size = 20),axis.text = element_text(size=20))
    
  print(p)
   dev.off()

}
