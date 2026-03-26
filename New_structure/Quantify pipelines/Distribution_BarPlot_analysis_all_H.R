##################################
# Parameter Distribution Barplot #
##################################

rm(list = ls())

source('/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/git/EEGManyPipelines-personal/R codes/log_output.R')
library(rstudioapi)

script_path <- rstudioapi::getSourceEditorContext()$path
script_name <- basename(script_path)
script_dir  <- dirname(script_path)

#-----Loading  data and packages-----#
library(dplyr)
library(ggplot2)
library(entropy)
library(RColorBrewer)
library(ggpubr)
library(paletteer)

# Loading the data, change path accordingly! ~
H1 <- read.csv('/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/data/Main Analysis/all_var_AQ_h1_corrected.csv')
H1$topo_region_h1[H1$topo_region_h1=='posterior'] <- 'parietal'
indx_stat <- c(10,11,13:16,20)
AQ_analyse_h1 <- H1[,indx_stat]
AQ_analyse_h1$nr_chan_h1[AQ_analyse_h1$nr_chan_h1 == AQ_analyse_h1$nr_chan_h1[4]] <-NA # change the mean values that were used for stat analyses to NaN as these fileds were not reported in the original data

#H2
H2 <- read.csv('/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/data/Main Analysis/all_var_AQ_h2a_corrected.csv')
# remove colomns we will not use
H2 <- H2[,c(-1:-10,-13,-14, -20:-33)]
H2$nr_ch_h2a[H2$nr_ch_h2a == H2$nr_ch_h2a[4]] <-NA # change the mean values that were used for stat analyses to NaN as these fileds were not reported in the original data

# H3
H3 <- read.csv('/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/data/Main Analysis/all_var_AQ_h3a_corrected.csv')
# remove colomns we will not use
H3 <- H3[,c(-1:-16, -19,-20, -26:-32)]

all_hypotheses <- c('AQ_analyse_h1','H2','H3')

num_chan_file_names <- c('AQ_analyse_nr_chan_H1.png', 'AQ_analyse_nr_chan_H2.png', 'AQ_analyse_nr_chan_H3.png')
categorical_file_names <- c('AQ_analyse_barplots_H1.png','AQ_analyse_barplots_H2.png','AQ_analyse_barplots_H3.png')

H_col_names <- list(c("spatial avg.", "mult.comp.correction","temporal avg.","stat.method", "mult.comp.method", "topographic region"),
                    c("mult.comp.correction","temporal avg.","spatial avg.", "topographic region", "stat.method", "mult.comp.method"),
                    c("mult.comp.correction","temporal avg.","spatial avg.", "stat.method", "mult.comp.method","topographic region"))

for (i in 1:3) {
  
  #current hypothesis
  current_H <- get(all_hypotheses[i])  
  
  #exchange 'unknown' entries as NA
  current_H[] <- lapply(current_H, function(x) ifelse(x == "unknown", NA, x))
  current_H[] <- lapply(current_H, function(x) ifelse(x == "", NA, x))
  
  #-----Plot number of analyzed channels-----#
  tiff(paste0("/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/figures/", num_chan_file_names[i]),
       units="in", width=3.8, height=2.2, res=300)

    ggplot(current_H, aes(x=current_H[,grep("^nr", names(current_H))])) + 
    geom_histogram(aes(y = ..count.. / sum(..count..) * 100), colour="black", fill="#FC9E8BFF")+
    xlab("nr. of analyzed channels")+
    theme_bw() +labs(y = "% of teams")+
    theme(text = element_text(size = 20))
    
  dev.off()
  
  # Log the event
  log_output(rstudioapi::getSourceEditorContext()$path, num_chan_file_names[i])
  
  #----- Plot categorical variables -----#
  current_H_categorical <- current_H[,-grep("^nr", names(current_H))]
  
  # Correct values: convert binary to yes - no
  for (n in 1:length(current_H_categorical)){
    if (length(table(current_H_categorical[n]))==2){
      current_H_categorical[n][current_H_categorical[n]==1] <- "yes"
      current_H_categorical[n][current_H_categorical[n]==0] <- "no"
    }
  }
  
  # Counting the instances for the distribution and sorting data
  current_H_categorical_names <- colnames(current_H_categorical)
  data_distribution<-c()
  data_distribution_names<-c()
  num_elements <- c()
  
  for (p in 1:length(current_H_categorical)){
    #remove nan values
    one_step_choices <- current_H_categorical %>% 
      filter(!is.na(!!sym(current_H_categorical_names[p]))) %>% 
      count(!!sym(current_H_categorical_names[p]))
    
    num_elements[p] <- nrow(one_step_choices)
    
    data_distribution <- append(data_distribution, list(one_step_choices[,2]))
    data_distribution_names<-append(data_distribution_names, list(one_step_choices[,1]))
    data_distribution_names[[p]]<-data_distribution_names[[p]][order(-data_distribution[[p]])] #re-order so that the most common is always the first
    data_distribution[[p]]<-data_distribution[[p]][order(-data_distribution[[p]])]
  }
  
  # Turn into percentages
  data_percentages<-list()
  for (n in 1:length(current_H_categorical_names)){
    nteams <- sum(as.data.frame(data_distribution[n]))
    data_percentages<-append(data_percentages, lapply(data_distribution[n], function(x) {x*100/nteams}))
  }
  
  # Get max elements
  nrow_max<-0
  for (n in 1:length(current_H_categorical_names)){
    nrow_max <- max(nrow_max,length(data_distribution[[n]]))
  }
  
  # Sort percentages from largest to smallest number of choices
  data_percentages_sorted <- data_percentages[order(num_elements)]
  data_percentages_names_sorted <- data_distribution_names[order(num_elements)]
  plot_names_ordered <- current_H_categorical_names[order(num_elements)]
  
  #-----Plotting the Data-----#
  
  # Create matrix with zeros
  data_barplot <- matrix(0, ncol = length(plot_names_ordered), nrow = nrow_max)
  colnames(data_barplot)<-plot_names_ordered
  
  # Fill matrix with data
  for (n in 1:length(plot_names_ordered)) {
    for (j in 1:length(data_percentages_sorted[[n]])) {
      data_barplot[j,n]<-data_percentages_sorted[[n]][j]
    }
  }
  
  # change column names
  colnames(data_barplot) <- H_col_names[[i]]

  # Barplot of the data
  tiff(paste0("/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/figures/",categorical_file_names[i]), units="in", 
       width=8.25, height=3.5, res=300)
  par(mar=c(4,9,2,1), cex=1.5) #bottom, left, top, right
  barplot(data_barplot, col="#FC9E8BFF" , border="white", 
          horiz=T, las=1, xlab = "% of teams")
  
  # add most popular options
  
  for (n in 1:length(data_percentages_names_sorted)){
    #label_all<-c()
    label_x<-0
    if ((i == 2 & n == 4) | (i == 3 & n == 6)){
      labeltext<-as.character(data_percentages_names_sorted[[n]][1])
      
      #label_all<-append(label_all, labeltext)
      text(1+label_x,-0.67+n*1.2, labeltext, adj=c(0,0), cex = 0.8) #0.6 + 1.2
      label_x<-label_x+data_percentages_sorted[[n]][m] #1 + percentages
    }else if (i ==2 & n == 6){
      for (m in 1:2){
        labeltext<-as.character(data_percentages_names_sorted[[n]][m])
        
        #label_all<-append(label_all, labeltext)
        text(1+label_x,-0.67+n*1.2, labeltext, adj=c(0,0), cex = 0.8) #0.6 + 1.2
        label_x<-label_x+data_percentages_sorted[[n]][m] #1 + percentages
      }
    }else{
      for (m in 1:min(3, length(data_percentages_names_sorted[[n]]))){
        labeltext<-as.character(data_percentages_names_sorted[[n]][m])
        
        #label_all<-append(label_all, labeltext)
        text(1+label_x,-0.67+n*1.2, labeltext, adj=c(0,0), cex = 0.75) #0.6 + 1.2
        label_x<-label_x+data_percentages_sorted[[n]][m] #1 + percentages
      }
    }
    
  }
  
  dev.off()
  
  # Log the event
  log_output(rstudioapi::getSourceEditorContext()$path, categorical_file_names[i])
  
}
