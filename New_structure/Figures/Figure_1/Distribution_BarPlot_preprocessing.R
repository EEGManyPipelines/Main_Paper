##################################
# Parameter Distribution Barplot for the Figure 1#
##################################

rm(list = ls())
source('/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/git/EEGManyPipelines-personal/R codes/log_output.R')

#-----Loading the data and packages-----#

# Install and load packages if needed! ~
#install.packages("dplyr")
#install.packages("entropy")
#install.packages("ggplot2")
#install.packages("RColorBrewer")

library(dplyr)
library(ggplot2)
library(entropy)
library(RColorBrewer)
library(ggpubr)
library(paletteer)

# Loading the data, change path accordingly! ~
h1_preproc <- read.csv('/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/data/Main Analysis/all_var_AQ_h1_corrected.csv')
h1_preproc$topo_region_h1[h1_preproc$topo_region_h1=='posterior'] <- 'parietal'

#exchange 'unknown' entries as NA
h1_preproc[] <- lapply(h1_preproc, function(x) ifelse(x == "unknown", NA, x))

#-----Preparing Data and calculate percentages-----#

# remove columns not associated with pre-processing steps
AQ_preproc <- h1_preproc[,c(-1,-3,-10,-11,-13:-20,-24:-33)]

# Listing the plot-Labels
AQ_preproc_names <- c("software", "high-pass cutoff", "high-pass type", "high-pass direction","reference", "sampling freq.", "excl. participants",
                "segment. excl. crit.", "ICA algorithm", "visual bad comp. sel.", "plugin bad comp. sel.")
colnames(AQ_preproc) <- AQ_preproc_names

# FOR CONTINUOUS VARIABLES #
# Show min-max values and distribution of teams that chose those values
indx_contin <- c(2,6,7)
AQ_cont <- AQ_preproc[,indx_contin]

p1 <- ggplot(AQ_cont, aes(x=`high-pass cutoff`)) + 
  geom_histogram(aes(y = ..count.. / sum(..count..) * 100), colour="black", fill="#92BEBAFF")+
  theme_bw() +
  labs(y = "% of teams")+ 
  theme(text = element_text(size = 20) )# 

p2 <- ggplot(AQ_cont, aes(x=`sampling freq.`)) + 
  geom_histogram(aes(y = ..count.. / sum(..count..) * 100), colour="black", fill="#92BEBAFF")+
  theme_bw() +
  labs(y = "% of teams")+
  theme(text = element_text(size = 20))

p3 <- ggplot(AQ_cont, aes(x=`excl. participants`)) + 
  geom_histogram(aes(y = ..count.. / sum(..count..) * 100), colour="black", fill="#92BEBAFF")+
  theme_bw() +
  labs(y = "% of teams")+
  theme(text = element_text(size = 20))

tiff("/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/figures/AQ_preprocess_density.png", 
     units="in", width=3.5, height=5.8, res=300)
ggarrange(p1,p2,p3,nrow = 3,ncol = 1) #nrow & ncol depend on how you want to 
#organize your plots
dev.off()

# Log the event
log_output("Distribution_BarPlot_preproc_and_H1.R", "AQ_preprocess_density.png")

# FOR CATEGORICAL VARIABLES #
AQ_cat <- AQ_preproc[,-indx_contin]

# Correct values: convert binary to yes - no
for (i in 1:length(AQ_cat)){
  if (length(table(AQ_cat[i]))==2){
    AQ_cat[i][AQ_cat[i]==1] <- "yes"
    AQ_cat[i][AQ_cat[i]==0] <- "no"
  }
}

# Couting the Instances for distribution and sort data
AQ_cat_names <- colnames(AQ_cat)
data_distribution<-c()
data_distribution_names<-c()
num_elements <- c()

for (n in 1:length(AQ_cat)){
  #remove nan values
  one_step_choices <- AQ_cat %>% 
    filter(!is.na(!!sym(AQ_cat_names[n]))) %>% 
    count(!!sym(AQ_cat_names[n]))
  
  num_elements[n] <- nrow(one_step_choices)
  
  # new_item<-new_item[rev(order(new_item$n)),]
  data_distribution <- append(data_distribution, list(one_step_choices[,2]))
  data_distribution_names<-append(data_distribution_names, list(one_step_choices[,1]))
  data_distribution_names[[n]]<-data_distribution_names[[n]][order(-data_distribution[[n]])] #re-order so that the most common is always the first
  data_distribution[[n]]<-data_distribution[[n]][order(-data_distribution[[n]])]
}

# Turn into Percentages
data_percentages<-list()
for (n in 1:length(AQ_cat_names)){
  nteams <- sum(as.data.frame(data_distribution[n]))
  data_percentages<-append(data_percentages, lapply(data_distribution[n], function(x) {x*100/nteams}))
}

# Get max elements
nrow_max<-0
for (n in 1:length(AQ_cat_names)){
  nrow_max <- max(nrow_max,length(data_distribution[[n]]))
}

# Sort percentages from largest to smallest number of choices
data_percentages_sorted <- data_percentages[order(num_elements)]
data_percentages_names_sorted <- data_distribution_names[order(num_elements)]
plot_names_ordered <- AQ_cat_names[order(num_elements)]
#-----Plotting the Data-----#

# Create matrice with zeros
data_barplot <- matrix(0, ncol = length(plot_names_ordered), nrow = nrow_max)
colnames(data_barplot)<-plot_names_ordered


# Fill matrice with data
for (i in 1:length(plot_names_ordered)) {
  for (j in 1:length(data_percentages_sorted[[i]])) {
    data_barplot[j,i]<-data_percentages_sorted[[i]][j]
  }
}

coul <- c("#E9F8F6FF", "#CCE5E2FF", "#AFD1CEFF", "#92BEBAFF", "#75AAA6FF", "#579791FF",
          "#3A837DFF", "#1D7069FF", "#005C55FF" )
extended_palette <- colorRampPalette(coul)(12)


# Barplot the data
tiff("/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/figures/AQ_preprocess_barplots_v2.png", units="in", 
     width=11, height=5.2, res=300)
par(mar=c(4,9,2,1), cex=2) #bottom, left, top, right
barplot(data_barplot, col="#92BEBAFF" , border="white", 
        horiz=T, las=1, xlab = "% of teams")

# add most popular options

for (n in 1:length(data_percentages_names_sorted)){
  #label_all<-c()
  label_x<-0
  
  if (n == 5 | n == 3) {
    num_choices = 2; # pipeline options 3 and 5 third label doesn't fit into the image. Reduce to 2
  }else{
    num_choices = 3;
  }
 
      for (m in 1:min(num_choices, length(data_percentages_names_sorted[[n]]))){
        labeltext<-as.character(data_percentages_names_sorted[[n]][m])
        
        #label_all<-append(label_all, labeltext)
        text(1+label_x,-0.67+n*1.2, labeltext, adj=c(0,0), cex = 0.75) #0.6 + 1.2
        label_x<-label_x+data_percentages_sorted[[n]][m] #1 + percentages
      }
  }
  
dev.off()

# Log the event
log_output("Distribution_BarPlot_preproc_and_H1.R", "AQ_preprocess_barplots.png")


