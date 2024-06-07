rm(list = ls())

#install.packages("jtools")
library(jtools)
library(readxl)
library(car)
library(ggplot2)

#Loading the data

h1_data <- read.csv("C:/Users/ecesnait/Desktop/EEGManyPipelines/Data/Big Analysis/all_var_AQ_h1.csv")
h1_data <- as.data.frame(h1_data)

## --------------------
# Correct values
## --------------------

# Software
h1_data$software[h1_data$software== 'eeglab_erplab'] <- 'eeglab'
h1_data$software[h1_data$software== 'eeglab_limo'] <- 'eeglab'

# #Fill in empty responses
# # High-pass filter type & direction
# h1_data$ans_hf_type[h1_data$ans_hf_type== ''] <- 'unknown'
# 
# h1_data$ans_hf_direction[h1_data$ans_hf_direction == ''] <- 'unknown'

#filter type
table(h1_data$ans_hf_type)
h1_data$ans_hf_type[h1_data$ans_hf_type == 'IIR'] <- 'hf_iir'

# Segment exclusion criteria
# h1_data$ans_exclusion_criteria_seg[h1_data$ans_exclusion_criteria_seg == ''] <- 'unknown'

#Time window start, end & length
#exchange missing values to the mean of the column
h1_data$ans_time_w_start_h1[is.na(h1_data$ans_time_w_start_h1)] <- mean((h1_data$ans_time_w_start_h1), na.rm = T) 

h1_data$ans_time_w_end_h1[is.na(h1_data$ans_time_w_end_h1)] <- mean((h1_data$ans_time_w_end_h1), na.rm = T) 
h1_data$time_w_length_h1[is.na(h1_data$time_w_length_h1)] <- mean((h1_data$time_w_length_h1), na.rm = T) 
# ICA algorythm
h1_data$ans_ica_algo[h1_data$ans_ica_algo == ''] <- 'unknown'

#Baseline start and stop
h1_data$ans_baseline_start[h1_data$ans_baseline_start == '-200 ms'] <- '-200'
h1_data$ans_baseline_start[h1_data$ans_baseline_start == '-200ms'] <- '-200'

h1_data$ans_baseline_start <- as.numeric(h1_data$ans_baseline_start)
h1_data$ans_baseline_start[is.na(h1_data$ans_baseline_start)] <- mean((h1_data$ans_baseline_start), na.rm = T) 
h1_data$ans_baseline_stop[is.na(h1_data$ans_baseline_stop)] <- mean((h1_data$ans_baseline_stop), na.rm = T)

# Change baseline window length 0 to the mean of the column because 0 is dragging the effect
h1_data$bs_window_length[h1_data$bs_window_length == 0] <- mean(h1_data$bs_window_length[h1_data$bs_window_length != 0], na.rm = T) 


#remove binary response and p-values
data <- h1_data[,c(-1,-27,-28)]
data$ans_ica_algo[data$ans_ica_algo=='runica'] <- 'infomax'

## --------------------
# outliers removal
## --------------------
# 
hf_out <- boxplot(data$hf_cutoff)$out
data$hf_cutoff[which(data$hf_cutoff == hf_out)] <- mean((data$hf_cutoff), na.rm = T) # removed one team that had 3 for a  HP filter

## --------------------
# Load mean N1 difference wave
## --------------------
# 
diff_wave <- read.csv("C:/Users/ecesnait/Desktop/EEGManyPipelines/Data/Big Analysis/n1_amplitudes_all_chan.csv")
AQ_ID <- read.csv("C:/Users/ecesnait/Desktop/EEGManyPipelines/Data/Big Analysis/all_IDs_168.csv")
# Match IDs
ID_diff <- diff_wave[,1]

loc <- match(ID_diff,AQ_ID$Var1, nomatch = 0)
ID_matched <- AQ_ID$Var1[loc]

setdiff(ID_diff,ID_matched)
data_matched <- data[loc,]

#test <- cbind(ID_matched, data_matched)
which(data_matched$software=='R') # one team indicated R in a software, but Python in the host. From the scale of their time-series data, it looks like they used mne to prepro-cess
data_matched$software[data_matched$software=='R'] <- 'mnepython'
indx_mne <- which(data_matched$software=='mnepython')
diff_wave[indx_mne,2:ncol(diff_wave)] <- diff_wave[indx_mne,2:ncol(diff_wave)] * 10^6

#same problem with brainstorm team
indx_brainstorm <- which(data_matched$software=='brainstorm')
diff_wave[indx_brainstorm,2:ncol(diff_wave)] <- diff_wave[indx_brainstorm,2:ncol(diff_wave)] * 10^6

## --------------------
# scale continuous data
## --------------------
# 

continuous <- c(3, 7, 8, 10, 16:18, 23:25)

#normalizing data
for (i in 1:length(continuous)) {
  contin_data_col <- data_matched[,continuous[i]]
  data_matched[,continuous[i]] <- scale(contin_data_col)
  #hist(scale(contin_data_col))
}


#---------------------------------------------------------------------------------------------------------------------------

# exclude variables that have high collinearity >0.6
collinearity_indx <- match(c("topo_region_h1","ans_software_host","mc_method_h1","ans_exclusion_criteria_seg","ans_baseline_stop",
                             "ans_time_w_end_h1", "ans_time_w_start_h1", "ans_baseline_start","ans_hf_direction","software"), colnames(data)) # 37

data_reduced <- data_matched[,-collinearity_indx] 
#inclusing categorical data
#install.packages('ggcorrplot')
library(ggcorrplot)
library(tidyr)
#find the ones above 0.6
all_cor <- model.matrix(~0+., data=data_reduced) %>% 
  cor(use="pairwise.complete.obs")
which(abs(all_cor)>0.6 & abs(all_cor)!=1, arr.ind=TRUE) # 

cor(data_reduced$hf_cutoff, data_reduced$ds_fs)

# remove missing values
which(is.na(data_reduced), arr.ind=TRUE)
data_reduced<-data_reduced[-43,]
diff_wave <- diff_wave[-43,]
#---------------------------------------------------------------------------------------------------------------------------

fullmodel<- lm(diff_wave$CPz~ . , data = data_reduced)
summary(fullmodel)
#Approach 1. Using the step function.
#Checking different models using the step function. 
#loop over channels
library(MASS)

sig_names <- data.frame(a=NA, b=NA, c=NA, d=NA, e=NA)
for (x in 1:64) {
  fullmodel<- lm(diff_wave[,x+1]~ . , data = data_reduced)
  
  # Stepwise model
  stepwise_model <- stepAIC(fullmodel, direction = "both", trace = T, k = log(nrow(data_reduced)))
  pvalues <- as.data.frame(summary(stepwise_model)$coefficients[,4])
  indx_sig <- which(pvalues$`summary(stepwise_model)$coefficients[, 4]`<=0.001)
  if (!any(indx_sig)){
    next
    cat(x)
  }
  
  names_model <- variable.names(stepwise_model)
  if (any(indx_sig==1)) {
    indx_sig <- indx_sig[-1]
  }
  
  if (!any(indx_sig)){
    next
    cat(x)
  }
  sig_names[x,1:length(names_model[indx_sig])] <- names_model[indx_sig]
  
}
unique(sig_names)
combined <- as.data.frame(apply(sig_names, 1, 
             function(x) paste(x[!is.na(x)], collapse = ", ")))
colnames(combined) <- "var"
classes <- unique(combined)
colnames(classes) <- "var"
combined$class <- 0
for (x in 1:nrow(classes)) {
  indx <- which(combined$var==classes$var[x])
  combined$class[indx] <- x
}
# PLOT DATA ON A TOPOPLOT

myData <-read.csv("C:/Users/ecesnait/Desktop/EEGManyPipelines/git/EEGManyPipelines-personal/Matlab/Result forms/Data/res_topoplot_h1.csv")
# add info on the class of different combinations
myData$value <- combined$class

#add x and y locations
myData$radianTheta <- pi/180*myData$theta
myData <- myData %>%
  mutate(x = .$radius*sin(.$radianTheta),
         y = .$radius*cos(.$radianTheta))

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

headShape <- circleFun(c(0, 0), round(max(myData$x)), npoints = 100) # 0
nose <- data.frame(x = c(-0.075,0,.075),y=c(.495,.575,.495))

ggplot(headShape,aes(x,y))+
  geom_path()+
  geom_text(data = myData,
            aes(x, y, label = electrodes))+
  geom_line(data = nose,
            aes(x, y, z = NULL))+
  theme_topo()+
  coord_equal()

#set color
#install.packages("scales")
library(scales)
MyColor<-RColorBrewer::brewer.pal(12, "Set3")
#"#EF7F4FFF","#B6308BFF", "#5901A5FF"))
#jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))

#data
#allData <- topotest %>% left_join(electrodeLocs, by = "electrode")
#singleTimepoint <- filter(allData,Times == 170.90)

## TOPOPLOT FOR ALL TEAMS ##
#tiff("C:/Users/ecesnait/Desktop/EEGManyPipelines/git/EEGManyPipelines-personal/Result forms/figures/topoplot_h3a.png", units="in", width=4, height=3, res=300)
max_scale <- max(myData$value)
myData$class <- combined$var

ggplot(headShape,aes(x,y))+
  geom_path(linewidth = 1.5)+
  geom_point(data = myData,aes(x,y,colour = class),size = 3)+ #note: oob = squish forces everything outside the colour limits to equal nearest colour boundary (i.e. below min colours = min colour)
  geom_line(data = nose,aes(x, y, z = NULL),size = 1.5)+
  theme_topo()+ theme(legend.text=element_text(size=15), legend.title = element_text(size=15))+
  coord_equal() 
#dev.off()


#Barpltos for H
bar <- read.csv("C:/Users/ecesnait/Desktop/EEGManyPipelines/git/EEGManyPipelines-personal/Result forms/Data/results_h3b_overview.csv")
#read.csv("/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/data/results_resubmitted_h1_168.csv", sep=";")

#bar <- bar[1:240,]
bar_u <- bar[match(unique(bar$team_identifier), bar$team_identifier), ]
indx_sig <- which(bar_u$significance == "no?")
bar_u$significance[indx_sig] <- "no"

bar_T <- as.data.frame(table(bar_u$significance))
bar_T <- bar_T[-1,] # 3 teams with no answer - inspect later

tiff("C:/Users/ecesnait/Desktop/EEGManyPipelines/git/EEGManyPipelines-personal/Result forms/figures/h3b_yes_no.png", units="in", width=4, height=5, res=300)

ggplot(data=bar_T, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal() +  scale_x_discrete(name ="")+
  theme(text = element_text(size = 27))
dev.off()

##plot scatter of pvalues
pval <- as.numeric(bar$p_values)
pval_ord <- as.data.frame(pval[order(pval)])
pval_ord$y <- c(1:240)
colnames(pval_ord) <- c("pval", "y")

ggplot(pval_ord, aes(x=pval, y=y)) + 
  geom_point(color="steelblue", size = 3, alpha = 0.2) + xlim(0,0.5)+
  theme_minimal() +  ylim(name ="") +
  theme(text = element_text(size = 20))

t_stand_p <- as.data.frame(table(bar$p_values_stand))
t_stand_p<-t_stand_p[order(t_stand_p$Freq),]
colors <- magma(30)

ggplot(t_stand_p, aes(x="", y=Freq, fill=Var1)) +
  geom_bar(stat="identity", width=1, color="white") + scale_fill_manual(values = colors, name = "p-val")+
  coord_polar("y", start=0) +
  theme_void(base_size = 20) # remove background, grid, numeric labels
#pie(table(bar_u$p_values_stand),cex = 2)





