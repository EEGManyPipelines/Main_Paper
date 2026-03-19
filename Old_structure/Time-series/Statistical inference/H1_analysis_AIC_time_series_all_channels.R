rm(list = ls())
Sys.setenv(LANG = "en")
#install.packages("jtools")
library(jtools)
#library(readxl)
library(car)
library(ggplot2)
#library(ggcorrplot)
library(tidyr)
library(MASS)
library(dplyr)
#install.packages("scales")
library(scales)


#Loading the AQ data

h1_data <- read.csv("C:/Users/ecesnait/Desktop/EEGManyPipelines/Data/Big Analysis/all_var_AQ_h1_corrected.csv")

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
data_matched <- h1_data[loc,]

# bring diff wave from volts to microvolts
V_IDs <- c("08de3c5e092173e4", "0ba1c7f1dafc1134", "0bc9ee704db74104", "19e8ad8bf94af489",
           "344dd59ded90cb34", "48e64dc185199502", "628a18bc8a3d36dd", 
           "77fddd91c557626d", "7f3fe2bc79a9d3f9","8559e4d7314e45ec", "8c587a4cbf53865d",
           "90420442f22fa870", "9985e8ae6679b0e2","CIMHPipe", "TheCodeMechanics", "Varuwa",
           "a0cf32754296214f","a25b8419335d2131","aa6aa366e9788967", 
           "b0edd369b6d8f4f1", "bd3077a83b5b16bd", "c0c75576f9cd0b2a", "c577d3cdf78548ce",
           "c91e489c4acd0bf4","da33cf2264c9baa2", "e13e7e07b99d853b", "e146a94b29a41713", 
           "e2d0d90e5cf594ed","e69a83408d1f3811","e72d90a6ff4b5108", "ea6b1d1870708b82",
           "ee8c062e3dc35b1d", "eef1406b3fca3e9c", "f92a1d6a49d0d40a", "ff8bf48d04d11c84") 

diff_wave[diff_wave$ID %in% V_IDs,2] <- diff_wave[diff_wave$ID %in% V_IDs,2] * 10^6

## --------------------
# scale continuous data
## --------------------
# remove data that will not be used in LMs
data_matched <- data_matched[,c(-1,-27: -29)]

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
                             "ans_time_w_end_h1", "ans_time_w_start_h1", "ans_baseline_start","ans_hf_direction","software"), colnames(data_matched)) # 37

data_reduced <- data_matched[,-collinearity_indx] 

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

#Approach 1. Using the step function.
#Checking different models using the step function. 
#loop over channels

sig_names <- data.frame(a=NA, b=NA, c=NA, d=NA)
for (x in 1:64) {
  # remove outliers
  cha_diff_wave <- diff_wave[,x+1]
  lower_bound <-quantile(cha_diff_wave, 0.015, na.rm=T)
  upper_bound <- quantile(cha_diff_wave, 0.985, na.rm=T)
  outlier_ind <- which(cha_diff_wave < lower_bound | cha_diff_wave > upper_bound)
  
  cha_diff_wave <- cha_diff_wave[-outlier_ind] # remove outliers
  data_model_chan <- data_reduced[-outlier_ind,]
  #run a full model first for individual channels
  fullmodel<- lm(cha_diff_wave~ . , data = data_model_chan)
  
  # Stepwise model
  stepwise_model <- stepAIC(fullmodel, direction = "both", trace = T, k = log(nrow(data_model_chan)))
  pvalues <- as.data.frame(summary(stepwise_model)$coefficients[,4])
  indx_sig <- which(pvalues$`summary(stepwise_model)$coefficients[, 4]`<=0.01)
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

# unique(sig_names)
combined <- as.data.frame(apply(sig_names, 1, 
             function(x) paste(x[!is.na(x)], collapse = ", ")))

colnames(combined) <- "var"
classes <- unique(combined)
colnames(classes) <- "var"

# combined$class <- 0
# for (x in 1:nrow(classes)) {
#   indx <- which(combined$var==classes$var[x])
#   combined$class[indx] <- x
# }
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

MyColor<-RColorBrewer::brewer.pal(12, "Set3")

#augment colorscale 
MyColor_extend <- colorRampPalette(MyColor)(13)

## TOPOPLOT FOR ALL TEAMS ##
#tiff("C:/Users/ecesnait/Desktop/EEGManyPipelines/git/EEGManyPipelines-personal/Result forms/figures/topoplot_h3a.png", units="in", width=4, height=3, res=300)
max_scale <- max(myData$value)
myData$class <- combined$var

ggplot(headShape,aes(x,y))+
  geom_path(linewidth = 1.5)+
  geom_point(data = myData,aes(x,y,colour = class),size = 3)+ #note: oob = squish forces everything outside the colour limits to equal nearest colour boundary (i.e. below min colours = min colour)
  geom_line(data = nose,aes(x, y, z = NULL),size = 1.5)+
  scale_colour_manual(values = MyColor_extend)+
  theme_topo()+ theme(legend.text=element_text(size=15), legend.title = element_text(size=15))+
  coord_equal() 
#dev.off()







