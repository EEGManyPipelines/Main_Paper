rm(list = ls())

#install.packages("jtools")
library(jtools)
library(readxl)
library(car)
library(ggplot2)

#Loading the data
dataPath <- "/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/data/variables_h3a_0402.xlsx"
h3a_data <- read_excel(dataPath, col_names = TRUE)
# use h1_data frame from another script for completeness
#  read.csv("/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/data/variables_h2a_0801.csv", sep=";")
h3a_data <- as.data.frame(h3a_data)

# add order of steps
order_steps <- read.csv('/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/data/Main Analysis/Script_Order_Sorted.csv')
matched_ID <- match(order_steps$ID, h3a_data$Team)
which(is.na(matched_ID))
order_steps$ID[166] <- "Jack Lab"

match_ID2 <- match(h3a_data$Team,order_steps$ID)
order_steps_matched <- order_steps[match_ID2,]

order_steps_matched$ID == h3a_data$Team

h3a_data <- as.data.frame(c(h3a_data, order_steps_matched[,20:23]*1))

# Remove teams that didn't report the p-value
indx_no_p <- which(is.na(h3a_data$pval)) # 52 teams did not report a p-value
h3a_data <- h3a_data[-indx_no_p,] # 

## --------------------
# Correct values
## --------------------

# Software
h3a_data$software[h3a_data$software== 'eeglab_erplab'] <- 'eeglab'
h3a_data$software[h3a_data$software== 'eeglab_limo'] <- 'eeglab'

#Fill in empty responses
# High-pass filter type & direction
h3a_data$ans_hf_type[h3a_data$ans_hf_type== ''] <- 'unknown'

h3a_data$ans_hf_direction[h3a_data$ans_hf_direction == ''] <- 'unknown'

## --------------------
# outliers removal
## --------------------
# 3hz high pass has been removed beforehand
class(h3a_data$hf_cutoff)
#data$hf_cutoff <- as.numeric(gsub(",", ".", gsub("\\.", "", data$hf_cutoff)))
#hf_out <- boxplot(h2a_data$hf_cutoff)$out
h3a_data$hf_cutoff[is.na(h3a_data$hf_cutoff)] <- mean(h3a_data$hf_cutoff, na.rm = T) # 

#
table(h3a_data$mt_method_h3a)
h3a_data$stat_method_h3a[is.na(h3a_data$stat_method_h3a)] <- 'other'

#time window missing values. Fill with the mean of the column
h3a_data$tw_start_h3a[is.na(h3a_data$tw_start_h3a)] <- mean(h3a_data$tw_start_h3a, na.rm = T) # 
h3a_data$tw_end_h3a[is.na(h3a_data$tw_end_h3a)] <- mean(h3a_data$tw_end_h3a, na.rm = T)

# ICA algorythm
h3a_data$ans_ica_algo[is.na(h3a_data$ans_ica_algo)] <- 'unknown'
h3a_data$ans_ica_algo[h3a_data$ans_ica_algo=='runica'] <- 'infomax'

h3a_data$mt_method_h3a[is.na(h3a_data$mt_method_h3a)] <- 'other'

#Baseline start and stop: use baseline window length instead

h3a_data$bs_window_length[is.na(h3a_data$bs_window_length)] <- mean(h3a_data$bs_window_length, na.rm = T) 

# nr chan too many empty responses - remove

#remove binary response and p-values, as well as responses that are duplicated or can't be used due to too few responses
data <- h3a_data[,c(-1,-14,-15,-17, -18, -26, -27)]

## --------------------
# from p-val to z-val
## --------------------
# 
class(h3a_data$pval)

hist(qnorm(h3a_data$pval))
data$pval <- qnorm(h3a_data$pval)#qnorm(0.975,mean=0,sd=1)

## --------------------
# scale continuous data
## --------------------

continuous <- c(3, 7, 8,13:15)

#normalizing data
for (i in 1:length(continuous)) {
  contin_data_col <- data[,continuous[i]]
  data[,continuous[i]] <- as.numeric(scale(contin_data_col))
  #hist(scale(contin_data_col))
}

## ------------------------------------------------------------------------------------

# exclude variables that have high collinearity >0.6
collinearity_indx <- match(c("ans_software_host", "software","ans_hf_direction"), colnames(data)) # "mt_method_h2a", "ans_hf_direction"

data_reduced <- data[,-collinearity_indx] 
#inclusing categorical data
#install.packages('ggcorrplot')
#library(ggcorrplot)
library(tidyr)
#find the ones above 0.6
all_cor <- model.matrix(~0+., data=data_reduced) %>% 
  cor(use="pairwise.complete.obs")
which(abs(all_cor)>0.8 & abs(all_cor)!=1, arr.ind=TRUE) # 

#---------------------------------------------------------------------------------------------------------------------------
## --------------------
# look if there are any NaN values
## --------------------
# 
empty_row <- which(is.na(data_reduced),arr.ind=TRUE) # 
indx_row <- empty_row[,1]#61 empty rows. Too many
# Caution: remove them from the dataset
data_reduced <- data_reduced[-indx_row,]

#Approach 1. Using the step function.
#Checking different models using the step function. 
fullmodel<- lm(pval~ . , data = data_reduced)
summary(fullmodel)
car::vif(fullmodel)


# Stepwise model
library(MASS)
stepwise_model <- stepAIC(fullmodel, direction = "both", trace = T, k = log(nrow(data_reduced)))
summary(stepwise_model)

# remove the outlier
#all_out <- boxplot(data_reduced$pval)$out
#indx_out <- which(data_reduced$pval==min(all_out))
#data_reduced <- data_reduced[-indx_out,] 


# plot the baseline window length effect on a p-value (although n.s.)
library(ggplot2)

data_reduced$temp_roi_avg_h3a <- as.factor(data_reduced$temp_roi_avg_h3a)
color <- c("#476F84FF", "#A4BED5FF")
tiff("/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/figures/pval_avg_tw_plot_h3a.png", units="in", width=3, height=4, res=100)

ggplot(data=data_reduced, aes(x=temp_roi_avg_h3a, y=pval))+
  geom_boxplot(fill=color)+
  geom_point(alpha = 0.5, position=position_jitter(height=.5, width=.1))+
  theme_minimal()+
  theme(text = element_text(size=18))+
  
  ylab("qnorm(pval)")+
  xlab("temp roi avg")

dev.off()

