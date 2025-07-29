rm(list = ls())

#install.packages("jtools")
library(jtools)
library(readxl)
library(car)
library(ggplot2)

#Loading the data
dataPath <- "/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/data/variables_h2a_0801.xlsx"
h2a_data <- read_excel(dataPath, col_names = TRUE)
# use h1_data frame from another script for completeness
#  read.csv("/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/data/variables_h2a_0801.csv", sep=";")
h2a_data <- as.data.frame(h2a_data)

# add order of steps
order_steps <- read.csv('/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/data/Main Analysis/Script_Order_Sorted.csv')
matched_ID <- match(order_steps$ID, h2a_data$Team)
which(is.na(matched_ID))
order_steps$ID[166] <- "Jack Lab"

match_ID2 <- match(h2a_data$Team,order_steps$ID)
order_steps_matched <- order_steps[match_ID2,]

order_steps_matched$ID == h2a_data$Team

h2a_data <- as.data.frame(c(h2a_data, order_steps_matched[,20:23]*1))

# Remove teams that didn't report the p-value
indx_no_p <- which(is.na(h2a_data$pval)) 
h2a_data <- h2a_data[-indx_no_p,] # 20 teams had no p-value report

# remove absolute 0 values
indx_zero <- which(h2a_data$pval==0)
h2a_data <- h2a_data[-indx_zero,] 

## --------------------
# Correct values
## --------------------

# Software
h2a_data$software[h2a_data$software== 'eeglab_erplab'] <- 'eeglab'
h2a_data$software[h2a_data$software== 'eeglab_limo'] <- 'eeglab'

#Fill in empty responses
# High-pass filter type & direction
h2a_data$ans_hf_type[h2a_data$ans_hf_type== ''] <- 'unknown'

h2a_data$ans_hf_direction[h2a_data$ans_hf_direction == ''] <- 'unknown'

## --------------------
# outliers removal
## --------------------
# 
class(h2a_data$hf_cutoff)
#data$hf_cutoff <- as.numeric(gsub(",", ".", gsub("\\.", "", data$hf_cutoff)))
#hf_out <- boxplot(h2a_data$hf_cutoff)$out
h2a_data$hf_cutoff[is.na(h2a_data$hf_cutoff)] <- mean(h2a_data$hf_cutoff, na.rm = T) # removed one team that had 3 for a  HP filter

#
table(h2a_data$mt_method_h2a)


# ICA algorythm
h2a_data$ica_algo[h2a_data$ica_algo == ''] <- 'unknown'
h2a_data$ica_algo[h2a_data$ica_algo=='runica'] <- 'infomax'

#Baseline start and stop: use baseline window length instead

# Change baseline window length 0 to the mean of the column because 0 is dragging the effect
#h2a_data$bs_window_length <- as.numeric(gsub(",", ".", gsub("\\.", "", h2a_data$bs_window_length)))

h2a_data$bs_window_length[is.na(h2a_data$bs_window_length)] <- mean(h2a_data$bs_window_length, na.rm = T) 

# nr chan empty indeces exchaneg with mean
h2a_data$nr_ch_h2a[is.na(h2a_data$nr_ch_h2a)] <- mean(h2a_data$nr_ch_h2a, na.rm = T)

#remove binary response and p-values
data <- h2a_data[,c(-1,-10,-13,-14,-24,-25,-28)]

## --------------------
# from p-val to z-val
## --------------------
# 
class(h2a_data$pval)

hist(qnorm(h2a_data$pval))
data$pval <- qnorm(h2a_data$pval)#qnorm(0.975,mean=0,sd=1)


## --------------------
# scale continuous data
## --------------------
# 

continuous <- c(3, 7, 8,9, 20)

#normalizing data
for (i in 1:length(continuous)) {
  contin_data_col <- data[,continuous[i]]
  data[,continuous[i]] <- as.numeric(scale(contin_data_col))
  #hist(scale(contin_data_col))
}

## ------------------------------------------------------------------------------------

# exclude variables that have high collinearity >0.6
collinearity_indx <- match(c("topo_region_h2a","ans_software_host", "software",
                             "mt_method_h2a", "ans_hf_direction"), colnames(data)) # 37

data_reduced <- data[,-collinearity_indx] 
#inclusing categorical data
#install.packages('ggcorrplot')
#library(ggcorrplot)
#library(tidyr)
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

table(h2a_data$sig)

# plot the baseline window length effect on a p-value (although n.s.)
library(ggplot2)
ggplot(data=data_reduced, aes(x=bs_window_length, y=pval))+
  geom_point(size=2)+
  geom_smooth(method= "lm")+
  ylab("qnorm(pval)")+
  xlab("baseline wl")+
  ggtitle("H2a")

