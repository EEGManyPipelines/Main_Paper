rm(list = ls())
library(tidyverse)
library(broom)
library(readxl)

h1_data <- read.csv("C:/Users/ecesnait/Desktop/EEGManyPipelines/Data/Big Analysis/all_var_AQ_h1.csv")
h1_data <- as.data.frame(h1_data)

# --------------------
# Correct values
# --------------------

# Software
table(h1_data$software)
h1_data$software[h1_data$software== 'eeglab_erplab'] <- 'eeglab'
h1_data$software[h1_data$software== 'eeglab_limo'] <- 'eeglab'

#Fill in empty responses
# High-pass filter type & direction
table(h1_data$ans_hf_type)
h1_data$ans_hf_type[h1_data$ans_hf_type== ''] <- 'unknown'

table(h1_data$ans_hf_direction) # has almost mo variability - remove later
h1_data$ans_hf_direction[h1_data$ans_hf_direction == ''] <- 'unknown'

# Segment exclusion criteria
table(h1_data$ans_exclusion_criteria_seg)
h1_data$ans_exclusion_criteria_seg[h1_data$ans_exclusion_criteria_seg == ''] <- 'unknown'

#Time window start, end & length
table(h1_data$ans_time_w_start_h1)
#exchange missing values to the mean of the column
h1_data$ans_time_w_start_h1[is.na(h1_data$ans_time_w_start_h1)] <- mean((h1_data$ans_time_w_start_h1), na.rm = T) 

h1_data$ans_time_w_end_h1[is.na(h1_data$ans_time_w_end_h1)] <- mean((h1_data$ans_time_w_end_h1), na.rm = T) 

h1_data$time_w_length_h1[is.na(h1_data$time_w_length_h1)] <- mean((h1_data$time_w_length_h1), na.rm = T) 
 # ICA algorythm
table(h1_data$ans_ica_algo)
h1_data$ans_ica_algo[h1_data$ans_ica_algo == ''] <- 'unknown'

#Baseline start and stop
h1_data$ans_baseline_start[h1_data$ans_baseline_start == '-200 ms'] <- '-200'
h1_data$ans_baseline_start[h1_data$ans_baseline_start == '-200ms'] <- '-200'

h1_data$ans_baseline_start <- as.numeric(h1_data$ans_baseline_start)

h1_data$ans_baseline_start[is.na(h1_data$ans_baseline_start)] <- mean((h1_data$ans_baseline_start), na.rm = T) 

h1_data$ans_baseline_stop[is.na(h1_data$ans_baseline_stop)] <- mean((h1_data$ans_baseline_stop), na.rm = T) 

## --------------------
## Prepare data for regression analysis
## --------------------

# high collinearity problem removal
h1_data <- h1_data[-which(h1_data$mc_method_h1=='rft'),] # 3 teams removed

# too few samples that give Nan results in our models:
# ans_software_hostBESA Research 7.1  , ans_software_hostPython 3.8.10  , ans_hf_directionfwr_bwr, ans_ica_algoother
h1_data$ans_software_host[h1_data$ans_software_host == 'BESA Research 7.1'] <- 'unknown'
h1_data$ans_software_host[h1_data$ans_software_host == 'Python 3.8.10'] <- 'unknown'
h1_data$ans_software_host[h1_data$ans_software_host == 'R 4.1.3'] <- 'unknown'
h1_data$ans_hf_direction[h1_data$ans_hf_direction == 'fwr_bwr'] <- 'unknown'
h1_data$ans_ica_algo[h1_data$ans_ica_algo == 'other'] <- 'unknown'


# outliers removal
hf_out <- boxplot(h1_data$hf_cutoff)$out
h1_data$hf_cutoff[which(h1_data$hf_cutoff == hf_out)] <- mean((h1_data$hf_cutoff), na.rm = T) # removed one team that had 3 for a  HP filter

# cut data into dependent and independent variables

dependent_h1 <- h1_data[,c(27,28)] # binary H1 outcome and pvalues
h1_data <- h1_data[,c(2:26)] #delete the row numbers and dependent variables

# look if there are any NaN values
which(is.na(h1_data),arr.ind=TRUE) # row 157 column 15 is NA (Q: if team corrected for multiple comparisons 1 and 0)

# Caution: remove them from the dataset
dependent_h1 <- dependent_h1[-157,]
h1_data <- h1_data[-157,]

# Select only numeric predictors
mynumericdata <- h1_data %>%
  dplyr::select_if(is.numeric) 

shapiro.test(mynumericdata$hf_cutoff)#no data is normal

# ---------------
# Z-score p values and normalize data
# ---------------

hist(qnorm(dependent_h1$pval))
pval_zscore <- qnorm(dependent_h1$pval)#qnorm(0.975,mean=0,sd=1)

indx_inf <- which(!is.finite(pval_zscore))
pval_zscore <- pval_zscore[-indx_inf] # remove the infinite value due to a mistakenly assigned 0 to pval it also takeS NAN values
h1_data <- h1_data[-indx_inf,] # remove those rows from independent variables too

continuous <- c(3, 7, 8, 10, 16:18, 23:25)

#normalizing data
for (i in 1:length(continuous)) {
  contin_data_col <- h1_data[,continuous[i]]
  h1_data[,continuous[i]] <- scale(contin_data_col)
}

# -------------------
# Build models
# -------------------
#h1_data_reduced1 <- h1_data[,c(-16,-17,-23,-24)]
#h1_data_reduced2 <- h1_data[,c(-18,-25)]

#add pval
#h1_data_reduced1$pval <- pval_zscore
#h1_data_reduced2$pval <- pval_zscore
h1_data$pval <- pval_zscore

# models including full data and but no interactions

#model_complete_1 <- lm(pval ~ ., data = h1_data_reduced1)
#model_complete_2 <- lm(pval ~ ., data = h1_data_reduced2)
model_complete <- glm(pval ~ ., data = h1_data)


# Summarize the model: although data frames are similar, they produce completely different results
summary(model_complete)

#Software
test_soft <- h1_data
test_soft$software <- as.factor(test_soft$software)
test_soft <- within(test_soft, software <- relevel(software, ref = 'eeglab'))
model_soft <- glm(pval ~ software, data = test_soft)
summary(model_soft)

# reduced models including one predictor
# For data including time window and baseline length but not start and end of those windows (models range from 1 to 21)

models_list_single_1 = list()
for (g in 1:ncol(h1_data[,-1])){
  
  reduced_1 = glm(pval ~ h1_data[,g], data = h1_data)
  models_list_single_1 = append(models_list_single_1, list(reduced_1))
  
}

# For data including time window and baseline start and end but not length (models range from 22 to 25)

#models_list_single_2 = list()
#n_col = c(16,17,22,23)
#for (g in 1:4){
  
#  reduced_2 = lm(pval ~ h1_data_reduced2[,n_col[g]], data = h1_data_reduced2)
#  models_list_single_2 = append(models_list_single_2, list(reduced_2))
#}

# -------------------
# Compare models using AIC
# ----------------

#install.packages("AICcmodavg")
#install.packages("pbapply")
library(AICcmodavg)

#Put all models into the list:
#First try
#Models 1-21 are single variable prediction models with time window and baseline length
#Models 22-25 are single variable prediction models with the start and end of time window and baseline 
#Models 26-27 are complete models that include all of the variables from previous models
#models_list_full <- c(models_list_single_1, models_list_single_2, list(model_complete_1), list(model_complete_2))

#Second try
#Models 1-25 are from all single variables
#Model 26 is the complete one
models_list_full2 <- c(models_list_single_1, list(model_complete))

# Run AIC
aictab(cand.set = models_list_full2)
#Interpretation:
#K: The number of parameters in the model. The default K is 2, so a model with one parameter will have a K of 2 + 1 = 3.
#AICc: The information score of the model (the lower-case ‘c’ indicates that the value has been calculated from the AIC 
#test corrected for small sample sizes). The smaller the AIC value, the better the model fit.
#Delta_AICc: The difference in AIC score between the best model and the model being compared: In our data it looks like 
#the first two models are equally good (model 15 and 23).
#AICcWt: AICc weight, which is the proportion of the total amount of predictive power provided by the full set of models 
#contained in the model being assessed. In this case, the top model contains 23% of the total explanation that can be found 
#in the full set of models.
# Cum.Wt: The sum of the AICc weights. Here the top two models contain 100% of the cumulative AICc weight.

#fit the best model acording to ACI
#MT
model_mt <- glm(pval ~ ans_mt_h1, data = h1_data)
summary(model_mt)

#Scatterplot
ggplot(h1_data, aes(x=pval, y=ans_mt_h1)) +
  geom_point() 

#Baseline start
model_bs <- glm(pval ~ ans_baseline_start, data = h1_data)
summary(model_bs)

#Scatterplot with scaling
ggplot(h1_data, aes(x=pval, y=ans_baseline_start)) +
  geom_point() +
  geom_smooth(method=lm, se=FALSE)

#Scatterplot without scaling

h1_data <- read.csv("C:/Users/ecesnait/Desktop/EEGManyPipelines/Data/Big Analysis/all_var_AQ_h1.csv")
h1_data <- as.data.frame(h1_data)
#Baseline start and stop
h1_data$ans_baseline_start[h1_data$ans_baseline_start == '-200 ms'] <- '-200'
h1_data$ans_baseline_start[h1_data$ans_baseline_start == '-200ms'] <- '-200'
h1_data$ans_baseline_start <- as.numeric(h1_data$ans_baseline_start)
h1_data$ans_baseline_start[is.na(h1_data$ans_baseline_start)] <- mean((h1_data$ans_baseline_start), na.rm = T) 
h1_data$ans_baseline_stop[is.na(h1_data$ans_baseline_stop)] <- mean((h1_data$ans_baseline_stop), na.rm = T) 

ggplot(h1_data, aes(x=qnorm(pval), y=ans_baseline_start)) +
  geom_point() +
  geom_smooth(method=lm, se=FALSE)



#How many corrected for multiple comparissons
sum(h1_data$ans_mt_h1, na.rm = T)

#scatterplot
ggplot(h1_data, aes(x=pval, y=ans_mt_h1)) +
  geom_point() 

# Find team ID's that corrected for MT
IDs <- read.csv("C:/Users/ecesnait/Desktop/EEGManyPipelines/Data/Big Analysis/all_IDs_168.csv")
indx_ID_mt <- which(h1_data$ans_mt_h1==T,arr.ind=TRUE)
IDs[indx_ID_mt,]

#scatterplot of how many teams that did cluster statistics said that it accounted for multiple comparisons
table(h1_data$stat_method_h1[indx_ID_mt])
indx_no_mt <- which(h1_data$ans_mt_h1==F,arr.ind=TRUE)
table(h1_data$stat_method_h1[indx_no_mt])

#those that did cluster statistics, which kind of mc method they report
indx_clust <- which(h1_data$stat_method_h1=='cluster',arr.ind=TRUE)
table(h1_data$mc_method_h1[indx_clust])

#Model for cluster stat as predictor for pval
model_stat <- glm(pval ~ stat_method_h1, data = h1_data)
summary(model_stat)

#check those that did smth else than cluster stat - did they report corrected or not pval?
table(h1_data$mc_method_h1[indx_ID_mt])
indx_bhfdr <- which(h1_data$mc_method_h1=='bhfdr',arr.ind=TRUE)
IDs[indx_bhfdr,]

table(h1_data$mc_method_h1[indx_ID_mt])
table(h1_data$stat_method_h1)


#------------------------------------------------------------------------------------------------------------------------------------
# Old part of the code

library(hrbrthemes)
library(viridis)
data %>%
  ggplot( aes(x=mc_method_h1, y=pval, fill=mc_method_h1)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(data = data,color="black", size=0.4, alpha=0.9) +
  theme(legend.position="none", text = element_text(size=20))+
  labs(y= "z-scored p-val", x = "multiple comparissons")

data_org <- read_excel("/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/git/EEGManyPipelines-personal/Matlab/Big Analysis/initial_variables_h1_pval.xlsx")
data_org <- as.data.frame(data_org)
mc_none <- data_org[data_org$mc_method_h1=='none',]
boxplot(mc_none$pval) # non-corrected pvalues on average are bigger (less significant). 
#The only exolanation of this could be that we correct for pvalues only if they are already verz small and we know it will survive multiple comparissons.
#if the pval is close to significcance level (high), it's less likely to correct it?

#check pvalues for true/false h0 testing
# Boxplot basic
data_org %>%
  ggplot( aes(x=result_h1, y=pval, fill=result_h1)) +
  geom_boxplot() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) 

#zscored
data_org %>%
  ggplot( aes(x=result_h1, y=qnorm(pval), fill=result_h1)) +
  geom_boxplot() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) 
