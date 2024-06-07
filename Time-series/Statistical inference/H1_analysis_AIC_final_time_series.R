rm(list = ls())

#install.packages("jtools")
library(jtools)
library(readxl)
library(car)
library(ggplot2)

#Loading the data
#dataPath <- "C:\\Users\\ecesnait\\Downloads\\all_var_AQ_h1_(version_3).xlsx"
#data <- read_excel(dataPath, col_names = TRUE)
#head(data)

# use h1_data frame from another script for completeness
h1_data <- read.csv("C:/Users/ecesnait/Desktop/EEGManyPipelines/Data/Big Analysis/all_var_AQ_h1.csv")
h1_data <- as.data.frame(h1_data)

## --------------------
# Correct values
## --------------------

# Software
h1_data$software[h1_data$software== 'eeglab_erplab'] <- 'eeglab'
h1_data$software[h1_data$software== 'eeglab_limo'] <- 'eeglab'

#Fill in empty responses
# High-pass filter type & direction
h1_data$ans_hf_type[h1_data$ans_hf_type== ''] <- 'unknown'

h1_data$ans_hf_direction[h1_data$ans_hf_direction == ''] <- 'unknown'

#filter type
table(h1_data$ans_hf_type)
h1_data$ans_hf_type[h1_data$ans_hf_type == 'IIR'] <- 'hf_iir'

# Segment exclusion criteria
h1_data$ans_exclusion_criteria_seg[h1_data$ans_exclusion_criteria_seg == ''] <- 'unknown'

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
diff_wave <- read.csv("C:/Users/ecesnait/Desktop/EEGManyPipelines/Data/Big Analysis/n1_amplitudes_v2.csv")
AQ_ID <- read.csv("C:/Users/ecesnait/Desktop/EEGManyPipelines/Data/Big Analysis/all_IDs_168.csv")
# Match IDs
ID_diff <- diff_wave[,1]

loc <- match(ID_diff,AQ_ID$Var1, nomatch = 0)
ID_matched <- AQ_ID$Var1[loc]

setdiff(ID_diff,ID_matched)
# which(ID_diff %in% "TheCodeMechanics")
# ID_diff<- ID_diff[-82]
# diff_wave <- diff_wave[-82,]

data_matched <- data[loc,]

#test <- cbind(ID_matched, data_matched)
which(data_matched$software=='R') # one team indicated R in a software, but Python in the host. From the scale of their time-series data, it looks like they used mne to prepro-cess
data_matched$software[data_matched$software=='R'] <- 'mnepython'
indx_mne <- which(data_matched$software=='mnepython')
diff_wave$cpz[indx_mne] <- diff_wave$cpz[indx_mne] * 10^6

#same problem with brainstorm team
indx_brainstorm <- which(data_matched$software=='brainstorm')
diff_wave$cpz[indx_brainstorm] <- diff_wave$cpz[indx_brainstorm] * 10^6

which(data_matched$software=='custom')
#visualize it to see if everyone is now on the same scale

library(ggplot2)

# Top Right: Set a different color for each group
ggplot(diff_wave, aes(x=data_matched$software, y=diff_wave$cpz, fill=data_matched$software)) + 
  geom_boxplot(alpha=0.3) +
  theme(legend.position="none",axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

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


# --------------------
#combine multiple comparison and statistical test columns (avoid the cluster problem)
# --------------------
# #
# table(data$mc_method_h1)
# # add correction method next to the stat model
# for (i in 1:nrow(data)) {
#   if (data$mc_method_h1[i] == 'bonferroni') {
#     data$stat_method_h1[i] <- paste0(data$stat_method_h1[i],'_bonf')
#   } else if (data$mc_method_h1[i] == 'bhfdr') {
#     data$stat_method_h1[i] <- paste0(data$stat_method_h1[i],'_bhfdr')
#   } else if (data$mc_method_h1[i] =='holm-bonferroni'){
#     data$stat_method_h1[i] <- paste0(data$stat_method_h1[i],'_hbonf')
#   }else if (data$mc_method_h1[i] =='rft'){
#     data$stat_method_h1[i] <- paste0(data$stat_method_h1[i],'_rft')
#   }
# }
# 
# # Now i can remove the multiple comparisons questions from the table
# data <- data[,c(-12)]

## ------------------------------------------------------------------------------------

#Converting the categorical variables in the data to factors
# data$software <- as.factor(data$software)
# #data$hf_cutoff <- as.factor(data$hf_cutoff) # not a categorical variable?
# data$ans_hf_type <- as.factor(data$ans_hf_type)
# data$ans_hf_direction <- as.factor(data$ans_hf_direction)
# data$reref <- as.factor(data$reref)
# data$topo_region_h1 <- as.factor(data$topo_region_h1)
# data$ans_exclusion_criteria_seg <- as.factor(data$ans_exclusion_criteria_seg)
# data$mc_method_h1 <- as.factor(data$mc_method_h1)
# data$stat_method_h1 <- as.factor(data$stat_method_h1)
# data$ans_spa_roi_avg_h1 <- as.factor(data$ans_spa_roi_avg_h1)
# data$ans_mt_h1 <- as.factor(data$ans_mt_h1)
# data$ans_temp_roi_avg_h1 <- as.factor(data$ans_temp_roi_avg_h1)
# data$ans_ica_algo <- as.factor(data$ans_ica_algo)
# data$ans_bad_comp_sel_visual <- as.factor(data$ans_bad_comp_sel_visual)
# data$ans_bad_comp_sel_plugin <- as.factor(data$ans_bad_comp_sel_plugin)

#---------------------------------------------------------------------------------------------------------------------------
# #remove dummy variables that have too few observations
# #install.packages('fastDummies')
# library(fastDummies)
# dummy_data <- dummy_cols(data, remove_selected_columns = TRUE)
# #remove columns with few observations
# n=1
# columns_exclude <- c()
# for (i in 12:length(dummy_data)) {
#   if (sum(dummy_data[,i]) < 6) {
#     columns_exclude[n] = i
#     n = n+1
#   }
# }
# names <- colnames(dummy_data)
# names[columns_exclude]
# dummy_data <- dummy_data[,-columns_exclude]

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

# Visualizing the correlation matrix
image(all_cor, main = "Correlation Matrix", col = colorRampPalette(c("blue", "white", "red"))(20))

# remove missing values
which(is.na(data_reduced), arr.ind=TRUE)
data_reduced<-data_reduced[-42,]
diff_wave <- diff_wave[-42,]
#---------------------------------------------------------------------------------------------------------------------------

#Approach 1. Using the step function.
#Checking different models using the step function. 
fullmodel<- lm(diff_wave$cpz~ . , data = data_reduced)
summary(fullmodel)
car::vif(fullmodel)

# Stepwise model
library(MASS)
stepwise_model <- stepAIC(fullmodel, direction = "both", trace = T, k = log(nrow(data_reduced)))
summary(stepwise_model)

all_aic_values = data.frame(stepwise_model$anova$Step,stepwise_model$anova$AIC)

#The factors that had the most effect on the model are: ans_mt_h1,bs_window_start
#So I decided to use those along with their interaction effect

winmodel <- lm(diff_wave$cpz~ reref + ans_bad_comp_sel_plugin, data=data_reduced)
summary(winmodel)

summ(winmodel, confint = TRUE, digits = 4) # nicer vizualisation

#create scatterplot
# split based on ref
plot_data_ref <- as.data.frame(data_reduced$reref)
plot_data_ref$group<- 0
plot_data_ref$group[data_reduced$reref=="avg"] <- 1
plot_data_ref$group[data_reduced$reref=="mastoid"] <- 2
plot_data_ref$group[data_reduced$reref=="original"] <- 3


#add fitted regression line to scatterplot
fit <-  lm(pval ~ bs_window_length, data=data_reduced)

#create scatterplot
plot(pval ~ bs_window_length, data=data_reduced)
abline(fit)
summ(fit,confint = TRUE, digits = 4)

#Interaction
ggplot(data=data_reduced, aes(x=bs_window_length, y=pval, group=ans_mt_h1))+
  geom_point(size=2, aes(color=ans_mt_h1))+
  geom_smooth(method= "lm")+
  ylab("qnorm(pval)")+
  xlab("baseline start")+
  ggtitle("Interaction effect")

# with original values
h1_data$ans_mt_h1 <- as.factor(h1_data$ans_mt_h1)
h1_data <- h1_data[!is.na(h1_data$ans_mt_h1),]

table(h1_data$mc_method_h1)
h1_data$mc_method_h1[!h1_data$mc_method_h1 == "permutation"] <- "not_prm"

tiff("C:/Users/ecesnait/Desktop/EEGManyPipelines/Figures/h1_interaction_baseline_mc.png", units="in", width=7, height=4.5, res=300)

ggplot(data=h1_data, aes(x=bs_window_length, y=qnorm(pval), group=ans_mt_h1 ))+
  geom_point(size=4, aes(color=ans_mt_h1, shape = mc_method_h1), alpha = 0.5)+
  geom_smooth(method= "lm")+
  scale_shape_manual(values = c(16,17)) +
  scale_color_manual(values = c("#00AFBB", "#E7B800"))+
  theme_minimal()+
  ylab("qnorm(pval)")+
  xlab("baseline length(ms)")+
  ggtitle("Interaction effect")+ theme(text = element_text(size=15))
dev.off()

# baseline window without the mt
ggplot(data=h1_data, aes(x=bs_window_length, y=qnorm(pval) ))+
  geom_point(size=2)+
  geom_smooth(method= "lm")+
  ylab("qnorm(pval)")+
  xlab("baseline length(ms)")+
  ggtitle("Interaction effect")


## Check assumptions ##

par(mfrow = c(2, 2))
plot(interactionmodel)

