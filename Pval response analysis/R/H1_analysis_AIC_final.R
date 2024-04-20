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


#remove binary response
data <- h1_data[,c(-1,-27)]
data$ans_ica_algo[data$ans_ica_algo=='runica'] <- 'infomax'

## --------------------
# outliers removal
## --------------------
# 
hf_out <- boxplot(data$hf_cutoff)$out
data$hf_cutoff[which(data$hf_cutoff == hf_out)] <- mean((data$hf_cutoff), na.rm = T) # removed one team that had 3 for a  HP filter

## --------------------
# from p-val to z-val
## --------------------
# 
hist(qnorm(data$pval))
data$pval <- qnorm(data$pval)#qnorm(0.975,mean=0,sd=1)

## --------------------
# look if there are any NaN values
## --------------------
# 
empty_row <- which(is.na(data),arr.ind=TRUE) # 
indx_row <- empty_row[,1]
# Caution: remove them from the dataset
data <- data[-indx_row,]


## --------------------
# scale continuous data
## --------------------
# 

continuous <- c(3, 7, 8, 10, 16:18, 23:25)

#normalizing data
for (i in 1:length(continuous)) {
  contin_data_col <- data[,continuous[i]]
  data[,continuous[i]] <- scale(contin_data_col)
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
data$software <- as.factor(data$software)
#data$hf_cutoff <- as.factor(data$hf_cutoff) # not a categorical variable?
data$ans_hf_type <- as.factor(data$ans_hf_type)
data$ans_hf_direction <- as.factor(data$ans_hf_direction)
data$reref <- as.factor(data$reref)
data$topo_region_h1 <- as.factor(data$topo_region_h1)
data$ans_exclusion_criteria_seg <- as.factor(data$ans_exclusion_criteria_seg)
data$mc_method_h1 <- as.factor(data$mc_method_h1)
data$stat_method_h1 <- as.factor(data$stat_method_h1)
data$ans_spa_roi_avg_h1 <- as.factor(data$ans_spa_roi_avg_h1)
data$ans_mt_h1 <- as.factor(data$ans_mt_h1)
data$ans_temp_roi_avg_h1 <- as.factor(data$ans_temp_roi_avg_h1)
data$ans_ica_algo <- as.factor(data$ans_ica_algo)
data$ans_bad_comp_sel_visual <- as.factor(data$ans_bad_comp_sel_visual)
data$ans_bad_comp_sel_plugin <- as.factor(data$ans_bad_comp_sel_plugin)

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
collinearity_indx <- match(c("topo_region_h1","ans_software_host","mc_method_h1",
                             "ans_time_w_end_h1", "ans_time_w_start_h1", "ans_baseline_start","ans_hf_direction","software"), colnames(data)) # 37

data_reduced <- data[,-collinearity_indx] 
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

#---------------------------------------------------------------------------------------------------------------------------

#Approach 1. Using the step function.
#Checking different models using the step function. 
fullmodel<- lm(pval~ . , data = data_reduced)
summary(fullmodel)
car::vif(fullmodel)


# Stepwise model
library(MASS)
stepwise_model <- stepAIC(fullmodel, direction = "both", trace = T, k = log(nrow(data_reduced)))
summary(stepwise_model)

all_aic_values = data.frame(stepwise_model$anova$Step,stepwise_model$anova$AIC)

#The factors that had the most effect on the model are: ans_mt_h1,bs_window_start
#So I decided to use those along with their interaction effect

winmodel <- lm(pval~ bs_window_length+ans_mt_h1+bs_window_length*ans_mt_h1, data=data_reduced)

winmodel2 <- lm(pval~nr_chan_h1 + ans_mt_h1 + bs_window_length + nr_chan_h1*ans_mt_h1+ bs_window_length*ans_mt_h1,data=data_reduced)

summary(winmodel2)
summ(winmodel, confint = TRUE, digits = 4) # nicer vizualisation

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

#Approach 2. Manually selecting the variables to include in the model
#Setting the formulas for the models that we would like to compare
#Starting off with the full model:
formula1 = z_value ~ software + hf_cutoff + ans_hf_type + ans_hf_direction +
  reref + ds_fs + subj_excluded + topo_region_h1 + nr_chan_h1 + 
  ans_exclusion_criteria_seg + mc_method_h1 + ans_spa_roi_avg_h1 + ans_mt_h1 +
  time_w_length_h1 + ans_temp_roi_avg_h1 + ans_ica_algo + ans_bad_comp_sel_plugin +
  ans_bad_comp_sel_visual + bs_window_length

model1 = lm(formula1, data=data)
summary(model1)
AIC(model1)
################################
formula2 = z_value ~ software + hf_cutoff + ans_hf_type + ans_hf_direction +
  reref + ds_fs + subj_excluded + topo_region_h1 + nr_chan_h1 + 
  ans_exclusion_criteria_seg + mc_method_h1 + ans_spa_roi_avg_h1 + ans_mt_h1 +
  time_w_length_h1 + ans_temp_roi_avg_h1 + ans_ica_algo + ans_bad_comp_sel_plugin +
  ans_bad_comp_sel_visual

model2 = lm(formula2, data=data)
summary(model2)
AIC(model2)
################################
formula3 = z_value ~ software + hf_cutoff + ans_hf_type + ans_hf_direction +
  reref + ds_fs + subj_excluded + topo_region_h1 + nr_chan_h1 + 
  ans_exclusion_criteria_seg + mc_method_h1 + ans_spa_roi_avg_h1 + ans_mt_h1 +
  time_w_length_h1 + ans_temp_roi_avg_h1 + ans_ica_algo + ans_bad_comp_sel_plugin + bs_window_length

model3 = lm(formula3, data=data)
summary(model3)
AIC(model3)
################################
formula4 = z_value ~ software + hf_cutoff + ans_hf_type + ans_hf_direction +
  reref + ds_fs + subj_excluded + topo_region_h1 + nr_chan_h1 + 
  ans_exclusion_criteria_seg + mc_method_h1 + ans_spa_roi_avg_h1 + ans_mt_h1 +
  time_w_length_h1 + ans_temp_roi_avg_h1 + ans_ica_algo + bs_window_length

model4 = lm(formula4, data=data)
summary(model4)
AIC(model4)
################################
formula5 = z_value ~ software + hf_cutoff + ans_hf_type + ans_hf_direction +
  reref + ds_fs + subj_excluded + topo_region_h1 + nr_chan_h1 + 
  ans_exclusion_criteria_seg + mc_method_h1 + ans_spa_roi_avg_h1 + ans_mt_h1 +
  time_w_length_h1 + ans_temp_roi_avg_h1 + bs_window_length

model5 = lm(formula5, data=data)
summary(model5)
AIC(model5)
################################
formula6 = z_value ~ software + hf_cutoff + ans_hf_type + ans_hf_direction +
  reref + ds_fs + subj_excluded + topo_region_h1 + nr_chan_h1 + 
  ans_exclusion_criteria_seg + mc_method_h1 + ans_spa_roi_avg_h1 + ans_mt_h1 +
  time_w_length_h1 + ans_ica_algo + bs_window_length

model6 = lm(formula6, data=data)
summary(model6)
AIC(model6)
################################
formula7 = z_value ~ software + hf_cutoff + ans_hf_type + ans_hf_direction +
  reref + ds_fs + subj_excluded + topo_region_h1 + nr_chan_h1 + 
  ans_exclusion_criteria_seg + mc_method_h1 + ans_spa_roi_avg_h1 + ans_mt_h1 +
  ans_temp_roi_avg_h1 + ans_ica_algo + bs_window_length

model7 = lm(formula7, data=data)
summary(model7)
AIC(model7)
################################
formula8 = z_value ~ software + hf_cutoff + ans_hf_type + ans_hf_direction +
  reref + ds_fs + subj_excluded + topo_region_h1 + nr_chan_h1 + 
  ans_exclusion_criteria_seg + mc_method_h1 + ans_spa_roi_avg_h1 + 
  ans_temp_roi_avg_h1 + ans_ica_algo + bs_window_length

model8 = lm(formula8, data=data)
summary(model8)
AIC(model8)
################################
formula9 = z_value ~ software + hf_cutoff + ans_hf_type + ans_hf_direction +
  reref + ds_fs + subj_excluded + topo_region_h1 + nr_chan_h1 + 
  ans_exclusion_criteria_seg + mc_method_h1  + ans_mt_h1 +
  ans_temp_roi_avg_h1 + ans_ica_algo + bs_window_length

model9 = lm(formula9, data=data)
summary(model9)
AIC(model9)
################################
formula10 = z_value ~ software + hf_cutoff + ans_hf_type + ans_hf_direction +
  reref + ds_fs + subj_excluded + topo_region_h1 + nr_chan_h1 + 
  ans_exclusion_criteria_seg  + ans_spa_roi_avg_h1 + ans_mt_h1 +
  ans_temp_roi_avg_h1 + ans_ica_algo + bs_window_length

model10 = lm(formula10, data=data)
summary(model10)
AIC(model10)
################################
formula11 = z_value ~ software + hf_cutoff + ans_hf_type + ans_hf_direction +
  reref + ds_fs + subj_excluded + topo_region_h1 + nr_chan_h1 +
  mc_method_h1 + ans_spa_roi_avg_h1 + ans_mt_h1 +
  ans_temp_roi_avg_h1 + ans_ica_algo + bs_window_length

model11 = lm(formula11, data=data)
summary(model11)
AIC(model11)
################################
formula12 = z_value ~ software + hf_cutoff + ans_hf_type + ans_hf_direction +
  reref + ds_fs + subj_excluded + topo_region_h1 + 
  ans_exclusion_criteria_seg + mc_method_h1 + ans_spa_roi_avg_h1 + ans_mt_h1 +
  ans_temp_roi_avg_h1 + ans_ica_algo + bs_window_length

model12 = lm(formula12, data=data)
summary(model12)
AIC(model12)
################################
formula13 = z_value ~ software + hf_cutoff + ans_hf_type + ans_hf_direction +
  reref + ds_fs + subj_excluded +  
  ans_exclusion_criteria_seg + mc_method_h1 + ans_spa_roi_avg_h1 + ans_mt_h1 +
  ans_temp_roi_avg_h1 + ans_ica_algo + bs_window_length

model13 = lm(formula13, data=data)
summary(model13)
AIC(model13)
################################
formula14 = z_value ~ software + hf_cutoff + ans_hf_type + ans_hf_direction +
  reref + ds_fs + topo_region_h1 + 
  ans_exclusion_criteria_seg + mc_method_h1 + ans_spa_roi_avg_h1 + ans_mt_h1 +
  ans_temp_roi_avg_h1 + ans_ica_algo + bs_window_length

model14 = lm(formula14, data=data)
summary(model14)
AIC(model14)
################################
formula15 = z_value ~ software + hf_cutoff + ans_hf_type + ans_hf_direction +
  reref  + subj_excluded + topo_region_h1 + 
  ans_exclusion_criteria_seg + mc_method_h1 + ans_spa_roi_avg_h1 + ans_mt_h1 +
  ans_temp_roi_avg_h1 + ans_ica_algo + bs_window_length

model15 = lm(formula15, data=data)
summary(model15)
AIC(model15)
################################
formula16 = z_value ~ software + hf_cutoff + ans_hf_type + ans_hf_direction +
  subj_excluded + topo_region_h1 + 
  ans_exclusion_criteria_seg + mc_method_h1 + ans_spa_roi_avg_h1 + ans_mt_h1 +
  ans_temp_roi_avg_h1 + ans_ica_algo + bs_window_length

model16 = lm(formula16, data=data)
summary(model16)
AIC(model16)
################################
formula17 = z_value ~ software + hf_cutoff + ans_hf_type +
  reref  + subj_excluded + topo_region_h1 + 
  ans_exclusion_criteria_seg + mc_method_h1 + ans_spa_roi_avg_h1 + ans_mt_h1 +
  ans_temp_roi_avg_h1 + ans_ica_algo + bs_window_length

model17 = lm(formula17, data=data)
summary(model17)
AIC(model17)
################################
formula18 = z_value ~ software + hf_cutoff +
  reref  + subj_excluded + topo_region_h1 + 
  ans_exclusion_criteria_seg + mc_method_h1 + ans_spa_roi_avg_h1 + ans_mt_h1 +
  ans_temp_roi_avg_h1 + ans_ica_algo + bs_window_length

model18 = lm(formula18, data=data)
summary(model18)
AIC(model18)
################################
formula19 = z_value ~ software + 
  reref  + subj_excluded + topo_region_h1 + 
  ans_exclusion_criteria_seg + mc_method_h1 + ans_spa_roi_avg_h1 + ans_mt_h1 +
  ans_temp_roi_avg_h1 + ans_ica_algo + bs_window_length

model19 = lm(formula19, data=data)
summary(model19)
AIC(model19)
################################
formula20 = z_value ~  hf_cutoff +
  reref  + subj_excluded + topo_region_h1 + 
  ans_exclusion_criteria_seg + mc_method_h1 + ans_spa_roi_avg_h1 + ans_mt_h1 +
  ans_temp_roi_avg_h1 + ans_ica_algo + bs_window_length

model20 = lm(formula20, data=data)
summary(model20)
AIC(model20)
#Adding all the interaction terms to the formula
#Creating a data frame with only the remaining factors
selected = c("z_value","hf_cutoff", "reref", "subj_excluded", "topo_region_h1",
            "ans_exclusion_criteria_seg", "mc_method_h1", "ans_spa_roi_avg_h1",
            "ans_mt_h1", "ans_temp_roi_avg_h1", "ans_ica_algo", "bs_window_length")
datasub = data[selected]
#Creating the model with the interactions. .^2 just includes all the possible pairwise
#interactions between the independent variables
model21 = lm(z_value ~ . + .^2, data = datasub)
summary(model21)
AIC(model21)
#There are many interaction terms, resulting in no degrees of freedom to estimate the model
#Adding interaction terms manually
formula22 = z_value ~ . + hf_cutoff*reref

model22 = lm(formula22, data=data)
summary(model22)
AIC(model22)
################################
formula23 = z_value ~ . + hf_cutoff*reref + hf_cutoff*subj_excluded

model23 = lm(formula23, data=data)
summary(model23)
AIC(model23)
################################
formula24 = z_value ~ . + hf_cutoff*reref + hf_cutoff*subj_excluded +
  hf_cutoff*subj_excluded

model24 = lm(formula24, data=data)
summary(model24)
AIC(model24)
################################
formula25 = z_value ~ . + hf_cutoff*reref + hf_cutoff*subj_excluded +
  hf_cutoff*topo_region_h1

model25 = lm(formula25, data=data)
summary(model25)
AIC(model25)
################################

#Approach number 3. Using a loop and starting off with an empty model then iterativly
#checking if adding the terms (or the interaction of two terms) would improve the AIC.
#To deal with the issue of multicolinearity because of adding many variables to the
#model, I decided to add a condition which is that the new term has to have a
#VIF less than 5.

#List of variables without the ans_exclusion_criteria_seg variable
all_vars <- c("mc_method_h1", "stat_method_h1",
              "ans_spa_roi_avg_h1", "ans_mt_h1", "time_w_length_h1", 
              "ans_temp_roi_avg_h1", "ans_ica_algo", "ans_bad_comp_sel_visual",
              "ans_bad_comp_sel_plugin", "bs_window_length",
              "software", "hf_cutoff", "ans_hf_type", "ans_hf_direction", "reref",
              "ds_fs", "subj_excluded", "topo_region_h1", " nr_chan_h1")


#Creating the model with an intercept and one variable only
main_formula = "z_value ~ 1 + ans_exclusion_criteria_seg"
full_model <- lm(main_formula, data = data)
#c is dummy variable that is necessary for the loop to start
c = 2
# Forward selection loop
for (var in all_vars) {
  #Add main effect to a temporary model
  temp_formula_main <- paste(main_formula, "+", var)
  #Try catch block. This is to avoid the loop from stopping if the vif function
  #throws an error due to high multicolinearity
  tryCatch({
    temp_model_main <- lm(temp_formula_main, data = data)
    current_aic_main <- AIC(temp_model_main)
    vif_values <- car::vif(temp_model_main)
    
    #Check if adding the main effect improves AIC
    #Also checks if the VIF of the last variable is acceptable
    if ((current_aic_main < AIC(full_model)) && (vif_values[c] < 5)) {
      print(vif_values[c])
      c <- c + 1
      main_formula <- temp_formula_main
      full_model <- temp_model_main
      print(var)
      print(current_aic_main)
    }
  }, error = function(err) {
    cat("VIF too high when adding the variable", "\n")
    cat("Skipping current variable and continuing with the loop...\n")
  })
}

#This is a full list of the variable that we want to use to check the interaction
#effects
all_vars <- c("ans_exclusion_criteria_seg","mc_method_h1", "stat_method_h1",
              "ans_spa_roi_avg_h1", "ans_mt_h1", "time_w_length_h1", 
              "ans_temp_roi_avg_h1", "ans_ica_algo", "ans_bad_comp_sel_visual",
              "ans_bad_comp_sel_plugin", "bs_window_length",
              "software", "hf_cutoff", "ans_hf_type", "ans_hf_direction", "reref",
              "ds_fs", "subj_excluded", "topo_region_h1", " nr_chan_h1")


i = 1 #Dummy variable used in the loop
d = 0 #Dummy variable to check if the loop ran correctly. 20Choose2 = 190 after the loop runs

#Similar to the loop above but this loop checks the interaction effects of all
#the variables specified in all_vars
#Note that this is a nested loop because we want to check if adding the terms
#var1 * var2, var1 * var3 ..... var20 * var 19 changes the AIC
for (added_var in all_vars) {
  i = i + 1
  if(i == 21) {
    break
    }
  for (var in all_vars[i:length(all_vars)]) {
    d = d + 1
    temp_formula_main <- paste(main_formula, "+", added_var, "*", var)
    tryCatch({
    temp_model_main <- lm(temp_formula_main, data = data)
    current_aic_main <- AIC(temp_model_main)
    vif_values <-  car::vif(temp_model_main)
    if ((current_aic_main < AIC(full_model)) && (vif_values[c] < 5)) {
      print(vif_values[c])
      c = c + 1
      main_formula = temp_formula_main
      full_model <- temp_model_main
      print(paste(added_var, var, sep="*"))
      print(current_aic_main)
    }
    }, error = function(err) {
      cat("VIF too high when adding the variable", "\n")
      cat("Skipping current variable and continuing with the loop...\n")
    })
  }}

summary(full_model)
AIC(full_model)

