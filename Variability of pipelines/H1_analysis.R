library(readxl)
library(car)

#Loading the data
dataPath <- "C:\\Users\\User\\Desktop\\Neurocognitive\\Internship\\munster\\Vibration_of_effect\\all_var_AQ_h1_(version_5).xlsx"
data <- read_excel(dataPath, col_names = TRUE)
head(data)

#Converting the categorical variables in the data to factors
data$software <- as.factor(data$software)
data$hf_cutoff <- as.factor(data$hf_cutoff)
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
data$z_value = qnorm(data$pval)
data$pval = NULL

#Approach 1. Using the step function.
#Checking different models using the step function. 
fullmodel<- lm(z_value~ . , data = data)
summary(fullmodel)
stepwise_model <- step(fullmodel, direction = "both", trace = FALSE, k = log(nrow(data)))
summary(stepwise_model)
all_aic_values = data.frame(stepwise_model$anova$Step,stepwise_model$anova$AIC)

#The factors that had the most effect on the model are:nr_chan_h1,ans_mt_h1,bs_window_length 
#So I decided to use those along with their interaction effect
interactionformula = z_value~nr_chan_h1 + ans_mt_h1 + bs_window_length + 
  nr_chan_h1*ans_mt_h1 + nr_chan_h1*bs_window_length + ans_mt_h1*bs_window_length

interactionmodel <- lm(interactionformula, data=data)

summary(interactionmodel)

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
################################
#Approach 4.A vibration of effect

source('C:/Users/User/Desktop/Neurocognitive/Internship/munster/vibration_of_effect/vibration.R')

covariates <- ~ ans_exclusion_criteria_seg + mc_method_h1 + stat_method_h1 + 
              ans_spa_roi_avg_h1 + ans_mt_h1 + ans_time_w_start_h1 +
              ans_temp_roi_avg_h1 + ans_ica_algo + ans_bad_comp_sel_visual +
              ans_bad_comp_sel_plugin + 
              software + hf_cutoff + ans_hf_type + ans_hf_direction + reref +
              subj_excluded + topo_region_h1 + nr_chan_h1 + bs_window_length + ds_fs

# covariates <- ~ ds_fs +  time_w_length_h1  + subj_excluded

basemodel = z_value ~ 1


vib <- conductVibration(basemodel, dat, covariates, family='gaussian')


# Original covariates and base model
original_covariates <- ~ ans_exclusion_criteria_seg + mc_method_h1 + stat_method_h1 + 
  ans_spa_roi_avg_h1 + ans_mt_h1 + time_w_length_h1 +
  ans_temp_roi_avg_h1 + ans_ica_algo + ans_bad_comp_sel_visual +
  ans_bad_comp_sel_plugin + 
  software + hf_cutoff + ans_hf_type + ans_hf_direction + reref +
  subj_excluded + topo_region_h1 + nr_chan_h1 + ans_baseline_start + ds_fs + ans_baseline_stop +
  ans_time_w_end_h1

base_model <- z_value ~ 1

# Create a copy of the original covariates to use in the loop
current_covariates <- original_covariates

# Iterate over each term in the covariates
for (term in attr(terms(original_covariates), 'term.labels')) {
  # Create a new formula by adding the current term to the base model
  current_formula <- update.formula(base_model, as.formula(sprintf('~ 1 + %s', term)))
  
  # Update the current covariates to exclude the term that was added
  current_covariates <- update.formula(original_covariates, as.formula(sprintf('~ . - %s', term)))
  
  # Run conductVibration with the current formula and updated covariates
  vib <- conductVibration(current_formula, data, current_covariates, family='gaussian')
  
  # Save the vib variable to a file with the name of the current term
  saveRDS(vib, file = paste0(term, '_vib.rds'))
  
}

################################
# Approach 4.B Parallelized version of the vibration of effect code
# Remember to change the directories to match the directories on your machine

library(foreach)
library(doSNOW)
library(progress)
library(parallel)

# Set your working directory here
setwd("C:/Users/User/Desktop/Neurocognitive/.../Vibration_of_effect")

# Load the Vibration function
source('vibration.R')


# Set the number of cores to be used
num_cores <- 4  # Adjust this based on your machine capabilities

# Register parallel backend
cl <- makeCluster(num_cores,outfile="")
registerDoSNOW(cl)

dir.create("results")
output_directory <- "results"


terms_to_iterate <- c('ans_exclusion_criteria_seg' , 'mc_method_h1' , 'stat_method_h1' , 
                        'ans_spa_roi_avg_h1' , 'ans_mt_h1' , 'ans_time_w_start_h1' ,
                        'ans_temp_roi_avg_h1' , 'ans_ica_algo' , 'ans_bad_comp_sel_visual' ,
                        'ans_bad_comp_sel_plugin' , 
                        'software' , 'hf_cutoff' , 'ans_hf_type' , 'ans_hf_direction' , 'reref' ,
                        'subj_excluded' , 'topo_region_h1' , 'nr_chan_h1' , 'ans_baseline_start' , 'ds_fs' , 'ans_baseline_stop' ,
                        'ans_time_w_end_h1')

#terms_to_iterate <- c('ans_exclusion_criteria_seg' , 'mc_method_h1' , 'stat_method_h1' , 
#                        'ans_spa_roi_avg_h1' , 'ans_mt_h1' , 'ans_time_w_start_h1' ,
#                        'ans_temp_roi_avg_h1' , 'ans_ica_algo' , 'ans_bad_comp_sel_visual' ,
#                        'ans_bad_comp_sel_plugin' , 
#                        'software')




conductVibrationForTerm <- function(dat, terms_to_iterate, term, out_dir) {
  # Update covariates excluding the current term
  covariates <- as.formula(paste("~", paste(setdiff(terms_to_iterate, term), collapse = " + ")))
  
  # Create the base model formula
  basemodel <- as.formula(paste("z_value ~ 1 +", term))
  
  # Run the conductVibration function
  vib <- conductVibration(basemodel, dat, covariates, family = 'gaussian')
  
  # Save the result as an RDS file
  output_file <- file.path(out_dir, paste0(term, "_vib.rds"))
  saveRDS(vib, output_file)
  
  
  return(NULL)
}

# Creates a progress bar to track the progress


iterations <- length(terms_to_iterate)

pb <- progress_bar$new(
  format = "progress = :percent [:bar] Time elapsed: :elapsed",
  total = iterations,    
  width = 60)

progperc = seq(0, 100, by = ((1/iterations)*100))
progress_percentage <- floor(progperc)  # token reported in progress bar

# Allowing progress bar to be used in foreach 
progress <- function(n){
  pb$tick(tokens = list(percentage = progress_percentage[n+1]))
}

opts <- list(progress = progress)

# Initialize an empty list to store results
result <- foreach(i = 1:iterations, .combine = 'c', .options.snow = opts) %dopar% {
  term = terms_to_iterate[i]
  conductVibrationForTerm(data,terms_to_iterate,term, output_directory)
}
# Stop the parallel backend
stopCluster(cl)

################################
# Approach 4.C Plotting the estimates
library(tidyverse)
est_df <- data.frame()

# Choose the directory where the results from the previous step were saved
directory <- "C:/Users/User/Desktop/Neurocognitive/Internship/munster/Vibration_of_effect/results"

rds_files <- list.files(directory, pattern = "\\.rds$", full.names = TRUE)

for (rds_file in rds_files) {
  # Load the vib variable
  vib <- readRDS(rds_file)
  
  # Extract estimates from the vib data frame
  est <- vib$vibFrame$estimate
  #aic1 = vib$bicFrame[ ,2]
  # Create a data frame with the current term and its estimates
  term <- str_remove(basename(rds_file), "_vib.rds")
  current_df <- data.frame(Term = term, estimates = est)
  
  # Append the current data frame to the overall est_df
  est_df <- bind_rows(est_df, current_df)
}

# Calculate mean and standard deviation for each term
result <- aggregate(estimates ~ Term, data = est_df, FUN = function(x) c(mean = mean(x), sd = sd(x)))

# Extract mean and sd into separate columns
result <- cbind(result["Term"], as.data.frame(result$estimates, stringsAsFactors = FALSE))

# Rename columns
colnames(result) <- c("Term", "Mean", "SD")

# Removing the est_df from the workspace
rm(est_df)

# Plotting the results
p <- ggplot(result, aes(x = Term, y = Mean, fill = Term)) +
  geom_boxplot() +
  geom_point(aes(y = Mean), color = "red", size = 3, position = position_dodge(0.75)) +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), position = position_dodge(0.75), width = 0.2) +
  labs(title = "Box Plot of Estimates",
       x = "Term",
       y = "Estimates") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")  # Remove the legend

# Set plot size
options(repr.plot.width = 10, repr.plot.height = 6)

# Show the plot
print(p)

################################
# Entropy Analysis

# Install the entropy package 
install.packages("entropy")

library(entropy)

# Loading the data
dataPath <- "C:\\Users\\User\\Desktop\\Neurocognitive\\Internship\\munster\\Vibration_of_effect\\all_var_AQ_h1_(version_5).xlsx"
data <- read_excel(dataPath, col_names = TRUE)
head(data)

#Converting all the variables in the data to factors
data$software <- as.factor(data$software)
data$hf_cutoff <- as.factor(data$hf_cutoff)
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
data$z_value = qnorm(data$pval)
data$ds_fs <- as.factor(data$ds_fs)
data$subj_excluded <- as.factor(data$subj_excluded)
data$nr_chan_h1 <- as.factor(data$nr_chan_h1)
data$ans_time_w_start_h1 <- as.factor(data$ans_time_w_start_h1)
data$ans_time_w_end_h1 <- as.factor(data$ans_time_w_end_h1)
data$ans_baseline_start <- as.factor(data$ans_baseline_start)
data$ans_baseline_stop <- as.factor(data$ans_baseline_stop)

# Select the features that I would like to calculate the entropy for
feature_data <- data[, 1:22]

# Need to convert the factors to character vectors for the entropy package to work
encoded_data <- lapply(feature_data, function(x) as.numeric(factor(x)))

# Calculate entropy for each feature
entropy_values <- lapply(encoded_data, entropy)

# Print the results
print(entropy_values)

################################
# Kullback-Leibler divergence.
# This is not an entropy measure, rather a measure of similarity between two different distributions
# It is important to mention here that the KL divergence is affected by the granuality of the 
# distributions, that is, if our distributions have a different number of possible values
# such as 2 values in binomial and multiple values in multinomial distributions


# Select the features
feature_data <- data[, 1:22]

# Encode the data and transform it into a 
encoded_data <- lapply(feature_data, function(x) as.numeric(factor(x)))
encoded_data <- as.data.frame(encoded_data)

# Define a function to calculate KL divergence between two distributions
KL_divergence <- function(p, q) {
  sum(p * log(p / q), na.rm = TRUE)
}

# Calculate KL divergence for each pair of features
num_features <- ncol(encoded_data)
kl_divergence_matrix <- matrix(NA, nrow = num_features, ncol = num_features)
colnames(kl_divergence_matrix) <- rownames(kl_divergence_matrix) <- colnames(encoded_data)

epsilon <- 1e-10 # Small epsilon value to avoid division by zero

for (i in 1:num_features) {
  for (j in 1:num_features) {
    if (i != j) {
      # Check if both columns are numeric
      if (is.numeric(encoded_data[, i]) && is.numeric(encoded_data[, j])) {
        # Get unique categories from both features
        categories <- unique(c(names(table(encoded_data[, i])), names(table(encoded_data[, j]))))
        
        # Calculate distributions with zero-padding
        p <- table(encoded_data[, i], dnn = "categories")[categories]
        q <- table(encoded_data[, j], dnn = "categories")[categories]
        all_categories <- union(names(p), names(q))
        # Ensure both distributions have the same categories and apply zero-padding
        p <- ifelse(names(p) %in% all_categories, p, 0)
        q <- ifelse(names(q) %in% all_categories, q, 0)
        p[is.na(p)] <- 0
        q[is.na(q)] <- 0
        p <- (p + epsilon) / sum(p + epsilon)
        q <- (q + epsilon) / sum(q + epsilon)
        
        kl_divergence_matrix[i, j] <- KL_divergence(p, q)
      }
    }
  }
}

# Display the KL divergence matrix
print(kl_divergence_matrix)