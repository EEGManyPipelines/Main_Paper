rm(list = ls())

#Load libraries
#install.packages("jtools")
library(jtools)
library(readxl)
library(car)
library(ggplot2)
library(ggcorrplot)
library(tidyr)
library(MASS)
library(lmtest)
library(sandwich)

#Loading the data
h1_data <- read.csv("/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/data/Main Analysis/all_var_AQ_h1_corrected.csv")
#plugin <- read.csv('/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/data/Main Analysis/all_var_AQ_h1_corrected_with_plugins_1.csv',sep = ';')
#table(plugin$plugin_desc_clean)
#h1_data <- cbind(h1_data,plugin$plugin_desc_clean)#remove binary response and p-values

# prepricessing only
preproc_data <- h1_data[,c(-1,-10,-11,-13:-20,-24,-25,-27:-29)] # all variables that could not explain difference wave are removed

## --------------------
# Load mean N1 difference wave
## --------------------
# 
diff_wave <- read.csv("/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/data/Main Analysis/n1_amplitudes_v3.csv")
AQ_ID <- read.csv("/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/data/Main Analysis/IDs_linear_models.csv")

# Match IDs
ID_diff <- diff_wave[,1]
setdiff(ID_diff,AQ_ID$teamID)

ID_diff[ID_diff %in% "TheCodeMechanics"] <- "The Code Mechanics" # correct a typo
ID_diff[ID_diff %in% "19068f1fe266c5e1_1"] <- "19068f1fe266c5e1" # correct a typo
ID_diff[ID_diff %in% "356c77bfd2662b9a_H1"] <- "356c77bfd2662b9a" # correct a typo
ID_diff[ID_diff %in% "gNeC.mat"] <- "gNeC" # correct a typo

loc <- match(ID_diff,AQ_ID$teamID, nomatch = 0)
ID_matched <- AQ_ID$teamID[loc]

# check if all IDs now exist
setdiff(ID_diff,ID_matched)

data_matched <- preproc_data[loc,]

# bring diff wave from volts to microvolts - only relevant for hypothesis 1. Later hypotheses had this implementation for amplitude calculation
V_IDs <- c("08de3c5e092173e4", "0ba1c7f1dafc1134", "0bc9ee704db74104", "19e8ad8bf94af489",
           "344dd59ded90cb34", "48e64dc185199502", "628a18bc8a3d36dd", 
           "77fddd91c557626d", "7f3fe2bc79a9d3f9","8559e4d7314e45ec", "8c587a4cbf53865d",
           "90420442f22fa870", "9985e8ae6679b0e2","CIMHPipe", "TheCodeMechanics", "Varuwa",
           "a0cf32754296214f","a25b8419335d2131","aa6aa366e9788967", "d5c8ed05b7af02a3",
           "b0edd369b6d8f4f1", "bd3077a83b5b16bd", "c0c75576f9cd0b2a", "c577d3cdf78548ce",
           "c91e489c4acd0bf4","da33cf2264c9baa2", "e13e7e07b99d853b", "e146a94b29a41713", 
           "e2d0d90e5cf594ed","e69a83408d1f3811","e72d90a6ff4b5108", "ea6b1d1870708b82",
           "ee8c062e3dc35b1d", "eef1406b3fca3e9c", "f92a1d6a49d0d40a", "ff8bf48d04d11c84",
           "c4ed0094fc18efbc") 

diff_wave[diff_wave$id %in% V_IDs,]
diff_wave[diff_wave$id %in% V_IDs,2] <- diff_wave[diff_wave$id %in% V_IDs,2] * 10^6

## --------------------
# scale continuous data
## --------------------
# 
continuous <- c(3, 7, 8, 13)

#normalizing data
for (i in 1:length(continuous)) {
  contin_data_col <- data_matched[,continuous[i]]
  data_matched[,continuous[i]] <- scale(contin_data_col)
  #hist(scale(contin_data_col))
}

#---------------------------------------------------------------------------------------------------------------------------

# exclude variables that have high collinearity >0.6
collinearity_indx <- match(c("ans_software_host","ans_hf_direction"), colnames(data_matched)) # 37


data_reduced <- data_matched[,-collinearity_indx] 

#find the ones above 0.6
all_cor <- model.matrix(~0+., data=data_reduced) %>% 
  cor(use="pairwise.complete.obs")
which(abs(all_cor)>0.6 & abs(all_cor)!=1, arr.ind=TRUE) # 

#remove influential cases - see how we identified them below
rownames(data_reduced) <- NULL

data_reduced <- data_reduced[c( -25, -80, -91 ,-100),]#c(-25, -80, -91 ,-100)
diff_wave <- diff_wave[c(-25, -80, -91 ,-100),]

#change names
colnames(data_reduced) <- c("software", "high-pass cutoff", "high-pass type", "reference", "sampling freq.",
                            "partic. excl.", "seg. excl. crit.", "ICA alg.", "bad cmp. visual", "bad cmp. plugin",
                            "bs.window length","epoch bef. hp.", "lp. bef.correct.", "ref.bef.correct.", "correct. bef. rej.")

#Approach 1. Using the step function.
#Checking different models using the step function. 
fullmodel_h1<- lm(diff_wave$cpz~ . , data = data_reduced)
summary(fullmodel_h1)
car::vif(fullmodel_h1)

# Stepwise model
stepwise_model <- stepAIC(fullmodel_h1, direction = "both", trace = T, k = log(nrow(data_reduced)))
summary(stepwise_model)
confint(stepwise_model)

summ(stepwise_model, confint = TRUE, digits = 4) # nicer vizualisation

#check assumptions

# Check for individual influential cases

studres <- rstudent(stepwise_model)
cooksd <- cooks.distance(stepwise_model) # Cooks distance measure how much the regression would change given if an observation was remoevd

#Plot 
plot(studres, main="Studentized Residuals", ylab="Studentized Residuals")
abline(h=c(-3,3), col="red", lty=2) # one outlier

outliers <- which(abs(studres)>3) # 

lev <- hatvalues(stepwise_model) # Leverage detects observations with unusual predictor values

lev[outliers] # leverage values. not a problem. Should not be larger than 2*length(coef(stepwise_model))/length(lev)
cooksd[outliers] # cooks distance should not be. a problem. larger than 4/length(cooksd)

# correlation between residuals and fitted values - linearity assumption. Fine
# Null hypothesis is that correlation is 0 (there is no systematic pattern and linearity holds)
cor.test(fitted(stepwise_model),residuals(stepwise_model)) # p=1,cor very small - assumption holds

#normality of residuals. Fine
resid <- residuals(stepwise_model)

#shapiro-wilk test
shapiro.test(resid) # significant, not normal residuals

# kolmogorov-smirnov test
ks.test(scale(resid),'pnorm') # not significant

#Homoscedacisity - fine
#install.packages(c("lmtest","sandwich"))
bptest(stepwise_model) # not ignificant

# Plot the effects

# violin plot
plot_data <- data.frame(plugin=data_reduced[,16],diff_wave=diff_wave$cpz)
plot_data$plugin <- as.factor(plot_data$plugin)

png("/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/figures/plug_diff_wave_violin_h1_v2.png", units="in", width=2, height=2.5, res=200)

ggplot(plot_data, aes(x=plugin, y=diff_wave, fill=plugin)) + 
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.1, fill="white")+
  scale_fill_manual( values=c( "#E69F00", "#56B4E9"))+
  labs(x="plugin use", y = "difference wave")+ theme_classic()+
  theme(text = element_text(size=20), legend.position = "none")
dev.off()

