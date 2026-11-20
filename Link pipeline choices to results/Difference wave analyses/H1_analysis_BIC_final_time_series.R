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

# prepricessing only
preproc_data <- h1_data[,c(-1,-10,-11,-13:-20,-24,-25,-27:-29)] # all variables that could not explain difference wave are removed

## --------------------
# Load mean N1 difference wave
## --------------------
# 
diff_wave <- read.csv("/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/data/Main Analysis/h1_difference_wave_cpz_n100.csv")
AQ_ID <- read.csv("/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/data/Main Analysis/IDs_linear_models.csv")

diff_wave <- diff_wave[-which(is.na(diff_wave$DW)),]

# Match IDs
ID_diff <- diff_wave[,1]
setdiff(ID_diff,AQ_ID$teamID)

ID_diff[ID_diff %in% "TheCodeMechanics"] <- "The Code Mechanics" # correct a typo
ID_diff[ID_diff %in% "19068f1fe266c5e1_1"] <- "19068f1fe266c5e1" # correct a typo
ID_diff[ID_diff %in% "356c77bfd2662b9a_H1"] <- "356c77bfd2662b9a" # correct a typo
ID_diff[ID_diff %in% "ChileMaule"] <- "Chile Maule" # correct a typo
ID_diff[ID_diff %in% "CognitiveSystems_KU"] <- "CognitiveSystems-KU" # correct a typo
ID_diff[ID_diff %in% "KaiserSoze"] <- "Kaiser Soze" # correct a typo
ID_diff[ID_diff %in% "TMS_EEG_DREAM"] <- "TMS-EEG-DREAM" # correct a typo
ID_diff[ID_diff %in% "TheHanncanny"] <- "The Hanncanny" # correct a typo

loc <- match(ID_diff,AQ_ID$teamID, nomatch = 0)
ID_matched <- AQ_ID$teamID[loc]

# check if all IDs now exist
setdiff(ID_diff,ID_matched)

data_matched <- preproc_data[loc,]

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

data_matched[] <- lapply(data_matched, function(col) {
  if (is.factor(col)) {
    col <- as.character(col)
    col[col %in% "other"] <- "unknown"
    factor(col)
  } else if (is.character(col)) {
    col[col %in% "other"] <- "unknown"
    col
  } else {
    col
  }
})

#check for single categorical values in the data
sapply(data_matched[sapply(data_matched, is.factor) | sapply(data_matched, is.character)],
       function(x) sum(table(x) == 1))
data_matched$ans_hf_direction[data_matched$ans_hf_direction=='fwr_bwd'] <- 'fwd_bwd'

#---------------------------------------------------------------------------------------------------------------------------

# exclude variables that have high collinearity >0.6
collinearity_indx <- match(c("ans_software_host"), colnames(data_matched)) # 37

data_reduced <- data_matched[,-collinearity_indx] 

#find the ones above 0.6
all_cor <- model.matrix(~0+., data=data_reduced) %>% 
  cor(use="pairwise.complete.obs")
which(abs(all_cor)>0.6 & abs(all_cor)!=1, arr.ind=TRUE) # 

#remove influential cases - see how we identified them below
rownames(data_reduced) <- NULL

data_reduced <- data_reduced[c( -29, -65, -69),]#
diff_wave <- diff_wave[c(-29, -65, -69),]
rownames(data_reduced) <- NULL

#change names
colnames(data_reduced) <- c("software", "high-pass cutoff", "high-pass type", "high-pass direction","reference", "sampling freq.",
                            "partic. excl.", "seg. excl. crit.", "ICA alg.", "bad cmp. visual", "bad cmp. plugin",
                            "bs.window length","epoch bef. hp.", "lp. bef.correct.", "ref.bef.correct.", "correct. bef. rej.")

#Approach 1. Using the step function.
#Checking different models using the step function. 
fullmodel_h1<- lm(diff_wave$DW~ . , data = data_reduced)
summary(fullmodel_h1)
car::vif(fullmodel_h1)

# Stepwise model
stepwise_model <- stepAIC(fullmodel_h1, direction = "both", trace = T, k = log(nrow(data_reduced)))
summary(stepwise_model)
confint(stepwise_model)

summ(stepwise_model, confint = TRUE, digits = 4) # nicer vizualisation

## Check assumptions

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

names(outliers)
# correlation between residuals and fitted values - linearity assumption. Fine
# Null hypothesis is that correlation is 0 (there is no systematic pattern and linearity holds)
cor.test(fitted(stepwise_model),residuals(stepwise_model)) # p=1,cor very small - assumption holds

#normality of residuals. Fine
resid <- residuals(stepwise_model)

#shapiro-wilk test
shapiro.test(resid) # significant, not normal residuals
plot(stepwise_model, which = 2)

# kolmogorov-smirnov test
ks.test(scale(resid),'pnorm') # not significant

#Homoscedacisity - fine
#install.packages(c("lmtest","sandwich"))
bptest(stepwise_model) # significant

#Fix by performing robust covariance estimators
robust_se <- coeftest(stepwise_model,vcov=vcovHC(stepwise_model, type="HC3")) # still one outlier remaining
robust_se
confint(robust_se)

