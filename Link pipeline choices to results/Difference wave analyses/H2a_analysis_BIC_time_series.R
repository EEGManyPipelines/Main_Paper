rm(list = ls())

#install.packages("jtools")
library(jtools)
library(readxl)
library(car)
library(ggplot2)
library(MASS)
library(lmtest)
library(sandwich)

#Loading the data
h2a_data <- read.csv('/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/data/Main Analysis/all_var_AQ_h2a_corrected.csv')
table(h2a_data$ans_hf_type)

h2a_data$ans_hf_type[h2a_data$ans_hf_type=="IIR"]<-"hf_iir"
#remove binary response and p-values

data <- h2a_data[,c(-1,-10:-19,-24,-25,-27,-28)]
pipe_ID <- h2a_data$Team
data_plot <- h2a_data[,c(-1,-10:-19,-24,-25,-27,-28)] #h2a_data[,c(-1,-10,-11:-19,-24,-25,-27,-28)]

## --------------------
# scale continuous data
## --------------------
# 
continuous <- c(3, 7,8, 13)

#normalizing data
for (i in 1:length(continuous)) {
  contin_data_col <- data[,continuous[i]]
  data[,continuous[i]] <- as.numeric(scale(contin_data_col))
  #hist(scale(contin_data_col))
}

## ------------------------------------------------------------------------------------
## Load time-series data
EEG <- read.csv('/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/data/Main Analysis/h2a_difference_wave_front_tw_v2_081026.csv')

EEG <- EEG[-which(is.na(EEG$DW)),]
EEG_ID <- EEG$ID

indx <- pipe_ID %in% EEG_ID
pipe_ID[!indx]
#find IDs that don't exist
indx2 <-  EEG_ID %in% pipe_ID 
EEG_ID[!indx2]

EEG_ID[EEG_ID=='19068f1fe266c5e1_1'] <- '19068f1fe266c5e1'
EEG_ID[EEG_ID=='ChileMaule'] <- 'Chile Maule'
EEG_ID[EEG_ID=='CognitiveSystems_KU'] <- 'CognitiveSystems-KU'
EEG_ID[EEG_ID=='TMS_EEG_DREAM'] <- 'TMS-EEG-DREAM'
EEG_ID[EEG_ID=='TheCodeMechanics']<-  'The Code Mechanics'
EEG_ID[EEG_ID=='KaiserSoze']<-  'Kaiser Soze'

indx <- pipe_ID %in% EEG_ID
sum(indx)
pipe_ID_reduced <- pipe_ID[indx]
data_reduced <- data[indx,]

#data_plot <- data_plot[indx,]

pl_indx <- match(EEG_ID,pipe_ID_reduced)
all.equal(pipe_ID_reduced[pl_indx], EEG_ID)

data_reduced <- data_reduced[pl_indx,]
data_plot <- data_plot[pl_indx,]

boxplot(EEG$DW)

#inspect dependent variable
#boxplot(EEG$DW)
data_reduced$diffwave <- EEG$DW
data_plot$diffwave <- EEG$DW

# replace every "other" with "unknown" in the whole data frame
data_reduced[] <- lapply(data_reduced, function(col) {
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

#identify and remove individual cases from categorical varaibles
sapply(data_reduced[sapply(data_reduced, is.factor) | sapply(data_reduced, is.character)],
       function(x) sum(table(x) == 1))
table(data_reduced$software)
data_reduced$ans_hf_direction[data_reduced$ans_hf_direction=='fwr_bwd'] <- 'fwd_bwd'

## ------------------------------------------------------------------------------------

# exclude variables that have high collinearity >0.6
collinearity_indx <- match(c("ans_software_host"), colnames(data_reduced)) # 37

data_reduced <- data_reduced[,-collinearity_indx] 
rownames(data_reduced) <- NULL

#remove influential cases based on Cook's distance and homoscedasticity
data_reduced <- data_reduced[c(-24,-28),]
rownames(data_reduced) <- NULL


#correct the names
colnames(data_reduced) <- c("software", "high-pass cutoff", "high-pass type", "high-pass dir.",
                            "reference", "sampling freq.","partic. excl.", "seg. excl. crit.",
                            "ICA alg.", "bad cmp. visual", "bad cmp. plugin",
                            "bs.window length","epoch bef. hp.", "lp. bef.correct.", "ref.bef.correct.",
                            "correct. bef. rej.","diffwave")

#---------------------------------------------------------------------------------------------------------------------------

fullmodel_h2a<- lm(diffwave~ . , data = data_reduced)
summary(fullmodel_h2a)
car::vif(fullmodel_h2a)

# Stepwise model
stepwise_model <- stepAIC(fullmodel_h2a, direction = "both", trace = T, k = log(nrow(data_reduced)))
summary(stepwise_model)
summ(stepwise_model, confint = TRUE, digits = 4) # nicer vizualisation
confint(stepwise_model)

# Take POz reference as a scheme to compare the other two to
data_reduced$reference <- relevel(factor(data_reduced$reference), ref = "original")
m2 <- lm(diffwave ~ `high-pass cutoff` + reference, data = data_reduced)
summary(m2)   # "referencemastoid" is now mastoid vs. POz
confint(m2)

# Check for individual influential cases

studres <- rstudent(stepwise_model)
cooksd <- cooks.distance(stepwise_model) # Cooks distance measure how much the regression would change given if an observation was remoevd

#Plot 
plot(studres, main="Studentized Residuals", ylab="Studentized Residuals")
abline(h=c(-3,3), col="red", lty=2) # one outlier

outliers <- which(abs(studres)>3) # 22

lev <- hatvalues(stepwise_model) # Leverage detects observations with unusual predictor values

lev[outliers] # leverage values. not a problem. Should not be larger than 2*length(coef(stepwise_model))/length(lev)
cooksd[outliers] # cooks distance should not be. a problem. larger than 4/length(cooksd)

#check assumptions
# correlation between residuals and fitted values - linearity assumption. Fine
# Null hypothesis is that correlation is 0 (there is no systematic pattern and linearity holds)
cor.test(fitted(stepwise_model),residuals(stepwise_model)) # p=1,cor very small - assumption holds

#normality of residuals. Fine
resid <- residuals(stepwise_model)

#shapiro-wilk test
shapiro.test(resid) # significant, not normal residuals

# kolmogorov-smirnov test
ks.test(scale(resid),'pnorm') # significant  - correct it below with robus SE

#Homoscedacisity - does not meet expectations

bptest(stepwise_model) #p-value = 0.02491 suggests homoscedasticity

#Fix by performing robust covariance estimators
robust_se <- coeftest(stepwise_model,vcov=vcovHC(stepwise_model, type="HC3")) 
robust_se
confint(robust_se)


# check all plots
par(mfrow=c(2,2))
plot(stepwise_model)
par(mfrow=c(1,1))


