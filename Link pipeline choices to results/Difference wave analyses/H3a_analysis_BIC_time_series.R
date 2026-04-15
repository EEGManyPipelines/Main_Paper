rm(list = ls())

library(jtools)
library(car)
library(ggplot2)
library(MASS)
#install.packages(c("lmtest","sandwich"))
library(lmtest)
library(sandwich)

#Loading the data
h3a_data <- read.csv('/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/data/Main Analysis/all_var_AQ_h3a_corrected.csv')

data <- h3a_data[,c(-1,-14,-15,-17:-28)]
pipe_ID <- h3a_data$Team

data_plot <- h3a_data[,c(-1,-14,-15,-17:-28)]

## --------------------
# scale continuous data
## --------------------

continuous <- c(3, 7, 8, 13)

#normalizing data
for (i in 1:length(continuous)) {
  contin_data_col <- data[,continuous[i]]
  data[,continuous[i]] <- as.numeric(scale(contin_data_col))
  #hist(scale(contin_data_col))
}

## ------------------------------------------------------------------------------------
## Load time-series data
# Fz data:
EEG <- read.csv('/Volumes/aebusch/aebuschgold/ecesnait/EMP/EMP time series exp/TimelockAVG_Hyp3a/h3a_difference_wave_FN400_reversed.csv')

#Pz data:
#EEG <- read.csv('/Volumes/aebusch/aebuschgold/ecesnait/EMP/EMP time series exp/TimelockAVG_Hyp3a/h3a_difference_wave_LPC_reversed.csv')

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

indx <- pipe_ID %in% EEG_ID
sum(indx)
pipe_ID_reduced <- pipe_ID[indx]
data_reduced <- data[indx,]
data_plot <- data_plot[indx,]

pl_indx <- match(EEG_ID,pipe_ID_reduced)
all.equal(pipe_ID_reduced[pl_indx], EEG_ID)

data_reduced <- data_reduced[pl_indx,]
data_plot <- data_plot[pl_indx,]

#inspect dependent variable
#boxplot(EEG$DW)
data_reduced$diffwave <- EEG$DW
data_plot$diffwave <- EEG$DW

## --------------------
# look if there are any NaN values
## --------------------
# 
empty_row <- which(is.na(data_reduced),arr.ind=TRUE) # 
indx_row <- empty_row[,1]#
# Caution: remove them from the dataset
data_reduced <- data_reduced[-indx_row,]
data_plot <- data_plot[-indx_row,]

## ------------------------------------------------------------------------------------

# exclude variables that have high collinearity >0.6
collinearity_indx <- match(c("ans_software_host"), colnames(data_reduced)) # 37

data_reduced <- data_reduced[,-collinearity_indx] 

#---------------------------------------------------------------------------------------------------------------------------
rownames(data_reduced) <- NULL

# remove an outlier based on Cook's distance
data_reduced <- data_reduced[c(-19,-21),]#for Pz data it's c(-19,-21), for Fz c(-20, -22, -36)

#correct names
colnames(data_reduced) <- c("software", "high-pass cutoff", "high-pass type", "high-pass dir.",
                            "reference", "sampling freq.","partic. excl.", "seg. excl. crit.",
                            "ICA alg.", "bad cmp. visual", "bad cmp. plugin",
                            "bs.window length","epoch bef. hp.", "lp. bef.correct.", "ref.bef.correct.",
                            "correct. bef. rej.","diffwave")
#-----
fullmodel_h3a<- lm(diffwave~ . , data = data_reduced)
summary(fullmodel_h3a)
car::vif(fullmodel_h3a)

# Stepwise model
stepwise_model <- stepAIC(fullmodel_h3a, direction = "both", trace = T, k = log(nrow(data_reduced)))
summary(stepwise_model)
summ(stepwise_model, digits = 4)
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
ks.test(scale(resid),'pnorm') # not significant - therefore it's ok

#Homoscedacisity - does not meet expectations

bptest(stepwise_model) #p-value = 0.02491 suggests homoscedasticity

#Fix by performing robust covariance estimators
robust_se <- coeftest(stepwise_model,vcov=vcovHC(stepwise_model, type="HC3")) # still one outlier remaining
robust_se
confint(robust_se)

#find the observation that doesn't converge
## 1) Find extreme leverage (hat) values
h <- hatvalues(stepwise_model)

# any with h extremely close to 1?
bad_hat_idx <- which(h > 1 - 1e-8)                 # e.g., 0.99999999+
bad_hat_idx

# check all plots
par(mfrow=c(2,2))
plot(stepwise_model)
par(mfrow=c(1,1))

##--- PLOT SIGNIFICANT PREDICTORS ----#
# plot the reference channel and the high pass filter cutoff

tiff("/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/figures/hf_diff_wave_plot_h3a.png", units="in", width=4, height=3, res=100)

ggplot(data=data_plot, aes(x=hf_cutoff, y=diffwave))+
  geom_point(size=2, alpha = 0.5)+
  geom_smooth(method= "lm", colour = "grey")+
  theme_minimal()+ theme(text = element_text(size=20))+
  ylab("diff. wave")+
  xlab("hf cutoff")
dev.off()

# reference
# split based on ref
plot_data_ref <- as.data.frame(data_reduced$reref)
plot_data_ref$diffwave <- data_reduced$diffwave
colnames(plot_data_ref) <- c('ref','diffwave')
plot_data_ref$ref[plot_data_ref$ref=='other'] <- 'unknown'
indx_rm_unknown <- which(plot_data_ref$ref=='unknown')
plot_data_ref <- plot_data_ref[-indx_rm_unknown,]
colors <- c("#E9F7B9","#86D0B9","#4BBAC3")

tiff("/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/figures/ref_diff_wave_plot_h3a.png", units="in", width=3, height=3.5, res=100)

ggplot(plot_data_ref, aes(x=ref, y=diffwave, fill=diffwave)) + 
  geom_boxplot(fill=colors)+
  theme_minimal()+ theme(text = element_text(size=20), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ylab("diff. wave")+
  xlab("reference")

dev.off()  
