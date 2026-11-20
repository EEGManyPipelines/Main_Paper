rm(list = ls())

#install.packages("jtools")
library(jtools)
library(car)
library(ggplot2)
library(tidyr)
library(MASS)
library(lmtest)
library(sandwich)
#install.packages("readxl")  # only needed once
library(readxl)

#Loading the data
h1_data <- read.csv('/Effect sizes/all_var_AQ_h1_with_cohens_d_new.csv')
table(h1_data$topo_region_h1)

## -------------------- 
# Correct values
## --------------------
h1_data$topo_region_h1[h1_data$topo_region_h1=="posterior"] <- "parietal"

#remove binary response and d-value
data <- h1_data[,c(-1,-17,-18,-24,-25,-27, -28, -34,-35,-38,-39)]

## --------------------
# from p-val to z-val
## --------------------

zval <- qnorm(1 - h1_data$pval_med/2) #z-score that corresponds to a two-tailed test statistic
data$cohens_d_v2 <- zval / sqrt(33-h1_data$subj_excluded) # recreate Cohen's d from the table - more data points available

## identify effects going the other direction

indx_neg_effect <- which(data$direction == 'natural>man-made')

data$cohens_d_v2[indx_neg_effect] <- data$cohens_d_v2[indx_neg_effect]*-1 # add a direction to cohens_d_v2 column

## find a team that filled in the wrong hypothesis reply and was left blank
indx_nan <- which(data$direction=="")

data <- data[-indx_nan,]

## plot Cohe's d extracted from a free-text result forms against the one calculated using pval
p<-ggplot(data, aes(x=cohens_d_new, y=cohens_d_v2)) + 
  geom_point(colour = 4,size = 2, alpha=.6) + theme_minimal() +theme(text=element_text(size=15))+
  geom_vline(xintercept = 0, alpha=.4)+geom_hline(yintercept = 0, alpha=.4)+
  xlim(-2.1,2.1)+ylim(-2.1,2.1)+
  xlab("Based on RQ")+ylab("Based on AQ") + ggtitle("H1 Cohen's d")
#ggsave('/Effect sizes/figures/cohens d scatter H1.png')

##remove cohens_d_old and direction columns 
data <- data[,c(-22,-27,-29)]

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
continuous <- c(3, 7, 8,10,16,21)

#continuous <- c(3, 7, 9, 15,20)

#normalizing data
for (i in 1:length(continuous)) {
  contin_data_col <- data[,continuous[i]]
  data[,continuous[i]] <- as.numeric(scale(contin_data_col))
  #hist(scale(contin_data_col))
}

# transform 'other' responses to 'unknown'
data[] <- lapply(data, function(col) {
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
sapply(data[sapply(data, is.factor) | sapply(data, is.character)],
       function(x) sum(table(x) == 1))

#---------------------------------------------------------------------------------------------------------------------------

# exclude variables that have high collinearity >0.6
collinearity_indx <- match(c("ans_software_host"), colnames(data)) #

data_reduced <- data[,-collinearity_indx] 

#find the ones above 0.6
all_cor <- model.matrix(~0+., data=data_reduced) %>% 
  cor(use="pairwise.complete.obs")
which(abs(all_cor)>0.8 & abs(all_cor)!=1, arr.ind=TRUE) # 

#---------------------------------------------------------------------------------------------------------------------------
#rename
colnames(data_reduced) <- c("software", "high-pass cutoff", "high-pass type", "high-pass direction","reference", "sampling freq.",
                            "sbj.excl.","topo.region", "nr. channels", 
                            "seg. excl. crit.","mult.comp.method","stat.method","ROI avg.","mult.comp.cor.","time w. length",
                            "temp. avg.","ICA alg.", "bad cmp. visual", "bad cmp. plugin",
                            "bs.window.length","epoch bef. hp.", "lp. bef.correct.", "ref.bef.correct.", "correct. bef. rej.", "dval")

#colnames(data_reduced) <- c("software", "high-pass cutoff", "high-pass type", "reference", "sampling freq.",
#                             "topo. region", "nr. channels", 
 #                           "seg. excl. crit.","stat.method","ROI avg.","mult.comp.cor.","time w. length",
  #                          "Temp. avg.","ICA alg.", "bad cmp. visual", "bad cmp. plugin",
   #                         "bs.window length","epoch bef. hp.", "lp. bef.correct.", "ref.bef.correct.", "correct. bef. rej.", "dval")

#Approach 1. Using the step function.
#Checking different models using the step function. 

fullmodel_h1<- lm(dval ~ . , data = data_reduced)
summary(fullmodel_h1)
car::vif(fullmodel_h1)

# Stepwise model
stepwise_model <- stepAIC(fullmodel_h1, direction = "both", trace = T, k = log(nrow(data_reduced)))
summary(stepwise_model)
confint(stepwise_model)
car::vif(fullmodel_h1)

#summ(winmodel, confint = TRUE, digits = 4) # nicer vizualisation

# Check assumptions #
studres <- rstudent(stepwise_model)
cooksd <- cooks.distance(stepwise_model) # Cooks distance measure how much the regression would change given if an observation was remoevd

#Plot 
plot(studres, main="Studentized Residuals", ylab="Studentized Residuals")
abline(h=c(-3,3), col="red", lty=2) # 

outliers <- which(abs(studres)>3) #

lev <- hatvalues(stepwise_model) # Leverage detects observations with unusual predictor values

lev[outliers] # leverage values. not a problem. Should not be larger than 2*length(coef(stepwise_model))/length(lev)
cooksd[outliers] # cooks distance. a problem. should not be larger than 4/length(cooksd)

#check assumptions
# correlation between residuals and fitted values - linearity assumption. Fine
# Null hypothesis is that correlation is 0 (there is no systematic pattern and linearity holds)
cor.test(fitted(stepwise_model),residuals(stepwise_model)) # p=1,cor very small - assumption holds

#normality of residuals. Fine
resid <- residuals(stepwise_model)

#shapiro-wilk test
shapiro.test(resid) # normal residuals

# kolmogorov-smirnov test
ks.test(scale(resid),'pnorm') 

#Homoscedacisity 
bptest(stepwise_model) #

#Fix by performing robust covariance estimators
robust_se <- coeftest(stepwise_model,vcov=vcovHC(stepwise_model, type="HC3")) # 
robust_se
confint(robust_se)

interaction_robust_se <- coeftest(winmodel,vcov=vcovHC(winmodel, type="HC3"))
confint(interaction_robust_se)

# check all plots
par(mfrow=c(2,2))
plot(stepwise_model)
par(mfrow=c(1,1))


