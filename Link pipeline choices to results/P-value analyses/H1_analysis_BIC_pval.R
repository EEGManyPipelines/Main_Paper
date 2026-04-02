rm(list = ls())

#install.packages("jtools")
library(jtools)
library(car)
library(ggplot2)
library(tidyr)
library(MASS)
library(lmtest)
library(sandwich)

#Loading the data
h1_data <- read.csv('/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/data/Main Analysis/all_var_AQ_h1_corrected.csv')

table(h1_data$topo_region_h1)
## -------------------- 
# Correct values
## --------------------
h1_data$topo_region_h1[h1_data$topo_region_h1=="posterior"] <- "parietal"

#remove binary response and p-values
data <- h1_data[,c(-1,-17,-18,-24,-25,-27, -28)]

## --------------------
# from p-val to z-val
## --------------------

data$pval_med <- qnorm(data$pval_med)#
which(is.na(h1_data$pval_med))
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
continuous <- c(3, 7, 8, 10, 16,21)

#normalizing data
for (i in 1:length(continuous)) {
  contin_data_col <- data[,continuous[i]]
  data[,continuous[i]] <- as.numeric(scale(contin_data_col))
  #hist(scale(contin_data_col))
}

#---------------------------------------------------------------------------------------------------------------------------

# exclude variables that have high collinearity >0.6
collinearity_indx <- match(c("ans_software_host","mc_method_h1",
                              "ans_hf_direction"), colnames(data)) # 37

data_reduced <- data[,-collinearity_indx] 
#find the ones above 0.6
all_cor <- model.matrix(~0+., data=data_reduced) %>% 
  cor(use="pairwise.complete.obs")
which(abs(all_cor)>0.6 & abs(all_cor)!=1, arr.ind=TRUE) # 

#---------------------------------------------------------------------------------------------------------------------------
#rename 
#names(data_reduced)[names(data_reduced) == 'ans_mt_h1'] <- 'mult_comp_correction'
rownames(data_reduced) <- c(1:153)

# remove an outlier identified below by Studentized Residuals and Cook's distance
data_reduced <- data_reduced[-31,]

#rename
colnames(data_reduced) <- c("software", "high-pass cutoff", "high-pass type", "reference", "sampling freq.",
                            "partic. excl.", "topo. region", "nr. channels", 
                            "seg. excl. crit.","stat.method","ROI avg.","mult.comp.cor.","time w. length",
                            "Temp. avg.","ICA alg.", "bad cmp. visual", "bad cmp. plugin",
                            "bs.window length","pval_med","epoch bef. hp.", "lp. bef.correct.", "ref.bef.correct.", "correct. bef. rej.")
#Approach 1. Using the step function.
#Checking different models using the step function. 
fullmodel_h1<- lm(pval_med ~ . , data = data_reduced)
summary(fullmodel_h1)
car::vif(fullmodel_h1)

# Stepwise model
stepwise_model <- stepAIC(fullmodel_h1, direction = "both", trace = T, k = log(nrow(data_reduced)))
summary(stepwise_model)
confint(stepwise_model)
car::vif(fullmodel_h1)

#The factors that had the most effect on the model are: ans_mt_h1,bs_window_start
#So I decided to use those along with their interaction effect
winmodel <- lm(pval_med ~ `nr. channels`+`bs.window length`+mult.comp.cor.+
                 `bs.window length`*mult.comp.cor. + `nr. channels`*mult.comp.cor., data=data_reduced)
summary(winmodel)
summ(winmodel, confint = TRUE, digits = 4) # nicer vizualisation

# Check assumptions #
studres <- rstudent(stepwise_model)
cooksd <- cooks.distance(stepwise_model) # Cooks distance measure how much the regression would change given if an observation was remoevd

#Plot 
plot(studres, main="Studentized Residuals", ylab="Studentized Residuals")
abline(h=c(-3,3), col="red", lty=2) # one outlier

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
ks.test(scale(resid),'pnorm') # not significant - therefore it's ok

#Homoscedacisity - does not meet expectations

bptest(stepwise_model) #p-value = 0.0029 suggests homoscedasticity

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

#-- PLOT EFFECTS --#
#Interaction
ggplot(data=data_reduced, aes(x=`bs.window length`, y=pval_med, group=mult.comp.cor.))+
  geom_point(size=2, aes(color=mult.comp.cor.))+
  geom_smooth(method= "lm")+
  ylab("qnorm(pval)")+
  xlab("baseline start")+
  ggtitle("Interaction effect")

# with original values

#h1_data$pval_med <- pvalM#qnorm(0.975,mean=0,sd=1)
h1_data$ans_mt_h1 <- factor(h1_data$ans_mt_h1, levels = c(0, 1), labels = c("no","yes"))

empty_row_2 <- which(is.na(h1_data),arr.ind=TRUE) # 
indx_row2 <- empty_row_2[,1]
# Caution: remove them from the dataset
h1_data <- h1_data[-indx_row2,]

png("/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/figures/h1_interaction_baseline_mc.png", units="in", width=7, height=4, res=300)

ggplot(data=h1_data, aes(x=bs_window_length, y=qnorm(pval_med), group=ans_mt_h1 ))+
  geom_point(size=4, aes(color=ans_mt_h1), alpha = 0.5)+
  geom_smooth(method= "lm", color = "grey")+
  scale_color_manual(values = c("#00AFBB", "#E7B800"))+
  theme_minimal()+ labs(x = "baseline length(ms)",y="qnorm(pval)",color = "cor.mult.com.")+
  theme(text = element_text(size=25), )

dev.off()


png("/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/figures/h1_nr_chan.png", units="in", width=4.5, height=3.5, res=300)

ggplot(data=h1_data, aes(x=nr_chan_h1, y=qnorm(pval_med)))+
  geom_point(size=4, alpha = 0.5, colour="chocolate")+
  geom_smooth(method= "lm", color = "grey")+
  theme_minimal()+ labs(x = "nr. channels",y="qnorm(pval)")+
  theme(text = element_text(size=25), )

dev.off()

# with permutation vs not

ggplot(data=h1_data, aes(x=bs_window_length, y=qnorm(pval_med), group=ans_mt_h1 ))+
  geom_point(size=4, aes(color=ans_mt_h1, shape = mc_method_h1), alpha = 0.5)+
  geom_smooth(method= "lm")+
  scale_shape_manual(values = c(16,17)) +
  scale_color_manual(values = c("#00AFBB", "#E7B800"))+
  theme_minimal()+
  ylab("qnorm(pval)")+
  xlab("baseline length(ms)")+
  ggtitle("Interaction effect")+ theme(text = element_text(size=15))


# check if outlier cases innfluence the result
rm <- which(data_reduced$`bs.window length`>2.5)
data_reduced2 <- data_reduced[-rm,]
winmodel2 <- lm(pval_med ~ `nr. channels`+`bs.window length`+mult.comp.cor.+
                 `bs.window length`*mult.comp.cor. + `nr. channels`*mult.comp.cor., data=data_reduced2)
summary(winmodel2)
summ(winmodel2, confint = TRUE, digits = 4) # nicer vizualisation




