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
h3a_data <- read.csv('/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/data/Main Analysis/all_var_AQ_h3a_corrected.csv')


# Remove teams that didn't report the p-value
indx_no_p <- which(is.na(h3a_data$pval)) # 52 teams did not report a p-value
h3a_data <- h3a_data[-indx_no_p,] # 

#remove binary response and p-values, as well as responses that are duplicated or can't be used due to too few responses
data <- h3a_data[,c(-1,-14,-15,-17, -18:-20, -26, -27)]

## --------------------
# from p-val to z-val
## --------------------
# 
class(h3a_data$pval)

#hist(qnorm(h3a_data$pval))
data$pval <- qnorm(h3a_data$pval)#qnorm(0.975,mean=0,sd=1)

## --------------------
# scale continuous data
## --------------------

continuous <- c(3, 7, 8,13,19)

#normalizing data
for (i in 1:length(continuous)) {
  contin_data_col <- data[,continuous[i]]
  data[,continuous[i]] <- as.numeric(scale(contin_data_col))
  #hist(scale(contin_data_col))
}

## ------------------------------------------------------------------------------------

# exclude variables that have high collinearity >0.6
collinearity_indx <- match(c("ans_software_host", "ans_hf_direction","mt_method_h3a"), colnames(data)) # "mt_method_h2a", "ans_hf_direction"

data_reduced <- data[,-collinearity_indx] 
#find the ones above 0.6
all_cor <- model.matrix(~0+., data=data_reduced) %>% 
  cor(use="pairwise.complete.obs")
which(abs(all_cor)>0.8 & abs(all_cor)!=1, arr.ind=TRUE) # 

#---------------------------------------------------------------------------------------------------------------------------
## --------------------
# look if there are any NaN values
## --------------------
# 
empty_row <- which(is.na(data_reduced),arr.ind=TRUE) # 
indx_row <- empty_row[,1]#
# Caution: remove them from the dataset
data_reduced <- data_reduced[-indx_row,]
rownames(data_reduced) <- c(1:114)

# Remove an outlier based on Cooks distance
data_reduced <- data_reduced[-105,]

#rename columns
colnames(data_reduced) <- c("software", "high-pass cutoff", "high-pass type", "reference", "sampling freq.",
                            "partic. excl.","seg. excl. crit.","ICA alg.","bad cmp. visual", "bad cmp. plugin",
                            "bs.window length",
                            "stat.method","mult.comp.cor.","Temp. avg.","Spat. avg.","time w. length",
                          "epoch bef. hp.", "lp. bef.correct.", "ref.bef.correct.", "correct. bef. rej.","pval")

#Approach 1. Using the step function.
#Checking different models using the step function. 
fullmodel_h3a<- lm(pval~ . , data = data_reduced)
summary(fullmodel_h3a)
car::vif(fullmodel_h3a)

# Stepwise model
stepwise_model <- stepAIC(fullmodel_h3a, direction = "both", trace = T, k = log(nrow(data_reduced)))
summary(stepwise_model)
summ(stepwise_model, confint = TRUE, digits = 4) # nicer vizualisation
confint(stepwise_model)


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
shapiro.test(resid) # significant, not normal residuals

# kolmogorov-smirnov test
ks.test(scale(resid),'pnorm') # not significant - therefore it's ok

#Homoscedacisity - fine

bptest(stepwise_model) #p-value = 0.1 

# check all plots
par(mfrow=c(2,2))
plot(stepwise_model)
par(mfrow=c(1,1))

#-- PLOT SIGNIFICANT EFFECTS --#
# plot the baseline window length effect on a p-value (although n.s.)
library(ggplot2)

data_reduced$`Spat. avg.` <- as.factor(data_reduced$`Spat. avg.`)
color <- c("#476F84FF", "#A4BED5FF")
tiff("/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/figures/pval_avg_spat_plot_h3a.png", units="in", width=3, height=3, res=100)

ggplot(data=data_reduced, aes(x=`Spat. avg.`, y=pval))+
  geom_boxplot(fill=color)+
  geom_point(alpha = 0.5, position=position_jitter(height=.5, width=.1))+
  theme_minimal()+
  theme(text = element_text(size=20),plot.title = element_text(hjust = 1,size=18))+
  ylab("qnorm(pval)")+
  xlab("spatial avg.")
  #ggtitle(bquote(beta == .("-0.7") ~ ",p" ==.(0.003)))

dev.off()

