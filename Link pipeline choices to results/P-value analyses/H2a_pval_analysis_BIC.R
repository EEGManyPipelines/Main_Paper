rm(list = ls())

#install.packages("jtools")
library(jtools)
library(car)
library(ggplot2)

#Loading the data
h2a_data <- read.csv('/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/data/Main Analysis/all_var_AQ_h2a_corrected.csv')

# Remove teams that didn't report the p-value
indx_no_p <- which(is.na(h2a_data$pval)) 
h2a_data <- h2a_data[-indx_no_p,] # 20 teams had no p-value report

# remove absolute 0 values
indx_zero <- which(h2a_data$pval==0) # 23 teams
h2a_data <- h2a_data[-indx_zero,] 

#remove columns of no interest
data <- h2a_data[,c(-1,-10,-13,-14,-24,-25,-28)]

## --------------------
# from p-val to z-val
## --------------------
# 
class(h2a_data$pval)

#hist(qnorm(h2a_data$pval))
data$pval <- qnorm(h2a_data$pval)#qnorm(0.975,mean=0,sd=1)


## --------------------
# scale continuous data
## --------------------
# 

continuous <- c(3, 7, 8,9, 20)

#normalizing data
for (i in 1:length(continuous)) {
  contin_data_col <- data[,continuous[i]]
  data[,continuous[i]] <- as.numeric(scale(contin_data_col))
  #hist(scale(contin_data_col))
}

## ------------------------------------------------------------------------------------

# exclude variables that have high collinearity >0.6
collinearity_indx <- match(c("ans_software_host",  "ans_hf_direction","mt_method_h2a"), colnames(data)) # "topo_region_h2a","software",

data_reduced <- data[,-collinearity_indx] 

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

#rename columns
colnames(data_reduced) <- c("software", "high-pass cutoff", "high-pass type", "reference", "sampling freq.",
                            "partic. excl.",  "nr. channels", "topo. region",
                            "stat.method","mult.comp.cor.","Temp. avg.","ROI avg.",
                            "seg. excl. crit.","ICA alg.","bad cmp. visual", "bad cmp. plugin",
                            "bs.window length","pval","epoch bef. hp.", "lp. bef.correct.", "ref.bef.correct.", "correct. bef. rej.")
#Approach 1. Using the step function.
#Checking different models using the step function. 
fullmodel_h2a<- lm(pval~ . , data = data_reduced)
summary(fullmodel)
car::vif(fullmodel)

# Stepwise model
library(MASS)
stepwise_model <- stepAIC(fullmodel, direction = "both", trace = T, k = log(nrow(data_reduced)))
summary(stepwise_model)



