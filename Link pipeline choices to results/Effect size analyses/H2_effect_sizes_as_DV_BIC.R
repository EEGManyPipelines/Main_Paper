rm(list = ls())

#install.packages("jtools")
library(jtools)
library(car)
library(ggplot2)
library(MASS)
library(gridExtra)

#Loading the data
h2a_data <- read.csv('/Effect sizes/all_var_AQ_h2a_with_cohens_d.csv')

#remove columns of no interest
data <- h2a_data[,c(-1,-10,-13,-14,-24,-25,-28, -35)]

## --------------------
# from p-val to z-val
## --------------------
# 
# remove absolute 0 values
indx_zero <- which(data$pval==0) # 23 teams
data <- data[-indx_zero,] 

#hist(qnorm(h2a_data$pval))
zval <- qnorm(1 - data$pval/2)

data$cohens_d_v2 <- zval / sqrt(33-data$subj_excluded) # recreate Cohen's d from the table - more data points available

## identify effects going the other direction

indx_neg_effect <- which(data$direction == 'old>new (old larger amplitude)')

data$cohens_d_v2[indx_neg_effect] <- data$cohens_d_v2[indx_neg_effect]*-1 # add a direction to cohens_d_v2 column

## plot Cohe's d extracted from a free-text result forms against the one calculated using pval
p<-ggplot(data, aes(x=cohens_d, y=cohens_d_v2)) + 
  geom_point(colour = 4,size = 2, alpha=.6) + theme_minimal() +theme(text=element_text(size=15))+
  geom_vline(xintercept = 0, alpha=.4)+geom_hline(yintercept = 0, alpha=.4)+
  xlim(-2.1,2.1)+ylim(-2.1,2.1)+
  xlab("Based on RQ")+ylab("Based on AQ") + ggtitle("H2 Cohen's d")
#ggsave('/Effect sizes/figures/cohens d scatter H2.png')

##remove cohens_d_old and direction columns 
data <- data[,c(-21,-27,-28)]
## --------------------
# scale continuous data
## --------------------

continuous <- c(3, 7, 8,9, 20)

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
## ------------------------------------------------------------------------------------

# exclude variables that have high collinearity >0.6
collinearity_indx <- match(c("ans_software_host"), colnames(data)) # "topo_region_h2a","software",

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
colnames(data_reduced) <- c("software", "high-pass cutoff", "high-pass type","direction","reference", "sampling freq.",
                            "partic. excl.",  "nr. channels", "topo. region",
                            "stat.method","mult.comp.cor.","Temp. avg.","ROI avg.",
                            "seg. excl. crit.","ICA alg.","bad cmp. visual", "bad cmp. plugin",
                            "bs.window length","epoch bef. hp.", "lp. bef.correct.", "ref.bef.correct.", "correct. bef. rej.","dval")

data_reduced <- data_reduced[-which(data_reduced$software=="brainstorm"),] # influential case that drags all the result
data_reduced <- data_reduced[-which(data_reduced$software=="custom"),] # influential case that drags all the result

rownames(data_reduced) <- NULL
data_reduced <- data_reduced[-40,]

#Approach 1. Using the step function.
#Checking different models using the step function. 
fullmodel_h2a<- lm(dval~ . , data = data_reduced)
summary(fullmodel_h2a)
car::vif(fullmodel_h2a)

# Stepwise model

stepwise_model <- stepAIC(fullmodel_h2a, direction = "both", trace = T, k = log(nrow(data_reduced)))
summary(stepwise_model)

txt <- paste(capture.output(summary(stepwise_model)), collapse = "\n")

png("/Users/ecesnaite/Desktop/lm_output.png", width = 1000, height = 1600, res = 150)
grid::grid.text(txt, x = 0.01, y = 0.99, just = c("left","top"),
                gp = grid::gpar(fontfamily = "mono", fontsize = 9))
dev.off()

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

#Homoscedacisity 

bptest(stepwise_model) #




