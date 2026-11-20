rm(list = ls())

#install.packages("jtools")
library(jtools)
library(car)
library(ggplot2)

#Loading the data
h3a_data <- read.csv('/Effect sizes/all_var_AQ_h3a_with_cohens_d.csv')

#remove columns of no interest
data <- h3a_data[,c(-1,-10,-13,-14,-24,-25,-28, -35)]

## --------------------
# from p-val to z-val
## --------------------
# 
zval <- qnorm(1 - data$pval/2)

data$cohens_d_v2 <- zval / sqrt(33-data$subj_excluded) # recreate Cohen's d from the table - more data points available

## identify effects going the other direction

indx_neg_effect <- which(data$direction == 'misses>hits')

data$cohens_d_v2[indx_neg_effect] <- data$cohens_d_v2[indx_neg_effect]*-1 # add a direction to cohens_d_v2 column

## plot Cohe's d extracted from a free-text result forms against the one calculated using pval
p<-ggplot(data, aes(x=cohens_d, y=cohens_d_v2)) + 
  geom_point(colour = 4,size = 2, alpha=.6) + theme_minimal() + theme(text=element_text(size=15))+
  geom_vline(xintercept = 0, alpha=.4)+geom_hline(yintercept = 0, alpha=.4)+
 # xlim(-0.6,1.7)+ylim(-0.6,1.7)+
  xlab("Based on RQ")+ylab("Based on AQ") + ggtitle("H3 Cohen's d")
ggsave('/Effect sizes/figures/cohens d scatter H3.png')
