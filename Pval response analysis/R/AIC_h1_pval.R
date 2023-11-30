rm(list = ls())
library(tidyverse)
library(broom)
library(readxl)

data <- read_excel("/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/git/EEGManyPipelines-personal/Matlab/Big Analysis/initial_variables_h1_pval.xlsx")
data <- as.data.frame(data)
#correct values
table(data$software)
data$software[data$software== 'eeglab_erplab'] <- 'eeglab'
data$software[data$software== 'eeglab_limo'] <- 'eeglab'

#high collinearity problem removal
data <- data[-which(data$mc_method_h1=='rft'),]

#outliersremoval
hf_out <- boxplot(data$hf_cutoff)$out
data$hf_cutoff[which(data$hf_cutoff == hf_out)] <- mean((data$hf_cutoff), na.rm = T)

# Select only numeric predictors
data <- data[,c(-1,-13)] #delete the row numbers and binary reponse to hypothesis testing
data <- na.omit(data)
mynumericdata <- data %>%
  dplyr::select_if(is.numeric) 

shapiro.test(mynumericdata$hf_cutoff)#no data is normal

hist(qnorm(data$pval))
pval_zscore <- qnorm(data$pval)

indx_inf <- which(!is.finite(pval_zscore))
pval_zscore <- pval_zscore[-indx_inf] # remove the infinite value due to a mistakenly assigned 0 to pval
data <- data[-indx_inf,]

#zscore numeric data
for (i in 1:length(data)) {
  
  if (is.numeric(data[,i])){
    data[,i] <- data[,i]-mean(data[,i])/sd(data[,i])
  }
}
data$pval <- pval_zscore

model <- glm(pval ~., data = data)

# Summarize the model
summary(model)

library(hrbrthemes)
library(viridis)
data %>%
  ggplot( aes(x=mc_method_h1, y=pval, fill=mc_method_h1)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(data = data,color="black", size=0.4, alpha=0.9) +
  theme(legend.position="none", text = element_text(size=20))+
  labs(y= "z-scored p-val", x = "multiple comparissons")

data_org <- read_excel("/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/git/EEGManyPipelines-personal/Matlab/Big Analysis/initial_variables_h1_pval.xlsx")
data_org <- as.data.frame(data_org)
mc_none <- data_org[data_org$mc_method_h1=='none',]
boxplot(mc_none$pval) # non-corrected pvalues on average are bigger (less significant). 
#The only exolanation of this could be that we correct for pvalues only if they are already verz small and we know it will survive multiple comparissons.
#if the pval is close to significcance level (high), it's less likely to correct it?

#check pvalues for true/false h0 testing
# Boxplot basic
data_org %>%
  ggplot( aes(x=result_h1, y=pval, fill=result_h1)) +
  geom_boxplot() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) 

#zscored
data_org %>%
  ggplot( aes(x=result_h1, y=qnorm(pval), fill=result_h1)) +
  geom_boxplot() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) 
