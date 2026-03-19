rm(list = ls())

#install.packages("jtools")

library(readxl)
library(ggplot2)
library(tidyverse)

# load data of all 3 hypotheses

h1_data <- read.csv("C:/Users/ecesnait/Desktop/EEGManyPipelines/Data/Big Analysis/all_var_AQ_h1.csv", sep = ';')
h2a_data <- read_excel("C:/Users/ecesnait/Desktop/EEGManyPipelines/Data/Big Analysis/variables_h2a_0801.xlsx", col_names = T)
h3a_data <- read_excel("C:/Users/ecesnait/Desktop/EEGManyPipelines/Data/Big Analysis/variables_h3a_0402.xlsx", col_names = T)

# H1
#data_corrected_pvalM <- gsub(",", ".", h1_data$pval_med)
#pval_h1 <- as.numeric(data_corrected_pvalM)

plot_sig_h1 <- matrix(data= NaN, nrow=168 , ncol= 1)
#plot_pval_h1$sig <- "NA"
plot_sig_h1[h1_data$result_h1] <- 'yes'
plot_sig_h1[!h1_data$result_h1] <- 'no'
plot_sig_h1 <- as.data.frame(plot_sig_h1)

color <- c("#476F84FF","#A4BED5FF", "#023743FF")

tiff("C:/Users/ecesnait/Desktop/EEGManyPipelines/Figures/h1_confirmed_barplot.png", units="in", width=5, height=2, res=100)

ggplot(plot_sig_h1, aes(x=as.factor(V1), fill=as.factor(V1) )) + 
  geom_bar( ) +
  coord_flip()   + theme_minimal()+
  scale_x_discrete(limits = c("NaN", "no","yes"))+ scale_fill_manual(values=color)+
  theme(text = element_text(size = 20),legend.position="none",axis.text = element_text(size=20))+
  labs(x="Confirmed",y = "Nr. teams")

dev.off()

# H2a
plot_sig_h2a <- matrix(data= NaN, nrow=168 , ncol= 1)

plot_sig_h2a[h2a_data$sig=='yes'] <- 'yes'
plot_sig_h2a[h2a_data$sig=='no'] <- 'no'
plot_sig_h2a <- as.data.frame(plot_sig_h2a)

color2 <- c( "#023743FF","#476F84FF","#A4BED5FF")

tiff("C:/Users/ecesnait/Desktop/EEGManyPipelines/Figures/h2a_confirmed_barplot.png", units="in", width=5, height=2, res=100)

ggplot(plot_sig_h2a, aes(x=as.factor(V1), fill=as.factor(V1) )) + 
  geom_bar( ) +
  coord_flip() + scale_fill_manual(values=color2)  + theme_minimal()+
  theme(text = element_text(size = 20),legend.position="none",axis.text = element_text(size=20))+
  labs(x="Confirmed",y = "Nr. teams")

dev.off()


# H3a
plot_sig_h3a <- matrix(data= NaN, nrow=168 , ncol= 1)
plot_sig_h3a[h3a_data$sig=='yes'] <- 'yes'
plot_sig_h3a[h3a_data$sig=='no'] <- 'no'
plot_sig_h3a <- as.data.frame(plot_sig_h3a)

tiff("C:/Users/ecesnait/Desktop/EEGManyPipelines/Figures/h3a_confirmed_barplot.png", units="in", width=5, height=2, res=100)

ggplot(plot_sig_h3a, aes(x=as.factor(V1), fill=as.factor(V1) )) + 
  geom_bar( ) +
  coord_flip() + scale_fill_manual(values=color2)  + theme_minimal()+
  theme(text = element_text(size = 20),legend.position="none",axis.text = element_text(size=20))+
  labs(x="Confirmed",y = "Nr. teams")

dev.off()



