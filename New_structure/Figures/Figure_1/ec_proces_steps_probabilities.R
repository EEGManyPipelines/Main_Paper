rm(list = ls())

library(dplyr)
library(ggplot2)
# Loading the data
h1_preproc <- read.csv("/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/data/Main Analysis/all_var_AQ_h1.csv")
h1_preproc <- h1_preproc[,c(-1,-17,-18,-24,-25:-28)]
h1_preproc <- as.data.frame(h1_preproc)
h1_preproc$ica <- 1
h1_preproc$ica[h1_preproc$ans_ica_algo==""] <- 0 #teams that didn't do ICA

## --------------------
# transform preprocessing choices to the % of how common/uncommon they are
## --------------------
all_steps <- colnames(h1_preproc)
trans_percent_all_team <- data.frame(matrix(NA, nrow = 168, ncol = ncol(h1_preproc)))

for (i in 1:ncol(h1_preproc)){
  one_step <- as.data.frame(unlist(h1_preproc[i]))#take one processing step
  
  appear_nr <- as.data.frame(table(one_step[,1])) # see how many different choices it has
  percent_each_val <- appear_nr$Freq/sum(appear_nr$Freq) # which % of teams chose which answer
  appear_nr$percent <- percent_each_val # 
     
  indx <- match(one_step[,1],appear_nr$Var1)
  
  trans_percent_all_team[,i] <-appear_nr$percent[indx]
}
colnames(trans_percent_all_team) <- all_steps
mean_percent_all_team <- rowMeans(trans_percent_all_team, na.rm = T)

#plot
plotData <- as.data.frame(mean_percent_all_team)

tiff("/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/figures/perc_common_proces_steps.png", units="in", width=4, height=3, res=300)

ggplot(plotData, aes(x=mean_percent_all_team)) +
  theme_bw() +
  geom_histogram( binwidth=0.02, fill="#3A837DFF", color="#e9ecef", alpha=0.9) +
  theme(text=element_text(size=15))+
  xlab("Mean prob.") 
dev.off()

#write a file
ID_preproc <- read.csv("/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/data/Main Analysis/IDs_linear_models.csv", sep=",")

choice_prot_index <- cbind(ID_preproc,plotData)
write.csv(choice_prot_index,"/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/data/choice_prototypicality_index.csv")

mean(mean_percent_all_team)
sd(mean_percent_all_team)

# correlate with GA ERP med distance - OLD
ID_preproc <- read.csv("/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/data/Main Analysis/IDs_linear_models.csv", sep=",")

GA_ERP <- read.csv("/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/data/Main Analysis/Variability results/channel_distance_to_median_112.csv", sep=",")
ID_ERP <- read.csv("/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/data/Main Analysis/Variability results/distance_med_GA_ERP_ID.csv", sep=",")
# NOT BASELINE CORRECTED. read.csv("C:/Users/ecesnait/Desktop/EEGManyPipelines/git/EEGManyPipes org/Main_Paper/johannes/timeseries/Elena plot data/distance_med_GA_ERP_74.csv", sep=",")
# print IDs that do not match and change them to the regular format

ID_ERP[!is.element(ID_ERP$ID,ID_preproc$teamID),1]
ID_ERP$ID[ID_ERP$ID=="19068f1fe266c5e1_1"] <-"19068f1fe266c5e1"
ID_ERP$ID[ID_ERP$ID=="356c77bfd2662b9a_H1"] <-"356c77bfd2662b9a"
ID_ERP$ID[ID_ERP$ID=="TheCodeMechanics"] <- "The Code Mechanics"  
indx_h3 <- which(ID_ERP$ID=="356c77bfd2662b9a_H3")

GA_ERP <- GA_ERP[-indx_h3,]
ID_ERP <- ID_ERP$ID[-indx_h3]

ID_ERP[!is.element(ID_ERP,ID_preproc$teamID)]

indx_ID <- is.element(ID_preproc$teamID,ID_ERP)
ID_matched <-ID_preproc$teamID[indx_ID]

nm1 <- match(ID_ERP,ID_matched)
ID_matched_new <- ID_matched[nm1]
all(ID_matched_new==ID_ERP)


# reorder pre-processing step unusualness according to the distance array 
prob_preproc <- mean_percent_all_team[indx_ID]
prob_preproc <- prob_preproc[nm1]

#loop correlations over channels

pval <- c()
rho <- c()
for (i in 1:ncol(GA_ERP)){
  cor_output <- cor.test(prob_preproc, GA_ERP[,i], method = "spearman", exact = F) #one cor for each channel
  pval[i] <- cor_output$p.value
  rho[i] <- cor_output$estimate
}

indx_pval <- which(pval<=0.01)
chan_label <-c("AF3","AF4","AF7","AF8","AFz","C1","C2","C3","C4","C5","C6","CP1",
               "CP2","CP3","CP4","CP5","CP6","CPz","Cz","F1","F2","F3","F4","F5","F6","F7","F8","FC1",
               "FC2","FC3","FC4","FC5","FC6","FCz","FT7","FT8","Fp1","Fp2","Fpz","Fz","Iz","O1","O2","Oz",
               "P1","P10","P2","P3","P4","P5","P6","P7","P8","P9","PO3","PO4","PO7","PO8","POz","Pz","T7","T8",
               "TP7","TP8")
data_pval <- as.data.frame(pval)
ggplot(data_pval, aes(x = pval)) + 
  geom_histogram()+
  scale_x_continuous(breaks = seq(0, 0.5, 0.1), lim = c(0, 0.5))+
  geom_vline(xintercept = 0.05,
             color = "red", size=1)+
  geom_vline(xintercept = 0.01, linetype="dotted",
             color = "red", size=1)+theme(text=element_text(size=14))
write.csv(estimate_chan,"/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/data/Main Analysis/Variability results/chan_var_preproc_GA_ERP_med_cor.csv", row.names = FALSE)
write.csv(pval_chan,"/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/data/Main Analysis/Variability results/chan_var_preproc_GA_ERP_med_pval.csv", row.names = FALSE)

