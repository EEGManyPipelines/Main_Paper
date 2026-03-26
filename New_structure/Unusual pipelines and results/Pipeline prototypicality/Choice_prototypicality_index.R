rm(list = ls())

#Load libraries
library(dplyr)
library(ggplot2)

# Loading the data
h1_preproc <- read.csv("/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/data/Main Analysis/all_var_AQ_h1_corrected.csv")
preproc <- h1_preproc[,c(-1,-3,-10,-11,-13:-20,-24:-33)] # take only the preprocessing part
preproc <- as.data.frame(preproc)

## --------------------
# transform preprocessing choices to the % of how common they are
## --------------------
all_steps <- colnames(preproc)
trans_percent_all_team <- data.frame(matrix(NA, nrow = 168, ncol = ncol(preproc)))

for (i in 1:ncol(preproc)){
  one_step <- as.data.frame(unlist(preproc[i]))#take one processing step
  
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

png("/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/figures/perc_common_proces_steps.png", units="in", width=4, height=3, res=300)

ggplot(plotData, aes(x=mean_percent_all_team)) +
  theme_bw() +
  geom_histogram( binwidth=0.02, fill="#3A837DFF", color="#e9ecef", alpha=0.9) +
  theme(text=element_text(size=15))+
  xlab("Mean prob.") 
dev.off()

# add team ID and write a file
ID_preproc <- read.csv("/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/data/Main Analysis/IDs_linear_models.csv", sep=",")

choice_prot_index <- cbind(ID_preproc,plotData)
write.csv(choice_prot_index,"/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/data/choice_prototypicality_index_v2.csv")

mean(mean_percent_all_team)
sd(mean_percent_all_team)
