rm(list = ls())

Sys.setenv(LANG = "en")
#install.packages(c("stringdist","reshape2"))# try on real data
library(stringdist)
library(tidyverse)
library(reshape2)
library(ggExtra)

order_script <- read.csv("/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/data/Main Analysis/script_order_168_cor_filt.csv", sep=",")
ID_order <- order_script$ID

order_step <- order_script[,c(-1,-2,-15:-17)] #remove ID and software, TF steps
168 - colSums(is.na(order_step))# number of teams that did each step

#remove the steps that were done by less than 1/4 of teams
which(168 - colSums(is.na(order_step)) < 168/4)
order_step$Subj.excl<-c()
order_step$Detrending<- c()

outliers <- c(142,168) # teams that reported 1 or no steps
order_step <- order_step[-outliers,]
ID_order <- ID_order[-outliers]

#create matrix with steps in an ascending order for each team

data_reverse <- matrix(data=NA,nrow=nrow(order_step),ncol=ncol(order_step))

for (val in 1:nrow(order_step)){
  oneTeam <- order_step[val,]
  
  unl_order <- unlist(oneTeam, use.names=FALSE)
  reorder <-oneTeam[order(unl_order)]
  
  oneTsteps <- names(reorder[!is.na(unlist(reorder, use.names=F))])
  data_reverse[val,c(1:length(oneTsteps))]<- oneTsteps
}

data_reverse_df <-as.data.frame(data_reverse)

# calculate the Levenshtein distance - the distance
#between two strings is the minimum number of character substitutions, 
#insertions, and deletions required to transform one string into the other.

# dummy code all processing steps
all_step_names <- names(table(data_reverse))
all_step_names <- as.data.frame(all_step_names)
all_step_names$dummy <- letters[1:nrow(all_step_names)]

data_revers_dummy <- c()
for (d in 1:nrow(data_reverse_df)){
  indx <- match(data_reverse[d,] ,all_step_names$all_step_names)
  if (any(is.na(indx))){
    indx <- indx[-which(is.na(indx))]
  } 
  data_revers_dummy[d] <-  paste0(all_step_names$dummy[indx], collapse = "")
  
}
as.data.frame(data_revers_dummy)

#take the prototypical pipeline 99 based on script 'Transition_probabilites_and_parameter_choice.R'
prototyp_pipeline <- data_revers_dummy[99]

# count the number of steps in each pipeline
num_steps <- 10-rowSums(is.na(data_reverse))

#calculate the Levenshtein distance and divide it by the number of steps
lv_dist <- c()
for (i in 1:length(data_revers_dummy)){

  lv_dist[i]<-stringdist( prototyp_pipeline, data_revers_dummy[i], method="lv" ) # shows how many changes you have to make to have the same pipeline
  
}

mean(lv_dist)
sd(lv_dist)

# write a data frame
lv_distance_order <- data.frame(ID_order,lv_dist)
write.csv(lv_distance_order,file='/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/data/Main Analysis/levenstein_distance_order_steps.csv')

# Plot a histogram
tiff("/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/figures/Levenstein distance order of steps 2.png", units="in", width=4, height=3, res=300)

lv_dist_plot <- as.data.frame(lv_dist)
ggplot(lv_dist_plot, aes(x=lv_dist)) +
  geom_histogram(aes(y = ..count.. / sum(..count..) * 100), binwidth=1, fill="#EFB967", color="#e9ecef", alpha=0.9) + 
  theme_bw()+
  theme(text=element_text(size=20)) +
  scale_x_continuous(breaks = seq(0, 9, by = 1))+
  labs(y = "% of teams",x="Levenshtein distance")
dev.off()

