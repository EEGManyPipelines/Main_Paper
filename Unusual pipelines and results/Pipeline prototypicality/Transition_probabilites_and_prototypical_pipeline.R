rm(list = ls())

#Load packages

library(markovchain) 
library(tidyverse)
library(reshape2)

# Load data
order_script <- read.csv("/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/data/Main Analysis/script_order_168_cor_filt.csv", sep=",")
ID <- order_script$ID
order_step <- order_script[,c(-1,-2,-15:-17)] #remove ID and software, TF
168 - colSums(is.na(order_step))# number of teams that did each step

#remove the steps that were done by less than 1/4 of teams
which(168 - colSums(is.na(order_step)) < 168/4)
order_step$Subj.excl<-c()
order_step$Detrending<- c()

outliers <- c(142,168) # teams that reported 1 or no steps
order_step <- order_step[-outliers,]

#rename columns
colnames(order_step) <- c("downsampling", "high-pass filt.", "low-pass filt.", "notch filt.", "segment excl.",
                          "sensor excl.", "artifact cor.", "re-referencing", "epoching", "baseline cor.")

#create matrix with steps in an ascending order for each team
data_reverse <- matrix(data=NA,nrow=nrow(order_step),ncol=ncol(order_step)+1)

for (val in 1:nrow(order_step)){
  oneTeam <- order_step[val,]
  
  unl_order <- unlist(oneTeam, use.names=FALSE)
  reorder <-oneTeam[order(unl_order)] #order columns based on the report
  
  oneTsteps <- names(reorder[!is.na(unlist(reorder, use.names=F))])#take column names and remove nan
  #oneTsteps <- c(oneTsteps,"end") #add an absorbing state. Turn off for the calculation of a prototypical pipeline
  
  data_reverse[val,c(1:length(oneTsteps))]<- oneTsteps
}

data_reverse_df <-as.data.frame(data_reverse)

# Transition probabilities

mcFit <- markovchainFit(data=data_reverse)
trans_mat<-mcFit$estimate@transitionMatrix
trans_mat <- trans_mat[c(4,1:3,5:10),c(4,1:3,5:10)]# reorder to put End as the last step
mcFit$estimate@transitionMatrix[mcFit$estimate@transitionMatrix<0.1] <- 0

# Transform data to long format
longData<-melt(trans_mat)

#plot
png("/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/figures/transitionMatrix_order_steps.png", units="in", width=7.7, height=5.5, res=300)
par( cex = 2)
ggplot(longData, aes(x = Var2, y = Var1)) + 
  geom_raster(aes(fill=value)) + 
  scale_fill_gradient("Probabilities",low="grey90", high="#EFB967")+ # Adjust this value as needed
  geom_text( aes(x=Var2, y=Var1, label = round(value,2)), size=4)+
  theme_bw() + theme(axis.text.x=element_text(size=18, angle=45, vjust=0.5),
                     axis.text.y=element_text(size=18), axis.title.x = element_blank(),
                     axis.title.y = element_blank(),legend.position = "none",plot.title = element_text(size = 18, face = "bold"),
                     plot.margin = margin(t = 10, r = 15, b = 20, l = 10)) 
# legend.text = element_text(size = 13),legend.position = 'top',
                    # legend.title = element_text(size = 15))
  
dev.off()


#--- Score each team based on how common are the steps they did in that order---#
## -- Parameter-choice prototypicality index -- ##

#Probabilities of making the first step
prob_first <- table(data_reverse[,1])/166
team_score <- c()

for (val in 1:nrow(data_reverse)){
  prob_all = 0
  oneT <- data_reverse[val,] # one team order of steps
  oneT <- oneT[!is.na(oneT)] # remove NaN
  prob_first_team <- as.numeric( prob_first[is.element(names(prob_first), oneT[1])]) # probability of a first step
  prob_all <- c()
  for (i in 1:length(oneT)-1){
    indx_from <- which(is.element(rownames(trans_mat), oneT[i]))
    indx_to <- which(is.element(colnames(trans_mat), oneT[i+1]))
    prob_all[i] <- trans_mat[indx_from,indx_to]
  }
  team_score[val] <- (prob_first_team+sum(prob_all))/length(oneT) # sum probabilities and divide by the number of steps
  rm(oneT)
}

# find a pipeline with the highest transition probability score
which(team_score==max(team_score)) #two teams have an identical order of steps, use one team's data
data_reverse_df[99,]
data_reverse_df[48,]

#plot

team_score_plot <- as.data.frame(team_score)
ggplot(team_score_plot, aes(x=team_score)) +
  geom_histogram( binwidth=0.04, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  theme(text=element_text(size=15))+
  xlab("Mean % per team")


