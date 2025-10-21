rm(list = ls())

Sys.setenv(LANG = "en")
#install.packages(c("stringdist","reshape2"))# try on real data
library(stringdist)
library(tidyverse)
library(reshape2)
library(ggExtra)

order_script <- read.csv('/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/data/Main Analysis/script_order_168_cor_filt.csv')
  #read.csv("C:/Users/ecesnait/Desktop/EEGManyPipelines/Data/Analysis questionnaire/script_order_168.csv", sep=",")
ID_order <- order_script$ID


order_step <- order_script[,c(-1,-2,-15:-17)] #remove ID and software, TF steps
168 - colSums(is.na(order_step))# number of teams that did each step


#remove the steps that were done by less than 1/4 of teams
which(168 - colSums(is.na(order_step)) < 168/4)
order_step$Subj.excl<-c()
order_step$Detrending<- c()
order_step$Spatial.transform <- c()
order_step$TF.baseline <- c()
order_step$TF.decomp <- c()

order_step <- order_step[-168,] # the last team reported only 2 steps
ID_order <- ID_order[-168]


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

outliers <- c(142) # teams that reported 1 or 2 steps
data_reverse <- data_reverse[-outliers,]

ID_order <- ID_order[-outliers]
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

#take the prototypical pipeline 67 based on script 'ec_transition_probabilites.R'
prototyp_pipeline <- data_revers_dummy[67]

# count the number of steps in each pipeline

num_steps <- 10-rowSums(is.na(data_reverse))

#calculate the Levenshtein distance and divide it by the number of steps
lv_dist <- c()
for (i in 1:length(data_revers_dummy)){
  if (i==99) next

  lv_dist[i]<-stringdist( prototyp_pipeline, data_revers_dummy[i], method="lv" ) # shows how many changes you have to make to have the same pipeline
  
}

# write a data frame
lv_distance_order <- data.frame(ID_order,lv_dist)
write.csv(lv_distance_order,file='C:/Users/ecesnait/Desktop/EEGManyPipelines/Data/Pipeline variability/levenstein_distance_order_steps.csv')

# Plot a histogram
tiff("/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/figures/Levenstein distance order of steps.png", units="in", width=4, height=3, res=300)

lv_dist_plot <- as.data.frame(lv_dist)
ggplot(lv_dist_plot, aes(x=lv_dist)) +
  geom_histogram( binwidth=1, fill="#EFB967", color="#e9ecef", alpha=0.9) + 
  theme_bw()+
  theme(text=element_text(size=20))+
  xlab("Levenstein distance")
dev.off()

# plot it as a function of length of steps
plot_distance <- data.frame(num_steps=num_steps, distance=lv_dist)
ggplot(plot_distance,aes(x=num_steps, y=distance)) + 
  geom_point()+xlab("number of steps")+ylab("LV distance")+
  geom_smooth(method=lm)

## Correlate with unusualness of GA ERPs ## 

GA_ERP <- read.csv("C:/Users/ecesnait/Desktop/EEGManyPipelines/git/EEGManyPipes org/Main_Paper/johannes/timeseries/channel_distance_to_median_112.csv", sep=",")
  # NOT BASELINE CORRECTED. read.csv("C:/Users/ecesnait/Desktop/EEGManyPipelines/git/EEGManyPipes org/Main_Paper/johannes/timeseries/Elena plot data/distance_med_GA_ERP_74.csv", sep=",")
# print IDs that do not match and change them to the regular format
ERP_ID <- read.csv("C:/Users/ecesnait/Desktop/EEGManyPipelines/git/EEGManyPipes org/Main_Paper/johannes/timeseries/distance_med_GA_ERP_ID.csv")

ERP_ID[!is.element(ERP_ID$ID,ID_order),1]

ERP_ID$ID[ERP_ID$ID=="19068f1fe266c5e1_1"] <-"19068f1fe266c5e1"
ERP_ID$ID[ERP_ID$ID=="356c77bfd2662b9a_H1"] <-"356c77bfd2662b9a"
ERP_ID$ID[ERP_ID$ID=="TheCodeMechanics"] <- "The Code Mechanics"  
indx_h3 <- which(ERP_ID$ID=="356c77bfd2662b9a_H3")

GA_ERP <- GA_ERP[-indx_h3,]
ERP_ID <- ERP_ID$ID[-indx_h3]

ERP_ID[!is.element(ERP_ID$ID,ID_order),1]
indx_ID <- is.element(ID_order,ERP_ID)
ID_matched <-ID_order[indx_ID]

nm1 <- match(ERP_ID,ID_matched)
ID_matched_new <- ID_matched[nm1]
all(ID_matched_new==ERP_ID)

# reorder the distance array based on the GA ERP
lv_dist_match <- lv_dist[indx_ID]
lv_dist_match <- lv_dist_match[nm1]
library(Hmisc)
hist(GA_ERP[,18])
pval <- c()
rho <- c()
for (i in 1:ncol(GA_ERP)){
  cor_output <- cor.test(lv_dist_match, GA_ERP[,i], method = "spearman", exact = F)
  cor(lv_dist_match, GA_ERP[,i], use="complete.obs", method = 'spearman')
  
  pval[i] <- cor_output$p.value
  rho[i] <- cor_output$estimate
}
indx_pval <- which(pval==min(pval))

# using a different function
cor_output2 <- rcorr(lv_dist_match, GA_ERP[,64] , type = 'spearman')
cor_output2

#plot cor
library("ggpubr")
data=data.frame(lv_dist_match,GA_ERP[,indx_pval])
colnames(data) <- c("lv_dist", "erp_dist")
ggscatter(data, x = "erp_dist", y = "lv_dist", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "ERP dist", ylab = "LV dist")


dataPlot <- cbind(lv_dist_match, GA_ERP[,indx_pval] )
dataPlot <- as.data.frame(dataPlot)

#plot
p1 <- ggplot(dataPlot,aes(x=lv_dist_match, y=V2)) + 
  geom_point()+xlab("Levenshtein distance")+ylab("GA ERP distance")+
  geom_smooth(method=lm) + 
  theme_minimal()+theme(text=element_text(size=15))
ggMarginal(p1, type="density")



# Correlate with Andrea's distance matrix
andrea_dist <- read.csv("C:/Users/ecesnait/Desktop/EEGManyPipelines/git/EEGManyPipes org/Main_Paper/johannes/timeseries/Andrea_distance.csv", sep=",")
as.numeric(andrea_dist)
linMod<-lm(lv_dist ~ as.numeric(andrea_dist[-outliers]))
summary(linMod)

dataPlot2 <- cbind(lv_dist,as.numeric(andrea_dist[-outliers]))
dataPlot2 <- as.data.frame(dataPlot2)

#plot
ggplot(dataPlot2,aes(x=lv_dist, y=V2)) + 
  geom_point()+xlab("Levenshtein distance")+ylab("Graph path distance")+
  geom_smooth(method=lm) + 
  theme_minimal()+theme(text=element_text(size=15))

# GA ERP
GRAPH_DIST <- as.numeric(andrea_dist[-outliers])
GRAPH_DIST <- GRAPH_DIST[indx_ID]
GRAPH_DIST <- GRAPH_DIST[nm1]

linMod<-lm(GRAPH_DIST ~ GA_ERP$med_all1)
summary(linMod)

dataPlot3 <- cbind(GRAPH_DIST,GA_ERP$med_all1)
dataPlot3 <- as.data.frame(dataPlot3)

#plot
ggplot(dataPlot3,aes(x=GRAPH_DIST, y=V2)) + 
  geom_point()+xlab("Graph path distance")+ylab("GA ERP distance")+
  geom_smooth(method=lm) + 
  theme_minimal()+theme(text=element_text(size=15))

