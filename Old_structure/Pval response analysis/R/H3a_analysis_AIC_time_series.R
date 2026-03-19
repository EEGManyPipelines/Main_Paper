rm(list = ls())

#install.packages("jtools")
library(jtools)
library(readxl)
library(car)
library(ggplot2)

#Loading the data
dataPath <- "/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/data/variables_h3a_0402.xlsx"
h3a_data <- read_excel(dataPath, col_names = TRUE)
h3a_data <- as.data.frame(h3a_data)


# add order of steps
order_steps <- read.csv('/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/data/Main Analysis/Script_Order_Sorted.csv')
matched_ID <- match(order_steps$ID, h3a_data$Team)
which(is.na(matched_ID))
order_steps$ID[166] <- "Jack Lab"

match_ID2 <- match(h3a_data$Team,order_steps$ID)
order_steps_matched <- order_steps[match_ID2,]

order_steps_matched$ID == h3a_data$Team

h3a_data <- as.data.frame(c(h3a_data, order_steps_matched[,20:23]*1))

## --------------------
# Correct values
## --------------------

# Software
h3a_data$software[h3a_data$software== 'eeglab_erplab'] <- 'eeglab'
h3a_data$software[h3a_data$software== 'eeglab_limo'] <- 'eeglab'

#Fill in empty responses
# High-pass filter type & direction
h3a_data$ans_hf_type[h3a_data$ans_hf_type== ''] <- 'unknown'
h3a_data$ans_hf_direction[h3a_data$ans_hf_direction == ''] <- 'unknown'

## --------------------
# outliers removal
## --------------------
# 
# 3hz high pass has been removed beforehand
class(h3a_data$hf_cutoff)
#data$hf_cutoff <- as.numeric(gsub(",", ".", gsub("\\.", "", data$hf_cutoff)))
#hf_out <- boxplot(h2a_data$hf_cutoff)$out
h3a_data$hf_cutoff[is.na(h3a_data$hf_cutoff)] <- mean(h3a_data$hf_cutoff, na.rm = T) # 

# ICA algorythm
h3a_data$ans_ica_algo[is.na(h3a_data$ans_ica_algo)] <- 'unknown'
h3a_data$ans_ica_algo[h3a_data$ans_ica_algo=='runica'] <- 'infomax'

#Baseline start and stop: use baseline window length instead
h3a_data$bs_window_length[is.na(h3a_data$bs_window_length)] <- mean(h3a_data$bs_window_length, na.rm = T) 

# remove number channels. Do not play a role

data <- h3a_data[,c(-1,-14,-15,-17:-27)]
pipe_ID <- h3a_data$Team

data_plot <- h3a_data[,c(-1,-14,-15,-17:-27)]

## --------------------
# scale continuous data
## --------------------

continuous <- c(3, 7, 8, 13)

#normalizing data
for (i in 1:length(continuous)) {
  contin_data_col <- data[,continuous[i]]
  data[,continuous[i]] <- as.numeric(scale(contin_data_col))
  #hist(scale(contin_data_col))
}

## ------------------------------------------------------------------------------------
## Load time-series data
EEG <- read.csv('/Volumes/aebusch/nbuschgold/ecesnait/EMP/EMP time series exp/TimelockAVG_Hyp3a/h3a_difference_wave_LPC.csv')
EEG_ID <- EEG$ID

indx <- pipe_ID %in% EEG_ID
pipe_ID[!indx]
#find IDs that don't exist
indx2 <-  EEG_ID %in% pipe_ID 
EEG_ID[!indx2]

EEG_ID[EEG_ID=='19068f1fe266c5e1_1'] <- '19068f1fe266c5e1'
EEG_ID[EEG_ID=='ChileMaule'] <- 'Chile Maule'
EEG_ID[EEG_ID=='CognitiveSystems_KU'] <- 'CognitiveSystems-KU'
EEG_ID[EEG_ID=='TMS_EEG_DREAM'] <- 'TMS-EEG-DREAM'
EEG_ID[EEG_ID=='TheCodeMechanics']<-  'The Code Mechanics'

indx <- pipe_ID %in% EEG_ID
sum(indx)
pipe_ID_reduced <- pipe_ID[indx]
data_reduced <- data[indx,]
data_plot <- data_plot[indx,]

pl_indx <- match(EEG_ID,pipe_ID_reduced)
all.equal(pipe_ID_reduced[pl_indx], EEG_ID)

data_reduced <- data_reduced[pl_indx,]
data_plot <- data_plot[pl_indx,]

#inspect dependent variable
boxplot(EEG$DW)
data_reduced$diffwave <- EEG$DW
data_plot$diffwave <- EEG$DW

## --------------------
# look if there are any NaN values
## --------------------
# 
empty_row <- which(is.na(data_reduced),arr.ind=TRUE) # 
indx_row <- empty_row[,1]#
# Caution: remove them from the dataset
data_reduced <- data_reduced[-indx_row,]
data_plot <- data_plot[-indx_row,]

## ------------------------------------------------------------------------------------

# exclude variables that have high collinearity >0.6
collinearity_indx <- match(c("ans_software_host"), colnames(data_reduced)) # 37

data_reduced <- data_reduced[,-collinearity_indx] 

#---------------------------------------------------------------------------------------------------------------------------

fullmodel<- lm(diffwave~ . , data = data_reduced)
summary(fullmodel)
car::vif(fullmodel)

# Stepwise model
library(MASS)
stepwise_model <- stepAIC(fullmodel, direction = "both", trace = T, k = log(nrow(data_reduced)))
summary(stepwise_model)


# plot the reference channel and the high pass filter cutoff
library(ggplot2)
tiff("/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/figures/hf_diff_wave_plot_h3a.png", units="in", width=4, height=3, res=100)

ggplot(data=data_plot, aes(x=hf_cutoff, y=diffwave))+
  geom_point(size=2, alpha = 0.5)+
  geom_smooth(method= "lm", colour = "grey")+
  theme_minimal()+ theme(text = element_text(size=20))+
  ylab("diff. wave")+
  xlab("hf cutoff")
dev.off()

# reference
# split based on ref
plot_data_ref <- as.data.frame(data_reduced$reref)
plot_data_ref$diffwave <- data_reduced$diffwave
colnames(plot_data_ref) <- c('ref','diffwave')
plot_data_ref$ref[plot_data_ref$ref=='other'] <- 'unknown'
indx_rm_unknown <- which(plot_data_ref$ref=='unknown')
plot_data_ref <- plot_data_ref[-indx_rm_unknown,]
colors <- c("#E9F7B9","#86D0B9","#4BBAC3")

tiff("/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/figures/ref_diff_wave_plot_h3a.png", units="in", width=3, height=3.5, res=100)

ggplot(plot_data_ref, aes(x=ref, y=diffwave, fill=diffwave)) + 
  geom_boxplot(fill=colors)+
  theme_minimal()+ theme(text = element_text(size=20), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ylab("diff. wave")+
  xlab("reference")

dev.off()  
