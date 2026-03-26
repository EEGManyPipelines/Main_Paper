rm(list = ls())

#Load libraries
library(R.matlab)
library(paletteer)
library(tidyverse)
library(ggplot2)

# set paths
outDir <- '/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/figures/'
inDir <- '/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/data/for plotting/FINAL 3 H difference wave plotting/'

# file names
all_amp <- c('H1_amplitude.mat','H2a_amplitude.mat','H3a_amplitude_reversed_Fz.mat')
all_time <- c('H1_time_xaxis.mat','H2a_time_xaxis.mat','H3a_time_xaxis.mat')
all_IDs <- c('IDs_h1_diff_wave.mat','IDs_h2a_diff_wave.mat', 'IDs_h3a_diff_wave.mat')

# set variable names inside files
in_amp_name <- c('all.y','amp.h2a','amp.h3a')
in_time_name <- c('time.x','time.h2a', 'time.h3a')
in_ID_name <- c('IDs.H1','IDs.h2a','IDs.h3a')

# set names of figures
outName <- c("LV_diff_wave_H1_abs_median.png","LV_diff_wave_H2a_abs_median.png","LV_diff_wave_H3a_abs_median.png")

#xmin_rect <- c(80, 300, 300)
#xmax_rect <- c(120, 500, 500)

#Levenshtein distance - load files
lv <- read.csv('/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/data/Main Analysis/levenstein_distance_order_steps.csv')
ID_order <- lv$ID_order
lv_dist <- lv$lv_dist

stim_y_coord <- c(0.4,0.25,0.4) # coordinates for the 'stimulus' notation
no_diff_coord <- c(0.05,0.02,0.03)
stat_output = c()
stat_output_choices = c()

# Unusualness of pipeline choices - choice prototypicality index
mean_pipe_prob <- read.csv("/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/data/choice_prototypicality_index_v2.csv")

#match order of IDs
setdiff(mean_pipe_prob$teamID,ID_order)
mean_pipe_prob$teamID[which(mean_pipe_prob$teamID=="Jack Lab")] <- 'JackLab'

loc_indx <- match(ID_order,mean_pipe_prob$teamID, nomatch = 0)
choice_prot_ordered <- mean_pipe_prob[loc_indx,]

identical(choice_prot_ordered$teamID,ID_order)

## plot all difference waves in a loop

for (i in c(1:3)) {
  # read in our data
  amp <- readMat(paste0(inDir,all_amp[i]))
  amp <- eval(parse(text=paste0('amp$',in_amp_name[i])))  # rows are teams and columns are time points
 
   if (i==1) {
    amp <- as.data.frame(t(amp))
  } else {
    amp <- as.data.frame(amp)
  }
  
  #find outliers and remove them: 3*IQR
  mean_wave_amp <- colMeans(amp, na.rm = T)
  qnt <- quantile(mean_wave_amp, probs=c(.25, .75), na.rm = T)
  H <- 3 * IQR(mean_wave_amp, na.rm = T)
  indx_rm <- c(which(mean_wave_amp < (qnt[1] - H)), which(mean_wave_amp > (qnt[2] + H)))
  amp <- amp[,-indx_rm]
  
  time <- readMat(paste0(inDir,all_time[i]))
  time <- eval(parse(text=paste0('time$',in_time_name[i])))
  
  # color code based on Levenshtein distance
  ID <- readMat(paste0(inDir,all_IDs[i]))
  ID <- unlist(eval(parse(text=paste0('ID$',in_ID_name[i]))) )
  ID <- ID[-indx_rm]
 
   # Match ID
  loc <- match(ID,ID_order, nomatch = 0)
  
  setdiff(ID,ID_order)
  
  if (i==1){
    ID_order[which(ID_order %in% "The Code Mechanics")] <- "TheCodeMechanics"
    ID[which(ID %in% "19068f1fe266c5e1_1")] <- "19068f1fe266c5e1"
    ID[which(ID %in% "356c77bfd2662b9a_H1")] <- "356c77bfd2662b9a"
    
  }else {
    ID[which(ID %in% "19068f1fe266c5e1_1")] <- "19068f1fe266c5e1"
    ID[which(ID %in% "ChileMaule")] <- "Chile Maule"
    ID[which(ID %in% "CognitiveSystems_KU")] <- "CognitiveSystems-KU"
    ID[which(ID %in% "TMS_EEG_DREAM")] <- "TMS-EEG-DREAM"
    
  } 
  
  loc <- match(ID,ID_order, nomatch = 0)
  ID_matched <- ID_order[loc]
  
 ID <-  as.data.frame(ID)
 ID <- ID$ID
  if (!identical(ID_matched, ID)) stop("IDs are not matching")
  
  data_matched_lv_dist <- lv_dist[loc]

  # correlate data absolute mean amplitude with lv dist
  time_indx <- which(time>100) # above 100ms past the stimulus
  mean_abs_amp <- apply(abs(amp[time_indx,]), 2, median, na.rm = TRUE) 
  cor_output <- cor.test(data_matched_lv_dist,mean_abs_amp, method = "spearman", 
                                   exact = F) #
  stat_output[i] <- paste0("pval=",cor_output$p.value, ", rho=",cor_output$estimate)
  
  #plot
  data_plot <- data.frame(mean_amp = mean_abs_amp, lv_distance = data_matched_lv_dist)
  
  p <- ggplot(data_plot,aes(x=lv_distance, y=mean_amp)) + 
    geom_point(color="#00AFBB", alpha=0.6)+xlab("Levenshtein distance")+ylab("median abs.diff.wave")+
    geom_smooth(method=lm, color="#00AFBB") + scale_x_continuous(breaks = seq(0, 9, by = 1))+
    theme_minimal()+theme(text=element_text(size=15))+
    scale_color_brewer(palette="Dark2")
  
  ggsave(p, file=paste0("/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/figures/cor_diff_wave_lv_dist",i,".png"), width=4, height=3, units = "in")
  
  # correlate it with the prototypicallity in pipeline choices
  choice_prot_ordered_amp <- choice_prot_ordered$mean_percent_all_team[loc]
  cor_output_choices <- cor.test(choice_prot_ordered_amp, mean_abs_amp, method = "spearman", 
                         exact = F) #
  stat_output_choices[i] <- paste0("pval=",cor_output_choices$p.value,
                                   ", rho=",cor_output_choices$estimate)
 
   #split amplitudes based on low and high LV distance
  indx_low <- which(data_matched_lv_dist < 5)
  indx_high <- which(data_matched_lv_dist >=5)
  
  # calculate a median difference wave per class
  median_wave_low_lv <- apply(abs(amp[,indx_low]), 1, median, na.rm = TRUE) 
  se_wave_low_lv <- apply(abs(amp[,indx_low]), 1, function(x) sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x))))
  
  median_wave_high_lv <- apply(abs(amp[,indx_high]), 1, median, na.rm = TRUE) 
  se_wave_high_lv <- apply(abs(amp[,indx_high]), 1, function(x) sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x))))
  
  median_waves <- data.frame(median_wave_low_lv,median_wave_high_lv)
  se_waves <- data.frame(se_wave_low_lv,se_wave_high_lv)# standard errors
  
  long_median <- median_waves %>% 
    pivot_longer(cols = colnames(median_waves), 
                 names_to = "condition",
                 values_to = "value")
  long_se <- se_waves %>% 
    pivot_longer(cols = colnames(se_waves), 
                 names_to = "condition",
                 values_to = "value") 
  # add a class for colour
  long_median$color[long_median$condition == 'median_wave_low_lv'] <- '0-4'
  long_median$color[long_median$condition == 'median_wave_high_lv']<- '5-9'
  
  joined_plotting <- cbind(long_median,long_se)

  joined_plotting$time <- rep(time, each = 2)
  
  #change names
  colnames(joined_plotting) <- c("amp_cond", "amp_value","color","se_cond", "se_value","time")
  
  # Plot difference wave
  p1 <- joined_plotting %>%
    ggplot( aes(x=time, y=amp_value, group = color, color = color)) +
    geom_line()  +                                      
    geom_ribbon(aes(ymin = amp_value - se_value,      # Shaded SE area
                    ymax = amp_value + se_value,
                    fill = color), alpha = 0.3, color = NA) + 
    scale_color_manual(name="Levenshtein dist.", values = c("#00A087","#8491B4"))+
    scale_fill_manual(name="Levenshtein dist.",values = c("#00A087","#8491B4"))+
    theme_bw() +
    theme(text = element_text(size = 20),legend.text = element_text(size = 15),     # legend labels
          legend.title = element_text(size = 15,margin = margin(b = 10)) ) + 
    geom_vline(xintercept = 0, linetype="dashed", color = "grey", size=1) +
    ylab("abs. diff. wave") +xlab("Time (ms)")+ xlim(-100,500)+
    geom_hline(yintercept = 0,color = "grey", size=0.5)+
    annotate(geom="text",x=-30, y=stim_y_coord[i],
             label="stimulus", color = "#696969",angle = 90, size =5) +
    annotate(geom="text",x=410, y=no_diff_coord[i],
             label="no difference", color = "#696969", size =5) 

  #save
  ggsave(p1, file=paste0(outDir,outName[i]), width=6.5, height=3.8, units = "in")
}

