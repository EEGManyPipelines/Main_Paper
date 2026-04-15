## Correlate with unusualness of difference waves ## 
## --------------------
# Correlated mean difference waves of 3 hypotheses with Levenstein distance in a loop
## --------------------
# 

rm(list = ls())

library(R.matlab)
library(paletteer)
library(RColorBrewer)
library(tidyverse)
library(ggplot2)

# To color code difference waves based on the high-pass or reference? 'ref' or 'hf' or 'bad_cmp'
color_value <- c('ref')
outName <- c(paste0("avg_diff_wave_H1_",color_value,".png"), paste0("avg_diff_wave_H2a_",color_value,".png"),
             paste0("avg_diff_wave_H3a_Fz_",color_value,".png"), paste0("avg_diff_wave_H3a_Pz_",color_value,".png"))
stim_y_coord <- c(0.8,-0.5,-1)#for reference channel: c(0.8,-0.5,-1)#  # for other: c(0.3,-0.25,-0.5)

#Load data
# Mean difference wave
# set paths
inDir <- '/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/data/for plotting/FINAL 3 H difference wave plotting/'

all_amp <- c('H1_amplitude.mat','H2a_amplitude_v2.mat','H3a_amplitude_reversed_Fz.mat', 'H3a_amplitude_reversed_Pz.mat')
all_time <- c('H1_time_xaxis.mat','H2a_time_xaxis.mat','H3a_time_xaxis.mat','H3a_time_xaxis.mat')
all_IDs <- c('IDs_h1_diff_wave.mat','IDs_h2a_diff_wave.mat', 'IDs_h3a_diff_wave.mat', 'IDs_h3a_diff_wave.mat')

in_amp_name <- c('all.y','amp.h2a','amp.h3a','amp.h3a')
in_time_name <- c('time.x','time.h2a', 'time.h3a', 'time.h3a')
in_ID_name <- c('IDs.H1','IDs.h2a','IDs.h3a','IDs.h3a')

xmin_rect <- c(80, 300, 300, 300)
xmax_rect <- c(120, 500, 500, 500)

# reference channel
AQ_data <- read.csv('/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/data/Main Analysis/all_var_AQ_h1_corrected.csv')
ref <- AQ_data$reref
ref[ref=="other"] <- "unknown"

#high-pass cutoff
hf <- AQ_data$hf_cutoff

#correct values based on other hypothesis reports for ERP hypotheses
ref[42] <- "mastoid"

# component plugin
bad_cmp <- AQ_data$ans_bad_comp_sel_plugin

AQ_ID <- read.csv("/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/data/Main Analysis/IDs_linear_models.csv")
outDir <- "/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/figures/"

for (i in c(1:4)) {
  
  # read in our data
  amp <- readMat(paste0(inDir,all_amp[i]))
  amp <- eval(parse(text=paste0('amp$',in_amp_name[i])))  # rows are teams and columns are time points
  
  if (i==1) {
    amp <- as.data.frame(t(amp))
  } else{
    amp <- as.data.frame(amp)
  }
  
  #find outliers and remove them: 3*IQR
  mean_wave_amp <- colMeans(amp, na.rm = T)
  qnt <- quantile(mean_wave_amp, probs=c(.25, .75), na.rm = T)
  H <- 3 * IQR(mean_wave_amp, na.rm = T)
  indx_rm <- c(which(mean_wave_amp < (qnt[1] - H)), which(mean_wave_amp > (qnt[2] + H)))
  
  
  time <- readMat(paste0(inDir,all_time[i]))
  time <- eval(parse(text=paste0('time$',in_time_name[i])))
  
  # 
  ID_diff <- readMat(paste0(inDir,all_IDs[i]))
  ID_diff <- unlist(eval(parse(text=paste0('ID_diff$',in_ID_name[i]))) )
  
  if (!is_empty(indx_rm)){
    amp <- amp[,-indx_rm]
    ID_diff <- ID_diff[-indx_rm]
  }
  
  if (color_value=='ref'){
    current_AQ <- ref
  }else if (color_value=='bad_cmp'){
    current_AQ <- bad_cmp
  }else{
    current_AQ <- hf
  }
  
  # Match ID
  ID_AQ <- AQ_ID$teamID
  loc <- match(ID_diff,ID_AQ, nomatch = 0)
  setdiff(ID_diff,ID_AQ)
  
  if (i==1){
    ID_diff[which(ID_diff %in% "TheCodeMechanics")] <- "The Code Mechanics"
    ID_diff[which(ID_diff %in% "19068f1fe266c5e1_1")] <- "19068f1fe266c5e1"
    ID_diff[which(ID_diff %in% "356c77bfd2662b9a_H1")] <- "356c77bfd2662b9a"
    
  }else {
    ID_diff[which(ID_diff %in% "TheCodeMechanics")] <- "The Code Mechanics"
    ID_diff[which(ID_diff %in% "19068f1fe266c5e1_1")] <- "19068f1fe266c5e1"
    ID_diff[which(ID_diff %in% "ChileMaule")] <- "Chile Maule"
    ID_diff[which(ID_diff %in% "CognitiveSystems_KU")] <- "CognitiveSystems-KU"
    ID_diff[which(ID_diff %in% "TMS_EEG_DREAM")] <- "TMS-EEG-DREAM"
    
  } 
  
  loc <- match(ID_diff,ID_AQ, nomatch = 0)
  ID_matched <- ID_AQ[loc]
  
  if (!identical(ID_matched, as.character(ID_diff))) stop("IDs are not matching")
  
  data_matched_AQ <- current_AQ[loc]
  
  # get mean difference waves
  table(data_matched_AQ)
  if (color_value=='ref'){
    indx_unknown <- data_matched_AQ=="unknown"
    
    #remove unknown
    amp <- amp[,-which(indx_unknown)] 
    data_matched_AQ <- data_matched_AQ[-which(indx_unknown)]
    
    #avg ref
    indx_avg <- data_matched_AQ=="avg"
    mean_amp_avg <- apply(amp[,which(indx_avg)], 1, function(x) median(x, na.rm = TRUE))
    se_amp_avg <- apply(amp[,which(indx_avg)], 1, function(x) sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x))))
    # POz
    indx_poz <- data_matched_AQ=="original"
    mean_amp_poz <- apply(amp[,which(indx_poz)], 1, function(x) median(x, na.rm = TRUE))#rowMeans(amp[,which(indx_poz)], na.rm=T)
    se_amp_poz <- apply(amp[,which(indx_poz)], 1, function(x) sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x))))
    
    #Mastoid
    indx_mastoid <- data_matched_AQ=="mastoid"
    mean_amp_mastoid <- apply(amp[,which(indx_mastoid)], 1, function(x) median(x, na.rm = TRUE))
    se_amp_mastoid <- apply(amp[,which(indx_mastoid)], 1, function(x) sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x))))
    
    
    #join data
    joint_for_amp <- data.frame(mean_amp_avg,mean_amp_poz,mean_amp_mastoid)
    joint_for_se <- data.frame(se_amp_avg,se_amp_poz,se_amp_mastoid)
    
    # transform to long format
    long_format_amp <- joint_for_amp %>% 
      pivot_longer(cols = colnames(joint_for_amp), 
                   names_to = "condition",
                   values_to = "value")
    long_format_se <- joint_for_se %>% 
      pivot_longer(cols = colnames(joint_for_se), 
                   names_to = "condition",
                   values_to = "value")
    joined_plotting <- cbind(long_format_amp,long_format_se)
    # add time
    joined_plotting$time <- rep(time, each = 3)
    #add condition
    if (i==1){
      joined_plotting$color <- rep(c("avg","POz", "mastoid"), 151)
    }else {
      joined_plotting$color <- rep(c("avg","POz", "mastoid"), 205)
    }
    
  }else if (color_value=='bad_cmp'){
    #calculate median amplitude for teams that used and did not use bad component selection plugin
    indx_yes <- as.logical(data_matched_AQ)
    indx_no <- !as.logical(data_matched_AQ)
    
    mean_amp_plugin <- apply(amp[,which(indx_yes)], 1, function(x) median(x, na.rm = TRUE))
    se_amp_plugin <- apply(amp[,which(indx_yes)], 1, function(x) sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x))))
    
    mean_amp_no_plugin <- apply(amp[,which(indx_no)], 1, function(x) median(x, na.rm = TRUE))
    se_amp_no_plugin <- apply(amp[,which(indx_no)], 1, function(x) sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x))))
   
     #join data
    joint_for_amp <- data.frame(mean_amp_plugin,mean_amp_no_plugin)
    joint_for_se <- data.frame(se_amp_plugin,se_amp_no_plugin)
    
    # transform to long format
    long_format_amp <- joint_for_amp %>% 
      pivot_longer(cols = colnames(joint_for_amp), 
                   names_to = "condition",
                   values_to = "value")
    long_format_se <- joint_for_se %>% 
      pivot_longer(cols = colnames(joint_for_se), 
                   names_to = "condition",
                   values_to = "value")
    
    joined_plotting <- cbind(long_format_amp,long_format_se)
    
    # add time
    joined_plotting$time <- rep(time, each = 2)
    
    #add condition
    if (i==1){
      joined_plotting$color <- rep(c("yes","no"), 151) # hypothesis 1 has a shorter baseline length
    }else {
      joined_plotting$color <- rep(c("yes","no"), 205)
    }
    
  }else{

    #add amplitude
    avg_below <- apply(amp[,which(data_matched_AQ<=0.1)], 1, function(x) median(x, na.rm = TRUE)) #rowMeans(amp[,which(data_matched_AQ<=0.1)], na.rm=T) 
    se_below <- apply(amp[,which(data_matched_AQ<=0.1)], 1, function(x) sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x))))
    
    avg_above <- apply(amp[,which(data_matched_AQ>0.1)], 1, function(x) median(x, na.rm = TRUE) )
    se_above <- apply(amp[,which(data_matched_AQ>0.1)], 1, function(x) sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x))))
    #join data
    joint_for_amp <- data.frame(avg_below,avg_above)
    joint_for_se <- data.frame(se_below,se_above)
    
    # transform to long format
    long_format_amp <- joint_for_amp %>% 
      pivot_longer(cols = colnames(joint_for_amp), 
                   names_to = "condition",
                   values_to = "value")
    long_format_se <- joint_for_se %>% 
      pivot_longer(cols = colnames(joint_for_se), 
                   names_to = "condition",
                   values_to = "value")
    joined_plotting <- cbind(long_format_amp,long_format_se)
    
    # add time
    joined_plotting$time <- rep(time, each = 2)
    #add condition
    if (i==1){
      joined_plotting$color <- rep(c('<=0.1','>0.1'), 151)
    }else {
      joined_plotting$color <- rep(c('<=0.1','>0.1'), 205)
    }
    
  }
  
  
  if (color_value=='ref'){
    #change names
    colnames(joined_plotting) <- c("amp_cond", "amp_value","se_cond", "se_value","time","reference")
    # Plot difference wave
    p1 <- ggplot(joined_plotting, aes(x = time, y = amp_value, color = reference, group = reference)) +
      geom_line(size = 1) +                                      # Line for mean amplitude
      geom_ribbon(aes(ymin = amp_value - se_value,      # Shaded SE area
                      ymax = amp_value + se_value,
                      fill = reference), alpha = 0.3, color = NA) + 
      scale_color_manual(values = c("#E69F00","#56B4E9","#009E73")) +        # Grey shades for ribbon fill
      labs(x = "Time (ms)", y = "Amplitude (µV)", 
           title = "Reference choice")  + theme_bw() +
      theme(text = element_text(size = 20),legend.text = element_text(size = 15),     # legend labels
            legend.title = element_text(size = 15,margin = margin(b = 10)) ) + 
      geom_vline(xintercept = 0, linetype="dashed", color = "grey", size=1) +
      geom_hline(yintercept = 0,color = "grey", size=0.5)+
      annotate(geom="text",x=-30, y=stim_y_coord[i],
               label="stimulus", color = "#696969",angle = 90, size =5) +
      annotate("rect", xmin = xmin_rect[i], xmax = xmax_rect[i], ymin = -Inf, ymax = Inf, alpha = .1)
    
  } else if (color_value=='bad_cmp'){
    #change names
    colnames(joined_plotting) <- c("amp_cond", "amp_value","se_cond", "se_value","time","plugin use")
    # Plot difference wave
    p1 <- ggplot(joined_plotting, aes(x = time, y = amp_value, color = `plugin use`, group = `plugin use`)) +
      geom_line(size = 1) +                                      # Line for mean amplitude
      geom_ribbon(aes(ymin = amp_value - se_value,      # Shaded SE area
                      ymax = amp_value + se_value,
                      fill = `plugin use`), alpha = 0.3, color = NA) + 
      scale_color_manual(values = c("#7876B1", "#20854E")) + 
      scale_fill_manual(values = c("#7876B1", "#20854E")) +# Grey shades for ribbon fill
      labs(x = "Time (ms)", y = "Amplitude (µV)", 
           title = "Component plugin use")  + theme_bw() +
      theme(text = element_text(size = 20),legend.text = element_text(size = 15),     # legend labels
            legend.title = element_text(size = 15,margin = margin(b = 10)) ) + 
      geom_vline(xintercept = 0, linetype="dashed", color = "grey", size=1) +
      geom_hline(yintercept = 0,color = "grey", size=0.5)+
      annotate(geom="text",x=-30, y=stim_y_coord[i],
               label="stimulus", color = "#696969",angle = 90, size =5) +
      annotate("rect", xmin = xmin_rect[i], xmax = xmax_rect[i], ymin = -Inf, ymax = Inf, alpha = .1) 
  }else{
    #change names
    colnames(joined_plotting) <- c("amp_cond", "amp_value","se_cond", "se_value","time","hp.cutoff")
    # Plot difference wave
     #"#E69F00", "#56B4E9", "#009E73","#E41A1C" "#377EB8" "#4DAF4A" "#AF46B4"
    p1 <- ggplot(joined_plotting, aes(x = time, y = amp_value, color = hp.cutoff, group = hp.cutoff)) +
      geom_line(size = 1) +                                      # Line for mean amplitude
      geom_ribbon(aes(ymin = amp_value - se_value,      # Shaded SE area
                      ymax = amp_value + se_value,
                      fill = hp.cutoff), alpha = 0.3, color = NA) + 
      scale_color_manual(values = c("#E41A1C", "#377EB8")) + 
      scale_fill_manual(values = c("#E41A1C", "#377EB8"))+# Grey shades for ribbon fill
      labs(x = "Time (ms)", y = "Amplitude (µV)", 
           title = "High-pass filter cutoff")  + theme_bw() +
      theme(text = element_text(size = 20),legend.text = element_text(size = 15),     # legend labels
            legend.title = element_text(size = 15,margin = margin(b = 10)) ) + 
     # scale_y_continuous(breaks = seq(-0.6, 0.5, by = 0.2),
                  #       labels = function(x) round(x, 1))+
      geom_vline(xintercept = 0, linetype="dashed", color = "grey", size=1) +
      geom_hline(yintercept = 0,color = "grey", size=0.5)+
      annotate(geom="text",x=-30, y=stim_y_coord[i],
               label="stimulus", color = "#696969",angle = 90, size =5) +
      annotate("rect", xmin = xmin_rect[i], xmax = xmax_rect[i], ymin = -Inf, ymax = Inf, alpha = .1) 
  }
  
    ggsave(p1, file=paste0(outDir,outName[i]), width=6.5, height=3.8, units = "in")
  
}
