## Plot difference waves for 3 hypotheses
# 
rm(list = ls())

#Load libraries
#install.packages("scico")
library(R.matlab)
library(paletteer)
library(tidyverse)
library(ggplot2)
library(scico)
library(svglite)

# set paths
outDir <- '/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/figures/'
inDir <- '/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/data/for plotting/FINAL 3 H difference wave plotting/'

# file names
all_amp <- c('H1_amplitude_v2_2.mat','H2a_amplitude_v2_2.mat','H3a_amplitude_Fz_2.mat','H3a_amplitude_Pz_2.mat')
all_time <- c('H1_time_xaxis_2.mat','H2a_time_xaxis.mat','H3a_time_xaxis.mat','H3a_time_xaxis.mat')
outName <- c("diff_wave_H1_v2.svg","diff_wave_H2a_v2.svg","diff_wave_H3a_Fz_v2.svg","diff_wave_H3a_Pz_v2.svg")

#variable names
in_amp_name <- c('amp.h1','amp.h2a','amp.h3a', 'amp.h3a')
in_time_name <- c('time.h1','time.h2a', 'time.h3a','time.h3a')

#xmin_rect <- c(80, 300, 300)
#xmax_rect <- c(120, 500, 500)

stimulus_y_all = c(1.5,-1,-2) # y-axis coordinates for 'stimulus' notation
# plot all difference waves in a loop

for (i in c(1:4)) {
  # read in our data
  amp <- readMat(paste0(inDir,all_amp[i]))
  amp <- eval(parse(text=paste0('amp$',in_amp_name[i])))  # rows are teams and columns are time points
  
    amp <- as.data.frame(amp)

  
  time <- readMat(paste0(inDir,all_time[i]))
  time <- eval(parse(text=paste0('time$',in_time_name[i])))
  
  # transform to long format
  long_format_amp <- amp %>% 
    pivot_longer(cols = colnames(amp), 
                 names_to = "condition",
                 values_to = "value")
  
  # add time
  n_teams <- ncol(amp)
  long_format_amp$time <- rep(time, each = n_teams)
  
  #colors
  color <- paletteer_c("ggthemes::Blue", n_teams)
  
  # Plot difference wave
  stimulus_y <- stimulus_y_all[i]
  
  svglite(paste0(outDir,outName[i]), width=5.5, height=3.8)
  
  p <- long_format_amp %>%
    ggplot( aes(x=time, y=value, colour = condition)) +
    geom_line(alpha = 0.6) + 
    scale_color_manual(values = color) +
    theme_bw() +
    theme(text = element_text(size = 25),legend.position="none") + 
    geom_vline(xintercept = 0, linetype="dashed", 
               color = "grey", size=1) +
    ylab(expression(paste(mu,"V"))) + xlab("Time (ms)") + xlim(-100,500)+
    annotate(geom="text",x=-30, y=stimulus_y, label="stimulus", color = "#696969",angle = 90, size =7)# +
    #annotate("rect", xmin = xmin_rect[i], xmax = xmax_rect[i], ymin = -Inf, ymax = Inf, alpha = .1)
  print(p)
dev.off()  
}
