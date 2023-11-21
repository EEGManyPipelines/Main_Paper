
rm(list = ls())
# --------------
# H2b
# --------------
myData_h2b <-read.csv("/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/git/EEGManyPipelines-personal/Matlab/Big Analysis/independent_variables_for_models_h2b_complete.csv",
                      header= TRUE, sep = ',')

myData_h2b$hf_cutoff <- as.numeric(myData_h2b$hf_cutoff)

myData_h2b$bs_window[myData_h2b$bs_window=="99,22"] <- '99'
myData_h2b$bs_window[myData_h2b$bs_window=="199,22"] <- '199'
myData_h2b$bs_window[myData_h2b$bs_window=="0,2"] <- '200'
table(myData_h2b$bs_window)
myData_h2b$bs_window <- as.numeric(myData_h2b$bs_window)

table(myData_h2b$time_window_interest_h2b)
myData_h2b$time_window_interest_h2b[myData_h2b$time_window_interest_h2b=="210,8"] <- '211'
myData_h2b$time_window_interest_h2b <- as.numeric(myData_h2b$time_window_interest_h2b)


myData_h2b$reref[myData_h2b$reref=="unknown"] <- 'original'
myData_h2b$reref[myData_h2b$reref==""] <- 'original'
myData_h2b$reref[myData_h2b$reref=="average"] <- 'avg'

myData_h2b$software[myData_h2b$software == "EEGLAB"] <- 'eeglab'

myData_h2b$mc_method_h2b[myData_h2b$mc_method_h2b==""] <- 'none'

myData_h2b$ds_fs[is.na(myData_h2b$ds_fs)] <- 512

circle_plots <- myData_h2b[,c(1,3,10,11)]

hist_plots <- myData_h2b[,c(2,4,6,8,9,12:17)]

#install.packages("Hmisc")
library(Hmisc)

tiff("/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/figures/Main Analysis/variability_h2b_circle_1_4.png", units="in", width=5, height=3, res=100)
par(mar = rep(1, 4))
hist.data.frame(circle_plots)

dev.off()

#install.packages("cowplot")
library(cowplot)
library(ggplot2)


par(mar = rep(1, 4))
p_hf <- ggplot(hist_plots, aes(x=hist_plots[,1])) + 
  geom_histogram(color="black", fill="white") + theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous("high-pass cutoff",  breaks = c(0.1, 0.3, 0.5, 1)) + theme(text = element_text(size=15))
p_fs <- ggplot(hist_plots, aes(x=hist_plots[,2])) + 
  geom_histogram(color="black", fill="white") + theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous("sampling rate",  breaks = c(100, 256, 512)) + theme(text = element_text(size=15))

p_bs <- ggplot(hist_plots, aes(x=hist_plots[,3])) + 
  geom_histogram(color="black", fill="white") + theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous("base win length(ms)",  breaks = c(0, 200, 400, 600, 800, 1000)) + theme(text = element_text(size=15))

p_nr_chan <- ggplot(hist_plots, aes(x=hist_plots[,4])) + 
  geom_histogram(color="black", fill="white") + theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous("nr channels",  breaks = c(1, 10, 32, 64)) + theme(text = element_text(size=15))

p_epoch <- ggplot(hist_plots, aes(x=hist_plots[,5])) + 
  geom_histogram(color="black", fill="white") + theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous("epoch length(ms)",  breaks = c(200, 1000, 3000)) + theme(text = element_text(size=15))

p_mc <- ggplot(hist_plots, aes(x=hist_plots[,7])) + 
  geom_bar()  +
  scale_x_continuous("multiple comp",  breaks = c(0,1)) + theme(text = element_text(size=15))

p_avg_sensor <- ggplot(hist_plots, aes(x=hist_plots[,8])) + 
  geom_bar()  +
  scale_x_continuous("avg sensors",  breaks = c(0,1)) + theme(text = element_text(size=15))

p_avg_freq <- ggplot(hist_plots, aes(x=hist_plots[,9])) + 
  geom_bar()  +
  scale_x_continuous("avg freq",  breaks = c(0,1)) + theme(text = element_text(size=15))

p_theta <- ggplot(hist_plots, aes(x=hist_plots[,10])) + 
  geom_histogram(color="black", fill="white")  +
  scale_x_continuous("theta range",  breaks = c(1,3,6,10,15,30)) + theme(text = element_text(size=15))

p_avg_time <- ggplot(hist_plots, aes(x=hist_plots[,11])) + 
  geom_bar() +
  scale_x_continuous("avg time",  breaks = c(0,1)) + theme(text = element_text(size=15))

p_sig <- ggplot(hist_plots, aes(x=hist_plots[,6])) + 
  geom_bar()  +
  scale_x_continuous("confirmed h2b",  breaks = c(0,1)) + theme(text = element_text(size=15))

tiff("/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/figures/Main Analysis/variability_h2b_analysis_hist.png", units="in", width=12, height=5, res=300)

plot_grid(p_hf, p_fs, p_bs, p_nr_chan, p_epoch, p_mc, p_avg_sensor, p_avg_freq, p_theta,
          p_avg_time,p_sig, labels = c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K'))

dev.off()


write.csv(myData_h2b, "/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/git/EEGManyPipelines-personal/Matlab/Big Analysis/independent_variables_for_models_h2b_cor_for_analysis.csv", row.names=FALSE)

# --------------
# H1
# --------------

myData_h1 <-read.csv("C:/Users/ecesnait/Desktop/EEGManyPipelines/Data/Big Analysis/variables_for_models_h1.csv.csv")

myData_h1$hf_cutoff[myData_h1$hf_cutoff == '1 Hz'] <- 1
myData_h1$hf_cutoff <- as.numeric(myData_h1$hf_cutoff)
#bob <- data.frame(lapply(myData_h1[,-12], as.character), stringsAsFactors=FALSE)
#library(Hmisc)
#hist.data.frame(bob)

myData_h1$reref[myData_h1$reref=="unknown"] <- 'original'
myData_h1$reref[myData_h1$reref==""] <- 'original'
myData_h1$reref[myData_h1$reref=="average"] <- 'avg'

myData_h1$software[myData_h1$software == "EEGLAB"] <- 'eeglab'

myData_h1$topo_region_h1[myData_h1$topo_region_h1==""] <- 'not-reported'
myData_h1$stat_method_h1[myData_h1$stat_method_h1==""] <- 'not-reported'

myData_h1$mc_method_h1[myData_h1$mc_method_h1==""] <- 'none'

circle_plots <- myData_h1[,c(1,3,7,10,11)]

hist_plots <- myData_h1[,c(2,4:6,8,9)]

#install.packages("Hmisc")
library(Hmisc)

tiff("C:/Users/ecesnait/Desktop/EEGManyPipelines/git/EEGManyPipelines-personal/Matlab/Result forms/figures/Analysis/variability_h1_analysis_char.png", units="in", width=8, height=6, res=300)
%par(cex=1.5)
hist.data.frame(circle_plots)

dev.off()

#install.packages("cowplot")
library(cowplot)
library(ggplot2)


p_hf <- ggplot(hist_plots, aes(x=hist_plots[,1])) + 
  geom_histogram(color="black", fill="white") + theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous("high-pass cutoff",  breaks = c(0.1, 0.3, 0.5, 3)) + theme(text = element_text(size=15))
p_fs <- ggplot(hist_plots, aes(x=hist_plots[,2])) + 
  geom_histogram(color="black", fill="white") + theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous("sampling rate",  breaks = c(100, 256, 512)) + theme(text = element_text(size=15))
p_sbj_ex <- ggplot(hist_plots, aes(x=hist_plots[,3])) + 
  geom_histogram(color="black", fill="white") + theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous("subjects excl",  breaks = c(1, 5, 10, 15)) + theme(text = element_text(size=15))
p_bs <- ggplot(hist_plots, aes(x=hist_plots[,4])) + 
  geom_histogram(color="black", fill="white") + theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous("base win length(ms)",  breaks = c(0, 200, 400, 600, 800, 1000)) + theme(text = element_text(size=15))
p_nr_chan <- ggplot(hist_plots, aes(x=hist_plots[,5])) + 
  geom_histogram(color="black", fill="white") + theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous("nr channels",  breaks = c(1, 10, 32, 64)) + theme(text = element_text(size=15))
p_epoch <- ggplot(hist_plots, aes(x=hist_plots[,6])) + 
  geom_histogram(color="black", fill="white") + theme(axis.text.x = element_text(angle = 90)) +
  scale_x_continuous("epoch length(ms)",  breaks = c(50, 200, 400, 600, 800, 1000, 3000)) + theme(text = element_text(size=15))

tiff("C:/Users/ecesnait/Desktop/EEGManyPipelines/git/EEGManyPipelines-personal/Matlab/Result forms/figures/Analysis/variability_h1_analysis.png", units="in", width=12, height=5, res=300)

plot_grid(p_hf, p_fs, p_sbj_ex, p_bs, p_nr_chan, p_epoch, labels = c('A', 'B', 'C', 'D', 'E', 'F'))
  
dev.off()


write.csv(myData_h1, "C:/Users/ecesnait/Desktop/EEGManyPipelines/Data/Big Analysis/variables_for_models_h1.csv", row.names=FALSE)

