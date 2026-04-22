rm(list = ls())

library(tidyr)
library(ggplot2)
library(paletteer)
library(RColorBrewer)

# Load data
diff_wave_across_subj <- read.csv('/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/data/Main Analysis/Variability results/difference_wave_H1_across_subjects.csv')
diff_wave_across_teams <- read.csv('/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/data/Main Analysis/Variability results/difference_wave_H1_across_teams.csv')

time <- read.csv('/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/data/Main Analysis/SEM variability/standard_time.csv')#("C:/Users/ecesnait/Desktop/EEGManyPipelines/git/EEGManyPipes org/Main_Paper/johannes/timeseries/Elena plot data/standard_time.csv", sep = ",")

#across participants , n=97 teams
nTeams <- nrow(diff_wave_across_subj)
long_time <- time %>% 
  pivot_longer(cols = c(1:205))

long_data_team <- diff_wave_across_subj %>% 
  pivot_longer(cols = c(1:205))
long_data_team$time <-rep(long_time$value, times=nTeams) 
long_data_team$team <-rep(c(1:nTeams), each=205) 


png("/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/figures/diff_wave_bs_cor_across_participants_97teams.png", units="in", width=4, height=3, res=300)
mycolors_team <- paletteer_c("ggthemes::Brown",97)

ggplot(long_data_team, aes(x=time, y=value, group=team, color=as.factor(team))) + 
  geom_line(linewidth=0.7, alpha = 0.3) + 
  scale_color_manual(values = mycolors_team) + ylab(expression(mu * "V"))+ylim(c(-2,3))+
  xlab("Time (ms)")+
  theme_minimal()+theme(text=element_text(size=20), legend.position = "none")+
  geom_vline(xintercept = 0, linetype="dashed", color = "grey", size=1) +
  annotate(geom="text",x=-30, y=2,
           label="stimulus", color = "#696969",angle = 90, size =4)
dev.off()

## -- across teams, n=33 participants ##
nsbj <- 33
long_data_subjects <- as.data.frame(diff_wave_across_teams) %>% 
  pivot_longer(cols = c(1:205))
long_data_subjects$time <-rep(long_time$value, times=33) 
long_data_subjects$sbj <-rep(c(1:33), each=205) 

# Define the number of colors you want

mycolors_part <- paletteer_c("ggthemes::Blue-Teal", 33)

png("/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/figures/diff_wave_bs_cor_across_teams.png", units="in", width=4, height=3, res=300)

ggplot(long_data_subjects, aes(x=time, y=value, group=sbj, color=as.factor(sbj))) + 
  geom_line(linewidth=0.7, alpha = 0.5) + 
  scale_color_manual(values = mycolors_part) + ylab(expression(mu * "V"))+ylim(c(-2,3))+
  xlab("Time (ms)")+
  theme_minimal()+theme(text=element_text(size=20), legend.position = "none")+
  geom_vline(xintercept = 0, linetype="dashed", color = "grey", size=1) +
  annotate(geom="text",x=-30, y=2,
           label="stimulus", color = "#696969",angle = 90, size =4)

dev.off()

## -- Plot Variance -- ##
# Calculate variance for each column (time point)
variance_subject <- apply(diff_wave_across_teams, 2, var)
variance_team <- apply(diff_wave_across_subj, 2, var, na.rm=T)

plot_variance <- data.frame(
  time = long_time$value,
  variance_subject = variance_subject,variance_team=variance_team
)

var_long <- pivot_longer(plot_variance,
                         cols = -time,           # columns to pivot (everything except ID)
                         names_to = "Variance_across", # new column for variable names
                         values_to = "Value")   # new column for values

var_colors <- c("#327399FF","#A53C32FF")

png("/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/figures/difference_wave_variance.png", units="in", width=4, height=3, res=300)

ggplot(var_long, aes(x=time, y=Value, group=Variance_across, color=as.factor(Variance_across))) + 
  geom_line(linewidth=0.7, alpha = 0.9) + 
  scale_color_manual(values = var_colors, labels = c("across teams", "across participants")) + ylab("variance")+
  theme_minimal()+theme(text=element_text(size=20),legend.position = "none")+ylim(c(0,0.6))+
  xlab("Time (ms)")+
  geom_vline(xintercept = 0, linetype="dashed", color = "grey", size=1) +
  annotate(geom="text",x=-30, y=0.2,
           label="stimulus", color = "#696969",angle = 90, size =4)+ labs(color = NULL)

dev.off()
