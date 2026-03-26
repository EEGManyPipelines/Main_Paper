rm(list = ls())

source('/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/git/EEGManyPipelines-personal/R codes/log_output.R')
library(rstudioapi)

script_path <- rstudioapi::getSourceEditorContext()$path
script_name <- basename(script_path)
script_dir  <- dirname(script_path)

################################
# Entropy Analysis

# Install the entropy package 
#install.packages("entropy")
library(ggplot2)
library(entropy)
library(tidyr)
library(dplyr)
#install.packages("survminer")
library(survminer)

# Loading the data
data1 <- read.csv("/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/data/Main Analysis/all_var_AQ_h1_corrected.csv")
data2 <- read.csv("/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/data/Main Analysis/all_var_AQ_h2a_corrected.csv")
data3 <- read.csv('/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/data/Main Analysis/all_var_AQ_h3a_corrected.csv')
  
#take prepricessing steps only
preprocess <- data1[,c(-1,-10,-11,-13:-20, -26:-33)]

#take H1 analysis steps
h1_analysis <- data1[,c(-1:-9,-12,-19,-21:-33)]
h2_analysis <- data2[,c(-1:-10,-20:-32)]
h3_analysis <- data3[,c(-1:-16,-26:-32)]

hypotheses <- c("h1_analysis","h2_analysis","h3_analysis")

# ------------------ START ----------------------------
preprocess <- lapply(preprocess, as.factor)
# Need to convert the factors to character vectors for the entropy package to work
encoded_data_preproc <- lapply(preprocess, function(x) as.numeric(factor(x)))
entropy_out_preproc <- c()

for (ei in 1:length(encoded_data_preproc)){
    prob_one <- table(encoded_data_preproc[ei])/nrow(as.data.frame(encoded_data_preproc[ei]))
    entropy_one <- entropy(prob_one, unit= "log2")
    entropy_out_preproc[ei] <- entropy_one/log2(nrow(table(encoded_data_preproc[ei])))
}

# Calculate rpobabilities for each variable each list
plot_entropy_process <- as.data.frame(entropy_out_preproc)

#for preprocessing
plot_entropy_process$class <- c("software", "software host", "high-pass cutoff", "high-pass type", "high-pass direction","reference", 
                        "sampling freq.", "excl. participants", "segment excl.crit.", 
                        "ICA algorithm", "visual bad comp.sel.", "plugin bad comp.sel.", "baseline start", "baseline stop")

#order based on the values
# Order by value

plot_entropy <- plot_entropy_process[order(plot_entropy_process$entropy_out_preproc, decreasing = TRUE), ]

#plot preprocessing
tiff("/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/figures/entropy preprocess wl.png", units="in", width=7.5, height=6, res=300)

# Higher entropy value indicates a more random/uncertain distribution. A lower value indicates more deterministic, predictable distribution 
par(mar = c(2,15,2,2)) # Set the margin on all sides to 2
barplot(height=plot_entropy$entropy_out_preproc, names=plot_entropy$class, 
        col="#69b3a2", xlim = c(0,1),cex.lab=1, 
        horiz=T, las=1,cex.names=1.75,cex.axis=1.75)+abline(v = 0.98, col="red", lwd=2, lty=2)
dev.off()

log_output(paste0('dir: ', script_dir, '; name:', script_name), "entropy preprocess wl.png")

#same for 3 h analysis part
for (i in 1:3){
  
  #all columns to factors
  analysis <- lapply(eval(parse(text=hypotheses[i])), as.factor)
  
  encoded_data_analyse <- lapply(analysis, function(x) as.numeric(factor(x)))
  entropy_out <- c()
  
    for (ei in 1:length(encoded_data_analyse)){
      prob_one <- table(encoded_data_analyse[ei])/nrow(as.data.frame(encoded_data_analyse[ei]))
      entropy_one <- entropy(prob_one, unit= "log2")
      entropy_out[ei] <- entropy_one/log2(nrow(table(encoded_data_analyse[ei])))
    }
  
    #assign(paste0("entropy_out_",hypotheses[i]),entropy_out)
    if (i==1){
      plot_entropy_analyse <- as.data.frame(entropy_out)
      ## assign names to entropy values
      #for h1 analysis
      plot_entropy_analyse$class <- c("topographic region", "nr.channels", "mult.comp.method", "stat.method", 
                              "spatial avg.","mult.comp.correction", 
                              "time win.start", "time win.end", "temporal avg.")
    }  else{
      
      #for h2a + h3a analysis
      plot_entropy_analyse2 <- as.data.frame(entropy_out)
      plot_entropy_analyse2$class <- c("nr.channels", "topographic region", "time win.start", "time win.end",
                              "stat.method", "mult.comp.correction", "mult.comp.method",
                              "temporal avg.","spatial avg.")
      plot_entropy_analyse <- merge(plot_entropy_analyse, plot_entropy_analyse2, by= "class") 
    }
   
}
colnames(plot_entropy_analyse) <- c("class","H1","H2","H3")

#names(preprocess)
#names(h1_analysis)

long_analysis_entropy <- plot_entropy_analyse %>% 
  pivot_longer(
    cols = "H1":"H3")

#plot analysis entropy

p1 <- long_analysis_entropy %>%
  filter(name == unique(name)[1]) %>%
  ggplot(aes(fill = name, y = value, x = reorder(class, -value))) +
  geom_bar(position = "identity", stat = "identity", alpha = .8) +
  coord_flip() +scale_fill_manual(values = "#FFA07A")+ labs(x = NULL, y = NULL) +
  theme_minimal()+
  theme(legend.position = "none",text = element_text(size = 20))+
  geom_hline(yintercept = 0.98, color = "red", linetype = "dashed", size = 1)

p2 <- long_analysis_entropy %>%
  filter(name == unique(name)[2]) %>%
  ggplot(aes(fill = name, y = value, x = reorder(class, -value))) +
  geom_bar(position = "identity", stat = "identity", alpha = .8) +
  coord_flip() +scale_fill_manual(values = "#FF6347")+labs(x = NULL, y = NULL) +
  theme_minimal()+
  theme(legend.position = "none",text = element_text(size = 20))+geom_hline(yintercept = 0.98, color = "red", linetype = "dashed", size = 1)

p3 <- long_analysis_entropy %>%
  filter(name == unique(name)[3]) %>%
  ggplot(aes(fill = name, y = value, x = reorder(class, -value))) +
  geom_bar(position = "identity", stat = "identity", alpha = .8) +
  coord_flip() +scale_fill_manual(values = "#B22222")+labs(x = NULL, y = NULL) +
  theme_minimal()+
  theme(legend.position = "none",text = element_text(size = 20))+geom_hline(yintercept = 0.98, color = "red", linetype = "dashed", size = 1)

ggarrange(p1,p2,p3,nrow = 3,ncol = 1) #nrow & ncol depend on how you want to 

tiff("/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/figures/entropy analysis wl 3H.png", 
     units="in", width=6.5, height=3.8, res=300)

ggplot(long_analysis_entropy,
       aes(x = reorder(class, -value),
           y = value,
           fill = name)) +     # <-- fill is mapped to `name`!
  geom_bar(position="dodge", stat="identity", alpha=.7)+   # geom_col is shorthand for geom_bar(stat="identity")
  coord_flip() +
  theme_classic2() + scale_fill_manual(name=NULL, values=c("#FFA07A", "#FF6347" ,"#B22222"))+
  theme(text = element_text(size = 25),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(), legend.position = "top") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  geom_hline(yintercept = 0.98, color = "red", linetype = "dashed", size = 1)

dev.off()

# Check if the normalized entropy at 0.95 is statistically different from uniform
preprocess <- data1[,c(-1,-10,-11,-13:-20, -26:-33)]
sum(preprocess$ans_bad_comp_sel_visual)
binom.test(91, 168, p = 0.5)

sum(preprocess$ans_bad_comp_sel_plugin)
binom.test(69, 168, p = 0.5)

sum(h2_analysis$temp_roi_avg_h2a, na.rm = T)
binom.test(101, 168, p = 0.5)
