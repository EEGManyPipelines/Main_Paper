# Plot bad cmp
table(data_reduced$ans_bad_comp_sel_plugin)
plot_dat <- data_reduced$ans_bad_comp_sel_plugin

diff_wave$plugin[plot_dat == 1 ] <- "yes"
diff_wave$plugin[plot_dat == 0 ] <- "no"

tiff("C:/Users/ecesnait/Desktop/EEGManyPipelines/Figures/h1_LM_time_series_plugin.png", units="in", width=3, height=4, res=300)

ggplot(diff_wave, aes(x=plugin, y=cpz, fill=plugin)) + 
  geom_boxplot(alpha=0.3) +
  theme(legend.position="none") +
  scale_fill_brewer(palette="GnBu") + xlab('plugin') + ylab('N1 difference')+
  theme(text = element_text(size = 15))
dev.off()

#Plot plugin
plugin <- read.table("C:/Users/ecesnait/Desktop/EEGManyPipelines/git/EEGManyPipes org/Main_Paper/Time-series/Statistical inference/plugin_artifact_correct.csv", 
                   header = T, sep = ";")
plugin <- plugin[which(plugin$ans_bad_comp_sel_plugin==1),]
table(plugin$ans_plugin_desc)
plugin_plot <- as.data.frame(plugin$ans_plugin_desc)
plugin_plot[plugin_plot == "ADJUSTd"] <- "ADJUST"
table(plugin_plot)

ggplot(data.frame(plugin), aes(x=plugin)) +
  geom_bar()

# Plot ref
data_plot <- as.data.frame(data_reduced$reref)
data_plot$amp <- diff_wave$cpz
indx <- which(data_plot$`data_reduced$reref`=='other')
indx2 <- which(data_plot$`data_reduced$reref`=='unknown')
data_plot <- data_plot[-c(indx,indx2),]

tiff("C:/Users/ecesnait/Desktop/EEGManyPipelines/Figures/h1_LM_time_series_reference.png", units="in", width=4, height=4, res=300)

ggplot(data_plot, aes(x=`data_reduced$reref`, y=amp, fill=`data_reduced$reref`)) + 
  geom_boxplot(alpha=0.3) +
  theme(legend.position="none") +
  scale_fill_brewer(palette="BuPu") + xlab('reference') + ylab('N1 difference')+
  theme(text = element_text(size = 15))
dev.off()

# post hoc tests
n1_avg = diff_wave$cpz[data_reduced$reref=='avg']
n1_mastoid = diff_wave$cpz[data_reduced$reref=='mastoid']
n1_orig = diff_wave$cpz[data_reduced$reref=='original']

t.test(n1_avg, n1_mastoid, paired=FALSE) # p = 0.64
t.test(n1_avg, n1_orig, paired=FALSE) # p < 0.0000004
t.test(n1_mastoid, n1_orig, paired=FALSE) # p < 0.0000009


#add fitted regression line to scatterplot
fit <-  lm(pval ~ bs_window_length, data=data_reduced)
#create scatterplot
plot(pval ~ bs_window_length, data=data_reduced)
abline(fit)
summ(fit,confint = TRUE, digits = 4)

#Interaction
ggplot(data=data_reduced, aes(x=bs_window_length, y=pval, group=ans_mt_h1))+
  geom_point(size=2, aes(color=ans_mt_h1))+
  geom_smooth(method= "lm")+
  ylab("qnorm(pval)")+
  xlab("baseline start")+
  ggtitle("Interaction effect")

# with original values
h1_data$ans_mt_h1 <- as.factor(h1_data$ans_mt_h1)
h1_data <- h1_data[!is.na(h1_data$ans_mt_h1),]

table(h1_data$mc_method_h1)
h1_data$mc_method_h1[!h1_data$mc_method_h1 == "permutation"] <- "not_prm"

tiff("C:/Users/ecesnait/Desktop/EEGManyPipelines/Figures/h1_interaction_baseline_mc.png", units="in", width=7, height=4.5, res=300)

ggplot(data=h1_data, aes(x=bs_window_length, y=qnorm(pval), group=ans_mt_h1 ))+
  geom_point(size=4, aes(color=ans_mt_h1, shape = mc_method_h1), alpha = 0.5)+
  geom_smooth(method= "lm")+
  scale_shape_manual(values = c(16,17)) +
  scale_color_manual(values = c("#00AFBB", "#E7B800"))+
  theme_minimal()+
  ylab("qnorm(pval)")+
  xlab("baseline length(ms)")+
  ggtitle("Interaction effect")+ theme(text = element_text(size=15))
dev.off()

# baseline window without the mt
ggplot(data=h1_data, aes(x=bs_window_length, y=qnorm(pval) ))+
  geom_point(size=2)+
  geom_smooth(method= "lm")+
  ylab("qnorm(pval)")+
  xlab("baseline length(ms)")+
  ggtitle("Interaction effect")


## Check assumptions ##

par(mfrow = c(2, 2))
plot(interactionmodel)

