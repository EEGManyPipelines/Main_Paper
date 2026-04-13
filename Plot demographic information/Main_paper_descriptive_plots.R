rm(list = ls())

library(tidyverse)
library(ggplot2)
library(forcats)
library(RColorBrewer)
library(stringr)
library(readr)
library(paletteer)

##---------- EEG years, papers, seniority ----------------## 
#load data
data <- read.csv('/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/Papers/Main paper/final_data.csv')

# remove the participant who quit after the data analysis phase
data <- data[-305,]

# Barplots for categorical data
#EEG field
table(data$eeg_field)

#Next, extract proportions of current position
data$job_position_recoded <- data$job_position_basedOnDeg
data$job_position_recoded <- as.character(data$job_position_recoded)
data$job_position_recoded[data$job_position_recoded=='Pre-doctoral student'] <- 'Pre-PhD'
data$job_position_recoded[data$job_position_recoded=='PhD student'] <- 'PhD student'
data$job_position_recoded[data$job_position_recoded=='Postdoc'] <- 'Postdoc'
data$job_position_recoded[data$job_position_recoded=='Group leader and faculty'] <- 'Independent group leader or above'
data$job_position_recoded[data$job_position_recoded=='Researchers with PhD'] <- 'Unclassified'
data$job_position_recoded[data$job_position_recoded=='Researcher without PhD'] <- 'Unclassified'
data$job_position_recoded[data$job_position_recoded=='Research assistants'] <- 'Unclassified'
data$job_position_recoded[data$job_position_recoded=='Outside academia'] <- 'Outside academia'
data$job_position_recoded <- as.factor(data$job_position_recoded)

#job position
plot_data <- as.data.frame(table(data$job_position_edit))
plot_data$percent <- as.character(paste0(round(plot_data$Freq/396*100, 1), '%')) # to percentage
plot_data$Freq <- plot_data$Freq/396*100

sum(plot_data$Freq)

plot_data <- plot_data %>%
  arrange(desc(Freq))

# The colors
BLUE <- "#0072B2"

png("/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/Papers/Main paper/Figures/Job_position.png", width = 1200, height = 400, res = 200)

ggplot(plot_data, aes(forcats::fct_reorder(Var1, Freq), Freq)) +
  geom_col(fill = BLUE, width = 0.6)  +
  geom_text(aes(label = forcats::fct_reorder(percent,Freq)), 
            ## make labels left-aligned
            hjust = -0.2, nudge_x = 0) + coord_flip() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.title.x=element_blank(),axis.text=element_text(size=12),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), axis.title.y = element_blank()) + ylim(0,50)

dev.off()

#EEG years
plot_data_years <- as.data.frame(data$eeg_years)
plot_data_years$group <- NaN
plot_data_years$group[plot_data_years$`data$eeg_years`<3] <- 'less than 3'
plot_data_years$group[plot_data_years$`data$eeg_years`>=3 & plot_data_years$`data$eeg_years`<= 6] <- '3-6 years'
plot_data_years$group[plot_data_years$`data$eeg_years`>6 & plot_data_years$`data$eeg_years`<= 10 ] <- '7-10 years'
plot_data_years$group[plot_data_years$`data$eeg_years`>10 ] <- 'more than 10'

plot_data_years_class <- as.data.frame(table(plot_data_years$group))
plot_data_years_class$percent <- as.character(paste0(round(plot_data_years_class$Freq/396*100, 1), '%')) # to percentage

# You can change the border of each area with the classical parameters:
# Compute the position of labels
plot_data_years_class <- plot_data_years_class %>% 
  arrange(desc(Var1)) %>%
  mutate(prop = Freq / sum(plot_data_years_class$Freq) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )

# Prepare a color palette. Here with R color brewer:
RColorBrewer::brewer.pal(8, "YlGnBu")

myPalette <- c("#7FCDBB", "#41B6C4", "#1D91C0", "#225EA8" )

png("/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/Papers/Main paper/Figures/EEG_years.png", width = 1200, height = 800, res = 200)

ggplot(plot_data_years_class, aes(x="", y=prop, fill=Var1)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() +
  theme(legend.title = element_blank(), legend.text=element_text(size=18))+
  geom_text(aes(y = ypos, label = percent), color = "white", size=6) +
  scale_fill_manual(values=myPalette)


dev.off()

# SOFTWARE

AQ <- read.csv("/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/data/Analysis questionnaire final sample 168 corrected.csv", sep = ';')
software <- as.data.frame(table(AQ$ans_main_software))
software$Var1 <- c("BESA","Brainstorm", "BrainVision", "Custom", "EEGLAB", "ERPLAB", "LIMO MEEG", "FieldTrip", 
                   "MNEPython", "Other", "SPM")

# Create dataset
software <- software[order(software$Freq),]
software$id <- c(1:11)

# Set a number of 'empty bar'
empty_bar <- 5

# Add lines to the initial dataset
to_add <- matrix(NA, empty_bar, ncol(software))
colnames(to_add) <- colnames(software)
software <- rbind(software, to_add)
software$id <- seq(1, nrow(software))

# ----- This section prepare a dataframe for labels ---- #
# Get the name and the y position of each label
label_data <- software

# calculate the ANGLE of the labels
number_of_bar <- nrow(label_data)
angle <-  90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)

# calculate the alignment of labels: right or left
# If I am on the left part of the plot, my labels have currently an angle < -90
label_data$hjust<-ifelse( angle < -90, 1, 0)

# flip angle BY to make them readable
label_data$angle<-ifelse(angle < -90, angle+180, angle)
label_data$Var1 <- as.character(label_data$Var1)

# Start the plot
png("/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/figures/softare circular.png", units="in", width=10, height=9, res=100)

ggplot(software, aes(x=as.factor(id), y=Freq)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  
  # This add the bars with a blue color
  geom_bar(stat="identity", fill=alpha("skyblue", 0.7)) +
  
  # Limits of the plot = very important. The negative value controls the size of the inner circle, the positive one is useful to add size over each bar
  ylim(-100,120) +
  
  # Custom the theme: no axis title and no cartesian grid
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm")      # Adjust the margin to make in sort labels are not truncated!
  ) +
  
  # This makes the coordinate polar instead of cartesian.
  coord_polar(start = 0) +
  
  # Add the labels, using the label_data dataframe that we have created before
  geom_text(data=label_data, aes(x=id, y=Freq+20, label=Var1, hjust=hjust), color="black", 
            fontface="bold",alpha=0.6, size=11, angle= label_data$angle, inherit.aes = FALSE ) +
  
  geom_text(data=label_data, aes(x = id, y = -22, label=Freq, hjust=hjust), 
            color = "black", alpha=0.6, size=8, fontface="bold", inherit.aes = FALSE)
dev.off()


# Prepare a color palette. Here with R color brewer:

#paletteer_c("ggthemes::Blue-Green Sequential", 20)
#myPalette <- c("#E7F6B7FF", "#E1F4B3FF", "#D9F1B2FF", "#C8EBB1FF", "#BDE7B2FF","#AEE0B4FF", "#9FDAB6FF",
  "#90D4B8FF", "#83CFBAFF","#5CC0C0FF", "#50BBC2FF","#41B7C4FF")
#myPalette <- paletteer_c("grDevices::Cold", 11)

## TOPICS ##
# write a column to a separate file that will be fed to LLM to derive categories of topics
eeg_topics <- as.data.frame(data$eeg_topics)
#write.csv(data$eeg_topics, "/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/Papers/Main paper/eeg_topics.csv")

# Load your data
data <- eeg_topics

# Define keyword-based rules for each category
category_keywords <- list(
  "Perception & Sensory Processing" = c("perception", "sensory", "visual", "auditory", "erp", "event-related"),
  "Memory & Learning" = c("memory", "learning", "hippocampus"),
  "Language and Communication" = c("language", "syntax", "semantics", "phonology", "linguistic"),
  "Executive Functions & Cognitive Control" = c("decision", "executive", "working memory", "attention", "conflict", "inhibition"),
  "Emotion & Social Cognition" = c("emotion", "empathy", "social", "moral", "theory of mind", "affective"),
  "Motor Control & Action" = c("motor", "movement", "mu rhythm", "beta rhythm", "action"),
  "Resting State and Brain Connectivity" = c("resting state", "connectivity", "network", "graph"),
  "Sleep and Consciousness" = c("sleep", "consciousness", "dream", "lucid"),
  "Neurodevelopment & Aging" = c("child", "development", "aging", "lifespan", "autism", "adhd"),
  "Neurological & Psychiatric Conditions" = c("schizophrenia", "depression", "anxiety", "ptsd", "epilepsy", "parkinson", "clinical"),
  "Methods Development" = c("method", "reliability", "validation", "artifact", "test-retest", "preprocessing"),
  "Brain–Computer Interfaces (BCI)" = c("bci", "brain-computer", "interface", "neurofeedback", "control", "cursor")
)

# Function to assign category based on keyword match
assign_category <- function(text) {
  text <- tolower(text)
  for (category in names(category_keywords)) {
    keywords <- category_keywords[[category]]
    if (any(str_detect(text, fixed(keywords)))) {
      return(category)
    }
  }
  return("Uncategorized")
}

# Apply category assignment
data$category <- sapply(data$`data$eeg_topics`, assign_category)

# Save results
#write_csv(data, "/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/Papers/Main paper/categorized_eeg_topics.csv")

# after visual check
topics_celaned <- read.csv("/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/Papers/Main paper/categorized_eeg_topics.csv", sep = ';')
topics_celaned <- topics_celaned[-305,]

# prepare data for plotting
plot_data_topics <- as.data.frame(table(topics_celaned$category))
plot_data_topics$percent <- as.character(paste0(round(plot_data_topics$Freq/396*100, 1), '%')) # to percentage

plot_data_topics <- plot_data_topics %>%
  arrange(desc(Freq))

# The colors
BLUE <- "#0072B2"

png("/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/Papers/Main paper/Figures/topics.png", width = 1200, height = 800, res = 200)

ggplot(plot_data_topics, aes(forcats::fct_reorder(Var1, Freq), Freq)) +
  geom_col(fill = BLUE, width = 0.6)  +
  coord_flip() +
  theme(panel.background = element_blank(),
        axis.text=element_text(size=12),
        axis.title.y = element_blank()) + ylim(0,80) + ylab("Number of teams")

dev.off()

