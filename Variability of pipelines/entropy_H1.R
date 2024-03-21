rm(list = ls())

################################
# Entropy Analysis

# Install the entropy package 
install.packages("entropy")

library(entropy)

# Loading the data
data <- read.csv("C:/Users/ecesnait/Desktop/EEGManyPipelines/Data/Big Analysis/all_var_AQ_h1.csv")
h1_data <- data[,c(-1,-27,-28)]
h1_data <- as.data.frame(h1_data)

## --------------------
# Correct values
## --------------------

# Software
h1_data$software[h1_data$software== 'eeglab_erplab'] <- 'eeglab'
h1_data$software[h1_data$software== 'eeglab_limo'] <- 'eeglab'

#Fill in empty responses
# High-pass filter type & direction
h1_data$ans_hf_type[h1_data$ans_hf_type== ''] <- 'unknown'

h1_data$ans_hf_direction[h1_data$ans_hf_direction == ''] <- 'unknown'

# Segment exclusion criteria
h1_data$ans_exclusion_criteria_seg[h1_data$ans_exclusion_criteria_seg == ''] <- 'unknown'

#Time window start, end & length
#exchange missing values to the mean of the column
h1_data$ans_time_w_start_h1[is.na(h1_data$ans_time_w_start_h1)] <- mean((h1_data$ans_time_w_start_h1), na.rm = T) 

h1_data$ans_time_w_end_h1[is.na(h1_data$ans_time_w_end_h1)] <- mean((h1_data$ans_time_w_end_h1), na.rm = T) 
h1_data$time_w_length_h1[is.na(h1_data$time_w_length_h1)] <- mean((h1_data$time_w_length_h1), na.rm = T) 
# ICA algorythm
h1_data$ans_ica_algo[h1_data$ans_ica_algo == ''] <- 'unknown'

#Baseline start and stop
h1_data$ans_baseline_start[h1_data$ans_baseline_start == '-200 ms'] <- '-200'
h1_data$ans_baseline_start[h1_data$ans_baseline_start == '-200ms'] <- '-200'

h1_data$ans_baseline_start <- as.numeric(h1_data$ans_baseline_start)
h1_data$ans_baseline_start[is.na(h1_data$ans_baseline_start)] <- mean((h1_data$ans_baseline_start), na.rm = T) 
h1_data$ans_baseline_stop[is.na(h1_data$ans_baseline_stop)] <- mean((h1_data$ans_baseline_stop), na.rm = T)

# Change baseline window length 0 to the mean of the column because 0 is dragging the effect
h1_data$bs_window_length[h1_data$bs_window_length == 0] <- mean(h1_data$bs_window_length[h1_data$bs_window_length != 0], na.rm = T) 

#all columns to factors
h1_data <- lapply(h1_data, as.factor)

# Select the features that I would like to calculate the entropy for
#feature_data <- h1_data[, 1:22]

# Need to convert the factors to character vectors for the entropy package to work
encoded_data <- lapply(h1_data, function(x) as.numeric(factor(x)))

# Calculate rpobabilities for each variable each list
#prob_encoded_data <- as.data.frame(table(encoded_data$software)) 
entropy_all <- c()
for (i in 1:length(encoded_data)){
  prob_one <- table(encoded_data[i])/nrow(as.data.frame(encoded_data[i]))
  entropy_all[i] <- entropy(prob_one, unit= "log2")
}

plot_entropy <- as.data.frame(entropy_all)
names(h1_data)
plot_entropy$class <- names(h1_data)
  #c("software", "software_host", "hf cutoff", "hf type", "hf direction","reref", "samp freq", "excl subj","topo_region",
                        "")

# Higher entropy value indicates a more random/uncertain distribution. A lower value indicates more deterministic, predictable distribution 
par(mar = c(2,10,2,2)) # Set the margin on all sides to 2
barplot(height=plot_entropy$entropy_all, names=plot_entropy$class, 
        col="#69b3a2",
        horiz=T, las=1)

# OR
#freqs <- table(encoded_data$software)/length(encoded_data$software)

# Calculate entropy for each feature - SHOULD BE DONE ON THE PROBABILITIES OF THE EVENT AND NOT THE LIST OF EVENTS!
entropy(prob_one,unit = "log2")



## BELOW IS ANAS SOLUTION ##
entropy_values <- lapply(encoded_data, entropy)
entropy(encoded_data)

freqs_values <- lapply(encoded_data, freqs)

# Print the results
print(freqs_values)

plot_entropy <- data.frame(entropy_values)
#library(data.table)
plot_entropy <- melt(plot_entropy)

par(mar = c(2,10,2,2)) # Set the margin on all sides to 2
barplot(height=plot_entropy$value, names=plot_entropy$variable, 
        col="#69b3a2",
        horiz=T, las=1)

################################
# Kullback-Leibler divergence.
# This is not an entropy measure, rather a measure of similarity between two different distributions
# It is important to mention here that the KL divergence is affected by the granuality of the 
# distributions, that is, if our distributions have a different number of possible values
# such as 2 values in binomial and multiple values in multinomial distributions


# Select the features
feature_data <- data[, 1:22]

# Encode the data and transform it into a 
encoded_data <- lapply(feature_data, function(x) as.numeric(factor(x)))
encoded_data <- as.data.frame(encoded_data)

# Define a function to calculate KL divergence between two distributions
KL_divergence <- function(p, q) {
  sum(p * log(p / q), na.rm = TRUE)
}

# Calculate KL divergence for each pair of features
num_features <- ncol(encoded_data)
kl_divergence_matrix <- matrix(NA, nrow = num_features, ncol = num_features)
colnames(kl_divergence_matrix) <- rownames(kl_divergence_matrix) <- colnames(encoded_data)

epsilon <- 1e-10 # Small epsilon value to avoid division by zero

for (i in 1:num_features) {
  for (j in 1:num_features) {
    if (i != j) {
      # Check if both columns are numeric
      if (is.numeric(encoded_data[, i]) && is.numeric(encoded_data[, j])) {
        # Get unique categories from both features
        categories <- unique(c(names(table(encoded_data[, i])), names(table(encoded_data[, j]))))
        
        # Calculate distributions with zero-padding
        p <- table(encoded_data[, i], dnn = "categories")[categories]
        q <- table(encoded_data[, j], dnn = "categories")[categories]
        all_categories <- union(names(p), names(q))
        # Ensure both distributions have the same categories and apply zero-padding
        p <- ifelse(names(p) %in% all_categories, p, 0)
        q <- ifelse(names(q) %in% all_categories, q, 0)
        p[is.na(p)] <- 0
        q[is.na(q)] <- 0
        p <- (p + epsilon) / sum(p + epsilon)
        q <- (q + epsilon) / sum(q + epsilon)
        
        kl_divergence_matrix[i, j] <- KL_divergence(p, q)
      }
    }
  }
}

# Display the KL divergence matrix
print(kl_divergence_matrix)