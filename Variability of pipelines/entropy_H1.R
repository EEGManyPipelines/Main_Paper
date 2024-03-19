################################
# Entropy Analysis

# Install the entropy package 
install.packages("entropy")

library(entropy)

# Loading the data
dataPath <- "C:\\Users\\User\\Desktop\\Neurocognitive\\Internship\\munster\\Vibration_of_effect\\all_var_AQ_h1_(version_5).xlsx"
data <- read_excel(dataPath, col_names = TRUE)
head(data)

#Converting all the variables in the data to factors
data$software <- as.factor(data$software)
data$hf_cutoff <- as.factor(data$hf_cutoff)
data$ans_hf_type <- as.factor(data$ans_hf_type)
data$ans_hf_direction <- as.factor(data$ans_hf_direction)
data$reref <- as.factor(data$reref)
data$topo_region_h1 <- as.factor(data$topo_region_h1)
data$ans_exclusion_criteria_seg <- as.factor(data$ans_exclusion_criteria_seg)
data$mc_method_h1 <- as.factor(data$mc_method_h1)
data$stat_method_h1 <- as.factor(data$stat_method_h1)
data$ans_spa_roi_avg_h1 <- as.factor(data$ans_spa_roi_avg_h1)
data$ans_mt_h1 <- as.factor(data$ans_mt_h1)
data$ans_temp_roi_avg_h1 <- as.factor(data$ans_temp_roi_avg_h1)
data$ans_ica_algo <- as.factor(data$ans_ica_algo)
data$ans_bad_comp_sel_visual <- as.factor(data$ans_bad_comp_sel_visual)
data$ans_bad_comp_sel_plugin <- as.factor(data$ans_bad_comp_sel_plugin)
data$z_value = qnorm(data$pval)
data$ds_fs <- as.factor(data$ds_fs)
data$subj_excluded <- as.factor(data$subj_excluded)
data$nr_chan_h1 <- as.factor(data$nr_chan_h1)
data$ans_time_w_start_h1 <- as.factor(data$ans_time_w_start_h1)
data$ans_time_w_end_h1 <- as.factor(data$ans_time_w_end_h1)
data$ans_baseline_start <- as.factor(data$ans_baseline_start)
data$ans_baseline_stop <- as.factor(data$ans_baseline_stop)

# Select the features that I would like to calculate the entropy for
feature_data <- data[, 1:22]

# Need to convert the factors to character vectors for the entropy package to work
encoded_data <- lapply(feature_data, function(x) as.numeric(factor(x)))

# Calculate entropy for each feature
entropy_values <- lapply(encoded_data, entropy)

# Print the results
print(entropy_values)

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