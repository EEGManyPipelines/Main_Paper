#### All functions for analysis code ####

# ============================================================================ #
#### Recode character variables to proper factors ####

recode_char2fac <- function(data){
  #' Turn string variables into proper factors again
  #' @param data    data frame with trial-level data
  #' @return data   same data frame with all character variables turned into proper factors
  
  # -------------------------------------------------------------------------- #
  ### Check inputs:
  
  if(!(is.data.frame((data)))){stop("data must be a data frame")}
  
  # -------------------------------------------------------------------------- #
  ### Recode any character variable to factor:
  
  cat("Recode any character variable to factor\n")
  
  for (iCol in 1:ncol(data)){
    if (is.character(data[, iCol])){ # if character
      cat(paste0("Recode variable ", names(data)[iCol], "\n"))
      data[, iCol] <- factor(data[, iCol]) # recode to proper factor
    }
  }
  
  cat("Finished :-)\n")
  return(data)
}

# ============================================================================ #
#### Recode all numerical variables to factors ####

recode_num2fac <- function(data, variables = NULL){
  #' Recode selected numerical variable to factor
  #' @param data    data frame with trial-level data
  #' @variables     vector of strings, numerical variables to turn into factors (if not provided: all numerical variables in data frame)
  #' @return data   same data frame with all numerical variables also available as factor ("_f")
  
  # -------------------------------------------------------------------------- #
  ### Check inputs:
  
  if(!(is.data.frame((data)))){stop("data must be a data frame")}
  
  # -------------------------------------------------------------------------- #
  ### If not input vector of selected variables given, retrieve:
  
  ### Determine if input variables given, otherwise take all numerical variables: 
  if(is.null(variables)){
    cat("No variables given, take all numerical variables\n")
    variables <- c()
    for (iCol in 1:ncol(data)){
      if (is.numeric(data[, iCol])){ # if numeric
        variables <- c(variables, names(data)[iCol]) # add variable name
      }
    }
  } else {
    cat("Recode selected numerical variables to factors\n")
  }
  
  ### Number variables:
  nVar <- length(variables)
  
  # -------------------------------------------------------------------------- #
  ### Loop through variables, recode to factor:  
  
  for (iVar in 1:nVar){ # loop through variables
    varName <- variables[iVar]
    cat(paste0("Recode variable ", varName, "\n"))
    newVarName <- paste0(varName, "_f") # new variable name
    data[, newVarName] <- factor(data[, varName]) # recode to proper factor
  }
  
  cat("Finished :-)\n")
  return(data)
}

# ============================================================================ #
#### Create cleaned versions of foraging button presses and trial numbers: ####

clean_trialnr_forageBP <- function(data){
  
  # -------------------------------------------------------------------------- #
  ### Check inputs:
  
  if(!(is.data.frame((data)))){stop("data must be a data frame")}
  
  # -------------------------------------------------------------------------- #
  ### Create cleaned version of selected hard-coded variables:
  
  ## Forage button presses:
  cat("Create variable forage_presses_cleaned18_n \n")
  data$forage_presses_cleaned18_n <- data$forage_presses_n
  data$forage_presses_cleaned18_n[data$forage_presses_cleaned18_n > 18] <- NA
  table(data$forage_presses_cleaned18_n)
  
  cat("Create variable forage_presses_cleaned25_n \n")
  data$forage_presses_cleaned25_n <- data$forage_presses_n
  data$forage_presses_cleaned25_n[data$forage_presses_cleaned25_n > 25] <- NA
  table(data$forage_presses_cleaned25_n)
  
  ## Forage button presses lag 1:
  cat("Create variable forage_presses_lag1_cleaned18_n \n")
  data$forage_presses_lag1_cleaned18_n <- data$forage_presses_lag1_n
  data$forage_presses_lag1_cleaned18_n[data$forage_presses_lag1_cleaned18_n > 18] <- NA
  table(data$trialnr_patch_cleaned18_n)
  
  cat("Create variable trialnr_patch_cleaned18_n \n")
  data$trialnr_patch_cleaned18_n <- data$trialnr_patch_n
  data$trialnr_patch_cleaned18_n[data$trialnr_patch_cleaned18_n > 18] <- NA
  table(data$trialnr_patch_cleaned18_n)
  
  cat("Create variable trialnr_patch_rev_cleaned18_n \n")
  data$trialnr_patch_rev_cleaned18_n <- data$trialnr_patch_rev_n
  data$trialnr_patch_rev_cleaned18_n[data$trialnr_patch_rev_cleaned18_n > 18] <- NA
  data$trialnr_patch_rev_cleaned18_n[data$trialnr_patch_rev_cleaned18_n == 1] <- NA
  data$trialnr_patch_rev_cleaned18_n <- data$trialnr_patch_rev_cleaned18_n * -1
  table(data$trialnr_patch_rev_cleaned18_n)
  
  cat("Create variable trialnr_patch_rev_cleaned18no2_n \n")
  data$trialnr_patch_rev_cleaned18no2_n <- data$trialnr_patch_rev_n
  data$trialnr_patch_rev_cleaned18no2_n[data$trialnr_patch_rev_cleaned18no2_n > 18] <- NA
  data$trialnr_patch_rev_cleaned18no2_n[data$trialnr_patch_rev_cleaned18no2_n == 2] <- NA
  data$trialnr_patch_rev_cleaned18no2_n[data$trialnr_patch_rev_cleaned18no2_n == 1] <- NA
  data$trialnr_patch_rev_cleaned18no2_n <- data$trialnr_patch_rev_cleaned18no2_n * -1
  table(data$trialnr_patch_rev_cleaned18no2_n)
  
  cat("Finished :-)\n")
  return(data)
  
}

# ============================================================================ #
#### Z-standardize variables for mixed models ####

z_standardize_variables <- function(data, varVec = NULL, perSub = F, subVar = "subject_n"){
  #' Standardize predictors (hard-coded selection)
  #' @param data data frame with trial-level data
  #' @param varVec vector of strings, variables to standardize
  #' @return data with hard-coded selection of variables z-standardize (ending "_z")
  
  # -------------------------------------------------------------------------- #
  ### Check inputs:
  
  if(!(is.data.frame((data)))){stop("data must be a data frame")}
  
  cat("Z-standardize selected variables\n")
  
  # -------------------------------------------------------------------------- #
  ### Specify default variables to standardize:
  
  if (is.null(varVec)){
    varVec <- c("FRR_stz_n", "BRR_stz_n", "TT_stz_n", "valence_stz_n", "effort_n", "outcome_lag1_stz_n",
                "forage_RT_first_cleaned_n", "forage_RT_complete_cleaned_n", "forage_RT_last_cleaned_n",
                "response_rate_n", "response_rate_log_n",
                "forage_presses_n", "forage_presses_lag1_n")
  }
  
  ## Check if variable available in data set:
  for (iVar in varVec){
    if(!(iVar %in% names(data))){stop("Variable ", iVar, " not found in data")}
  }
  
  # -------------------------------------------------------------------------- #
  ### Loop through vector, standardize:
  
  ## Standardize on subject-level:
  if (perSub){ # standardize separately per subject
    subVec <- sort(unique(data[, subVar]))
    for (iSub in subVec){
      cat(paste0("Start subject ", iSub, "\n"))
      rowIdx <- which(data[, subVar] == iSub) # rows of this subject
      for (nVar in varVec){
        zVar <- gsub("_n$", "_z", nVar)
        data[rowIdx, zVar] <- as.numeric(scale(data[rowIdx, nVar]))
        cat(paste0("Standardize variable ", nVar, " for subject ", iSub, ": "))
        cat(paste0("Mean = ", round(mean(data[rowIdx, zVar], na.rm = T), 3), ", SD = ", round(sd(data[rowIdx, zVar], na.rm = T), 3), "\n"))
      }
    }
    
    ## Standardize on group-level:
  } else { # standardize on group-level
    for (nVar in varVec){
      cat(paste0("Standardize variable ", nVar, " on group level\n"))
      zVar <- gsub("_n$", "_z", nVar)
      data[, zVar] <- as.numeric(scale(data[, nVar]))
      cat(paste0("Variable ", zVar, ": Mean = ", round(mean(data[, zVar], na.rm = T), 3), ", SD = ", round(sd(data[, zVar], na.rm = T), 3), "\n"))
    }
  }
  
  cat("Finished :-)\n")
  return(data)
}

# ============================================================================ #
#### Retrieve default values for plots: #####

retrieve_plot_defaults <- function(input){
  #' Retrieve default for given plotting parameter.
  #' @param input scalar string, name of parameter for which to set default.
  #' @return scalar numeric, default value of that parameter
  # retrieve_plot_defaults("FTS")
  
  if (input == "FTS"){
    output <- 28
  } else if (input == "LWD"){
    output <- 1.5
  } else if (input == "dotSize"){
    output <- 2 # 0.5
  } else if (input == "dodgeVal"){
    output <- 0.6
  } else {
    stop("Unknown input to function retrieve_plot_defaults()")
  }
  
  return(output)
}

# ============================================================================ #
#### Automatically convert factor names to pretty names: #####

substitute_label <- function(labels){
  #' Substitute certain manually defined factor names for prettier names.
  #' @param labels vector of strings with names of factors in model.
  #' @return same vector with certain strings substituted.
  
  for (iItem in 1:length(labels)){
    
    cat(paste0("Automatically substitute axis labels for ", labels[iItem], " according to manual mapping\n"))
    
    labels[iItem] <- gsub("subjectID", "Subject", labels[iItem])
    labels[iItem] <- gsub("subject_n", "Subject", labels[iItem])
    labels[iItem] <- gsub("subject_f", "Subject", labels[iItem])
    
    labels[iItem] <- gsub("trialnr_patch_n", "Trial number within patch", labels[iItem])
    labels[iItem] <- gsub("trialnr_patch_f", "Trial number within patch", labels[iItem])
    labels[iItem] <- gsub("trialnr_patch_cleaned10_n", "Trial number within patch", labels[iItem])
    labels[iItem] <- gsub("trialnr_patch_cleaned10_f", "Trial number within patch", labels[iItem])
    labels[iItem] <- gsub("trialnr_patch_cleaned18_n", "Trial number within patch", labels[iItem])
    labels[iItem] <- gsub("trialnr_patch_cleaned18_f", "Trial number within patch", labels[iItem])
    
    labels[iItem] <- gsub("trialID", "Trial number", labels[iItem])
    labels[iItem] <- gsub("trialnr_n", "Trial number", labels[iItem])
    labels[iItem] <- gsub("trialnr_f", "Trial number", labels[iItem])
    
    labels[iItem] <- gsub("trialnr_patch_rev_n", "Trial number before leave", labels[iItem])
    labels[iItem] <- gsub("trialnr_patch_rev_f", "Trial number before leave", labels[iItem])
    labels[iItem] <- gsub("trialnr_patch_rev_cleaned18_f", "Trial number before leave", labels[iItem])
    
    labels[iItem] <- gsub("trialnr_patch_n", "Trial number (patch)", labels[iItem])
    labels[iItem] <- gsub("trialnr_patch_z", "Trial number (patch) (z)", labels[iItem])
    
    labels[iItem] <- gsub("patchnr_round_n", "Patch number in round", labels[iItem])
    labels[iItem] <- gsub("patchnr_round_z", "Patch number in round (patch) (z)", labels[iItem])
    
    labels[iItem] <- gsub("roundID", "Round number", labels[iItem])
    labels[iItem] <- gsub("round_n", "Round number", labels[iItem])
    labels[iItem] <- gsub("round_f", "Round number", labels[iItem])
    
    labels[iItem] <- gsub("(Intercept)", "Intercept", labels[iItem])
    labels[iItem] <- gsub("X.Intercept.", "Intercept", labels[iItem])
    
    labels[iItem] <- gsub("FRR_f1", "FRR1", labels[iItem])
    labels[iItem] <- gsub("FRR_f2", "FRR2", labels[iItem])
    labels[iItem] <- gsub("FRR_f", "FRR", labels[iItem])
    labels[iItem] <- gsub("FRR_cont_n", "FRR", labels[iItem])
    labels[iItem] <- gsub("FRR_cont_z", "FRR", labels[iItem])
    labels[iItem] <- gsub("FRR_stz_n", "FRR", labels[iItem])
    labels[iItem] <- gsub("FRR_stz_z", "FRR", labels[iItem])
    labels[iItem] <- gsub("FRR_long_f", "Foreground reward rate", labels[iItem])
    labels[iItem] <- gsub("FRR_short_f", "FRR", labels[iItem])
    labels[iItem] <- gsub("FRR_short2_f", "FRR", labels[iItem])
    
    labels[iItem] <- gsub("BRR_f1", "BRR", labels[iItem])
    labels[iItem] <- gsub("BRR_f", "BRR", labels[iItem])
    labels[iItem] <- gsub("BRR_cont_n", "BRR", labels[iItem])
    labels[iItem] <- gsub("BRR_cont_z", "BRR", labels[iItem])
    labels[iItem] <- gsub("BRR_stz_n", "BRR", labels[iItem])
    labels[iItem] <- gsub("BRR_stz_z", "BRR", labels[iItem])
    labels[iItem] <- gsub("BRR_long_f", "Background reward rate", labels[iItem])
    labels[iItem] <- gsub("BRR_short_f", "BRR", labels[iItem])
    labels[iItem] <- gsub("BRR_short2_f", "BRR", labels[iItem])
    
    labels[iItem] <- gsub("TT_f1", "TT", labels[iItem])
    labels[iItem] <- gsub("TT_f", "TT", labels[iItem])
    labels[iItem] <- gsub("TT_cont_n", "TT", labels[iItem])
    labels[iItem] <- gsub("TT_cont_z", "TTT", labels[iItem])
    labels[iItem] <- gsub("TT_stz_n", "TT", labels[iItem])
    labels[iItem] <- gsub("TT_stz_z", "TT", labels[iItem])
    labels[iItem] <- gsub("TT_long_f", "Travel time", labels[iItem])
    labels[iItem] <- gsub("TT_short_f", "TT", labels[iItem])
    
    labels[iItem] <- gsub("environment_n", "Env.", labels[iItem])
    labels[iItem] <- gsub("environment_f", "Env.", labels[iItem])
    labels[iItem] <- gsub("environment_f1", "Env.", labels[iItem])
    labels[iItem] <- gsub("environment_long_f", "Environment", labels[iItem])
    labels[iItem] <- gsub("environment_short_f", "Env.", labels[iItem])
    
    labels[iItem] <- gsub("valence_n", "Valence", labels[iItem])
    labels[iItem] <- gsub("valence_f1", "Valence", labels[iItem])
    labels[iItem] <- gsub("valence_f", "Valence", labels[iItem])
    labels[iItem] <- gsub("valence_short_f", "Val.", labels[iItem])
    labels[iItem] <- gsub("valence_cont_n", "Valence", labels[iItem])
    labels[iItem] <- gsub("valence_cont_z", "Valence", labels[iItem])
    labels[iItem] <- gsub("valence_stz_n", "Valence", labels[iItem])
    labels[iItem] <- gsub("valence_stz_z", "Valence", labels[iItem])
    labels[iItem] <- gsub("valence_f1", "Valence", labels[iItem])
    
    labels[iItem] <- gsub("effort_f1", "Effort1", labels[iItem])
    labels[iItem] <- gsub("effort_f2", "Effort2", labels[iItem])
    labels[iItem] <- gsub("effort_f3", "Effort3", labels[iItem])
    labels[iItem] <- gsub("effort_f4", "Effort4", labels[iItem])
    labels[iItem] <- gsub("effort_f5", "Effort5", labels[iItem])
    labels[iItem] <- gsub("effort_f", "Req. Effort", labels[iItem])
    labels[iItem] <- gsub("effort_n", "Req. Effort", labels[iItem])
    labels[iItem] <- gsub("effort_z", "Req. Effort", labels[iItem])
    
    labels[iItem] <- gsub("nStar_patch_n", "# stars", labels[iItem])
    labels[iItem] <- gsub("nStar_patch_f", "# stars", labels[iItem])
    labels[iItem] <- gsub("nStar_patch_lag1_n", "# stars prev. patch", labels[iItem])
    labels[iItem] <- gsub("nStar_patch_lag1_f", "# stars prev. patch", labels[iItem])
    
    ### Responses:
    labels[iItem] <- gsub("choice_n", "p(leave)", labels[iItem])
    labels[iItem] <- gsub("choice_f", "Choice", labels[iItem])
    labels[iItem] <- gsub("num_stay_n", "# stays", labels[iItem])
    
    ## Button presses:
    labels[iItem] <- gsub("button_presses_n", "# button presses", labels[iItem])
    labels[iItem] <- gsub("forage_presses_n", "# button presses", labels[iItem])
    labels[iItem] <- gsub("forage_presses_cleaned18_n", "# button presses", labels[iItem])
    labels[iItem] <- gsub("forage_presses_cleaned25_n", "# button presses", labels[iItem])
    labels[iItem] <- gsub("travel_presses_n", "# button presses", labels[iItem])
    
    ## Lagged button presses:
    labels[iItem] <- gsub("forage_presses_lag1_n", "# button presses last trial", labels[iItem])
    labels[iItem] <- gsub("forage_presses_lag1_f", "# button presses last trial", labels[iItem])
    
    labels[iItem] <- gsub("forage_presses_lag1_cleaned18_n", "# button presses last trial", labels[iItem])
    labels[iItem] <- gsub("forage_presses_lag1_cleaned18_f", "# button presses last trial", labels[iItem])
    labels[iItem] <- gsub("forage_presses_lag1_z", "Inst. BP lag 1 (z)", labels[iItem])
    labels[iItem] <- gsub("forage_presses_lag1_cleaned10_z", "Inst. BP lag 1 (z)", labels[iItem])
    labels[iItem] <- gsub("forage_presses_lag1_cleaned18_z", "Inst. BP lag 1 (z)", labels[iItem])
    
    labels[iItem] <- gsub("forage_presses_excess_lag1_z", "Excess # button presses last trial", labels[iItem])
    labels[iItem] <- gsub("forage_presses_excess_lag1_cleaned18_z", "Excess BP lag 1 (z)", labels[iItem])
    
    labels[iItem] <- gsub("forage_presses_cum_lag1_resid_n", "Cum. BP lag 1\n (ortho)", labels[iItem])
    labels[iItem] <- gsub("forage_presses_cum_lag1_resid_z", "Cum. BP lag 1\n (ortho) (z)", labels[iItem])
    
    ## RTs:
    labels[iItem] <- gsub("forage_RT_first_n", "RT first BP (in ms)", labels[iItem])
    labels[iItem] <- gsub("forage_RT_complete_n", "RT last req. BP (in ms)", labels[iItem])
    labels[iItem] <- gsub("forage_RT_last_n", "RT last BP (in ms)", labels[iItem])
    labels[iItem] <- gsub("forage_RT_first_z", "RT first BP (z)", labels[iItem])
    labels[iItem] <- gsub("forage_RT_complete_z", "RT last req. BP (z)", labels[iItem])
    labels[iItem] <- gsub("forage_RT_last_z", "RT last BP (z)", labels[iItem])
    labels[iItem] <- gsub("forage_RT_first_cleaned_n", "RT first BP (in ms)", labels[iItem])
    labels[iItem] <- gsub("forage_RT_complete_cleaned_n", "RT last req. BP (in ms)", labels[iItem])
    labels[iItem] <- gsub("forage_RT_last_cleaned_n", "RT last BP (in ms)", labels[iItem])
    labels[iItem] <- gsub("forage_RT_first_cleaned_z", "RT first BP (z)", labels[iItem])
    labels[iItem] <- gsub("forage_RT_complete_cleaned_z", "RT last req. BP (z)", labels[iItem])
    labels[iItem] <- gsub("forage_RT_last_cleaned_z", "RT last BP (z)", labels[iItem])
    
    labels[iItem] <- gsub("forage_RT_first_cleaned_lag1_n", "RT first BP \nlag 1 (in ms)", labels[iItem])
    labels[iItem] <- gsub("forage_RT_complete_cleaned_lag1_n", "RT last req. BP \nlag 1 (in ms)", labels[iItem])
    labels[iItem] <- gsub("forage_RT_first_cleaned_lag1_z", "RT first BP \nlag 1 (z)", labels[iItem])
    labels[iItem] <- gsub("forage_RT_complete_cleaned_lag1_z", "RT last req. BP \nlag 1 (z)", labels[iItem])
    
    labels[iItem] <- gsub("response_rate_n", "Resp. rate (1/s)", labels[iItem])
    labels[iItem] <- gsub("response_rate_cleaned_n", "Resp. rate (1/s)", labels[iItem])
    labels[iItem] <- gsub("response_rate_log_n", "Log. resp. rate (log(1/s))", labels[iItem])
    labels[iItem] <- gsub("response_rate_cleaned_log_n", "Log. resp. rate (log(1/s))", labels[iItem])
    labels[iItem] <- gsub("response_rate_cleaned_z", "Resp. rate (z)", labels[iItem])
    labels[iItem] <- gsub("response_rate_cleaned_log_z", "Log. resp. rate (log, z)", labels[iItem])
    
    labels[iItem] <- gsub("response_rate_cleaned_lag1_n", "Resp. rate \nlag 1 (1/s)", labels[iItem])
    labels[iItem] <- gsub("response_rate_cleaned_lag1_z", "Resp. rate \nlag 1 (z)", labels[iItem])
    labels[iItem] <- gsub("response_rate_cleaned_log_lag1_n", "Log. resp. rate \nlag 1 (log(1/s))", labels[iItem])
    labels[iItem] <- gsub("response_rate_cleaned_log_lag1_z", "Log. resp. rate \nlag 1 (z)", labels[iItem])
    
    ## Outcomes:    
    labels[iItem] <- gsub("outcome_n", "Outcome", labels[iItem])
    labels[iItem] <- gsub("outcome_f", "Outcome", labels[iItem])
    labels[iItem] <- gsub("outcome_short_f", "Out.", labels[iItem])
    labels[iItem] <- gsub("outcome_short2_f", "Out.", labels[iItem])
    
    labels[iItem] <- gsub("outcome_lag1_n", "Past outcome", labels[iItem])
    labels[iItem] <- gsub("outcome_lag1_f2", "Prev. out.", labels[iItem])
    labels[iItem] <- gsub("outcome_lag1_f", "Past outcome", labels[iItem])
    labels[iItem] <- gsub("outcome_lag1_cont_n", "Past outcome", labels[iItem])
    labels[iItem] <- gsub("outcome_lag1_cont_z", "Past outcome", labels[iItem])
    labels[iItem] <- gsub("outcome_lag1_stz_n", "Past outcome", labels[iItem])
    labels[iItem] <- gsub("outcome_lag1_stz_z", "Past outcome", labels[iItem])
    labels[iItem] <- gsub("outcome_lag1_short2_f", "Prev. out.", labels[iItem])
    
    labels[iItem] <- gsub("outcome_lag2_n", "Outcome lag 2", labels[iItem])
    labels[iItem] <- gsub("outcome_lag2_f", "Outcome lag 2", labels[iItem])
    labels[iItem] <- gsub("outcome_lag3_n", "Outcome lag 3", labels[iItem])
    labels[iItem] <- gsub("outcome_lag3_f", "Outcome lag 3", labels[iItem])
    labels[iItem] <- gsub("outcome_lag4_n", "Outcome lag 4", labels[iItem])
    labels[iItem] <- gsub("outcome_lag4_f", "Outcome lag 4", labels[iItem])
    labels[iItem] <- gsub("outcome_lag5_n", "Outcome lag 5", labels[iItem])
    labels[iItem] <- gsub("outcome_lag5_f", "Outcome lag 5", labels[iItem])
    labels[iItem] <- gsub("outcome_lag6_n", "Outcome lag 6", labels[iItem])
    labels[iItem] <- gsub("outcome_lag6_f", "Outcome lag 6", labels[iItem])
    labels[iItem] <- gsub("outcome_lag7_n", "Outcome lag 7", labels[iItem])
    labels[iItem] <- gsub("outcome_lag7_f", "Outcome lag 7", labels[iItem])
    labels[iItem] <- gsub("outcome_lag8_n", "Outcome lag 8", labels[iItem])
    labels[iItem] <- gsub("outcome_lag8_f", "Outcome lag 8", labels[iItem])
    labels[iItem] <- gsub("outcome_lag9_n", "Outcome lag 9", labels[iItem])
    labels[iItem] <- gsub("outcome_lag9_f", "Outcome lag 9", labels[iItem])
    
    ## Energy levels:    
    labels[iItem] <- gsub("nLives_cor_n", "Player lives (cor.)", labels[iItem])
    labels[iItem] <- gsub("nLives_cor_f", "Player lives (cor.)", labels[iItem])
    labels[iItem] <- gsub("nLives_n", "Player lives", labels[iItem])
    labels[iItem] <- gsub("nLives_f", "Player lives", labels[iItem])
    
    labels[iItem] <- gsub("barScore_cor_n", "Progress bar (cor.)", labels[iItem])
    labels[iItem] <- gsub("barScore_cor_f", "Progress bar (cor.)", labels[iItem])
    labels[iItem] <- gsub("barScore_binned_n", "Progress bar (binned)", labels[iItem])
    labels[iItem] <- gsub("barScore_binned_f", "Progress bar (binned)", labels[iItem])
    labels[iItem] <- gsub("barScore_int_n", "Progress bar (int.)", labels[iItem])
    labels[iItem] <- gsub("barScore_int_f", "Progress bar (int.)", labels[iItem])
    labels[iItem] <- gsub("barScore_n", "Progress bar", labels[iItem])
    labels[iItem] <- gsub("barScore_f", "Progress bar", labels[iItem])
    
    labels[iItem] <- gsub("nLives_cor_n", "Player lives (cor.)", labels[iItem])
    labels[iItem] <- gsub("nLives_cor_f", "Player lives (cor.)", labels[iItem])
    labels[iItem] <- gsub("nLives_n", "Player lives", labels[iItem])
    labels[iItem] <- gsub("nLives_f", "Player lives", labels[iItem])
    
    labels[iItem] <- gsub("nTotalReward_cor_binned_n", "Total reward score (cor., binned)", labels[iItem])
    labels[iItem] <- gsub("nTotalReward_cor_binned_f", "Total reward score (cor., binned)", labels[iItem])
    labels[iItem] <- gsub("nTotalReward_cor_int_n", "Total reward score (cor. int.)", labels[iItem])
    labels[iItem] <- gsub("nTotalReward_cor_int_f", "Total reward score (cor. int.)", labels[iItem])
    labels[iItem] <- gsub("nTotalReward_cor_n", "Total reward score (cor.)", labels[iItem])
    labels[iItem] <- gsub("nTotalReward_cor_f", "Total reward score (cor.)", labels[iItem])
    labels[iItem] <- gsub("nTotalReward_binned_n", "Total reward score (binned)", labels[iItem])
    labels[iItem] <- gsub("nTotalReward_binned_f", "Total reward score (binned)", labels[iItem])
    labels[iItem] <- gsub("nTotalReward_int_n", "Total reward score (int.)", labels[iItem])
    labels[iItem] <- gsub("nTotalReward_int_f", "Total reward score (int.)", labels[iItem])
    labels[iItem] <- gsub("nTotalReward_n", "Total reward score", labels[iItem])
    labels[iItem] <- gsub("nTotalReward_f", "Total reward score", labels[iItem])
    
    labels[iItem] <- gsub("isMed_n", "Medication status", labels[iItem])
    labels[iItem] <- gsub("isMed_f", "Medication status", labels[iItem])
    
  }
  
  return(labels)
  
}

# ============================================================================ #
#### Retrieve colour given independent variable: #####

retrieve_colour <- function(input){
  #' Retrieve colour scheme given variable name (pattern) as input.
  #' If none found, then use colorbrewer to create unidimensional YlOrRd color scheme,
  #' which is repeated as necessary to achieve number of required levels.
  #' Check for color-blindness friendliness: https://www.color-blindness.com/coblis-color-blindness-simulator/
  #' Search for neighboring colors: https://www.rapidtables.com/web/color/RGB_Color.html
  #' @param input scalar string, pattern within variable name to match to colour scheme.
  #' @return output vector of strings, colours to use.
  
  if (grepl("FRR", input, fixed = TRUE)){ 
    # output <- c("#76F013", "#FDF733", "#ED462F") # rich, mid, poor
    output <- c("#ED462F", "#FDF733", "#76F013") # poor, mid, rich
  } else if (grepl("BRR", input, fixed = TRUE)){ 
    # output <- c("#B2182B", "#2166AC") # rich, poor
    output <- c("#2166AC", "#B2182B") # poor, rich
  } else if (grepl("TT", input, fixed = TRUE)){ 
    output <- c("#B2182B", "#2166AC") # short, long
    # output <- c("#2166AC", "#B2182B") # short, long
  } else if (grepl("environment", input, fixed = TRUE)){ 
    output <- c("#B2182B", "#2166AC") # best, worst
  } else if (grepl("valence", input, fixed = TRUE)){ 
    output <- c("#4DAC26", "#D01C8B") # positive, negative
  } else if (grepl("effort_f", input, fixed = TRUE)){ 
    output <- brewer.pal(5, "YlOrRd")
  } else if (grepl("outcome_lag1_f", input, fixed = TRUE)){ 
    output <- c("#CC0000", "#009933")
  } else if (grepl("outcome_lag1_short2_f", input, fixed = TRUE)){ 
    output <- c("#CC0000", "#009933")
  } else if (grepl("outcome_", input, fixed = TRUE)){ 
    output <- c("#CC0000", "#009933")
  } else if (grepl("half", input, fixed = TRUE)){ 
    output <- c("orange", "purple")
  } else if (grepl("phase", input, fixed = TRUE)){ 
    output <- c("orange", "purple")
    
  } else  if (grepl("choice", input, fixed = TRUE)){ 
    output <- c("#B2182B", "#2166AC")
    
  } else  if (grepl("gender_f", input, fixed = TRUE)){ 
    output <- c("deeppink", "darkturquoise")
    
  } else  if (grepl("isMed_f", input, fixed = TRUE)){ 
    output <- c("#CC0000", "grey")
    
  } else {
    
    if (exists("plotData")){ # check if plotData exists
      
      cat("Could not find variable name, but found plotData, retrieve number of variable levels\n")
      
      if (any(grepl(input, names(plotData)))){ # if variable available in plot data
        
        cat(paste0("Retrieve variable \"", input, "\" from plotData\n"))
        
        if (is.numeric(plotData[, input])){ # if variable numeric: no levels, cannot return vector of colours
          
          output <- "black"
          cat(paste0("Variable ", input, " is numeric, just use colour ", output, " for all values\n"))
          
        } else { # if factor or character
          
          require(RColorBrewer)
          
          ## Count labels of variable:
          nLevel <- length(unique(plotData[, input]))
          cat(paste0("Found ", nLevel, " levels of variable ", input, " in plotData\n"))
          
          ## Retrieve number colours and repetitions:      
          cmap <- "YlOrRd"
          nColour <- min(nLevel, 9)
          nRep <- ceil(nLevel/9)
          cat(paste0("Retrieve ", nColour, " from colour map ", cmap, " from color brewer, repeat ", nRep, " times\n"))
          
          ## Create colour vector:
          output <- rep(brewer.pal(nColour, cmap), nRep)
          output <- output[1:nLevel]
          
        } # end if numeric or factor
        
      } else { # plotData available, but variable not found
        
        output <- "black" # ggplot default blue: #619CFF
        cat(paste0("Found plotData, but does not contain variable ", input, "; return colour ", output, "\n"))
        
      } # end if plotData available
      
    } else { # if plotData does not exist
      
      output <- "black" # ggplot default blue: #619CFF
      cat(paste0("Cannot find plotData, return colour ", output, "\n"))
      
    } # end if plotData exists
    
  } # end loop over input
  
  cat(paste0("Retrieve colour scheme given variable name ", input, ": ", paste0(output, collapse = ", "), "\n"))
  return(output)
}

# ============================================================================ #
#### Recode formula in Wilkinson notation to handle that can be used for saving models: ####

formula2handle <- function(formula){
  #' Create name based on formula with underscores instead of spaces without random-effects part to be used in plot names.
  #' @param formula   string, formula in Wilkinson notation.
  #' @return handle   string, converted to be without spaces but with underscores, with random-effects part removed.
  require(stringr)
  
  ## Check inputs:
  if(!(is.character(formula))){stop("formula has to be a character string")}
  
  cat(paste0("Input: ", formula, "\n"))
  # https://stackoverflow.com/questions/38291794/extract-string-before
  # https://statisticsglobe.com/extract-substring-before-or-after-pattern-in-r
  
  handle <- formula # copy over
  
  # -------------------------------------------------------------------------- #
  ### For survival models: delete Surv() surrounding:
  
  if (grepl("Surv\\(", handle)){
    
    handle <- sub(".*Surv\\(", "", handle) # delete everything up until Surv(
    handle <- sub("\\)", "", handle) # delete first )
  }
  
  # ------------------------------------------------------------------------ #
  ### For regressors created with poly():
  
  if (grepl("poly", handle)){
    
    startIdx <- unlist(gregexpr("poly", handle))[1] # only first list, first entry
    stopIdx <- unlist(gregexpr(")", handle))[1] # only first list, first entry
    subStr <- substring(handle, startIdx, stopIdx) # extract
    
    oldStr <- subStr # copy over
    oldStr <- sub("poly\\(", "", oldStr) # delete leading "("
    oldStr <- sub("\\)", "", oldStr) # delete trailing ")"
    oldStr <- paste0("poly\\(", oldStr, "\\)") # add back in with escape characters
    
    newStr <- subStr # copy over
    newStr <- sub("\\(", "_", newStr) # replace leading "(" with "_"
    newStr <- sub(", 2\\)", "", newStr) # delete final ", 2)"
    
    handle <- sub(oldStr, newStr, handle) # replace old with new string
  }
  
  # ------------------------------------------------------------------------ #
  ### Extract until random effects parentheses:
  
  handle  <- sub("\\(.*", "", handle) # delete everything after (
  
  # ------------------------------------------------------------------------ #
  ### Delete only very last (!) plus before random-effects part:
  # https://stackoverflow.com/questions/44687333/remove-characters-after-the-last-occurrence-of-a-specific-character
  # handle <- sub("+[^_]+$", "", handle)
  
  if(grepl( "+", str_sub(handle, -2, -1), fixed = T)){
    handle <- substring(handle, 1, nchar(handle) - 3)
    
  }
  
  ## Replace every * by x:
  handle <- gsub("\\*", "x", handle) # extract until last plus
  
  ## Substitute every space with underscore:
  handle <- gsub(" ", "_", handle)
  
  ## Remove old aliases:
  handle <- gsub("foreground_RR", "FRR", handle)
  handle <- gsub("background_RR", "BRR", handle)
  handle <- gsub("travel_time", "TT", handle)
  handle <- gsub("_cont_", "_stz_", handle)
  
  cat(paste0("Output: ", handle, "\n"))
  return(handle)
  
}

# ============================================================================ #
#### Retrieve or fit & save linear mixed-effects model based on formula: ####

fit_lmem <- function(formula, modData = modData, useLRT = FALSE){
  #' Retrieve previously fitted model or fit model anew and save it.
  #' @param formula   string, formula in Wilkinson notation.
  #' @param useLRT    Boolean, compute p-values with LRTs using afex (TRUE) or not (FALSE; default).
  #' @return mod    fitted model object.
  
  require(lme4)
  require(afex)
  require(car)
  
  # -------------------------------------------------------------------------- #
  ### Check inputs:
  
  if(!(is.character(formula))){stop("formula has to be a character string")}
  if(!(is.data.frame((modData)))){stop("modData must be a data frame")}
  
  # -------------------------------------------------------------------------- #
  ### Fixed settings:
  
  subVar <- "subject_n"
  
  # -------------------------------------------------------------------------- #
  ### Determine type of fitting:
  
  if (useLRT){
    fitType <- "LRT"
  } else {
    fitType <- "lme4"
  }
  
  # -------------------------------------------------------------------------- #
  ## Determine model family:
  
  DV <- sub("\\~.*", "", formula)
  DV <- sub(" ", "", DV)
  if (grepl( "choice", DV)){
    modFamily <- "binom"
  } else if (grepl( "presses", DV) | grepl( "stay", DV)){
    modFamily <- "pois"
  } else {
    modFamily <- "lin"
  } 
  
  ## Print specifics to console:
  cat(paste0(">>> Fit model ", formula, " of family ", modFamily, " using ", fitType, "\n"))
  cat(paste0(">>> Use data set modData with ", nrow(modData), " rows from ", length(unique(modData[, subVar])), " subjects\n"))
  
  # -------------------------------------------------------------------------- #
  ## Determine model name:
  
  modName <- paste0(fitType, "_", modFamily, "_", formula2handle(formula), ".rds")
  
  fullFileName <- paste0(dirs$modelDir, modName)
  
  ## If name too long:
  if (nchar(fullFileName) > 250){
    warning(paste0("Complete model path is ", nchar(fullFileName), " characters long, shorten\n"))
    modName <- gsub("_stz", "", modName)
    modName <- gsub("_z", "", modName)
    fullFileName <- paste0(dirs$modelDir, modName)
  }
  
  # -------------------------------------------------------------------------- #
  ### Check if to-be-fitted model already exists; if yes, retrieve; if not, fit anew:
  
  if (file.exists(fullFileName)){
    
    cat(paste0(">>> Found ", modName, ", load \n"))
    mod <- readRDS(paste0(dirs$modelDir, modName))
    if (useLRT){
      print(anova(mod))
    } else {
      print(summary(mod))
      print(Anova(mod, type = "3")) # 1 p-value per factor
    }
    
  } else {
    
    # ------------------------------------------------------------------------ #
    ### Fit model:
    
    ## Start time:
    start.time <- Sys.time();
    cat(paste0("Start time is ", start.time, "\n"))
    cat(paste0(">>> Fit model with name ", modName, " ...\n"))
    
    if (modFamily  == "binom"){ # if logistic regression
      if (useLRT){ # if LRT
        mod <- mixed(formula = formula, data = modData, method = "LRT", type = "III", family = binomial(), # all_fit = T,
                     control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
      } else { # if lme4
        mod <- glmer(formula = formula, data = modData, family = binomial(),
                     control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
      }
    } else if (modFamily  == "pois"){ # if Poisson regression
      if (useLRT){ # if LRT
        mod <- mixed(formula = formula, data = modData, method = "LRT", type = "III", family = poisson(), # all_fit = T,
                     control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
      } else { # if lme4
        mod <- glmer(formula = formula, data = modData, family = poisson(),
                     control = glmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
      }
    } else { # if linear regression
      if (useLRT){ # if LRT
        mod <- mixed(formula = formula, data = modData, method = "LRT", type = "III", # all_fit = T,
                     control = lmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
      } else { # if lme4
        mod <- lmer(formula = formula, data = modData, 
                    control = lmerControl(optCtrl = list(maxfun = 1e+9), calc.derivs = F, optimizer = c("bobyqa")))
      }      
    }
    
    ## Stop time:
    end.time <- Sys.time(); beep()
    cat(paste0("Stop time is ", end.time, "\n"))
    dif <- difftime(end.time, start.time)
    cat(paste0("Time difference is ", dif, " hours\n"))
    
    # ------------------------------------------------------------------------ #
    ### Print output:
    
    if (useLRT){
      print(anova(mod))
    } else {
      print(summary(mod))
      print(Anova(mod, type = "3")) # 1 p-value per factor
    }
    
    # ------------------------------------------------------------------------ #
    ### Save model:
    
    cat(paste0(">>> Save ", modName, "\n"))
    saveRDS(mod, fullFileName)
    cat(">>> Saved :-)\n")
    
  }
  
  return(mod)
  
}

# ============================================================================ #
#### Print probability of direction for all effects in brms model: ####

print_Pd <- function(mod, nPad = 40){
  #' Print probability of effect (Pd), 
  #' i.e., percentage of posterior that is above/below zero (whatever is smaller),
  #' for each fixed effect in brms model.
  #' @param mod       model object, model fitted in brms.
  #' @param nPad      numeric scalar, number of characters to pad effect name with spaces (default: 40).
  #' @return none, just printing to console.
  
  # Manually:
  # mean(c(as_draws_matrix(brms_mod, variable = "b_reqAction_f1")) < 0) # Probability of direction
  
  print(mod$formula)
  
  ## Extract names of all available fixed effects:
  draws_fit <- as_draws_array(mod) # convert model to large array.
  varVec <- dimnames(draws_fit)$variable # all variables in model.
  effVec <- varVec[grep("b_", varVec, fixed = T)] # retrieve names of fixed effects terms.
  
  ## Loop over effects:
  for (iEff in effVec){ # iEff <- effVec[5]
    
    ## Name of effect, padded:
    iEff_str <- str_pad(iEff, nPad, side = "right", pad = " ")
    
    ## Compute both possible Pds:
    posPd <- mean(c(as_draws_matrix(mod, variable = iEff)) < 0) # Pd for positive effects.
    negPd <- mean(c(as_draws_matrix(mod, variable = iEff)) > 0) # Pd for negative effects.
    
    ## Compare:
    isPos <- posPd < negPd # whether positive or negative effect.
    effDir <- ifelse(isPos, "positive", "negative") # label if positive or negative.
    selPd <- ifelse(isPos, posPd, negPd) # Pd that is smaller.
    
    ## Print:
    cat(paste0(iEff_str, ": ", effDir, " effect, Pd = ", selPd, "\n"))
    
  } # end of iEff for loop
  
} # end of function


# ============================================================================ #
####  Create smoothing function (convolution filter) that works at the edges ####

myFilter <- function(inputVec, window_width = 5){
  #' Smooth vector of numeric data, adaptively shorten smoothing kernel at the edges as appropriate
  #' @param inputVec    vector with numeric data, data to smooth.
  #' @param window_width  numeric scalar, length of kernel (default: 5).
  #' @return outputVec  output vector, smoothed version of input vector.     
  #' 
  cat(paste0("Start with window width = ",window_width, "\n"))
  
  outputVec <- stats::filter(inputVec,
                             filter = rep(1/window_width, window_width),
                             "convolution", sides = 2, circular = F)
  
  ## If NAs at the beginning/end: shorten window length, rerun filter:
  while(sum(is.na(endvariable)) > 0) {
    
    window_width <- max(1, window_width - 2) # shorten window width by 2, but don't got lower than 1
    cat(paste0("Reduce to window width = ", window_width, "\n"))
    outputVecNarrower <- stats::filter(inputVec,
                                       filter = rep(1/window_width, window_width),
                                       "convolution", sides = 2, circular = F)
    
    ## Go through all values of outputVec, if NA, retrieve from outputVecNarrower:
    for (i in 1:length(outputVec)){
      outputVec[i] <- ifelse(is.na(outputVec[i]), outputVecNarrower[i], outputVec[i])
    } 
  }
  return(outputVec)
}

# ============================================================================ #
####  Create percentiles of selected variable: ####

create_percentiles <- function(data, inputVar, nPerc = 5, perSub = F, subVar = "subject_n"){
  #' Create percentiles for numeric variables, either across data or separately for each subject.
  #' @param data      data frame with trial-level data.
  #' @param inputVar  string, variable for which to create percentiles.
  #' @param nPerc     scalar integer, number of percentiles to create.
  #' @param perSub    Boolean, create percentiles for each subject (T) or across all subjects (F), default F
  #' @param subVar    string, variable indicating subject identity (default: "subject_n").
  #' @return data     same data frame with variable added splitting inputVar in percentiles.
  
  # -------------------------------------------------------------------------- #
  ### Check inputs:
  
  if(!(is.data.frame((data)))){stop("data must be a data frame")}
  if(!(inputVar %in% names(data))){stop("inputVar not found in data")}
  if(!(is.numeric(data[, inputVar]))){stop("inputVar has to be numeric")}
  
  # -------------------------------------------------------------------------- #
  ### Create percentiles:
  
  nMissing <- sum(is.na(data[, inputVar]))
  nRow <- nrow(data)
  cat(paste0("Input variable ", inputVar, " has missing values on ", nMissing, " out of ", nRow, " rows (", round(nMissing/nRow*100, 2), "%)\n")) # report missingness
  
  ## Determine percentiles across all subjects:
  if (perSub == F){
    
    # a) Across entire data set:
    outputVar <- paste0(inputVar, "_", nPerc, "Perc_n") # output variable name
    data[, outputVar] <- NA # initialize
    
    cat(paste0("Create variable splitting variable ", inputVar, " into ", nPerc, " quantiles across subjects;\nsave as new variable ", outputVar, "\n"))
    
    ## Split based on quantiles:
    groupQuantiles <- quantile(data[, inputVar], probs = seq(0, 1, by = 1/nPerc), na.rm = TRUE) # create quantiles across all subjects
    data[, outputVar] <- with(data[, inputVar], breaks = groupQuantiles, include.lowest = TRUE)
    
    # -------------------------------------------------------------------------- #
    ### Determine percentiles for each subject separately:
  } else {
    
    ## b) For each subject separately:
    outputVar <- paste0(inputVar, "_", nPerc, "PercSub_n") # new variable name
    data[, outputVar] <- NA # initialize
    
    cat(paste0("Create variable splitting variable ", inputVar, " into ", nPerc, " quantiles for each subject separately;\nsave as new variable ", outputVar, "\n"))
    
    ## Count number of subjects:
    nSub <- length(unique(data[, subVar]))
    
    # Loop over subjects:
    for (iSub in 1:nSub){ # iSub <- 1
      subIdx <- which(data[, subVar] == iSub) # rows for this subject
      subData <- data[subIdx, ] # select data
      subQuantiles <- quantile(data[, inputVar], probs = seq(0, 1, by = 1/nPerc), na.rm = TRUE) # compute quantiles
      data[subIdx, outputVar] <- cut(subData[, inputVar], breaks = subQuantiles, include.lowest = TRUE) # split by quantiles
    }
  }
  
  # Turn output variable into numeric:
  data[, outputVar] <- as.numeric(data[, outputVar]) # into numeric
  
  # Print bin sizes to console:
  cat(paste0("\nBin sizes for ", nPerc, " requested quantiles:\n"))
  print(table(data[, outputVar])) # bin sizes
  
  # Output:
  cat("Finished :-)\n")
  return(data)
}

# ============================================================================ #
####  Create outer tertiles: ####

create_outer_tertiles <- function(data, inputVar, perSub = F, subVar = "subject_n"){
  #' Create tertiles for numeric variables across all subjects or within subjects.
  #' @param data      data frame with trial-level data.
  #' @param inputVar  string, variable for which to create percentiles.
  #' @param perSub    Boolean, create percentiles for each subject (T) or across all subjects (F), default F
  #' @param subVar    string, variable indicating subject identity (default: "subject_n").
  #' @return data     same data frame with variable added splitting inputVar in percentiles, both as numeric ("_n") and factor ("_f").
  
  # -------------------------------------------------------------------------- #
  ### Check inputs:
  
  if(!(is.data.frame((data)))){stop("data must be a data frame")}
  if(!(inputVar %in% names(data))){stop("inputVar not found in data")}
  if(!(is.numeric(data[, inputVar]))){stop("inputVar has to be numeric")}
  
  nMissing <- sum(is.na(data[, inputVar]))
  nRow <- nrow(data)
  cat(paste0("Input variable ", inputVar, " has missing values on ", nMissing, " out of ", nRow, " rows (", round(nMissing/nRow*100, 2), "%)\n")) # report missingness
  
  # -------------------------------------------------------------------------- #
  ### Initialize new variable:  
  
  nPerc <- 3
  if (perSub == F){
    # a) Across entire data set:
    outputVar <- paste0(inputVar, "_", nPerc, "Perc_n") # new variable name
    outputVar_f <- gsub("_n$", "_f", outputVar)
  } else {
    outputVar <- paste0(inputVar, "_", nPerc, "PercSub_n") # new variable name
    outputVar_f <- gsub("_n$", "_f", outputVar)
  }
  
  cat(paste0("Create variable indicating outer tertiles for variable ", inputVar, ";\nsave as new variables ", outputVar, " and ", outputVar_f, "\n"))
  
  # -------------------------------------------------------------------------- #
  ### Delete variables if they already exist:  
  
  if(outputVar %in% names(data)){
    cat("Delete old variables\n")
    data[, outputVar] <- NULL # delete
    data[, outputVar_f] <- NULL # delete
  }
  
  # -------------------------------------------------------------------------- #
  ### Create 3 percentiles:
  
  data <- create_percentiles(data, inputVar, nPerc = nPerc, perSub = perSub, subVar = subVar)
  
  # -------------------------------------------------------------------------- #
  ### Turn into factor:
  
  data[, outputVar_f] <- factor(ifelse(data[, outputVar] == 1, "low",
                                       ifelse(data[, outputVar] == 2, "middle",
                                              ifelse(data[, outputVar] == 3, "high",
                                                     NA))))
  
  # -------------------------------------------------------------------------- #
  ### Delete middle tertile:
  
  data[which(data[, outputVar] == 2), outputVar] <- NA # delete middle tertile
  data[which(data[, outputVar_f] == "middle"), outputVar_f] <- NA # delete middle tertile
  
  # -------------------------------------------------------------------------- #
  ### Print bin sizes to console:
  
  cat("\nNew bin sizes of outer tertiles:\n")
  print(table(data[, outputVar])) # bin sizes
  
  nMissing <- sum(is.na(data[, outputVar]))
  nRow <- nrow(data)
  cat(paste0("New output variable ", outputVar, " has missing values on ", nMissing, " out of ", nRow, " rows (", round(nMissing/nRow*100, 2), "%)\n")) # report missingness
  
  ## Output:
  cat("Finished :-)\n")
  return(data)
  
}

# ==================================================================================================== #
#### Process variables (demean/ standardize/ conditional probabilities) for plotting in long format: ####

prepare_data_plot_long <- function(data, xVar, yVar, subVar = "subject_n", jitterNum = 0.09){
  require(plyr)
  
  # -------------------------------------------------------------------------- #
  ### Input checks:
  if(!(is.data.frame((data)))){stop("data must be a data frame")}
  if(!(yVar %in% names(data))){stop("yVar not found in data")}
  if(!(xVar %in% names(data))){stop("xVar not found in data")}
  
  ## Check variable types:
  if(!(is.numeric(data[, yVar]))){stop("yVar has to be numeric")}
  if(!(is.numeric(data[, xVar]))){stop("xVar has to be numeric")}
  
  ## Exclude any rows with NAs: 
  rowIdx <- which(complete.cases(data[, xVar]) & complete.cases(data[, yVar]))
  cat(paste0("Excluding rows with NA on xVar or yVar: Retain ", length(rowIdx), " out of ", nrow(data), " rows (excluded ", nrow(data) - length(rowIdx), " rows)\n"))
  data <- droplevels(data[rowIdx, ])
  
  # -------------------------------------------------------------------------- #
  ### Assign to new variables:
  
  data$x <- data[, xVar]
  data$y <- data[, yVar]
  data$subject <- data[, subVar]
  
  # -------------------------------------------------------------------------- #
  ### Aggregate yVar per xVar level per subject:
  
  cat(paste0("Aggregate data for each level of variables ", xVar, " and ", subVar, "\n"))
  
  d <- ddply(data, .(x, subject), function(iData){
    
    y <- mean(iData$y, na.rm = T)
    
    return(data.frame(y))
    dev.off()
  })
  
  # -------------------------------------------------------------------------- #
  ### Format variables in aggregated data set:
  
  d$condition <- d$x # save condition labels 
  d$x <- as.numeric(d$x) # turn numeric variable
  d$subject_f <- factor(d$subject)   
  
  # -------------------------------------------------------------------------- #
  ### Add jitter to x (position):
  
  set.seed(321)
  if (jitterNum == 0){ # if no jitter
    d$j <- 0
  } else { # if jitter
    d$j <- jitter(rep(0, nrow(d)), amount = jitterNum) # default: 0.9
  }
  d$xj <- d$x + d$j
  
  cat(paste0("Min = ", round(min(d$y, na.rm = T), 3), "; max = ", round(max(d$y, na.rm = T), 3), "\n"))
  
  ## Return aggregated data set:
  return(d)
  
}

# ============================================================================ #
#### Select data by pattern and reshape into long format: ####

select_reshape_long <- function(data, selPattern){
  #' Select variables with given pattern, reshape into wide format.
  #' @param data data frame.
  #' @param selPattern string, pattern to select variables by.
  #' @return preprocessed data frame containing $x, $y, $xj, $subject, $subject_f
  
  # -------------------------------------------------------------------------- #
  ### Check if input variables included in data set:
  
  if(!(is.data.frame((data)))){stop("data must be a data frame")}
  
  # -------------------------------------------------------------------------- #
  ### Select all columns given selPattern:
  
  cat(paste0("Select variables that match pattern ", selPattern, "\n"))
  colIdx <- grepl(selPattern, names(data), fixed = TRUE)
  if(sum(colIdx) == 0){stop("Found no variables in data that match selPattern ", selPattern)}
  selData_wide <- data[, colIdx] # select only those variables
  
  # -------------------------------------------------------------------------- #
  ### Add subject ID:
  
  subVar <- "subject_n"
  cat(paste0("Add subject identifier ", subVar, " based on row number\n"))
  selData_wide[, subVar] <- 1:nrow(selData_wide) # add subject ID
  selVariables <- names(selData_wide)[grepl(selPattern, names(selData_wide), fixed = TRUE)] # save question names for later
  
  # -------------------------------------------------------------------------- #
  ### Reshape into long format:
  
  cat("Reshape data from wide into long format\n")
  selData_long <- reshape(selData_wide, varying = selVariables, idvar = subVar, 
                          timevar = "item", v.names = "rating", direction = "long")
  
  # -------------------------------------------------------------------------- #
  ## Retrieve question names:
  
  selData_long$item <- selVariables[selData_long$item] # copy over question names
  selData_long$item <- factor(selData_long$item) # to factor
  selData_long$subject_f <- factor(selData_long$subject_n) # to factor
  
  return(selData_long)
  
}

# ============================================================================ #
#### Plot histogram with bar plots for debriefQ: ####

custom_histogram_debriefQ <- function(data, selVar){
  
  # -------------------------------------------------------------------------- #
  ### Check if input variables included in data set:
  
  if(!(is.data.frame((data)))){stop("data must be a data frame")}
  if(!(selVar %in% names(data))){stop("selVar not found in data")}
  
  # -------------------------------------------------------------------------- #
  ### Initialize question details:
  
  questNumVec <- c("Q07a", "Q07b", "Q07c", "Q07d",
                   "Q09a", "Q09b",
                   "Q10a", "Q10b", "Q10c", "Q10d", "Q10e", "Q10f",
                   "Q11a", "Q11b", "Q11c", "Q11d", "Q11e", "Q11f",
                   "Q12a", "Q12b"
  )
  ## Question text:
  questTextVec <- c(# Q07
    "How motivated were you to gain extra lives in summer rounds?",
    "How motivated were you to not lose lives in winter rounds?",
    "How easy was it to estimate how many nutrients were available from each algae?",
    "To what extent did the time limit change your behaviour at the end of the rounds?",
    # Q09
    "Gain extra lives through gaining nutrients from algae in summer",
    "Avoid losing lives through gaining nutrients from algae in winter",
    # Q10
    "The button presses required to swim down to the algae.",
    "The time penalty/ button presses for moving to the next patch.",
    "The pie chart informing you of the proportion of poor, mid and rich algae in each round.",
    "Whether you gained nutrients on your last swim down to this algae.",
    "How long you had been at the particular algae.",
    "The amount of energy you had in your energy bar.",
    # Q11
    "The effort level required to swim down to the algae.",
    "The time penalty/ button presses for moving to the next patch.",
    "The pie chart informing you of the proportion of poor, mid and rich algae in each round.",
    "Whether you gained nutrients on your last swim down to this algae.",
    "How long you had been at the particular algae.",
    "The amount of energy you had in your energy bar.",
    # Q12
    "... when diving down to search for nutrients?",
    "... traveling to the next algae?"
    
  )
  
  ## X-axis minimum:
  questXMinVec <- c(0, 0, 0, 0, # Q07
                    -0, -0, # Q09
                    -0, -0, -0, -0, -0, -0, # Q10 
                    -0, -0, -0, -0, -0, -0, # Q11
                    -0, -0 # Q12
  )
  ## X-axis maximum:
  questXMaxVec <- c(6, 6, 6, 6, # Q07
                    4, 4, # Q09
                    4, 4, 4, 4, 4, 4, # Q10
                    5, 5, 5, 5, 5, 5, # Q11
                    4, 4 # Q12
  )
  
  ## ------------------------------------------------------------------------- #
  ## Identify question:
  
  selIdx <- which(questNumVec == selVar)
  xMin <- questXMinVec[selIdx]
  xMax <- questXMaxVec[selIdx]
  main <- questTextVec[selIdx]
  nSub <- nrow(data)
  
  ## Determine x-axis limits empirically:
  # xMin <- min(data[, selVar], na.rm = T)
  # xMax <- max(data[, selVar], na.rm = T)
  
  ## ------------------------------------------------------------------------- #
  ## Plot histogram:
  FTS <- 20
  
  ## Plot histogram:
  p <- ggplot(data, aes_string(x = selVar)) + 
    
    ## Add bar plot:
    geom_bar() + 
    
    ## Add scales:
    scale_x_continuous(limits = c(xMin - 0.5, xMax + 0.5), 
                       breaks = seq(xMin, xMax, 1)) + 
    coord_cartesian(ylim = c(0, nSub)) + 
    
    ## Add title:
    ggtitle(add_line_breaks(main, n = 40, max_n = 50)) + 
    
    ### Add title:
    theme_bw() +
    # theme_minimal() +
    theme(axis.text = element_text(size = FTS),
          axis.title = element_text(size = FTS), 
          plot.title = element_text(size = FTS, hjust = 0.5), # center title 
          legend.text = element_text(size = FTS))
  print(p)
  return(p)
}

# ============================================================================ #
#### Plot histogram with bar plots for debriefNewQ: ####

custom_histogram_debriefNewQ <- function(data, selVar){
  
  # -------------------------------------------------------------------------- #
  ### Check if input variables included in data set:
  
  if(!(is.data.frame((data)))){stop("data must be a data frame")}
  if(!(selVar %in% names(data))){stop("selVar not found in data")}
  
  # -------------------------------------------------------------------------- #
  ### Initialize question details:
  
  questNumVec <- c("Q06a", "Q06b", "Q06c", "Q06d", "Q06e", "Q06f", "Q06g",
                   "Q07a", "Q07b",
                   "Q08a", "Q08b", "Q08c", "Q08d", "Q08e", "Q08f",
                   "Q08g", "Q08h", "Q08i", "Q08j", "Q08k", "Q08l"
  )
  ## Question text:
  questTextVec <- c(
    ## Q06
    "The button presses required to swim down to the algae.",
    "The time penalty/ button presses for moving to the next patch.",
    "The pie chart informing you of the proportion of poor, mid and rich algae in each round.",
    "Whether you gained nutrients on your last swim down to this algae.",
    "How long you had been at the particular algae.",
    "How many stars you had already collected at the current algae.",
    "The amount of energy you had in your energy bar.",
    # Q07:
    "... when diving down to search for nutrients?",
    "... traveling to the next algae?",
    # Q08:
    "I used the pie chart (proportion of rich/ medium/ poor algae) to guess the quality of the current algae.",
    "I used the pie chart (proportion of rich/ medium/ poor algae) to guess the quality of the next algae.",
    "I tried to identify whether I was currently in a rich, medium, or poor algae.",
    "I only focused on seemingly rich algae and left other algae immediately.",
    "There was a maximum number of dives after which I always left an algae.",
    "There was a maximum number of collected stars after which I always left an algae.",
    "I followed a rule in which I stayed for a fixed number of dives at the beginning before starting to consider whether to leave or not.",
    "I followed a rule in which I moved to the next algae when I had not received any stars for a fixed number of dives in a row.",
    "In the winter rounds, I stayed longer at the current algae than in summer rounds.",
    "In the winter rounds, I moved on to the next algae sooner than in summer rounds.",
    "Towards the end of a round, I stayed at the current algae longer than usual.",
    "Towards the end of a round, I moved on to the next algae sooner than usual."
  )
  
  ## X-axis minimum:
  questXMinVec <- c(0, 0, 0, 0, 0, 0, 0, # Q06
                    0, 0, # Q07
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 # Q10 
  )
  ## X-axis maximum:
  questXMaxVec <- c(4, 4, 4, 4, 4, 4, 4, # Q06
                    4, 4, # Q07
                    6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6 # Q08
  )
  
  ## ------------------------------------------------------------------------- #
  ## Identify question:
  
  selIdx <- which(questNumVec == selVar)
  xMin <- questXMinVec[selIdx]
  xMax <- questXMaxVec[selIdx]
  main <- questTextVec[selIdx]
  nSub <- nrow(data)
  
  ## Determine x-axis limits empirically:
  # xMin <- min(data[, selVar], na.rm = T)
  # xMax <- max(data[, selVar], na.rm = T)
  
  ## ------------------------------------------------------------------------- #
  ## Plot histogram:
  FTS <- 20
  
  ## Plot histogram:
  p <- ggplot(data, aes_string(x = selVar)) + 
    
    ## Add bar plot:
    geom_bar() + 
    
    ## Add scales:
    scale_x_continuous(limits = c(xMin - 0.5, xMax + 0.5), 
                       breaks = seq(xMin, xMax, 1)) + 
    coord_cartesian(ylim = c(0, nSub)) + 
    
    ## Add title:
    ggtitle(add_line_breaks(main, n = 40, max_n = 50)) + 
    
    ### Add title:
    theme_bw() +
    # theme_minimal() +
    theme(axis.text = element_text(size = FTS),
          axis.title = element_text(size = FTS), 
          plot.title = element_text(size = FTS, hjust = 0.5), # center title 
          legend.text = element_text(size = FTS))
  print(p)
  return(p)
}

# ============================================================================ #
#### Determine y-axis limits based on case-by-case distinction: ####

# OUTDATED, uses ranges of xMin/ xMax; see better functions below

find_lim_cases <- function(data, yVar = "y"){
  #' Determine optimal y-axis limits based on some input heuristics.
  #' @param data data frame, ideally aggregated per subject, in long format, with variable \code{yVar}.
  #' @return yLim vector with to elements: minimal and maximal y-axis limit.
  
  require(plyr)
  
  # -------------------------------------------------------------------------- #
  ### Check if input variables included in data set:
  
  if(!(is.data.frame((data)))){stop("data must be a data frame")}
  if(!(yVar %in% names(data))){stop("yVar not found in data")}
  if(!(is.numeric(data[, yVar]))){stop("yVar has to be numeric")}
  
  cat("Automatically detect axis limits\n")
  
  # -------------------------------------------------------------------------- #
  ### Determine minimum and maximum:
  
  yMin <- min(data[, yVar], na.rm = T)
  yMax <- max(data[, yVar], na.rm = T)
  nRound <- 3
  cat(paste0("Detected yMin = ", round(yMin, nRound), ", yMax = ", round(yMax, nRound), "\n"))
  
  # -------------------------------------------------------------------------- #
  ### Determine axis limits based on case-by-case distinction: 
  
  ## If likely probability:
  if(yMin >= 0 & yMax <= 1){ 
    cat("Set automatic yLim to [0, 1]\n")
    yLim <- c(0, 1)
    
    # ------------------------------------- #
    ## If both positive numbers, but not huge:
    
  } else if(yMin >= 0 & yMax <= 30){ # if rather small positive number: plot from 0 onwards to 110% of yMax
    cat("Set automatic yLim to [yMin*0.90, round(yMax*1.1, 1)]\n")
    # yLim <- c(round(yMin * 0.90, 1), round(yMax * 1.1, 1))
    yLim <- c(floor(yMin * 0.95), ceiling(yMax * 1.05))
    
    # ------------------------------------- #
    ## If both positive numbers and yMax is huge:
    
  } else if ((yMin > 0) & (yMax > 100)) { # if very big number: plot from 0 onwards to next hundred:
    cat("Set automatic yLim to [0, next hundred]\n")
    yMin <- 0 # from zero onwards
    yMax <- round_any(yMax, 100, f = ceiling) # round up to next hundred
    yLim <- c(yMin, yMax)
    
    # ------------------------------------- #
    ## Else if yMin negative and yMax positive: enforce symmetric yLim using the bigger of yMin and yMax:
    
  } else if ((yMin < 0) & (yMax > 0)) { # take the numerically bigger one, symmetric around 0
    
    cat("Set automatic yLim to be symmetric, use bigger of yMinAbs and yMaxAbs\n")
    yMaxAbs <- ceiling(c(abs(yMin), yMax)) # round to next integer
    yMaxAbs <- yMaxAbs[1] # only first entry
    yLim <- c(-1*yMaxAbs, 1*yMaxAbs)
    
  } else { # take the numerically bigger one, symmetric around 0
    
    stop("Unclear axis setting")
    
  }
  
  ## Check if only 2 output elements:
  
  if (length(yLim) < 2){stop("yLim output has less than 2 elements, please check input\n")}
  if (length(yLim) > 2){stop("yLim output has more than 2 elements, please check input\n")}
  
  cat(paste0("Return axis limits ", yLim[1], " - ", yLim[2], "\n"))
  
  return(yLim)
} 

# ============================================================================ #
#### Find plausible axis limits: #####

round_lim <- function(input){
  #' For given input vector [xMin, xMax], round them such that they become plausible axis limits
  #' given their magnitude. 
  #' @param input vector of 2 numerics, axis limits.
  #' @return xStep scalar numeric, optimal step size
  
  # -------------------------------------------------------------------------- #
  ### Check input:  
  
  if(!(is.numeric(input))){stop("input has to be numeric")}
  if(length(input) != 2){stop("input has to be a vector of length 2")}
  if(input[2] < input[1]){stop("Expect input [xMin, xMax], but xMax is smaller than xMin")}
  
  # -------------------------------------------------------------------------- #
  ### Detect scale of both inputs:
  absVec <- abs(input) # take absolute for determining scale
  xUnit <- min(floor(log10(absVec))) # detect smaller of both magnitudes
  xScale <- 10^xUnit # compute scaling factor
  output <- round(input/xScale)*xScale # scale up/down, round, scale down/up again
  
  # -------------------------------------------------------------------------- #
  ### If both limits are identical: reduce unit by 1:
  while(output[1] == output[2]){
    xUnit <- xUnit - 1 # reduce unit 
    xScale <- 10^xUnit # scaling factor 
    output <- round(input/xScale)*xScale # scale up/down, round, scale down/up again
  }
  
  return(output)
  
}

# ============================================================================ #
#### Find plausible tick step size given axis limits: #####

find_step <- function(input, nTickTarget = 5){
  #' Plot learning curve per cue per subject using basic plot() function 
  #' @param input vector of 2 numerics, axis limits.
  #' @param nTickTarget scalar integer, number of desired axis ticks (default: 5).
  #' @return step scalar numeric, optimal step size.
  
  # -------------------------------------------------------------------------- #
  ### Check inputs:
  
  if(!(is.numeric(input))){stop("input has to be numeric")}
  if(length(input) != 2){stop("xLim must have 2 elements")}
  if(input[2] < input[1]){stop("xMax must be larger than xMin")}
  if(length(nTickTarget) != 1){stop("nTickTarget must have 1 element")}
  if(nTickTarget != round(nTickTarget)){stop("nTickTarget must be an integer")}
  
  # -------------------------------------------------------------------------- #
  ### Compute ideal step size:
  
  ## Repertoire of possible ticks:
  expVec <- seq(-5, 5, 1) # exponents for candidate ticks
  tickVec <- sort(c(10^expVec, 5 * 10^expVec)) # candidate steps: either 1 or 5 of different magnitudes
  
  ## Find optimal step target:
  stepTarget <- (input[2] - input[1]) / nTickTarget # desired length step
  stepIdx <- which(abs(tickVec - stepTarget) == min(abs(tickVec - stepTarget))) # find minimum
  stepIdx <- stepIdx[1] # select first in case of multiple minima
  step <- tickVec[stepIdx] # extract optimal step
  
  ## Return:
  cat(paste0("Use step size ", step, " for axis ticks\n"))
  return(step)
  
}

# ============================================================================ #
#### Find plausible axis break points: ####

find_break_points <- function(input, nTickTarget = 4){
  #' Find ideal break points given axis limits and desired number of ticks: 
  #' @param input vector of 2 numerics, axis limits.
  #' @param nTickTarget scalar integer, number of desired axis ticks (default: 5).
  #' @return step scalar numeric, optimal step size.
  
  # -------------------------------------------------------------------------- #
  ### Check inputs:
  
  if(!(is.numeric(input))){stop("input has to be numeric")}
  if(length(input) != 2){stop("xLim must have 2 elements")}
  if(length(nTickTarget) != 1){stop("nTickTarget must have 1 element")}
  if(nTickTarget != round(nTickTarget)){stop("nTickTarget must be an integer")}
  
  # -------------------------------------------------------------------------- #
  ### Old calculations (legacy):
  
  # iMag <- log10(abs(xMin)) # determine order of magnitude for rounding
  # iMag <- ifelse(iMag < 0, floor(iMag), ceil(iMag)) # round
  # xUnit <- 10 ^ iMag
  # xBreakMin <- floor(xMin/xUnit)*xUnit # remove post-digit part, round, transform back
  # 
  # ## Determine x-axis upper limit:
  # iMag <- log10(abs(xMax)) # determine order of magnitude for rounding
  # iMag <- ifelse(iMag < 0, floor(iMag), ceil(iMag)) # round
  # xUnit <- 10 ^ iMag
  # xBreakMax <- ceiling(xMax/xUnit)*xUnit # remove post-digit part, round, transform back
  
  # -------------------------------------------------------------------------- #
  ### Extract from input:
  
  xBreakMin <- input[1]
  xBreakMax <- input[2]
  
  # -------------------------------------------------------------------------- #
  ### xStep either 1 or 5; try out both:
  
  xStep <- find_step(c(xBreakMin, xBreakMax), nTickTarget = nTickTarget)
  
  ## Correct if one limit smaller than xStep:
  if (abs(xBreakMin) < xStep){xBreakMin <- 0}
  if (abs(xBreakMax) < xStep){xBreakMax <- 0}
  
  cat(paste0("xBreakMin = ", xBreakMin, ", xBreakMax = ", xBreakMax, ", xStep = ", xStep, "\n"))
  
  # -------------------------------------------------------------------------- #
  ### Distance between x-axis ticks:
  
  breakVec <- seq(xBreakMin, xBreakMax, xStep) # just very broad break points, aligned to magnitude
  if(!(0 %in% breakVec)){ # correct if zero not included
    closestToZero <- breakVec[abs(breakVec) == min(abs(breakVec))]
    breakVec <- breakVec - closestToZero # subtract so becomes zero
  }
  cat(paste0("Use axis break points from ", min(breakVec), " till ", max(breakVec), " in steps of ", breakVec[2] - breakVec[1], "\n"))
  
  return(breakVec)
  
}

# ============================================================================ #
#### Plot single bar plot with individual data points: ####

custom_singlebar <- function(data, yVar, yLim = c(0, 1), isViolin = FALSE, hLine = NULL,
                             selCol = "grey80", xLab = "x", yLab = NULL, main = NULL){
  #' Plot single bar or half-density (violin) with single points per data point.
  #' @param data data frame, with variable \code{yVar} to plot.
  #' @param yVar string, name of variable to plot.
  #' @param isViolin Boolean, plot half-density (violin) instead of bar (default: FALSE)
  #' @param hLine scalar numeric, y-axis coordinate for horizontal dashed line (default: NULL).
  #' @param selCol scalar string, color to use for violin/ bar (default: grey80).
  #' @param xLab string, label for x-axis (default: "x")
  #' @param yLab string, label for y-axis (default: "y")
  #' @param main string, title of plot (default: "NULL")
  #' @return prints and returns ggplot object.
  
  ## Load packages:
  require(ggplot2)
  require(gghalves)
  require(ggbeeswarm)
  
  ## Aggregate again with Rmisc:
  # library(Rmisc)
  # summary_d <- summarySEwithin(d,measurevar = "ACC", idvar = "sID", na.rm = T)
  
  # -------------------------------------------------------------------------- #
  ### Check inputs:
  
  ## Check if input variables included in data set:
  if(!(is.data.frame((data)))){stop("data must be a data frame")}
  if(!(yVar %in% names(data))){stop("yVar not found in data")}
  
  ## Check variable types:
  if(!(is.numeric(data[, yVar]))){stop("yVar has to be numeric")}
  
  ## y-axis label:
  if(is.null(yLab)){yLab <- substitute_label(yVar)}
  if(!(is.character(yLab))){stop("yLab has to be a character string")}
  
  # -------------------------------------------------------------------------- #
  ### Fixed settings:
  
  FTS <- 30
  LWD <- 1.5
  colAlpha <- .70
  
  # -------------------------------------------------------------------------- #
  ### Prepare data set:
  
  ## Add jittered x-axis position:
  nSub <- nrow(data)
  data$x <- rep(1, nSub)
  data$j <- jitter(rep(0, nrow(data)), amount = .05)
  data$xj <- data$x + data$j
  
  ## Repeat selected variable:
  data$y <- data[, yVar]
  
  # -------------------------------------------------------------------------- #
  ### Start plot:
  
  p <- ggplot(data = data, aes(x = x, y = y)) # initialize
  
  # -------------------------------------------------------------------------- #
  ## Bar or violin:
  
  if (isViolin){ # if violin
    p <- p + geom_half_violin(color = "black", fill = selCol, alpha = colAlpha, trim = FALSE)
  } else { # if bar
    p <- p + stat_summary(fun = mean, geom = "bar", fill = selCol, alpha = colAlpha,
                          color = 'black', width = 0.3, lwd = LWD)
  }
  
  # -------------------------------------------------------------------------- #
  ## Confidence intervals: 
  
  p <- p + stat_summary(fun.data = mean_cl_normal, geom =
                          "errorbar", width = 0.05, lwd = LWD)
  
  # -------------------------------------------------------------------------- #
  ## Individual data points:
  
  # p <- p + geom_beeswarm(color = "black", fill = color, alpha = colAlpha)
  p <- p + geom_point(data = data, aes(x = xj), color = "black", fill = "grey40",
                      shape = 21, size = 4,
                      alpha = colAlpha)
  
  # -------------------------------------------------------------------------- #
  ## Other settings:
  
  if (!(is.null(hLine))){
    p <- p + geom_hline(yintercept = hLine, linetype = 2, color = "black")
  }
  if (mean(yLim) == 0.5){ # if conditional probabilities:
    p <- p + geom_hline(yintercept = 0.5, linetype = 2, color = "black")
  }
  
  ## X-axis:
  p <- p + scale_x_continuous(limits = c(0.5, 1.5), breaks = c(0, 1, 2), labels = c("", "", ""))
  
  ## Y-axis:
  if (yLim[1] >= 0 & yLim[-1] <= 1){
    yBreak <- 0.25
    p <- p + scale_y_continuous(limits = yLim, breaks = seq(yLim[1], yLim[-1], yBreak))
  } else {
    yBreak <- ceiling((yLim[-1] - yLim[1])/5)
    cat(paste0("Use yBreak = ", yBreak))
    p <- p + scale_y_continuous(limits = yLim, breaks = seq(yLim[1], yLim[-1], yBreak))
  }
  if (yLim[1] > 0){
    p <- p + coord_cartesian(ylim = yLim) # to avoid bar being dropped
  }
  
  ## Axis labels:
  p <- p + xlab(xLab) + ylab(yLab)
  
  ## Add title:
  if(!is.null(main)){p <- p + ggtitle(main)}    
  
  ## Add theme:
  p <- p + theme_classic()
  
  ## Font sizes:
  p <- p + theme(axis.text = element_text(size = FTS),
                 axis.title = element_text(size = FTS), 
                 plot.title = element_text(size = FTS, hjust = 0.5), # center title 
                 legend.text = element_text(size = FTS))
  
  # Print plot in the end:
  print(p)
  return(p)
  
}

# ===========================================================================  #
#### Plot flexible raincloud plot: #####

plot_raincloud <- function(data, xVar, yVar, subVar = "subject_n",
                           isBar = F, isPoint = T, isViolin = F, isBoxplot = F, isMean = T, 
                           isMirror = F, useCondVec = NULL, jitterNum = 0.09,
                           yLim = NULL, color, condLabels = NULL, xLab = "x", yLab = "y", main = NULL, FTS = NULL){
  #' Make raincloud plot
  #' @param data data frame, with variables \code{variables}
  #' @param xVar string, name of variable that goes on x-axis. Factor or numeric.
  #' @param yVar string, name of variable that goes on y-axis. Variable needs to be numeric.
  #' @param subVar string, name of variable containing subject identifier (default: subject).
  #' @param isBar Boolean, plot means per condition as bar (default: FALSE)
  #' @param isPoint Boolean, plot individual data points per condition as small points (default: TRUE)
  #' @param isViolin Boolean, plot distribution per condition as violin plot (default: TRUE)
  #' @param isBoxplot Boolean, plot median and interquartile per condition with boxplot (default: FALSE)
  #' @param isMean Boolean, plot means per condition as thick connected points with error bars (default: TRUE) 
  #' @param isMirror Determine position of boxplots and violin plots: always on the left (FALSE, default) or mirred to the middle (TRUE)
  #' @param useCondVec vector of numbers, use only subset of input conditions (default: all conditions)
  #' @param jitterNum numeric, amount of jitter to use for points (default: 0.9).
  #' @param isMirror Determine position of boxplots and violin plots: always on the left (FALSE, default) or mirred to the middle (TRUE)
  #' @param yLim vector of two numbers for y-axis limits (default: determine based on min and max of input data).
  #' @param color vector of strings (HEX colors), colors for input conditions (d$x)
  #' @param condLabels vector of strings, labels for input conditions (d$x) on the x-axis
  #' @param xLab string, label for x-axis (default: retrieved via substitute_label()).
  #' @param yLab string, label for y-axis (default: retrieved via substitute_label()).
  #' @param main string, title of plot (optional).
  #' @param FTS integer, font size for axes ticks and labels and title.
  #' @return makes raincloud plot
  #'  See: https://github.com/jorvlan/open-visualizations/blob/master/R/repmes_tutorial_R.Rmd
  #'  See: https://github.com/jorvlan/open-visualizations/blob/master/R/repmes_tutorial_R.pdf
  
  require(ggplot2)
  require(ggstatsplot) # for geom_flat_violin
  require(gghalves) # for half plots
  
  # ---------------------------------------------------------------------------- #
  ### Check inputs:
  
  if(!(is.data.frame((data)))){stop("data must be a data frame")}
  if(!(yVar %in% names(data))){stop("yVar not found in data")}
  if(!(xVar %in% names(data))){stop("xVar not found in data")}
  if(!(subVar %in% names(data))){stop("subVar not found in data")}
  
  ## Check variable types:
  if(!(is.numeric(data[, yVar]))){stop("yVar has to be numeric")}
  if(!(is.factor(data[, xVar]))){stop("xVar has to be a factor")}
  
  ## Axis labels:
  if(is.null(xLab)){xLab <- substitute_label(xVar)}
  if(is.null(yLab)){yLab <- substitute_label(yVar)}
  if(!(is.character(xLab))){stop("xLab has to be a character string")}
  if(!(is.character(yLab))){stop("yLab has to be a character string")}
  
  ## Exclude any rows with NAs: 
  rowIdx <- which(complete.cases(data[, xVar]) & complete.cases(data[, yVar]))
  cat(paste0("Excluding rows with NA on xVar or yVar: Retain ", length(rowIdx), " out of ", nrow(data), " rows (excluded ", nrow(data) - length(rowIdx), " rows)\n"))
  data <- droplevels(data[rowIdx, ])
  
  # -------------------------------------------------------------------------- #
  ### General plotting settings:
  
  colAlpha <- .50
  LWD <- 1.5
  posNudge = 0 # .13
  
  # -------------------------------------------------------------------------- #
  ### Aggregate data per condition per subject into long format:
  
  d <- prepare_data_plot_long(data, x = xVar, y = yVar, subVar = subVar, jitterNum = jitterNum)
  
  # -------------------------------------------------------------------------- #
  ### Determine defaults:
  
  # Determine y limits if not given:
  if(is.null(yLim)){
    cat("Automatically determine y-axis\n")
    yLim <- find_lim_cases(d)
  }
  if (length(yLim) != 2){stop("yLim must be of length 2")}
  
  ## Determine number of conditions to use if not provided:
  if(is.null(useCondVec)){ # if not defined: use all conditions
    nCond <- length(unique(d$x))
    useCondVec <- 1:nCond #  sort(unique(d$x))
    d$condition <- as.numeric(as.factor(d$x)) # convert to numeric from 1 to nCond 
  } else {
    cat(paste0("Use only conditions ", toString(useCondVec)), "\n")
    nCond <- length(useCondVec)
    # Loop through selected conditions, recompute x so it is consecutive:
    ix <- 0 # initialize count
    for (iCond in useCondVec){ # loop through conditions, create numeric variable "x":
      ix <- ix + 1 # increment
      d[which(d$condition == iCond), "x"] <- ix
    } 
    d$xj <- d$x + d$j # recompute xj
  }
  
  if (is.null(condLabels)){
    cat("Automatically extract x-axis labels\n")
    condLabels <- sort(unique(d$x))
  }
  
  if(length(color)!=nCond){
    cat("Less colors provided than conditions, repeat first color for all conditions\n")
    color <- rep(color[1], nCond)
  }
  if(length(condLabels) != nCond){
    cat("Less labels provided than conditions, use just numbers from 1 to number of conditions\n")
    condLabels <- 1:nCond
  }
  
  if (is.null(FTS)){
    ## Font sizes for ordinary viewing: 15
    # FTS <- 15
    ## Font sizes for saving: 30
    FTS <- 30
    cat(paste0("No font size provided, use font size ",FTS), "\n")
  }
  
  # -------------------------------------------------------------------------- #
  ### Compute summary per condition across subjects:
  
  require(Rmisc)
  cat("Aggregate data across subjects\n")
  dsel = subset(d, condition %in% useCondVec) # select used conditions:
  summary_d <- summarySEwithin(dsel, measurevar = "y", withinvar = "x", idvar = "sID", na.rm = T)
  # automatically rescales x to 1:nCond
  # summary_d$x <- as.numeric(summary_d$x)
  if (is.numeric(d$x)){summary_d$x <- sort(unique(d$x))} # back to original numerical values
  summary_d$condition <- as.numeric(as.factor(summary_d$x)) # consecutive count (for labels)
  
  # -------------------------------------------------------------------------- #
  ## Determine position of boxplots and violin plots: always on the left (FALSE, default) or mirred to the middle (TRUE):
  
  if(isMirror == T){ # if mirror images for box plots and violin plots
    sideVec <- c(rep("l", floor(nCond/2)), rep("r", ceiling(nCond/2)))
  } else { # otherwise default
    sideVec <- rep("l", nCond)
  }
  
  # -------------------------------------------------------------------------- #
  ## Start ggplot: 
  
  p <- ggplot(data = d, aes(y = y)) # initialize
  
  # -------------------------------------------------------------------------- #
  ### Add bars:
  
  if (isBar == T){
    cat("Make bar plot\n")
    
    for(iCond in useCondVec){
      p <- p + geom_bar(data = subset(summary_d, condition == iCond), aes(x = x, y = y), stat = "identity",
                        fill = color[iCond], col = "black", width = 0.4, lwd = LWD, alpha = 0.3)
      p <- p + geom_errorbar(data = subset(summary_d, condition == iCond), 
                             aes(x = x, y = y, ymin = y-ci, ymax = y+ci),
                             color = "black", width = 0.10, lwd = LWD, alpha = 0.6) 
    }
  }
  
  # -------------------------------------------------------------------------- #
  ### Add individual data points:
  
  if(isPoint == T){
    cat("Make individual data points\n")
    
    for(iCond in useCondVec){
      p <- p + geom_point(data = subset(d, condition == iCond), aes(x = xj), color = color[iCond], size = 1.5, 
                          alpha = .35) # alpha = .50
    }
    # Add lines to combine points:
    p <- p + geom_line(data = subset(d, condition %in% useCondVec), aes(x = xj, group = subject), 
                       size = 1.0, color = 'grey40', alpha = 0.35) # lightgray
  }
  # Till 2021-11-22: color = grey70, size = 1
  
  # -------------------------------------------------------------------------- #
  ### Add box plots and/or violin plots:
  
  ## Add box plots:
  if (isBoxplot == T){
    
    cat("Make box plot\n")
    
    for(iCond in useCondVec){
      p <- p + geom_half_boxplot(
        data = subset(d, condition == iCond), aes(x=x, y = y), position = position_nudge(x = .15), 
        side = sideVec[iCond], outlier.shape = NA, center = TRUE, errorbar.draw = TRUE, width = .1, 
        fill = color[iCond], alpha = colAlpha)
    }
  }
  
  ## Violin plots:
  if (isViolin == T){
    
    cat("Make violin plot\n")
    
    for(iCond in useCondVec){
      p <- p + geom_half_violin(
        data = subset(d, condition == iCond),aes(x = x, y = y), position = position_nudge(x = -.12),
        side = sideVec[iCond], fill = color[iCond], alpha = colAlpha, trim = FALSE)
    }
  }
  
  # -------------------------------------------------------------------------- #
  ### Add mean (as point) and CI per condition:
  
  if (isMean == T){
    
    cat("Make point for mean\n")
    
    ## Thick line connecting means (plot line first and points on top):
    p <- p + geom_line(data = summary_d, aes(x = x, y = y), color = color[iCond],
                       position = position_nudge(x = 1*posNudge), size = 1.5)
    
    ## Error shades:
    p <- p + geom_ribbon(data = summary_d, aes(x = x, y = y, ymin = y-ci, ymax = y+ci),
                         color = color[iCond], alpha = 0.15, linetype = 0)
    
    ## Points for means and error bars:
    for(iCond in useCondVec){
      p <- p + geom_point(data = subset(summary_d, condition == iCond), aes(x = x, y = y), # point
                          position = position_nudge(x = -1*posNudge), color = color[useCondVec[iCond]], alpha = .6, size = 3) # size = 2
      
      p <- p + geom_errorbar(data = subset(summary_d, condition == iCond), aes(x = x, y = y, # error bar
                                                                               ymin = y-ci, ymax = y+ci),
                             position = position_nudge(-1*posNudge), color = color[useCondVec[iCond]], width = 0.05, size = 1.5, alpha = .6)
    }
  }
  
  if (mean(yLim) == 0.5){ # if conditional probabilities:
    p <- p + geom_hline(yintercept = 0.5, linetype = 2, color = "black", size = 1) # dotted line in the middle
  }
  if (yLim[1] == 0 & yLim[2] == 1){
    # p <- p + scale_y_break(c(0, 0.5, 1))
    p <- p + scale_y_continuous(breaks = seq(0, 1, by = 0.5)) # only 0, 0.5, 1 as axis labels
  }
  
  # -------------------------------------------------------------------------- #
  ### Add other settings:
  
  cat("Adjust axes, labels\n")
  
  ## Y-axis:
  # p <- p + scale_y_continuous(limits = yLim) 
  p <- p + coord_cartesian(ylim = yLim) 
  
  # Add labels for x-axis ticks:
  p <- p + scale_x_continuous(breaks = sort(unique(d$x)), labels = condLabels[useCondVec])
  
  ## Add axis labels:
  p <- p + xlab(xLab) + ylab(yLab)
  
  ## Add title:
  if (!(is.null(main))){
    p <- p + ggtitle(main) # title off for printing for poster
  }  
  
  ## Add theme:
  p <- p + theme_classic()
  
  ## Font sizes:
  p <- p + theme(axis.text = element_text(size = FTS),
                 axis.title = element_text(size = FTS), 
                 plot.title = element_text(size = FTS, hjust = 0.5), # center title 
                 legend.text = element_text(size = FTS))
  
  # Print plot in the end:
  print(p)
  cat("Finished :-)\n")
  return(p)
}

# ============================================================================ #
#### Plot beeswarm plot: #####

plot_beeswarm <- function(data, xVar, yVar, subVar = "subject_n",
                          nRound = NULL, yLim = NULL, selCol, condLabels, 
                          xLab = NULL, yLab = NULL, main = NULL, FTS = NULL){
  #' Make raincloud plot
  #' @param data data frame, with variables \code{variables}
  #' @param xVar string, name of variable that goes on x-axis. Factor or numeric.
  #' @param yVar string, name of variable that goes on y-axis. Variable needs to be numeric.
  #' @param subVar string, name of variable containing subject identifier (default: subject).
  #' @param nRound numeric, digits used for rounding dependent variable y.
  #' @param yLim vector of two numbers for y-axis limits (default: determine based on min and max of input data).
  #' @param selCol vector of strings (HEX colors), colors for input conditions (d$x).
  #' @param condLabels vector of strings, labels for input conditions (d$x) on the x-axis.
  #' @param xLab string, label for x-axis (default: retrieved via substitute_label()).
  #' @param yLab string, label for y-axis (default: retrieved via substitute_label()).
  #' @param main string, title of plot (optional).
  #' @param FTS integer, font size for axes ticks and labels and title.
  #' @return makes beeswarm and box plot for each condition.
  
  require(Rmisc)
  require(ggplot2)
  require(ggbeeswarm)
  
  # -------------------------------------------------------------------------- #
  ### Check inputs:
  
  if(!(is.data.frame((data)))){stop("data must be a data frame")}
  if(!(yVar %in% names(data))){stop("yVar not found in data")}
  if(!(xVar %in% names(data))){stop("xVar not found in data")}
  if(!(subVar %in% names(data))){stop("subVar not found in data")}
  
  ## Check variable types:
  if(!(is.numeric(data[, yVar]))){stop("yVar has to be numeric")}
  if(!(is.factor(data[, xVar]))){stop("xVar has to be a factor")}
  
  ## Axis labels:
  if(is.null(xLab)){xLab <- substitute_label(xVar)}
  if(is.null(yLab)){yLab <- substitute_label(yVar)}
  if(!(is.character(xLab))){stop("xLab has to be a character string")}
  if(!(is.character(yLab))){stop("yLab has to be a character string")}
  
  ## Exclude any rows with NAs: 
  rowIdx <- which(complete.cases(data[, xVar]) & complete.cases(data[, yVar]))
  cat(paste0("Excluding rows with NA on xVar or yVar: Retain ", length(rowIdx), " out of ", nrow(data), " rows (excluded ", nrow(data) - length(rowIdx), " rows)\n"))
  data <- droplevels(data[rowIdx, ])
  
  # -------------------------------------------------------------------------- #
  ### General plotting settings:
  
  ## Close any open plots:
  if (length(dev.list() != 0)){dev.off()}
  
  ## General settings:
  colAlpha <- .20
  LWD <- 1.5
  
  ## Font sizes for ordinary viewing and saving:
  if (is.null(FTS)){
    ## Font sizes for ordinary viewing: 15
    # FTS <- 15
    ## Font sizes for saving: 30
    FTS <- 30
    cat(paste0("No font size provided, use font size ",FTS), "\n")
  }
  
  # -------------------------------------------------------------------------- #
  ### Aggregate data per condition per subject into long format:
  
  d <- prepare_data_plot_long(data, x = xVar, y = yVar, subVar = subVar)
  
  # -------------------------------------------------------------------------- #
  ### Determine defaults:
  
  cat("Set defaults\n")
  
  # Determine y limits if not given:
  if(is.null(yLim)){
    yLim <- find_lim_cases(d)
  }
  if (length(yLim) != 2){stop("yLim must be of length 2")}
  
  ## Count conditions, save condition names:  
  nCond <- length(unique(d$condition))
  condNames <- sort(unique(d$condition))
  
  # -------------------------------------------------------------------------- #
  ### Aggregate across subjects:
  
  require(Rmisc)
  cat("Aggregate data across subjects\n")
  summary_d <- summarySEwithin(d, measurevar = "y", withinvar = "x", idvar = "sID", na.rm = T)
  # automatically rescales x to 1:nCond
  # summary_d$x <- as.numeric(summary_d$x)
  if (is.numeric(d$x)){summary_d$x <- sort(unique(d$x))} # back to original numerical values
  summary_d$condition <- as.numeric(as.factor(summary_d$x)) # consecutive count (for labels)
  
  if (!is.null(nRound)){
    cat(paste0("Round DV on ", nRound, " digits"))
    d$y <- round(d$y, nRound)
    # summary_d$y <- round(summary_d$y, nRound)
  }
  
  # -------------------------------------------------------------------------- #
  ### Start ggplot: 
  
  p <- ggplot(data = d, aes(x = x, y = y)) # initialize
  
  # -------------------------------------------------------------------------- #
  ### Add beeswarm:
  
  cat("Add beeswarm plot\n")
  
  p <- p + geom_beeswarm()
  
  # -------------------------------------------------------------------------- #
  ### Add box plots:
  
  cat("Add box plot\n")
  
  for(iCond in 1:nCond){
    p <- p + geom_boxplot(
      data = subset(d, condition == condNames[iCond]), aes(x = x, y = y), 
      width = 0.3,
      color = selCol[iCond], fill = selCol[iCond], alpha = colAlpha)
    
    # position = position_nudge(x = .15), 
    # side = sideVec[iCond], outlier.shape = NA, center = TRUE, errorbar.draw = TRUE, width = .1, 
    # fill = color[iCond], alpha = colAlpha)
  }
  
  # -------------------------------------------------------------------------- #
  ### Adjust axes, labels, etc.:
  
  cat("Adjust axes, labels\n")
  
  # Y-axis:
  # p <- p + scale_y_continuous(limits = yLim) 
  p <- p + coord_cartesian(ylim = yLim) 
  
  # Add labels for x-axis ticks:
  p <- p + scale_x_continuous(breaks = sort(unique(d$x)), labels = condLabels)
  
  ## Add axis labels:
  p <- p + xlab(xLab) + ylab(yLab)
  
  ## Add title:
  if (!(is.null(main))){
    p <- p + ggtitle(main) # title off for printing for poster
  }  
  
  ## Add theme:
  p <- p + theme_classic()
  
  ## Font sizes:
  p <- p + theme(axis.text = element_text(size = FTS),
                 axis.title = element_text(size = FTS), 
                 title = element_text(size = FTS), 
                 legend.text = element_text(size = FTS))
  
  # Print plot in the end:
  print(p)
  cat("Finished :-)\n")
}

# ============================================================================ #
#### REGRESSION LINES 1 IV: Plot regression line per group and per subject based on model output: #####

custom_regressionline1 <- function(mod, selEff, xLim = NULL, yLim = NULL, useEffect = TRUE, xVec = NULL,
                                   selCol = "red", margin = NULL, xLab = NULL, yLab = NULL, main = NULL, FTS = NULL){
  #' Plot group-level regression line and subject-level regression lines based on 1 continuous predictor.
  #' @param mod model fitted with lme4
  #' @param selEff string, name of predictor to plot
  #' @param xLim vector of two numbers for y-axis limits
  #' @param yLim vector of two numbers for y-axis limits (default: determine based on min and max of input data)
  #' @param subVar string, name of grouping variable (default: subject)
  #' @param useEffect boolean, extract upper and lower bound of confidence interval from effects(mod) (TRUE) or compute yourself based on vcov(mod) (FALSE)
  #' @param xVec vector of two numbers for x-axis ticks, to be used only if useEffect = FALSE.
  #' @param selCol strings (HEX colors), colors for line and error shade (default: red)
  #' @param margin vector of 4 numbers, margin of plot (default: NULL)
  #' @param xLab string, label for x-axis (default: retrieved via substitute_label()).
  #' @param yLab string, label for y-axis (default: retrieved via substitute_label()).
  #' @param main string, title of plot (optional).
  #' @param FTS integer, font size for axes ticks and labels and title.
  #' @return makes regression line plot
  #'  See: https://github.com/jorvlan/open-visualizations/blob/master/R/repmes_tutorial_R.Rmd
  #'  See: https://github.com/jorvlan/open-visualizations/blob/master/R/repmes_tutorial_R.pdf
  
  # -------------------------------------------------------------------------- #
  ### Load packages:
  
  require(ggplot2)
  require(lme4)
  require(effects)
  
  # -------------------------------------------------------------------------- #
  ### Check inputs:
  
  ## Check model and effects:
  validClassVec <- c("glmerMod", "lmerMod", "lmerTest", "lmerModLmerTest")
  if(!(class(mod) %in% validClassVec)){stop("mod needs to have one of a the following classes: ", paste0(validClassVec, collapse = ", "))}
  
  if(!(selEff %in% names(coef(mod)[[1]]))){stop(selEff, " not among predictors in mod")}
  
  ## Axis labels:
  if(is.null(xLab)){xLab <- substitute_label(xVar)}
  if(is.null(yLab)){yLab <- substitute_label(yVar)}
  if(!(is.character(xLab))){stop("xLab has to be a character string")}
  if(!(is.character(yLab))){stop("yLab has to be a character string")}
  
  # -------------------------------------------------------------------------- #
  ### Close any open plots:
  
  if (length(dev.list() != 0)){dev.off()}
  
  # -------------------------------------------------------------------------- #
  ## General plotting settings:
  
  colAlpha <- .95 # transparency
  LWD <- 1.5 # line width
  
  if (is.null(FTS)){
    ## Font sizes for ordinary viewing: 15
    # FTS <- 15
    ## Font sizes for saving: 30
    FTS <- 30
    cat(paste0("No font size provided, use font size ",FTS), "\n")
  }
  
  # -------------------------------------------------------------------------- #
  ### Extract relevant data from model:
  
  ## Extract group-level and subject-level data sets:
  groupCoefs <- fixef(mod) # fixed effects
  if(length(coef(mod)) > 1){warning("Model features > 1 grouping variable, extract random effects for 1st one")}
  subCoefs <- coef(mod)[[1]] # fixed + random effects (only random effects for first structure)
  
  ## Extract effect names, check:
  effNames <- names(subCoefs)
  if (sum(grepl(selEff, effNames, fixed = T)) == 0){stop("selEff not a predictor in mod")}
  selEff1 <- effNames[grep(selEff, effNames, fixed = T)]
  selEff1 <- selEff1[1] # first effect
  
  ## Check presence of interactions:
  if(sum(grepl(":", effNames, fixed = T)) > 0){warning("Model contains interactions; predictions might be off, especially if size of interaction effect is substantial")}
  
  ## Locate effect, count subjects:
  iCol <- which(names(subCoefs) == selEff1) # localize where in subCoefs effect of interest is
  allCols <- 1:length(subCoefs)
  otherCols <- allCols[allCols != iCol]
  nSub <- nrow(subCoefs)
  
  # -------------------------------------------------------------------------- #
  ### Select x-axis values for which to generate y-axis values:
  
  tmp <- effect(selEff, mod) # retrieve objects from effects
  
  if (useEffect){ # if x samples automatically generated by effects() to be used
    
    xVecEval <- as.numeric(tmp$model.matrix[, iCol])
    
  } else { # if use by hand
    
    if (is.null(xVec)){ # if no xVec provided
      
      cat("No x-axis samples provided, extract from effects(mod)\n")
      xVecEval <- as.numeric(tmp$model.matrix[, iCol])
      
    } else {
      
      xVecEval <- seq(xVec[1], xVec[2], (xVec[2] - xVec[1]) / 100) # 100 samples between min and max
      
    }
    
  }
  
  xLen <- length(xVecEval) # number of x-axis samples
  
  if (is.null(xLim)){ # if no x-axis limits provided
    xLim <- c(min(xVecEval), max(xVecEval))
  }
  if (length(xLim) != 2){stop("xLim must be of length 2")}
  
  # -------------------------------------------------------------------------- #
  ### Start ggplot: 
  
  d <- data.frame(x = xVecEval, y = xVecEval) #  just to initialize ggplot; also assign xVecEval to y
  p <- ggplot(data = d) # initialize
  
  # -------------------------------------------------------------------------- #
  ### Loop over subjects, create single subject lines + ribbons:
  
  cat("Draw random-effects lines\n")
  
  inputData <- mod@frame # extract input data
  subIdx <- inputData[, length(inputData)] # extract subject indices to compute marginal means
  subVec <- unique(subIdx)
  
  for (iSub in 1:nSub){ # iSub <- 1
    
    subID <- subVec[iSub]
    margMean <- colMeans(model.matrix(mod)[subIdx == subID, ]) # mean of each regressor in design matrix
    iIntercept <- as.numeric(as.numeric(subCoefs[iSub, otherCols]) %*% as.numeric(margMean[otherCols])) # intercept is estimate of y at mean of all other regressors (marginal effect)
    # iIntercept <- subCoefs[iSub, 1] # extract intercept --> works only if other variables centered or very small
    iSlope <- subCoefs[iSub, iCol] # extract slope
    yVecEval <- iIntercept + xVecEval * iSlope
    if (isGLMM(mod)){yVecEval <-mod@resp$family$linkinv(yVecEval)} # bring to response scale
    
    ## Create single data frame (2 points should be enough to draw line, i.e. xmin and xmax):
    d <- data.frame(x = xVecEval, y = yVecEval)
    
    ## Thick line connecting means (plot line first and points on top):
    p <- p + geom_path(data = d, aes(x = x, y = y), color = 'grey40', # color = 'grey70'
                       alpha = 0.35, linewidth = 1.0)
    # Used till 2021-11-21: grey50, size = 1
    
  }
  
  # -------------------------------------------------------------------------- #
  ### Overall group-level line:
  
  cat("Draw fixed-effects line\n")
  
  ## Compute group-level predicted variables:
  
  # iIntercept <- as.numeric(groupCoefs[1]) # extract intercept
  margMean <- colMeans(model.matrix(mod)) # mean of each regressor in design matrix
  iIntercept <- as.numeric(groupCoefs[otherCols] %*% margMean[otherCols]) # intercept is estimate of y at mean of all other regressors (marginal effect)
  # iIntercept <- groupCoefs[1] # extract intercept --> works only if other variables centered or very small
  iSlope <- as.numeric(groupCoefs[iCol]) # extract slope
  yVecEval <- iIntercept + xVecEval * iSlope # recompute before transform
  if (isGLMM(mod)){yVecEval <- mod@resp$family$linkinv(yVecEval)} # bring to response scale
  
  ## Based on effects output:
  # yVecEval2 <- as.numeric(tmp$fit) # y-axis coordinates (untransformed)
  # if (isGLMM(mod)){yVecEval2 <- mod@resp$family$linkinv(yVecEval2)} # bring to response scale
  
  
  ## Create single data frame (2 points should be enough to draw line, i.e. xmin and xmax):
  d <- data.frame(x = xVecEval, y = yVecEval)
  
  ## Thick line connecting means (plot line first and points on top):
  p <- p + geom_path(data = d, aes(x = x, y = y), color = selCol, linewidth = 1.5)
  
  # -------------------------------------------------------------------------- #
  ### Error shades:
  
  if (useEffect){
    
    # ------------------------------- #
    ## Option A: Extract from effect() object:
    
    tmp <- effect(selEff, mod)
    
    # Mean/ line itself:
    yVecEval <- as.numeric(tmp$fit) # y-axis coordinates (untransformed)
    if (isGLMM(mod)){yVecEval <-mod@resp$family$linkinv(yVecEval)} # transform to response scale
    
    # Lower and upper limit of CI interval:
    ymin <- t(tmp$lower) # lower bound of CI interval
    ymax <- t(tmp$upper) # upper bound of CI interval
    if (isGLMM(mod)){ymin <-mod@resp$family$linkinv(ymin)} # transform to response scale
    if (isGLMM(mod)){ymax <-mod@resp$family$linkinv(ymax)} # transform to response scale
    
  } else {
    
    # ------------------------------- #
    ## Option B: Compute SE yourself:
    # https://github.com/cran/effects/blob/master/R/Effect.R
    
    mmat <- matrix(0, xLen, ncol(subCoefs)) # initialize design matrix: by default all regressors 0
    mmat[, 1] <- rep(1, xLen) # add intercept
    mmat[, iCol] <- xVecEval # add regressor of interest
    V <- vcov(mod, complete = FALSE) # covariance matrix from model
    vcov <- mmat %*% V %*% t(mmat) # multiply with design matrix
    var <- diag(vcov) # extract variance
    se <- sqrt(var) # se # compute standard error of mean, i.e. se
    conf <- 1.96 # select width of confidence intervals
    
    # iIntercept and iSlope computed above:
    yVecEval <- iIntercept + xVecEval * iSlope # recompute before transform
    ymin <- yVecEval - se*conf # lower bound of CI interval
    ymax <- yVecEval + se*conf # upper bound of CI interval
    if (isGLMM(mod)){ymin <-mod@resp$family$linkinv(ymin)} # bring to response scale
    if (isGLMM(mod)){ymax <-mod@resp$family$linkinv(ymax)} # bring to response scale
    # ymin;ymax
  }
  
  # ------------------------------- #
  ## Plot error bars/ shades:
  
  d <- data.frame(x = xVecEval, y = yVecEval, ymin = ymin, ymax = ymax)
  p <- p + geom_ribbon(data = d, aes(x = x, y = y, ymin = ymin, ymax = ymax),
                       fill = selCol, alpha = 0.20, linetype = 0)
  
  # -------------------------------------------------------------------------- #
  ### Add axes, labels, etc.:
  
  cat("Adjust axes, labels\n")
  
  ## Y-axis:
  p <- p + coord_cartesian(xlim = xLim) 
  if (!is.null(yLim)){p <- p + coord_cartesian(ylim = yLim)} 
  
  ## X-axis:
  xTickVec <- round(seq(xLim[1], xLim[2],(xLim[2] - xLim[1]) / 2), 2)
  p <- p + scale_x_continuous(breaks=xTickVec, labels=xTickVec)
  
  ## Axis labels:
  p <- p + xlab(xLab) + ylab(yLab)
  
  # Add title:
  if (!is.null(main)){
    cat("Add title\n")
    p <- p + ggtitle(main)
  }
  
  ## Add margin:
  if (!is.null(margin)){
    cat("Adjust margin\n")
    p <- p + theme(plot.margin = unit(margin, "cm"))
  }
  
  ## Add theme:
  p <- p + theme_classic()
  
  ## Font sizes:
  p <- p + theme(axis.text = element_text(size = FTS),
                 axis.title = element_text(size = FTS), 
                 plot.title = element_text(size = FTS, hjust = 0.5), 
                 legend.text = element_text(size = FTS))
  
  # Print plot in the end:
  print(p)
  cat("Finished :-)\n")
  return(p)
}  

# ============================================================================ #
#### REGRESSION LINES 2 IV: Plot regression line per condition per group and per subject based on model output: #####

custom_regressionline2 <- function(mod, xVar, zVar, 
                                   xLim = NULL, yLim = NULL, xVec = NULL,
                                   selCol = c("red", "blue"), margin = NULL, 
                                   xLab = NULL, yLab = NULL, main = NULL, FTS = NULL){
  #' Plot group-level regression line and subject-level regression lines based on 1 continuous predictor and 1 binary predictor.
  #' @param mod model fitted with lme4.
  #' @param xVar string, name of continuous predictor to plot on x-axis.
  #' @param zVar string, name of binary predictor to plot with different colors.
  #' @param subVar string, name of grouping variable (default: subject).
  #' @param xLim vector of two numbers for y-axis limits.
  #' @param yLim vector of two numbers for y-axis limits (default: determine based on min and max of input data).
  #' @param xVec vector of two numbers for x-axis ticks.
  #' @param selCol vector of strings (HEX colors), colors for line and error shade (default: blue and red).
  #' @param margin vector of 4 numbers, margin of plot (default: NULL).
  #' @param xLab string, label for x-axis (default: retrieved via substitute_label()).
  #' @param yLab string, label for y-axis (default: retrieved via substitute_label()).
  #' @param main string, title of plot (optional).
  #' @param FTS integer, font size for axes ticks and labels and title.
  #' @return makes regression line plot.
  #'  See: https://github.com/jorvlan/open-visualizations/blob/master/R/repmes_tutorial_R.Rmd
  #'  See: https://github.com/jorvlan/open-visualizations/blob/master/R/repmes_tutorial_R.pdf
  
  # -------------------------------------------------------------------------- #
  ## Load packages:
  
  require(ggplot2)
  require(lme4)
  require(effects)
  
  # -------------------------------------------------------------------------- #
  ### Check inputs:
  
  ## Check model and effects:
  validClassVec <- c("glmerMod", "lmerMod", "lmerTest", "lmerModLmerTest")
  if(!(class(mod) %in% validClassVec)){stop("mod needs to have one of a the following classes: ", paste0(validClassVec, collapse = ", "))}
  
  if(!(xVar %in% names(coef(mod)[[1]]))){stop(xVar, " not among predictors in mod")}
  if(!(zVar %in% names(coef(mod)[[1]]))){stop(zVar, " not among predictors in mod")}
  
  ## Axis labels:
  if(is.null(xLab)){xLab <- substitute_label(xVar)}
  if(is.null(yLab)){yLab <- substitute_label(yVar)}
  if(!(is.character(xLab))){stop("xLab has to be a character string")}
  if(!(is.character(yLab))){stop("yLab has to be a character string")}
  
  # -------------------------------------------------------------------------- #
  ### General plotting settings:
  
  alphaSub <- 0.10
  alphaShade <- 0.20
  sizeGroup <- 1.5
  sizeSub <- 1
  LWD <- 1.5
  
  if (is.null(FTS)){
    ## Font sizes for ordinary viewing: 15
    # FTS <- 15
    ## Font sizes for saving: 30
    FTS <- 30
    cat(paste0("No font size provided, use font size ",FTS), "\n")
  }
  
  # -------------------------------------------------------------------------- #
  ## Extract relevant data from model:
  
  ## Extract group-level and subject-level data sets:
  groupCoefs <- fixef(mod) # fixed effects
  
  if(length(coef(mod)) > 1){warning("Model features > 1 grouping variable, extract random effects for 1st one")}
  subCoefs <- coef(mod)[[1]] # fixed + random effects (only random effects for first structure)
  
  ## Extract effect names, check:
  effNames <- names(subCoefs)
  
  if (sum(grepl(xVar, effNames, fixed = T)) == 0){stop("xVar not a predictor in mod")}
  xVar1 <- effNames[grep(xVar, effNames, fixed = T)] # check version in effect names
  xVar1 <- xVar1[1] # only first match
  
  if (sum(grepl(zVar, effNames, fixed = T)) == 0){stop("zVar not a predictor in mod")}
  zVar1 <- effNames[grep(zVar, effNames, fixed = T)] # check version in effect names
  zVar1 <- zVar1[1] # only first match
  
  intVar <- paste0(xVar, ":", zVar) # combine as name should look like
  intVar1 <- effNames[grep(":", effNames, fixed = T)] # check version in effect names
  intVar1 <- paste0(intVar, "1") # only first match
  
  ## Indices of effects:
  xCol <- which(names(subCoefs) == xVar) # localize where in subCoefs effect of interest is
  zCol <- which(names(subCoefs) == zVar1) # localize where in subCoefs effect of interest is
  intCol <- which(names(subCoefs) == intVar1) # localize where in subCoefs effect of interest is
  
  ## Number of subjects:
  nSub <- nrow(subCoefs)
  
  ## X-axis for which to generate plots:
  tmp <- effect(intVar, mod) # retrieve objects from effects
  if (is.null(xVec)){
    xVecEval <- sort(unique(as.numeric(tmp$model.matrix[, xCol])))
  } else {
    xVecEval <- xVec # copy over input
  }
  
  xLen <- length(xVecEval) # number of x-axis samples
  
  if (is.null(xLim)){ # if no x-axis limits provided
    xLim <- c(xVecEval[1], xVecEval[xLen])
  }
  if (length(xLim) != 2){stop("xLim must be of length 2")}
  
  ## Z-axis with how levels of factor are coded:
  zVecEval <- unique(as.numeric(tmp$model.matrix[, zCol]))
  if(length(zVecEval) > 2){stop("zVec is variable with > 2 levels, must have only 2 levels")}
  
  # -------------------------------------------------------------------------- #
  ### Initialize empty ggplot: 
  
  d <- data.frame(x = xVecEval, y = xVecEval) #  just to initialize ggplot; assign xVecEval also to y
  p <- ggplot(data = d) # initialize
  
  # -------------------------------------------------------------------------- #
  ### Loop over subjects, create single subject lines:
  
  cat("Draw random-effects lines\n")
  
  for (iSub in 1:nSub){ # iSub <- 1
    
    ## Intercepts:
    iInter1 <- subCoefs[iSub, 1] + zVecEval[1] * subCoefs[iSub, zCol] # extract intercept condition 1
    iInter2 <- subCoefs[iSub, 1] + zVecEval[2] * subCoefs[iSub, zCol] # extract intercept condition 2
    
    ## Slopes:
    iSlope1 <- subCoefs[iSub, xCol] + zVecEval[1] * subCoefs[iSub, intCol] # extract slope condition 1
    iSlope2 <- subCoefs[iSub, xCol] + zVecEval[2] * subCoefs[iSub, intCol] # extract slope condition 2
    
    # Simulated y-data per subject:
    yVecEval1 <- iInter1 + xVecEval * iSlope1
    yVecEval2 <- iInter2 + xVecEval * iSlope2
    
    if (isGLMM(mod)){yVecEval1 <- mod@resp$family$linkinv(yVecEval1)} # bring to response scale
    if (isGLMM(mod)){yVecEval2 <- mod@resp$family$linkinv(yVecEval2)} # bring to response scale
    
    ## Create single data points (2 should be enough, i.e. xmin and xmax)
    d <- data.frame(x = xVecEval, y1 = yVecEval1, y2 = yVecEval2)
    
    ## Thick line connecting means (plot line first and points on top):
    p <- p + 
      geom_path(data = d, aes(x = x, y = y1), color = selCol[1],
                alpha = alphaSub, linewidth = sizeSub) + 
      geom_path(data = d, aes(x = x, y = y2), color = selCol[2],
                alpha = alphaSub, linewidth = sizeSub)
    
  }
  
  # -------------------------------------------------------------------------- #
  ### Overall group-level line:
  
  cat("Draw fixed-effects line\n")
  
  ## Extract from effect() object:
  tmp <- effect(intVar, mod)
  idx1 <- 1:xLen # first half of predicted values
  idx2 <- (xLen+1):(2*xLen) # second half of predicted values
  
  ## Line itself (means): 
  yVecEval1 <- as.numeric(tmp$fit)[idx1] # y-axis coordinates (untransformed)
  yVecEval2 <- as.numeric(tmp$fit)[idx2] # y-axis coordinates (untransformed)
  if (isGLMM(mod)){yVecEval1 <- mod@resp$family$linkinv(yVecEval1)} # transform to response scale
  if (isGLMM(mod)){yVecEval2 <- mod@resp$family$linkinv(yVecEval2)} # transform to response scale
  
  ## Lower and upper limit of CI interval:
  ymin1 <- t(tmp$lower)[idx1] # lower bound of CI interval condition 1
  ymin2 <- t(tmp$lower)[idx2] # lower bound of CI interval condition 2
  ymax1 <- t(tmp$upper)[idx1] # upper bound of CI interval condition 1
  ymax2 <- t(tmp$upper)[idx2] # upper bound of CI interval condition 2
  if (isGLMM(mod)){ymin1 <- mod@resp$family$linkinv(ymin1)} # transform to response scale
  if (isGLMM(mod)){ymin2 <- mod@resp$family$linkinv(ymin2)} # transform to response scale
  if (isGLMM(mod)){ymax1 <- mod@resp$family$linkinv(ymax1)} # transform to response scale
  if (isGLMM(mod)){ymax2 <- mod@resp$family$linkinv(ymax2)} # transform to response scale
  
  # -------------------------------------------------------------------------- #
  ### Thick line connecting group-level means:
  
  d <- data.frame(x = xVecEval, y1 = yVecEval1, y2 = yVecEval2)
  p <- p + 
    geom_path(data = d, aes(x = x, y = y1), color = selCol[1], linewidth = sizeGroup) + 
    geom_path(data = d, aes(x = x, y = y2), color = selCol[2], linewidth = sizeGroup)
  
  # -------------------------------------------------------------------------- #
  ### Error shades:
  
  ## Plot error bars/ shades:
  d <- data.frame(x = xVecEval, y = yVecEval1, ymin = ymin1, ymax = ymax1)
  p <- p + geom_ribbon(data = d, aes(x = x, y = y, ymin = ymin, ymax = ymax),
                       fill = selCol[1], alpha = alphaShade, linetype = 0)
  d <- data.frame(x = xVecEval, y = yVecEval2, ymin = ymin2, ymax = ymax2)
  p <- p + geom_ribbon(data = d, aes(x = x, y = y, ymin = ymin, ymax = ymax),
                       fill = selCol[2], alpha = alphaShade, linetype = 0)
  
  # -------------------------------------------------------------------------- #
  ### Add axes, labels, etc.:
  
  cat("Adjust axes, labels\n")
  
  ## Y-axis:
  p <- p + coord_cartesian(xlim = xLim, ylim = yLim) 
  
  ## X-axis:
  # xTickVec <- round(seq(xLim[1], xLim[2], (xLim[2] - xLim[1]) / 2), 2) # extremes, middle
  xTickVec <- round(xVecEval, 2) # extremes, middle
  p <- p + scale_x_continuous(breaks=xTickVec, labels=xTickVec)
  
  ## Axis labels:
  p <- p + xlab(xLab) + ylab(yLab)
  
  ## Add title:
  if (!is.null(main)){
    cat("Add title\n")
    p <- p + ggtitle(main)
  }
  
  ## Add theme:
  p <- p + theme_classic()
  
  ## Add margin:
  if (!is.null(margin)){
    cat("Adjust margin\n")
    p <- p + theme(plot.margin = unit(margin, "cm"))
  }
  
  ## Add font sizes:
  p <- p + theme(axis.text = element_text(size = FTS),
                 axis.title = element_text(size = FTS), 
                 plot.title = element_text(size = FTS, hjust = 0.5), 
                 legend.text = element_text(size = FTS))
  
  # Print plot in the end:
  print(p)
  cat("Finished :-)\n")
  return(p)
}  

# ============================================================================ #
#### REGRESSION BARS 1 IV: Plot regression bars per group and per subject based on model output: #####

custom_regressionbar1 <- function(mod, selEff, selCol = "red", 
                                  xLab = NULL, yLab = NULL, main = NULL, xLabels = NULL,
                                  margin = NULL, FTS = NULL, yLim = NULL){
  #' Plot group-level regression bar and subject-level regression lines based on 1 binary predictor.
  #' @param mod model fitted with lme4.
  #' @param selEff string, name of predictor to plot.
  #' @param subVar string, name of grouping variable (default: subject).
  #' @param selCol strings (HEX colors), colors for line and error shade (default: "red" for all).
  #' @param xLab string, label for x-axis (default: retrieved via substitute_label()).
  #' @param yLab string, label for y-axis (default: retrieved via substitute_label()).
  #' @param main string, title of plot (optional).
  #' @param xLabels vector of strings, x-axis ticks (optional; otherwise retrieved from model).
  #' @param margin vector of 4 numbers, margin of plot (default: NULL).
  #' @param FTS integer, font size for axes ticks and labels and title.
  #' @param yLim vector of two numbers for y-axis limits (default: determine based on min and max of input data).
  #' @return makes regression line plot
  
  # -------------------------------------------------------------------------- #
  ### Load packages:
  
  require(ggplot2)
  require(lme4)
  require(effects)
  
  # -------------------------------------------------------------------------- #
  ### Check inputs:
  
  ## Check model and effects:
  validClassVec <- c("glmerMod", "lmerMod", "lmerTest", "lmerModLmerTest")
  if(!(class(mod) %in% validClassVec)){stop("mod needs to have one of a the following classes: ", paste0(validClassVec, collapse = ", "))}
  
  if(!(selEff %in% names(coef(mod)[[1]]))){stop(selEff, " not among predictors in mod")}
  
  ## Axis labels:
  if(is.null(xLab)){xLab <- substitute_label(xVar)}
  if(is.null(yLab)){yLab <- substitute_label(yVar)}
  if(!(is.character(xLab))){stop("xLab has to be a character string")}
  if(!(is.character(yLab))){stop("yLab has to be a character string")}
  
  # -------------------------------------------------------------------------- #
  ### Close any open plots:
  
  if (length(dev.list() != 0)){dev.off()}
  
  # -------------------------------------------------------------------------- #
  ### General plotting settings:
  
  colAlpha <- .95
  LWD <- 1.5
  
  if (is.null(FTS)){
    ## Font sizes for ordinary viewing: 15
    # FTS <- 15
    ## Font sizes for saving: 30
    FTS <- 30
    cat(paste0("No font size provided, use font size ",FTS), "\n")
  }
  
  # -------------------------------------------------------------------------- #
  ### Extract relevant data from model:
  
  ## Extract group level coefficients:
  groupCoefs <- fixef(mod) # fixed effects
  if(length(coef(mod)) > 1){warning("Model features > 1 grouping variable, extract random effects for 1st one")}
  subCoefs <- coef(mod)[[1]] # fixed + random effects
  
  ## Extract effect names, check:
  effNames <- names(subCoefs)
  if (sum(grep(selEff, effNames, fixed = T)) == 0){stop("selEff not a predictor in mod")}
  selEff1 <- effNames[grep(selEff, effNames, fixed = T)] # check version in effect names
  selEff1 <- selEff1[1] # only first match
  
  ## Check presence of interactions:
  if(sum(grepl(":", effNames, fixed = T)) > 0){
    useEffect = FALSE
    warning("Model contains interactions; set useEffect to FALSE, check if predictions make sense")
  }
  
  ## Locate effect:
  iCol <- which(names(subCoefs) == selEff1) # localize where in subCoefs effect of interest is
  
  ## Count subjects:
  nSub <- nrow(subCoefs)
  
  ## x-coordinates for which to plot:
  tmp <- effect(selEff, mod) # retrieve objects from effects
  xVecEval <- as.numeric(tmp$model.matrix[, iCol])
  xLen <- length(xVecEval)
  
  ## In case of sum-to-zero coding: Flip everything
  sum2zero <- F
  if(all(xVecEval == c(1, -1))){sum2zero <- T; message("Sum-to-zero coding detected, flip y values, colors, x-axis labels")}
  
  ## Complete color vector:
  selCol <- rep(selCol, length.out = xLen)
  if (sum2zero){selCol <- rev(selCol)} # flip of sum-to-zero coding
  
  ## X-axis tick labels:
  if (is.null(xLabels)){
    xLabels <- tmp$variables[[1]]$levels
  }
  if (sum2zero){xLabels <- rev(xLabels)} # flip of sum-to-zero coding
  
  # -------------------------------------------------------------------------- #
  ### Start ggplot: 
  
  d <- data.frame(x = xVecEval, y = xVecEval) #  just to initialize ggplot
  p <- ggplot(data = d) # initialize
  
  # -------------------------------------------------------------------------- #
  ### Loop over subjects, create single subject lines + ribbons:
  
  cat("Draw random-effects lines\n")
  
  for (iSub in 1:nSub){ # iSub <- 1
    
    iInter <- subCoefs[iSub, 1] # extract intercept
    iSlope <- subCoefs[iSub, iCol] # extract slope
    yVecEval <- iInter + xVecEval * iSlope # swap both x-axis positions
    if (isGLMM(mod)){yVecEval <- mod@resp$family$linkinv(yVecEval)} # bring to response scale
    if (sum2zero){yVecEval <- rev(yVecEval)} # flip of sum-to-zero coding
    
    ## Create single data frame (2 data points should be enough, i.e. xMin and xMax):
    d <- data.frame(x = xVecEval, y = yVecEval)
    
    ## Thick line connecting means (plot line first and points on top):
    p <- p + geom_path(data = d, aes(x = x, y = y), color = 'grey40', # color = 'grey70'
                       alpha = 0.35, linewidth = 1)
    
  }
  
  # -------------------------------------------------------------------------- #
  ### Overall group-level mean and error-bar:
  
  cat("Draw fixed-effects line\n")
  
  # groupSE <- VarCorr(mod)
  # iCol <- which(colnames(groupCoefs)==selEff) # localize where in groupCoefs effect of interest is
  iInter <- as.numeric(groupCoefs[1]) # extract intercept
  iSlope <- as.numeric(groupCoefs[iCol]) # extract slope
  yVecEval <- iInter + xVecEval * iSlope
  if (isGLMM(mod)){yVecEval <-mod@resp$family$linkinv(yVecEval)} # bring to response scale
  if (sum2zero){yVecEval <- rev(yVecEval)} # flip of sum-to-zero coding
  
  ## Create single data frame (2 data points should be enough, i.e. xMin and xMax):
  d <- data.frame(x = xVecEval, y = yVecEval)
  
  # ------------------------------- #
  ## Add thick line connecting means (plot line first and points on top):
  p <- p + geom_path(data = d, aes(x = x, y = y), color = "black", linewidth = 1.5)
  
  # ------------------------------- #
  ## Add point for mean:
  p <- p + geom_point(data = d, aes(x = x, y = y), # point
                      color = selCol, alpha = 1, size = 5) # size = 2
  
  # ------------------------------- #
  ## Add error shades:
  
  ymin <- tmp$lower 
  ymax <- tmp$upper 
  if (isGLMM(mod)){ymin <- mod@resp$family$linkinv(ymin)} # bring to response scale
  if (isGLMM(mod)){ymax <- mod@resp$family$linkinv(ymax)} # bring to response scale
  if (all(xVecEval == c(1, -1))){yVecEval <- rev(yVecEval)} # flip of sum-to-zero coding
  if (all(xVecEval == c(1, -1))){ymin <- rev(ymin)} # flip of sum-to-zero coding
  if (all(xVecEval == c(1, -1))){ymax <- rev(ymax)} # flip of sum-to-zero coding
  d <- data.frame(x = xVecEval, y = yVecEval, ymin = ymin, ymax = ymax)
  p <- p + geom_errorbar(data = d, aes(x = x, y = y, ymin = ymin, ymax = ymax),
                         color = selCol, width = 0.15, size = 1.5, alpha = .6)
  
  # -------------------------------------------------------------------------- #
  ### Axes, labels, etc.:
  
  cat("Adjust axes, labels\n")
  
  ## Add y-axis:
  if (!is.null(yLim)){
    p <- p + coord_cartesian(ylim = yLim) 
    if (yLim[1] == 0 & yLim[2] == 1){
      p <- p + scale_y_continuous(breaks = seq(0, 1, by = 0.5)) # only 0, 0.5, 1 as axis labels
    }
  }
  
  ## Add x-axis:
  p <- p + coord_cartesian(xlim = c(min(xVecEval) - 0.5, max(xVecEval) + 0.5)) 
  p <- p + scale_x_continuous(breaks = xVecEval, labels = xLabels)
  
  ## Add axes labels:
  p <- p +  xlab(xLab) + ylab(yLab)
  
  ## Add title:
  if (!is.null(main)){
    p <- p + ggtitle(main) # title off for printing for poster
  }
  
  ## Add theme:
  p <- p + theme_classic() # theme
  
  ## Add margin:
  if (!is.null(margin)){
    p <- p + theme(plot.margin = unit(margin, "cm"))
  }
  
  ## Add font sizes:
  p <- p + theme(axis.text = element_text(size = FTS),
                 axis.title = element_text(size = FTS), 
                 plot.title = element_text(size = FTS, hjust = 0.5), # center title 
                 legend.text = element_text(size = FTS))
  
  # Print plot in the end:
  print(p)
  cat("Finished :-)\n")
  return(p)
}  

# ============================================================================ #
#### REGRESSION BARS 1 IV flat: Plot regression bars per group based on model output: #####

custom_regressionbar_flat1 <- function(mod, selEff, selCol = "red", 
                                       xLab = "x", yLab = "y", main = NULL, xLabels = NULL,
                                       margin = NULL, FTS = NULL, yLim = NULL){
  #' Plot values predicted by lm/glm model for given categorical conditions based on 1 binary predictor.
  #' @param mod model fitted with base (lm or glm).
  #' @param selEff string, name of predictor to plot.
  #' @param selCol strings (HEX colors), colors for line and error shade (default: "red" for all).
  #' @param xLab string, label for x-axis (default: retrieved via substitute_label()).
  #' @param yLab string, label for y-axis (default: retrieved via substitute_label()).
  #' @param main string, title of plot (optional).
  #' @param xLabels vector of strings, x-axis ticks (default: numbered through).
  #' @param margin vector of 4 numbers, margin of plot (default: NULL).
  #' @param FTS integer, font size for axes ticks and labels and title.
  #' @param yLim vector of two numbers for y-axis limits (default: determine based on min and max of input data).
  #' @return makes regression line plot.
  
  # -------------------------------------------------------------------------- #
  ### Load packages:
  
  require(ggplot2)
  require(lme4)
  require(effects)
  
  # -------------------------------------------------------------------------- #
  ### Check inputs:
  
  ## Check model and effects:
  validClassVec <- c("glm", "lm")
  if(!(class(mod) %in% validClassVec)){stop("mod needs to have one of a the following classes: ", paste0(validClassVec, collapse = ", "))}
  
  if(!(selEff %in% names(coef(mod)))){stop(selEff, " not among predictors in mod")}
  
  ## Axis labels:
  if(is.null(xLab)){xLab <- substitute_label(xVar)}
  if(is.null(yLab)){yLab <- substitute_label(yVar)}
  if(!(is.character(xLab))){stop("xLab has to be a character string")}
  if(!(is.character(yLab))){stop("yLab has to be a character string")}
  
  # -------------------------------------------------------------------------- #
  ### Close any open plots:
  
  if (length(dev.list() != 0)){dev.off()}
  
  # -------------------------------------------------------------------------- #
  ### General plotting settings:
  
  colAlpha <- .95
  LWD <- 1.5
  
  if (is.null(FTS)){
    ## Font sizes for ordinary viewing: 15
    # FTS <- 15
    ## Font sizes for saving: 30
    FTS <- 30
    cat(paste0("No font size provided, use font size ",FTS), "\n")
  }
  
  # -------------------------------------------------------------------------- #
  ### Rely on output of effect(mod) object:
  
  cat("Extract y-axis values from effect object\n")
  tmp <- effect(selEff, mod) # extract CI end points from effect object
  
  ## Retrieve fitted y-values:
  yVecEval <- tmp$fit
  if(class(mod)[1] == "glm"){yVecEval <-mod$family$linkinv(yVecEval)}
  nVal <- length(yVecEval) # number of fitted values
  
  ## Count number of y-axis values, create corresponding number of x-axis values:
  xVecEval <- seq(1, nVal, 1)
  selCol <- rep(selCol, length.out = nVal) # repeated selCol until enough colors for conditions
  
  ## Compute end points of whiskers:
  ymin <- tmp$lower # lower CI end
  ymax <- tmp$upper  # upper CI end
  if(class(mod)[1] == "glm"){ymin <-mod$family$linkinv(ymin)} # bring to response scale
  if(class(mod)[1] == "glm"){ymax <-mod$family$linkinv(ymax)} # bring to response scale
  
  ## Create single data points (2 should be enough, i.e. xmin and xmax)
  d <- data.frame(x = xVecEval, y = yVecEval, ymin = ymin, ymax = ymax) # concatenate to data
  
  # -------------------------------------------------------------------------- #
  ### Start ggplot: 
  
  p <- ggplot(data = d) # initialize
  
  # ------------------------------- #
  ## Thick line connecting means (plot line first and points on top):
  cat("Add line connecting condition means\n")
  p <- p + geom_path(data = d, aes(x = x, y = y), color = "black", linewidth = 1.5)
  
  # ------------------------------- #
  ## Point for mean:
  cat("Add points for condition means\n")
  p <- p + geom_point(data = d, aes(x = x, y = y), # point
                      color = selCol, alpha = 1, size = 5) # size = 2
  
  # ------------------------------- #
  ## Error shades:
  cat("Add error shades\n")
  p <- p + geom_errorbar(data = d, aes(x = x, y = y, ymin = ymin, ymax = ymax),
                         color = selCol, width = 0.15, size = 1.5, alpha = .6)
  
  # -------------------------------------------------------------------------- #
  ### Add axes, labels, etc.:
  cat("Adjust axes, labels\n")
  
  ## Y-axis:
  if (yLim[1] == 0 & yLim[2] == 1){
    p <- p + scale_y_continuous(breaks = seq(0, 1, by = 0.5)) # only 0, 0.5, 1 as axis labels
  }
  p <- p + coord_cartesian(xlim = c(xVecEval[1] - 0.1, xVecEval[length(xVecEval)] + 0.1), ylim = yLim) 
  
  ## X-axis:
  if (is.null(xLabels)){xLabels <- xVecEval} # if no names provided: use position indices
  p <- p + scale_x_continuous(breaks = xVecEval, labels = xLabels)
  
  ## Labels:
  p <- p +  xlab(xLab) + ylab(yLab)
  
  ## Title:
  if (!is.null(main)){
    p <- p + ggtitle(main) # title off for printing for poster
  }
  
  ## Theme:  
  p <- p + theme_classic() # theme
  
  ## Margin:
  if (!is.null(margin)){
    p <- p + theme(plot.margin = unit(margin, "cm"))
  }
  
  ## Font sizes:
  p <- p + theme(axis.text = element_text(size = FTS),
                 axis.title = element_text(size = FTS), 
                 plot.title = element_text(size = FTS, hjust = 0.5), # center title 
                 legend.text = element_text(size = FTS))
  
  # Print plot in the end:
  print(p)
  cat("Finished :-)\n")
  return(p)
}  

# ============================================================================ #
#### Plot group-level and subject-level coefficients as dots in horizontal dot-plot: #####

custom_coefplot <- function(mod, plotSub = TRUE, plotText = FALSE, dropIntercept = FALSE, revOrder = FALSE,
                            xLab = "Regression weight", yLab = "Predictor", main = NULL,
                            selCol = "blue", yLabels = NULL, xLim = NULL, FTS = NULL){
  #' Plot group-level  and subject-level coefficients as dots in horizontal dot-plot.
  #' @param mod model fitted with lme4.
  #' @param plotSub Boolean, whether to plot per-subject effects (TRUE) or not (FALSE; default: TRUE).
  #' @param plotText Boolean, whether to print value of group-level coefficient next to dot (TRUE) or not (FALSE; default: FALSE).
  #' @param dropIntercept Boolean, do not plot intercept (TRUE; default: false).
  #' @param revOrder Boolean, revert order of predictors (first one on top, TRUE) or not (last one on top FALSE) (default: false).
  #' @param xLab string, label for x-axis (default: "Regression weight").
  #' @param yLab string, label for y-axis (default: "Predictor").
  #' @param main string, title of plot (default: NULL).
  #' @param selCol strings (HEX colors), colors for group-level dots and error lines (default: "blue" for all).
  #' @param yLabels vector of strings, y-axis ticks (default: terms extracted from mod).
  #' @param xLim vector of two numerics, x-axis limits (optional).
  #' @return coefplot created with ggplot.
  
  # -------------------------------------------------------------------------- #
  ## Required packages:
  
  require(ggplot2)
  require(lme4)
  require(arm) # for se.fixef
  
  # -------------------------------------------------------------------------- #
  ### Close any open plots:
  
  if (length(dev.list() != 0)){dev.off()}
  
  # -------------------------------------------------------------------------- #
  ### Fixed settings:
  
  SEweight <- 1.96 # width of whiskers (1.96 for two-sided 95%-CI)
  groupDotSize <- 5 # size of fixed-effects dot; used to be 5
  subDisplacement <- -0.20 # systematic vertical displacement of per-subject dots
  subJitter <- 0.05 # amount of jitter added to per-subject dots; used to be 0.07
  textOffset <- 0.35 # vertical upwards displacement of text; used to be 0.3
  lineWidth <- retrieve_plot_defaults("LWD") # linewidth of axes
  if (is.null(FTS)){
    FTS <- retrieve_plot_defaults("FTS") # font size for all text: 30 or 15 
  }
  colAlpha <- 0.6 # transparency of per-subject dots
  nRound <- 3 # how much to round plotted text.
  
  # -------------------------------------------------------------------------- #
  ### Extract group-level information from input:
  
  ## a) If mixed effects model: 
  modClass <- class(mod)
  if(modClass %in% c("glmerMod", "lmerMod", "lmerTest", "lmerModLmerTest")){ # from lme4
    
    # Extract fixed effect:
    meanVec <- as.numeric(fixef(mod))
    seVec <- se.fixef(mod)
    
    if (is.null(yLabels)){ # if not provided
      labelsVec <- colnames(mod@pp$X)
      labelsVec <- substitute_label(labelsVec) # translate labels
    } else { # if provided
      labelsVec <- yLabels
    }
    
    ## Concatenate group-level values to data frame:
    groupCoefs <- data.frame(labelsVec, meanVec, seVec)
    names(groupCoefs) <- c("label", "mean", "se")
    
    ## Subject-level coefficients:
    subCoefs <- coef(mod)[[1]]
    nSub <- nrow(subCoefs)
    
    ## b) If flat regression:
  } else if (is.list(mod) & "bMat" %in% names(mod)) {
    
    # Compute mean and SD of per-subject coefficients:
    meanVec <- colMeans(mod$bMat, na.rm = T)
    nSub <- nrow(mod$bMat)
    seVec <- as.numeric(sapply(data.frame(mod$bMat), sd, na.rm = T))/sqrt(nSub)
    
    ## y-axis labels for predictors:
    if (is.null(yLabels)){ # if not provided
      labelsVec <- names(coef(mod$modList[[1]]))
      labelsVec <- substitute_label(labelsVec) # translate labels
    } else { # if provided
      labelsVec <- yLabels
    }
    
    ## Concatenate group-level values to data frame:
    groupCoefs <- data.frame(labelsVec, meanVec, seVec)
    names(groupCoefs) <- c("label", "mean", "se")
    
    ## Subject-level coefficients:
    subCoefs <- data.frame(mod$bMat)
    names(subCoefs) <- names(coef(mod$modList[[1]])) # copy over regressor names
    nSub <- nrow(subCoefs)
    
  } else {
    stop("Unknown input")
  }
  
  # -------------------------------------------------------------------------- #
  ### Prepare text labels with significance stars based on z-values:
  
  ## Compute absolute z-value for determining significance:
  groupCoefs$z <- abs(groupCoefs$mean / groupCoefs$se) # z-value to evaluate significance
  ## Compute textual label:
  groupCoefs$zLabel <- as.character(round(groupCoefs$mean, nRound)) # copy over, to string
  ## Pad to desired string length:
  groupCoefs$zLabel <- ifelse(groupCoefs$mean > 0, str_pad(groupCoefs$zLabel, width = nRound + 2, pad = "0", side = "right"), groupCoefs$zLabel) # pad to nRound digits after comma
  groupCoefs$zLabel <- ifelse(groupCoefs$mean < 0, str_pad(groupCoefs$zLabel, width = nRound + 3, pad = "0", side = "right"), groupCoefs$zLabel) # pad to nRound digits after comma
  ## Handle cases of zero (no dot):
  groupCoefs$zLabel <- ifelse(!grepl("\\.", groupCoefs$zLabel), paste0("0.", paste0(rep("0", nRound), collapse = "")), groupCoefs$zLabel)
  ## Add stars and crosses for significance: 
  groupCoefs$zLabel <- ifelse(groupCoefs$z > 1.64 & groupCoefs$z < 1.96, paste0(groupCoefs$zLabel, "\U207A"), groupCoefs$zLabel) # latin cross
  groupCoefs$zLabel <- ifelse(groupCoefs$z > 1.96, paste0(groupCoefs$zLabel, "*"), groupCoefs$zLabel)
  groupCoefs$zLabel <- ifelse(groupCoefs$z > 3, paste0(groupCoefs$zLabel, "*"), groupCoefs$zLabel)
  
  # Alternatives to cross for marginally significant effects:
  # https://unicode-table.com/en/sets/crosses/
  # for (iLabel in 1:nrow(groupCoefs)){
  #   if (groupCoefs$z[iLabel] > 1.64 & groupCoefs$z[iLabel] < 1.96){
  #     groupCoefs$zLabel[iLabel] <- expression(paste(eval(groupCoefs$zLabel[iLabel]), "^+"))
  #     # groupCoefs$zLabel[iLabel] <- substitute(expression(n "^+"), list(n = groupCoefs$zLabel[iLabel]))
  #     # groupCoefs$zLabel[iLabel] <- bquote(.(groupCoefs$zLabel[iLabel])^+)
  #   }
  # } 
  
  # substitute(expression(a + b), list(a = groupCoefs$zLabel[iLabel]))
  # substitute(expression(a ^+), list(a = groupCoefs$zLabel[iLabel]))
  
  # groupCoefs$zLabel <- ifelse(groupCoefs$z > 1.64 & groupCoefs$z < 1.96, expression(paste0(groupCoefs$zLabel, "^+")), groupCoefs$zLabel) # latin cross
  # groupCoefs$zLabel <- ifelse(groupCoefs$z > 1.64 & groupCoefs$z < 1.96, paste0(groupCoefs$zLabel, "\U2670"), groupCoefs$zLabel) # latin cross
  # groupCoefs$zLabel <- ifelse(groupCoefs$z > 1.64 & groupCoefs$z < 1.96, paste0(groupCoefs$zLabel, "\U207A"), groupCoefs$zLabel) # superscript + (rather small)
  
  # -------------------------------------------------------------------------- #
  ### Final selection of coefficients to plot:
  
  ## Drop intercept or not:
  if(dropIntercept){
    groupCoefs <- groupCoefs[2:nrow(groupCoefs), ] 
    # labels <- labels[2:length(labels)]
  }
  
  ## Determine final number of effects:
  nEff <- nrow(groupCoefs)
  txtFTS <- FTS # for axis labels and regression weights
  if (nEff > 5){txtFTS <- FTS * 0.75}
  
  ## Adjust number of colors:
  if (length(selCol) > nEff){selCol <-  selCol[1:nEff]} # if too many colors: only use first
  selCol <- rep(selCol, length.out = nEff) # if too few colors: repeat until enough
  # selCol <- rev(COL2("RdBu", nEff))
  
  ## Reverse order (first regressor will be plotted on top):
  if (revOrder){
    groupCoefs <- groupCoefs[nrow(groupCoefs):1,]
    selCol <- rev(selCol)
  }
  
  ## Compute index and lower/upper confidence bound:
  groupCoefs$idx <- seq(1, nrow(groupCoefs), 1) # numerical index of each effect to loop through (in correct order)
  groupCoefs$lower <- groupCoefs$mean - groupCoefs$se * SEweight
  groupCoefs$upper <- groupCoefs$mean + groupCoefs$se * SEweight
  
  # -------------------------------------------------------------------------- #
  ### Group-level plot:
  
  p <- ggplot(groupCoefs, aes(x = mean, y = label)) # define ggplot and axed
  
  ## A) Add error bar lines:
  cat("Plot error bar whiskers\n")
  for (iEff in 1:nEff){ # iEff <- 1
    ## For this effect: extract index, upper and lower end of whisker
    effData <- data.frame(x = c(groupCoefs$lower[iEff], groupCoefs$upper[iEff]),
                          y = rep(groupCoefs$idx[iEff], 2))
    p <- p + geom_line(data = effData, aes(x = x, y = y), size = 1.2, color = selCol[iEff])
  }
  
  # -------------------------------------------------------------------------- #
  ### B) Add fixed-effects points:
  
  cat("Plot fixed-effect coefficients\n")
  p <- p + geom_point(aes(x = mean, y = idx, color = factor(idx)), size = groupDotSize) +  # points for point estimates; size = 5
    scale_color_manual(values = selCol)
  
  # -------------------------------------------------------------------------- #
  ### C) Add subject-level points:
  
  ## Drop intercept or not (do outside plotSub for xLim determination):
  if(dropIntercept){
    subCoefs <- subCoefs[, 2:ncol(subCoefs)] 
    # labels <- labels[2:length(labels)]
  }
  if (is.vector(subCoefs)){subCoefs <- data.frame(subCoefs)} # ensure it has a column dimension
  
  ## Reverse order (first regressor will be plotted on top):
  if (revOrder){
    subCoefs <- subCoefs[, ncol(subCoefs):1]
  }
  if (is.vector(subCoefs)){subCoefs <- data.frame(subCoefs)} # ensure it has a column dimension
  
  if (plotSub){
    
    cat("Plot effect per subject\n")
    for (iEff in 1:nEff){ # iEff <- 1
      ## For this effect: extract and plot effects per subject
      effData <- data.frame(x = subCoefs[, iEff], # per-subject effect
                            y = rep(groupCoefs$idx[iEff], nSub) + subDisplacement) # y-axis position
      effData$y <- jitter(effData$y, amount = subJitter) # add jitter to distinuish subjects
      p <- p + geom_point(data = effData, aes(x = x, y = y), size = 2, 
                          # shape = 16, color = "gray30", # all grey
                          # shape = 21, color = "black", fill = "gray70", stroke = 1.2, # black edge, white fill
                          shape = 1, color = "black", # or color = selCol[iEff],
                          alpha = colAlpha)
    } # end for nEff 
  } # end if plotSub
  
  # -------------------------------------------------------------------------- #
  ### Determine x-axis dimensions for scaling:
  
  ## Extract range of subject-level effects for axis limits:
  if (!is.null(xLim)){ # if provided: retrieve from input
    xMin <- xLim[1]
    xMax <- xLim[2]
  } else { # else determine empirically based on subject coefficients
    ## Extract:
    if (plotSub){
      xMin <- min(subCoefs, na.rm = T)
      xMax <- max(subCoefs, na.rm = T)
    } else {
      xMin <- min(groupCoefs$lower, na.rm = T)
      xMax <- max(groupCoefs$upper, na.rm = T)
    }
    ## Erode a bit (need 10% for printing text for most positive coefficient):
    if(xMin > 0){xMin <- xMin * 0.90} else (xMin <- xMin * 1.10)
    if(xMax > 0){xMax <- xMax * 1.10} else (xMax <- xMax * 0.90)
    ## Assign:
    xLim <- c(xMin, xMax)
  }
  if (length(xLim) != 2){stop("xLim must be of length 2")}
  
  # -------------------------------------------------------------------------- #
  ### D) Add group-level coefficient as text:
  
  if (plotText){
    textDisplacement <- (xMax - xMin) * 0.10 # 10% of x-axis width
    cat("Print values of group-level effects as text\n")
    p <- p + geom_text(data = groupCoefs, aes(x = mean, y = idx, label = zLabel),  
                       nudge_x = textDisplacement, nudge_y = textOffset, na.rm = T, check_overlap = T, size = txtFTS/3) # nudge_x = 0.20
  }
  
  # -------------------------------------------------------------------------- #
  ### Other settings:
  
  ## Horizontal line at x = 0:  
  p <- p + geom_vline(xintercept = 0, 
                      linetype = "dashed", colour = "#949494", lwd = 1) # line at zero
  
  # -------------------------------------------------------------------------- #
  ### X-axis ticks:
  
  # xMin = -0.049; xMax = 0.459
  cat(paste0("xMin = ", round(xMin, 3), "; xMax = ", round(xMax, 3), "\n"))
  xRange <- xMax - xMin # determine range
  
  ## If symmetric: 5 symmetric break points
  if (abs(xMin) == abs(xMax)){
    
    breakVec <- sort(c(xMin, (xMin + mean(xLim))/2, mean(xLim), (mean(xLim) + xMax)/2, xMax)) # extremes and middle between extreme and center
    breakVec <- round(breakVec, 2) # round to 2 decimals
    
    ## else: determine adaptively:
  } else {
    
    ## Determine x-axis lower limit:
    tmp <- round_lim(c(xMin, xMax))
    breakVec <- find_break_points(tmp, nTickTarget = 4)
    
  }
  
  # -------------------------------------------------------------------------- #
  ### Add axes, labels, etc.:
  
  ## Add axes:
  p <- p + coord_cartesian(xlim = xLim) # set x limits
  p <- p + scale_x_continuous(breaks = breakVec)
  p <- p + scale_y_continuous(breaks = 1:nEff, labels = groupCoefs$label)
  
  ## Add axis labels:
  p <- p + labs(x = xLab, y = yLab)
  
  ## Add title:
  if (!(is.null(main))){
    p <- p + ggtitle(main)  
  }
  
  ## Add theme:
  p <- p + theme_classic() # base_size = 14
  
  ## Add font sizes:
  p <- p + theme(axis.text.x = element_text(colour = "black", size = FTS),
                 axis.text.y = element_text(colour = "black", size = txtFTS),
                 axis.line = element_line(colour = "black"), # , linewidth = LWD), # fixed font sizes
                 axis.title = element_text(colour = "black", size = FTS), 
                 plot.title = element_text(colour = "black", size = FTS, hjust = 0.5), # center title 
                 legend.position = "none")
  
  print(p)
  cat("Finished :-)\n")
  
  return(p)
  
}

# ============================================================================ #
#### Plot intercorrelations of regressors in design matrix: #####

corrplot_regressors <- function(mod, perSub = F, varNames = NULL, FTS = NULL, savePNG = TRUE){
  #' Plot intercorrelations between regressors in design matrix using coefplot.
  #' @param mod model object fitted with lme4 or another package.
  #' @param perSub compute correlation between regressors separately per subject, than average (TRUE) (default: FALSE).
  #' @param varNames vector of strings, names of variables to use for rows/ columns of design matrix.
  #' @param savePNG Boolean, save as PNG to dirs$plot (TRUE, default) or not (FALSE).
  #' @return nothing, plots to console and saves in dirs$plot.
  
  require(psych) # for fisherz and fisherz2r
  require(corrplot)
  
  # -------------------------------------------------------------------------- #
  ### Check inputs:
  
  ## Check model and effects:
  validClassVec <- c("glmerMod", "lmerMod", "lmerTest", "lmerModLmerTest")
  if(!(class(mod) %in% validClassVec)){stop("mod needs to have one of a the following classes: ", paste0(validClassVec, collapse = ", "))}
  
  # -------------------------------------------------------------------------- #
  ### Close any open plots:
  
  if (length(dev.list() != 0)){dev.off()}
  
  # -------------------------------------------------------------------------- #
  ### Extract design matrix:
  
  DM <- model.matrix(mod) # extract design matrix
  DM <- DM[, 2:ncol(DM)] # drop intercept
  
  # -------------------------------------------------------------------------- #
  ### Compute correlation:
  
  if (perSub){
    
    cat("Separate DM per subject, average\n")
    subIdx <- mod@frame[, ncol(mod@frame)] # extract subject indices
    # table(subIdx)
    # table(modData$subject_n)
    stopifnot(nrow(DM) == length(subIdx)) # assure dimensions match
    subVec <- unique(subIdx)
    nSub <- length(subVec)
    validSubVec <- rep(NA, nSub) # vector with whether to retain subject or not
    
    Mlist <- vector(mode = "list", length = nSub) # initialize
    
    ## Loop ovoer subjects, create correlation matrix of regressors:
    for (iSub in 1:nSub){ # iSub <- 133
      
      subID <- subVec[iSub] # subject ID
      # cat(paste0("Start subject ", subID, "\n"))
      rowIdx <- which(subIdx == subID) # indices belonging to this subject
      
      if (length(rowIdx) == 1){ # if only 1 row: use overall M as placeholder, exclude later
        warning(paste0("Subject ", subID, ": only 1 row of data, exclude"))
        M <- cor(DM) # correlation
        validSubVec[iSub] <- 0
      } else {
        DMsub <- DM[rowIdx, ] # select part of design matrix for this subject
        sd(DMsub)
        M <- cor(DMsub) # correlation
        validSubVec[iSub] <- 1
      }
      
      ## Transform from r to z:
      MF <- fisherz(M) # Fisher-z transform
      diagIdx <- which(MF == Inf) # detect diagonal elements (infinite)
      MF[diagIdx] <- 1 # temporarily overwrite to 1, correct later after transforming back
      if(any(is.na(MF))){
        warning(paste0("Subject ", subID, ": correlation matrix has NAs, exclude"))
        validSubVec[iSub] <- 0
      }
      
      Mlist[[iSub]] <- MF # store
      
    }
    
    ## Average and transform back from z to r:
    cat(paste0("Exclude ", nSub - sum(validSubVec), " invalid subjects, which leaves ", sum(validSubVec), " subjects\n"))
    Mlist <- Mlist[validSubVec] # exclude invalid subjects before averaging
    M <- Reduce("+", Mlist)/nSub # average across subjects
    M <- fisherz2r(M) # transform back from z to r
    M[diagIdx] <- 1 # set diagonal back to 1 
    
  } else {
    M <- cor(DM)
  }
  
  ## Print range to console:
  Mvec <- as.vector(M) # to vector
  diagVec <- seq(1, length(Mvec), nrow(M) + 1) # identify indices of diagonal
  Mvec[diagVec] <- NA # set diagonal to NA
  nRound <- 2
  cat(paste0("All correlations between r = ", round(min(Mvec, na.rm = T), nRound), " and r = ", round(max(Mvec, na.rm = T), nRound), "\n"))
  
  # -------------------------------------------------------------------------- #
  ### Overwrite variables names:
  
  if (is.null(varNames)){
    rownames(M) <- substitute_label(rownames(M)) # substitute for known variable names
  } else {
    stopifnot(nrow(M) == length(varNames)) # check if same length
    rownames(M) <- varNames # overwrite
  }
  colnames(M) <- rownames(M)
  
  # -------------------------------------------------------------------------- #
  ## Title and name for saving:
  
  # https://stackoverflow.com/questions/14671172/how-to-convert-r-formula-to-text
  if (class(mod) %in% c("glmerMod")){
    
    formulaStr <- mod@call$formula
    
  } else {
    
    formulaStr <- attr(mod@frame, "formula")
    
  }
  formulaStr <- Reduce(paste, deparse(formulaStr, width.cutoff = 500)) # convert from formula to string object
  
  # https://stackoverflow.com/questions/40509217/how-to-have-r-corrplot-title-position-correct
  # titleStr <- paste0("Intercorrelation regressors for \n", deparse(formulaStr), width.cutoff = 20))
  
  plotName <- paste0("corr_reg_", formula2handle(formulaStr))
  if (perSub){plotName <- paste0(plotName, "_perSub")}
  plotName <- paste0(plotName, ".png")
  
  # -------------------------------------------------------------------------- #
  ### Make corrplot:
  
  if(savePNG) {
    plotNameFull <- paste0(dirs$plot, plotName)
    png(plotNameFull, width = 480, height = 480)
    cat(paste0("Save plot under ", plotNameFull, "\n"))
  }
  
  # https://stackoverflow.com/questions/40352503/change-text-color-in-corrplot-mixed
  
  # corrplot(M, method = "circle", col = rev(COL2('RdBu', 200))) # colored dots of different size
  # corrplot(M, method = "number", col = rev(COL2('RdBu', 200))) # numerals of different color
  
  ## Upper half colored dots of different size, lower half black numerals, variable names in diagonal:
  # corrplot.mixed(M, lower = "number", upper = "circle", lower.col = "black", upper.col = rev(COL2('RdBu', 200)), tl.col = "black")
  
  ## Colors dots of different size with black numerals in them:
  if (!(is.null(FTS))){
    corrplot::corrplot(M, col = rev(COL2('RdBu')), tl.col = "black", tl.pos = "lt", addCoef.col = 'black', 
                       title = "Regressor intercorrelations", mar = c(0, 0, 1, 0),
                       number.cex = FTS)
  } else {
    corrplot::corrplot(M, col = rev(COL2('RdBu')), tl.col = "black", tl.pos = "lt", addCoef.col = 'black', 
                       title = "Regressor intercorrelations", mar = c(0, 0, 1, 0))
  }
  # corrplot(M, addCoef.col = 'black', col = rev(COL2('RdBu')), tl.col = "black", tl.offset = 1, tl.srt = 0) # column labels higher, not rotated
  
  ## Also numerals in color, different color scale (uniform), variable names in diagonal:
  # corrplot.mixed(M, lower = "number", upper = "circle", lower.col = COL1('YlOrRd', 200), upper.col = rev(COL2('RdBu')))
  # corrplot.mixed(M, lower = "number", upper = "circle", lower.col = COL1('YlOrRd', 200), upper.col = COL1('YlOrRd', 200))
  
  ## Save if requested:
  if(savePNG){
    dev.off()
    ## Plot again:
    if (!(is.null(FTS))){
      corrplot::corrplot(M, col = rev(COL2('RdBu')), tl.col = "black", tl.pos = "lt", addCoef.col = 'black', 
                         title = "Regressor intercorrelations", mar = c(0, 0, 1, 0),
                         number.cex = FTS)
    } else {
      corrplot::corrplot(M, col = rev(COL2('RdBu')), tl.col = "black", tl.pos = "lt", addCoef.col = 'black', 
                         title = "Regressor intercorrelations", mar = c(0, 0, 1, 0))
    }
  }
}

# ============================================================================ #
#### Plot intercorrelations of coefficients from model: #####

corrplot_coefficients <- function(input, varNames = NULL, FTS = NULL, savePNG = TRUE, dropNA = FALSE){ 
  #' Plot intercorrelations between regressors in design matrix using coefplot.
  #' @param mod model object fitted with lme4 or another package.
  #' @param varNames vector of strings, names of variables to use for rows/ columns of design matrix.
  #' @param savePNG Boolean, save as PNG to dirs$plot (TRUE, default) or not (FALSE; default).
  #' @param dropNA Boolean, drop any rows with NAs before computing correlation (TRUE) or not (FALSE; default).
  #' @return nothing, plots to console and saves in dirs$plot.
  
  require(corrplot)
  
  # -------------------------------------------------------------------------- #
  ### Check inputs:
  
  ## Check model and effects:
  validClassVec <- c("glmerMod", "lmerMod", "lmerTest", "lmerModLmerTest", "lm", "glm", "list")
  if(!(class(input) %in% validClassVec)){stop("input needs to have one of a the following classes: ", paste0(validClassVec, collapse = ", "))}
  
  # -------------------------------------------------------------------------- #
  ### Close any open plots:
  
  if (length(dev.list() != 0)){dev.off()}
  
  # -------------------------------------------------------------------------- #
  ### Detect class and extract coefficients:
  
  ## Detect model class:  
  modClass <- class(input) # detect
  if (modClass == "list"){modClass <- class(input$modList[[1]]); modClass <- modClass[1]} # if list: take first object from modList
  cat(paste0("Input model of class ", modClass, "\n"))
  
  ## Extract coefficients, parameter names, formula:
  
  if(modClass %in% c("glmerMod", "lmerMod", "lmerTest", "lmerModLmerTest")){ # from lme4
    
    coefMat <- coef(input)[[1]]
    
    parNamesVec <- colnames(coefMat)
    
    if (modClass %in% c("glmerMod")){
      
      formulaStr <- input@call$formula
      
    } else {
      
      formulaStr <- attr(input@frame, "formula")
      
    }
    formulaStr <- Reduce(paste, deparse(formulaStr, width.cutoff = 500)) # convert from formula to string object
    
    # lmer
  } else if (modClass %in% c("mixed")){ # from afex
    
    coefMat <- coef(input$full_model)[[1]]
    parNamesVec <- rownames(coefMat)
    
    formulaStr <- input@call$formula
    formulaStr <- Reduce(paste, deparse(formulaStr, width.cutoff = 500)) # convert from formula to string object
    
  } else if (modClass %in% c("coxph", "lm", "glm")){ # from lm
    
    coefMat <- input$bMat
    parNamesVec <- names(coef(input$modList[[1]]))
    if (modClass == "glm"){
      formulaStr <- input$modList[[1]]$formula
    } else if (modClass == "lm"){
      formulaStr <- eval(input$modList[[1]]$call[[2]]) 
    } else if (modClass == "coxph"){
      formulaStr <- paste0(input$modList[[1]]$formula[[2]], "~", input$modList[[1]]$formula[[3]])
      formulaStr <- formulaStr[2]
    } else {
      stop("Unknown model class")
    }
    
  } else if (modClass %in% c("brmsfit")){ # from brms
    
    ## Parameter names and formula:
    parNamesVec <- row.names(fixef(input)) # names of all predictors
    nParam <- length(parNamesVec)
    
    formulaStr <- input$formula
    formulaStr <- formulaStr[[1]] # extract only first object
    formulaStr <- Reduce(paste, deparse(formulaStr, width.cutoff = 500)) # convert from formula to string object
    
    ## Exttract correlation estimates:
    brmsVarCorr <- VarCorr(input)[[1]]$cor 
    brmsVarCorr <- VarCorr(input)$subject_f$cor 
    
    coefMat <- matrix(NA, nParam, nParam) # initialize
    rownames(coefMat) <- parNamesVec
    colnames(coefMat) <- parNamesVec
    for (iParam1 in 1:nParam){ # iParam1 <- 1
      for (iParam2 in 1:nParam){ # iParam2 <- 2
        coefMat[iParam1, iParam2] <- brmsVarCorr[iParam1, 1, iParam2]
      }
    }
    
  } else {
    
    stop("Unknown model class")
    
  }
  
  # -------------------------------------------------------------------------- #
  ### Compute correlation:
  
  if (dropNA){
    coefMat_complete <- coefMat[complete.cases(coefMat), ]
    if(nrow(coefMat_complete) < nrow(coefMat)){cat(paste0("Dropped " , nrow(coefMat) - nrow(coefMat_complete), " rows from coefMat because of NAs\n"))}
    M <- cor(coefMat_complete)
    # M <- cor(coefMat, use = "complete.obs")
  } else {
    M <- cor(coefMat)
  }
  
  ## Print range to console:
  Mvec <- as.vector(M) # to vector
  diagVec <- seq(1, length(Mvec), nrow(M) + 1) # identify indices of diagonal
  Mvec[diagVec] <- NA # set diagonal to NA
  nRound <- 2
  cat(paste0("All correlations between r = ", round(min(Mvec, na.rm = T), nRound), " and r = ", round(max(Mvec, na.rm = T), nRound), "\n"))
  
  # -------------------------------------------------------------------------- #
  ### Overwrite variables names:
  
  if (is.null(varNames)){
    
    rownames(M) <- parNamesVec
    rownames(M) <- substitute_label(rownames(M)) # substitute for known variable names
    
  } else { # overwrite with inputs
    
    stopifnot(nrow(M) == length(varNames)) # check if same length
    rowNames(M) <- varNames # overwrite
  }
  
  colnames(M) <- rownames(M) # copy over
  
  # -------------------------------------------------------------------------- #
  ### Title and name for saving:
  
  # https://stackoverflow.com/questions/14671172/how-to-convert-r-formula-to-text
  
  plotName <- paste0("corr_coef_", modClass, "_", formula2handle(formulaStr), ".png")
  # plotNameFull <- paste0(dirs$plot, "ic_coef/", plotName)
  plotNameFull <- paste0(dirs$plot, plotName)
  cat(paste0("File path has ", nchar(plotNameFull), " characters\n"))
  if (nchar(plotNameFull) > 260){
    warning("File path too long, shorten\n")
    plotNameFull <- paste0(substr(plotNameFull, 1, 255)[1], ".png")
  }
  
  # -------------------------------------------------------------------------- #
  ### Visualize correlation matrix:
  # https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html
  
  ## Start saving if requested:
  if(savePNG) {
    png(plotNameFull, width = 480, height = 480)
    cat(paste0("Save plot under ", plotNameFull, "\n"))
  }
  
  ## Create plot:
  if (!is.null(FTS)){
    corrplot::corrplot(M, col = rev(COL2('RdBu')), tl.col = "black", tl.pos = "lt", addCoef.col = 'black', 
                       title = "Coefficient intercorrelations", mar = c(0, 0, 1, 0),
                       number.cex = FTS)
  } else {
    corrplot::corrplot(M, col = rev(COL2('RdBu')), tl.col = "black", tl.pos = "lt", addCoef.col = 'black',
                       title = "Coefficient intercorrelations", mar = c(0, 0, 1, 0))
  }
  
  ## Stop saving if requested:
  if(savePNG){
    dev.off(); 
    
    ## Plot again:
    if (!is.null(FTS)){
      corrplot::corrplot(M, col = rev(COL2('RdBu')), tl.col = "black", tl.pos = "lt", addCoef.col = 'black', 
                         title = "Coefficient intercorrelations", mar = c(0, 0, 1, 0),
                         number.cex = FTS)
    } else {
      corrplot::corrplot(M, col = rev(COL2('RdBu')), tl.col = "black", tl.pos = "lt", addCoef.col = 'black',
                         title = "Coefficient intercorrelations", mar = c(0, 0, 1, 0))
    }
    
  }
}

# ============================================================================ #
#### Save coefficients from model: #####

save_coefficients <- function(input){ 
  #' Save coefficients from model output as .csv-file.
  #' @param mod model object fitted with lme4 or another package.
  #' @return nothing, plots to console and saves in dirs$plot.
  
  # -------------------------------------------------------------------------- #
  ### Detect class and extract coefficients:
  
  ## Detect model class:  
  modClass <- class(input)
  if (modClass == "list"){modClass <- class(input$modList[[1]]); modClass <- modClass[1]}
  cat(paste0("Input model of class ", modClass, "\n"))
  
  ## Extract coefficients and formula:
  
  if(modClass %in% c("glmerMod", "lmerMod", "lmerTest", "lmerModLmerTest")){ # from lme4
    
    coefMat <- coef(input)[[1]]
    
    if (modClass %in% c("glmerMod")){
      
      formulaStr <- input@call$formula
      
    } else {
      
      formulaStr <- attr(input@frame, "formula")
      
    }
    formulaStr <- Reduce(paste, deparse(formulaStr, width.cutoff = 500)) # convert from formula to string object
    
    # lmer
  } else if (modClass %in% c("mixed")){ # from afex
    
    coefMat <- coef(input$full_model)[[1]]
    
    formulaStr <- input@call$formula
    formulaStr <- Reduce(paste, deparse(formulaStr, width.cutoff = 500)) # convert from formula to string object
    
  } else if (modClass %in% c("lm", "glm", "coxph")){ # from lm
    
    coefMat <- input$bMat
    colnames(coefMat) <- names(coef(input$modList[[1]])) # add variable names
    
    if (modClass == "glm"){
      formulaStr <- input$modList[[1]]$formula
    } else if (modClass == "lm"){
      formulaStr <- eval(input$modList[[1]]$call[[2]]) 
    } else if (modClass == "coxph"){
      formulaStr <- eval(input$modList[[1]]$formula) 
      formulaStr <- Reduce(paste, deparse(formulaStr, width.cutoff = 500))
    } else {
      stop("Unknown model class")
    }
    
  } else if (modClass %in% c("brmsfit")){ # from brms
    
    ## Parameter names and formula:
    
    formulaStr <- input$formula
    formulaStr <- formulaStr[[1]] # extract only first object
    formulaStr <- Reduce(paste, deparse(formulaStr, width.cutoff = 500)) # convert from formula to string object
    
    ## Extract mean of per-subject posterior:
    coefArray <- coef(input)[[1]] # extract array of dimensions nSub x 4 x nParam
    nSub <- dim(coefArray)[1]
    nParam <- dim(coefArray)[3]
    
    coefMat <- matrix(NA, nSub, nParam) # initialize
    colnames(coefMat) <- row.names(fixef(input))
    for (iSub in 1:nSub){ # iParam1 <- 1
      for (iParam in 1:nParam){ # iParam2 <- 2
        coefMat[iSub, iParam] <- coefArray[iSub, 1, iParam]
      }
    }
    
  } else {
    
    stop("Unknown model class")
    
  }
  
  # -------------------------------------------------------------------------- #
  ## Name for saving:
  
  fileName <- paste0("coefMat_", modClass, "_", formula2handle(formulaStr), ".csv")
  cat(paste0("Save data under ", fileName, "\n"))
  fileNameFull <- paste0(dirs$regCoef, fileName)
  cat(paste0("File path has ", nchar(fileNameFull), " characters\n"))
  if (nchar(fileNameFull) > 260){
    warning("File path too long, shorten\n")
    fileNameFull <- paste0(substr(fileNameFull, 1, 255)[1], ".csv")
  }
  
  # -------------------------------------------------------------------------- #
  ## Save:
  
  write.csv(coefMat, fileNameFull, row.names = F)
  
  cat("Finished :-)\n")
  
}

# ============================================================================ #
#### Quick CIs based on SEs from fitted model: ####

quickCI <- function(mod, selEff = NULL, level = 0.95, nRound = 2){
  #' Compute CIs for given lme4 model given SEs from mixed model.
  #' @param data mod model objected fitted with lme4.
  #' @param selEff vector of integers, index of effect in model for which to compute effect size (default: 2).
  #' @param level numeric, 0-1, level of CIs (default: 0.95).
  #' @param nRound integer, number of digits after comma to round to (default: 2).
  #' @return print to console.
  
  # -------------------------------------------------------------------------- #
  ### Load required packages:
  
  require(arm) # for se.fixef
  
  # -------------------------------------------------------------------------- #
  ### Check inputs:
  
  ## Check model and effects:
  validClassVec <- c("glmerMod", "lmerMod", "lmerTest", "lmerModLmerTest")
  if(!(class(mod) %in% validClassVec)){stop("mod needs to have one of a the following classes: ", paste0(validClassVec, collapse = ", "))}
  
  for (iElem in 1:length(selEff)){
    if(!(selEff[iElem] %in% names(coef(mod)[[1]]))){stop(selEff, " not among predictors in mod")}
  }
  
  # -------------------------------------------------------------------------- #
  ### Determine z-values corresponding to confidence intervals:
  
  twoSideLevel <- 1 - (1 - level) / 2 # correct for two-sided test
  zVal <- qnorm(twoSideLevel) # respective z-level threshold
  
  ## If no effect selected: print all effects in model (skipping intercept)
  if (is.null(selEff)){selEff <- 2:length(fixef(mod))}
  
  # -------------------------------------------------------------------------- #
  ### Loop through all effects, :
  
  ## Loop through effects:
  for (iEff in selEff){
    
    # print(round(c(fixef(mod)[iEff] - zVal*se.fixef(mod)[iEff], fixef(mod)[iEff] + zVal*se.fixef(mod)[iEff]), nRound))
    tmp <- round(c(fixef(mod)[iEff] - zVal*se.fixef(mod)[iEff], 
                   fixef(mod)[iEff] + zVal*se.fixef(mod)[iEff]), 
                 nRound)  
    cat(paste0(level*100, "%-CIs for ", colnames(model.matrix(mod))[iEff], ": ", 
               paste(tmp, collapse = " "), "\n"))
  }
}

# ============================================================================ #
#### Print effect from lme4 model: #####

print_effect <- function(mod, selEff, nDigit = 3){
  #' Print selected effect from lme4 model
  #' @param mod fitted model
  #' @param selEff string, name of effect for which to print effect
  #' @param nDigit integer, number of digits to round after comma, default 2
  #' @return nothing returned, but printed
  require(stringr)
  
  nPad <- nDigit + 2
  
  if (str_sub(selEff, -1) == "f"){selEff <- paste0(selEff)} # add 1 at the end if selEff is factor
  
  # Extract output of fixed effects:
  coefs <- summary(mod)$coefficients # extract coefficients
  idx <- which(rownames(coefs) == selEff) # find effect back
  if (length(idx)==0){stop(paste0("Effect ", selEff, " not found"))} 
  
  ## Retrieve coefficients:
  if (summary(mod)$objClass == "glmerMod"){ # glmer
    
    # Extract relevant info:
    b <- coefs[idx, 1]
    se <- coefs[idx, 2]
    zScore <- coefs[idx, 3]
    pVal <- coefs[idx, 4]
    
  } else if (summary(mod)$objClass == "lmerModLmerTest"){ # lmer
    
    # Extract relevant info:
    b <- coefs[idx, 1]
    se <- coefs[idx, 2]
    dfs <- coefs[idx, 3]
    zScore <- coefs[idx, 4]
    pVal <- coefs[idx, 5]
    
  } else {
    stop("Unknown model type")
  }
  
  # -------------------------------------------------------------------------- #  
  ### Regression coefficient and standard error: 
  
  ## Variable padding of b based on sign:
  bPad <- ifelse(b > 0, nPad, nPad+1) # pad to 5 digits if negative
  
  ## Handle b:
  if (round(b, nDigit) == 0){
    bText <- "0"
  } else {
    bText <- str_pad(round(b, nDigit), bPad, side = "right", pad = "0")
  }
  
  ## Handle se:
  if (round(se, nDigit) == 0){
    seText <- "0"
  } else {
    seText <- str_pad(round(se, nDigit), nPad, side = "right", pad = "0")
  }
  
  # -------------------------------------------------------------------------- #  
  ### Handle test statistic for given object:
  
  ## Variable padding based on sign of z-value:
  zPad <- ifelse(zScore > 0, nPad, nPad+1) # pad to 5 digits if negative
  
  if (summary(mod)$objClass == "glmerMod"){
    zStat <- ", z = "
  } else {
    zStat <- paste0(", t(", round(dfs, nDigit), ") = ")
  }
  
  if (round(zScore, nDigit) == 0){
    zText <- "0"
  } else {
    zText <- str_pad(round(zScore, nDigit), zPad, side = "right", pad = "0")
  }
  
  # -------------------------------------------------------------------------- #  
  ### Handle very small p-values:
  
  if (pVal < 0.001){
    pText <- "p < .001"
  } else {
    pText <- paste0("p = ", str_pad(round(pVal,(nDigit+1)), 5, side = "right", pad = "0")) # p-value: always 5 digits
  }
  
  # -------------------------------------------------------------------------- #  
  ### Print to console:
  
  cat(paste0("b = ", bText,
             ", se = ", seText,
             zStat, zText,
             ", ", pText, "\n"))
}

# ============================================================================ #
#### Fit lm per subject: #####

loop_lm_subject <- function(data, formula, isBinomial = F, family = "binomial"){
  #' Perform lm separately for each subject, store coefficients and models, 
  #' one-sample t-test across subjects for each effect, return.
  #' @param data data frame with variable subject and DVs and IVs.
  #' @param formula string with formula to fit in Wilkinson notation.
  #' @param isBinomial boolean, fit generalized lm with binomial link function (T) or not (F).
  #' @param family distribution of DV to use (default: binomial).
  #' @return output list with elements:
  #' output$bVec: vector of size nSub x nEff, b weights for each effect for each subject.
  #' output$modList: list of models of each subject.
  #' Prints results from t-test across subjects for each effect.
  
  require(DescTools)
  
  # -------------------------------------------------------------------------- #
  ### Check inputs:
  
  if(!(is.data.frame((data)))){stop("data must be a data frame")}
  if(!(is.character((formula)))){stop("formula must be a character string")}
  if(!(is.character((family)))){stop("family must be a character string")}
  
  # -------------------------------------------------------------------------- #
  ## Set fixed variables:
  
  subVar <- "subject_n"
  if (!(subVar %in% names(data))){stop(paste0("subVar ", subVar, "not contained in data"))}
  nDigit <- 3
  
  # -------------------------------------------------------------------------- #
  ### Determine number of subjects:
  
  subVec <- unique(data[, subVar])
  nSub <- length(subVec)
  cat(paste0("Found data from ", nSub, " subjects\n"))
  
  # -------------------------------------------------------------------------- #
  ### Fit model for first subject available to determine number of coefficients:
  
  subIdx <- which(data[, subVar] == subVec[1])
  subData <- data[subIdx, ]
  mod <- lm(formula = formula, data = subData) # fit model
  nEff <- length(mod$coefficients)
  
  # -------------------------------------------------------------------------- #
  ### Initialize matrix to store coefficients:
  
  bMat <- matrix(NA, nrow = nSub, ncol = nEff) # initialize
  modList <- list()
  
  # -------------------------------------------------------------------------- #
  ### Loop through all subjects:
  
  for (iSub in 1:nSub){ # subID <- 132
    # subVar <- "subject_n"; subVec <- unique(data[, subVar]); subID <- iSub
    
    ## Select subject and data:    
    subID <- subVec[iSub]
    cat(paste0("Start subject ", subID, "\n"))
    subIdx <- which(data[, subVar] == subID)
    subData <- data[subIdx, ]
    
    if (nrow(subData) > 0){
      ## Fit model:
      if (isBinomial) {
        if (family == "binomial"){
          mod <- tryCatch({
            glm(formula = formula, data = subData, family = binomial())
          }, error = function(e) {
            cat(paste0("Error for subject ", iSub, ": ", conditionMessage(e), "\n"))
          })
        }
        else if (family == "poisson"){
          mod <- glm(formula = formula, data = subData, family = poisson())
        } else {
          stop("Unknown family for DV")
        }
      } else {
        mod <- lm(formula = formula, data = subData)
      }
      
      ## Check if the result is not NULL (i.e., no error occurred):
      if (!is.null(mod)){
        bMat[iSub, ] <- mod$coefficients # store data
        modList[[iSub]] <- mod
      }
      
    } else { 
      cat(paste0("Data is empty for subject ", subID, "\n"))
    }
    
    
  } # end iSub
  
  # names(bMat) <- names(coef(mod)) # copy over regressor names
  
  # -------------------------------------------------------------------------- #
  ### Perform 1-sample t-test across subjects for each effect:
  
  cat(paste0("Results from fitted models from ", nSub, " subjects:\n"))
  if(nEff > 0) { # if any effects (note: some intercept-only models do not estimate intercept itself). 
    for (iEff in 1:nEff){ # iEff <- 1
      # out <- t.test(FisherZ(bMat[, iEff]) # one-sample t-test: only for correlations in range [-1, 1], so not for intercept, not for glm
      out <- t.test((bMat[, iEff])) # one-sample t-test
      cat(paste0("Effect for ", names(mod$coefficients)[iEff], 
                 ": t(", out$parameter, ") = ", round(out$statistic, nDigit), 
                 ", p = ", out$p.value, "\n"))
    }
    
  } else {
    cat("Intercept-only model, no effects printed\n")
  }
  
  cat("Finished! :-)\n")
  
  # -------------------------------------------------------------------------- #
  ### Populate output object:
  
  output <- list()
  output$bMat <- bMat
  output$modList <- modList
  
  ## Rename columns in bMat:
  output$bMat <- data.frame(output$bMat) # turn into data frame
  names(output$bMat) <- names(coef(output$modList[[1]])) # add column names
  
  return(output)
}

# =============================================================================================== #
#### Fit lm per subject: #####

loop_glm_subject <- function(data, formula, family = "binomial"){
  #' Wrapper on loop_lm_subject to perform glm separately for each subject.
  #' @param data data frame with variable subject and DVs and IVs.
  #' @param formula string with formula to fit in Wilkinson notation.
  #' @param family distribution of DV to use (default: binomial).
  #' @return modList list with elements:
  #' modList$bVec: vector of size nSub x nEff, b weights for each effect for each subject.
  #' modList$modList: list of models of each subject.
  #' Prints results from t-test across subjects for each effect.
  
  # -------------------------------------------------------------------------- #
  ### Check inputs:
  
  if(!(is.data.frame((data)))){stop("data must be a data frame")}
  if(!(is.character((formula)))){stop("formula must be a character string")}
  if(!(is.character((family)))){stop("family must be a character string")}
  
  # -------------------------------------------------------------------------- #
  ## Call loop_lm_subject with isBinomial = T:
  out <- loop_lm_subject(data, formula, isBinomial = T, family = family)
  
  return(out)
}

# ============================================================================ #
#### Fit Coxph per subject: #####

loop_coxph_subject <- function(data, formula){
  #' Perform coxph separately for each subject, then perform one-sample t-test for each coefficient across subjects.
  #' @param data data frame with variable subject and DVs and IVs.
  #' @param formula string with formula to fit in Wilkinson notation.
  #' @return output list with elements:
  #' output$bVec: vector of size nSub x nEff, b weights for each effect for each subject.
  #' output$modList: list of models of each subject.
  #' Prints results from t-test across subjects for each effect.
  require(survival)
  
  # -------------------------------------------------------------------------- #
  ### Check inputs:
  
  if(!(is.data.frame((data)))){stop("data must be a data frame")}
  if(!(is.character((formula)))){stop("formula must be a character string")}
  
  # -------------------------------------------------------------------------- #
  ### Fixed variables:
  
  subVar <- "subject_n"
  if (!(subVar %in% names(data))){stop(paste0("subVar ", subVar, "not contained in data"))}
  nDigit <- 2
  
  # -------------------------------------------------------------------------- #
  ### Determine number of subjects:
  
  subVec <- unique(data[, subVar])
  nSub <- length(subVec)
  cat(paste0("Found data from ", nSub, " subjects\n"))
  
  # -------------------------------------------------------------------------- #
  ### Fit model for first subject available to determine number of coeffficients:
  
  subIdx <- which(data[, subVar] == subVec[1])
  subData <- data[subIdx, ]
  mod <- coxph(formula = eval(parse(text = formula)), data = subData) # fit model
  nEff <- length(mod$coefficients)
  
  # -------------------------------------------------------------------------- #
  ### Initialize matrix to store coefficients:
  
  bMat <- matrix(NA, nrow = nSub, ncol = nEff) # initialize
  modList <- list() # initialize
  
  # -------------------------------------------------------------------------- #
  ### Loop over subjects:
  
  for (iSub in 1:nSub){ # iSub <- 1
    
    ## Select subject and data:
    subID <- subVec[iSub] # extract subject name
    cat(paste0("Start subject ", subID, "\n"))
    subIdx <- which(data[, subVar] == subID)
    subData <- data[subIdx, ]
    
    ## Fit model:
    # mod <- coxph(Surv(time1, time2, mortality) ~ age + sex + transplant, data = dat) # template
    mod <- coxph(formula = eval(parse(text = formula)), data = subData) # fit model
    bMat[iSub, ] <- mod$coefficients # store data
    modList[[iSub]] <- mod
  } 
  
  # names(bMat) <- names(coef(mod)) # copy over regressor names
  
  # -------------------------------------------------------------------------- #
  ### Perform 1-sample t-test across subjects for each effect:
  
  if(nEff > 0) {
    for (iEff in 1:nEff){ # iEff <- 1
      out <- t.test(bMat[, iEff]) # one-sample t-test
      cat(paste0("Effect for ", names(mod$coefficients)[iEff], 
                 ": t(", out$parameter, ") = ", round(out$statistic, nDigit), 
                 ", p = ", out$p.value, "\n"))
    }
    
  } else {
    cat("Intercept-only model, no effects printed\n")
  }
  
  cat("Finished! :-)\n")
  
  # -------------------------------------------------------------------------- #
  ### Populate output object:
  
  output <- list()
  output$bMat <- bMat
  output$modList <- modList
  
  ## Rename columns in bMat:
  output$bMat <- data.frame(output$bMat) # turn into data frame
  names(output$bMat) <- names(coef(output$modList[[1]])) # add column names
  
  return(output)
}

# ============================================================================ #
#### Perform likelihood ratio test on two lists of lm models: #####

LRT_on_modList <- function(modList1, modList2){
  #' Loop over both lists of models, compute log-likelihoods, perform LRT. 
  #' @param modList1 list with lm() models as elements.
  #' @param modList2 list with lm() models as elements.
  #' @return modList list with fields:
  
  cat("Perform likelihood ratio test\n")
  
  # -------------------------------------------------------------------------- #
  ### Check inputs:
  
  if(!(is.list(modList1))){stop("modList1 is not a list")}
  if(!(is.list(modList2))){stop("modList2 is not a list")}
  
  ## Check if same number of elements in input lists:
  nSub1 <- length(modList1)
  nSub2 <- length(modList2)
  if (nSub1 != nSub2){stop("List have different numbers of elements")}
  
  # -------------------------------------------------------------------------- #
  ### Loop over both lists, extract log-likelihoods:
  
  logLikMat <- matrix(NA, nrow = nSub1, ncol = 2) # initialize
  
  for (iSub in 1:nSub1){
    logLikMat[iSub, 1] <- as.numeric(logLik(modList1[[iSub]]))
    logLikMat[iSub, 2] <- as.numeric(logLik(modList2[[iSub]]))
  }
  
  # -------------------------------------------------------------------------- #
  ### Perform one-sided LRT, print, return:
  
  # sum log-likelihood over trials, why not also over subjects?
  # https://stats.stackexchange.com/questions/526936/how-to-aggregate-log-likelihood-score-of-many-modelsmod
  
  ## x^2-value:
  cat(paste0("Sum log-likelihood across subjects\n"))
  logLikVec <- colSums(logLikMat) # aggregate across subjects
  # logLikVec <- colMeans(logLikMat) # aggregate across subjects
  x2val <- -2*(logLikVec[1] - logLikVec[2]) # -2*log likelihood difference is x^2-distributed
  
  ## x^2-value:
  df1 <- modList1[[1]]$iter
  if (is.null(df1)){df1 <- 0}
  df2 <- modList2[[1]]$iter
  dfDif <- df2 - df1
  
  ## p-value:
  p <- pchisq(x2val, df = dfDif, lower.tail = FALSE)
  cat(paste0("LRT: x^2(", dfDif, ") = ", x2val, ", p = ", p, "\n"))
  
  cat("Finished :-)\n")
  return(p)
  
}

# ============================================================================ #
#### Perform Bayesian model selection on two lists of lm models: #####

BMS_on_modList <- function(modList1, modList2, n_samples = 1e6){
  #' Loop over both lists of models, compute BICs, perform random-effects Bayesian model selection
  #' using VB_bms() from mattelisi's bmsR package.
  #' @param modList1 list with lm() models as elements.
  #' @param modList2 list with lm() models as elements.
  #' @param n_samples integer, number of iterations in VB_bms (default: 1e6).
  #' @return bms_mod with alpha (Dirichlet parameters), r (expected model frequencies), xp (exceedance probability),
  #' bor (Bayesian omnibus risk), pxp (protected exceedance probabilities).
  
  ## For details on BMS implementation, see:
  # https://github.com/mattelisi/bmsR
  # remotes::install_github("mattelisi/mlisi")
  # require(mlisi)
  # remotes::install_github("mattelisi/bmsR")
  require(bmsR)
  cat("Perform Bayesian model selection using VB_BMS from mattelisi's bmsR package\n")
  
  # -------------------------------------------------------------------------- #
  ### Check inputs:
  
  if(!(is.list(modList1))){stop("modList1 is not a list")}
  if(!(is.list(modList2))){stop("modList2 is not a list")}
  
  ## Check if same number of elements in input lists:
  nSub1 <- length(modList1)
  nSub2 <- length(modList2)
  if (nSub1 != nSub2){stop("List have different numbers of elements")}
  
  # -------------------------------------------------------------------------- #
  ### Loop over both lists, extract BIC per model:
  
  BICmat <- matrix(NA, nrow = nSub1, ncol = 2)
  
  for (iSub in 1:nSub1){
    BICmat[iSub, 1] <- BIC(modList1[[iSub]])
    BICmat[iSub, 2] <- BIC(modList2[[iSub]])
  }
  
  cat(paste0("Mean BICs per model are ", paste0(colMeans(BICmat), collapse = ", "), "\n"))
  
  # -------------------------------------------------------------------------- #
  ### Perform BMS:
  
  bms_mod <- VB_bms(BICmat, n_samples = n_samples)
  
  cat("Finished :-)\n")
  return(bms_mod)
  
}

# ============================================================================ #
#### Permutation test: #####

permutation_test <- function(x, y, n = 10000){
  #' Permutation test of 2-sided paired samples t-test
  #' Inputs:
  #' x, y = two vectors of equal length that will be used for permutation. Hypothesis x != y is tested
  #' n = number of permutations (default: 10000)
  #' Outputs: 
  #' p = p-value (number of samples in permutation distribution more extreme than critical value)
  
  # -------------------------------------------------------------------------- #
  ### Check inputs:
  
  if(!(is.numeric(x))){stop("x must be numeric")}
  if(!(is.numeric(y))){stop("y must be numeric")}
  if(!(is.integer(n))){stop("n must be an integer")}
  
  if (length(x) != length(y)){stop(" x and y of different length")} 
  
  # -------------------------------------------------------------------------- #
  ### Compute test statistic for empirical data:
  
  nS <-  length(x) # number of samples in vectors 
  diff <- x-y # difference vector of x and y 
  testVal <- mean(diff, na.rm = T)/sd(diff, na.rm = T) # empirical test statistics (here: Cohen's d)
  
  # -------------------------------------------------------------------------- #
  ### Compute permutation null distribution:
  
  permdist <-  rep(NA, n) # initialize
  
  ## Loop over permutations: 
  for(i in 1:n){
    sign <- sample(c(1,-1), nS, replace = T) # sample vectors of signs with -1 or 1
    testVec <- (diff) * sign # multiply differences between x and y with randomly sampled signs
    permdist[i] <- mean(testVec, na.rm = T)/sd(testVec, na.rm = T) # Take the mean, divide by std (ie Cohen's d), store in permutation distribution
  }
  
  # -------------------------------------------------------------------------- #
  ### Compute two-sided p-value:
  
  p1 <- sum(permdist > testVal) / length(permdist) # number samples in permutation distribution that are larger than empirical test statistic. 
  p2 <- sum(permdist < testVal) / length(permdist) # number samples in permutation distribution that are smaller than empirical test statistic
  p <- min(p1, p2)*2 # times 2 because 2-sided test
  return(p)
}

# ============================================================================ #
#### Barplot 1 IV: Aggregate per condition per subject, plot (1 IV on x-axis): ####

custom_bar_between <- function(data, xVar = NULL, yVar = NULL, addPoint = T,
                               xLab = NULL, yLab = NULL, main = NULL, selCol = NULL, xAngle = 0,
                               yLim = NULL, FTS = NULL){
  #' Make bar plot with error bars and individual-subject data points.
  #' @param data data frame, trial-by-trial data.
  #' @param xVar string, name of variable that goes on x-axis. If numeric, it will be converted to an (ordered) factor.
  #' @param yVar string, name of variable that goes on y-axis. Needs to be numeric.
  #' @param xLab string, label for x-axis (default: retrieve appropriate name with substitute_label()).
  #' @param yLab string, label for y-axis (default: retrieve appropriate name with substitute_label()).
  #' @param main string, overall plot label (optional).
  #' @param selCol vector of strings (HEX colors), colors for bars (default: retrieve via retrieve_colour()).
  #' @param xAngle scalar integer, rotation angle for x-axis labels (default: 0).
  #' @param yLim vector of two numbers, y-axis (default: automatically determined by ggplot).
  #' @param FTS scalar integer, font size to use.
  #' @return creates and returns plot.
  
  # -------------------------------------------------------------------------- #
  ### Retrieve relevant packages:
  
  require(ggplot2)
  require(ggthemes)
  
  # -------------------------------------------------------------------------- #
  ### Check inputs:
  
  if(!(is.data.frame((data)))){stop("data must be a data frame")}
  if(!(yVar %in% names(data))){stop("yVar not found in data")}
  if(!(xVar %in% names(data))){stop("xVar not found in data")}
  
  ## Check variable types:
  if(!(is.numeric(data[, yVar]))){stop("yVar has to be numeric")}
  if(!(is.factor(data[, xVar]))){stop("xVar has to be a factor")}
  
  ## Axis labels:
  if(is.null(xLab)){xLab <- substitute_label(xVar)}
  if(is.null(yLab)){yLab <- substitute_label(yVar)}
  if (is.null(main)){main <- paste0(yLab,  " ~ ", xLab)}
  if(!(is.character(xLab))){stop("xLab has to be a character string")}
  if(!(is.character(yLab))){stop("yLab has to be a character string")}
  if(!(is.character(main))){stop("main has to be a character string")}
  
  ## Exclude any rows with NAs: 
  rowIdx <- which(complete.cases(data[, xVar]) & complete.cases(data[, yVar]))
  cat(paste0("Excluding rows with NA on xVar or yVar: Retain ", length(rowIdx), " out of ", nrow(data), " rows (excluded ", nrow(data) - length(rowIdx), " rows)\n"))
  data <- droplevels(data[rowIdx, ])
  
  # -------------------------------------------------------------------------- #
  ### Retrieve plotting defaults:
  
  ## Colour:
  if(is.null(selCol)){
    selCol <- retrieve_colour(xVar)
    selCol <- rep(selCol, length.out = length(unique(data[, xVar])))
  }
  
  ## Font size:
  if (is.null(FTS)){
    FTS <- 20 # retrieve_plot_defaults("FTS") # 30
  }
  FTSxAxis <- FTS
  if (length(unique(data[, xVar])) > 10){FTSxAxis <- FTS/2; cat("Half x-axis font size because many x levels\n")}
  FTSyAxis <- FTS
  if (nchar(yLab) > 30){FTSyAxis <- FTS/2; cat("Half y-axis font size because long yLab\n")}
  
  ## Line width:
  LWD <- retrieve_plot_defaults("LWD") # 1.3
  LWDBars <- LWD
  if (length(unique(data[, xVar])) > 10){LWDBars <- 1; cat("Set LWD of bars to 1 because many x levels\n")}
  
  ## Dot size:
  dotSize <- retrieve_plot_defaults("dotSize") # 2 0.5
  if (length(unique(data[, xVar])) > 15){dotSize <- dotSize/2; cat("Lower dotSize from 1 to 0.5 because many x/z levels\n")}
  
  ## Determine y limits if not given:
  if(is.null(yLim)){
    yLim <- find_lim_cases(data, yVar = yVar)
  }  
  if (length(yLim) != 2){stop("yLim must be of length 2")}
  
  colAlpha <- 0.5
  
  # -------------------------------------------------------------------------- #
  ### Rename variables:
  
  data$x <- data[, xVar]
  data$xj <- as.numeric(data$x) + jitter(rep(0, nrow(data)), amount = .05)
  data$y <- data[, yVar]
  
  # -------------------------------------------------------------------------- #
  ### Create ggplot:
  
  ## Start plot:
  p <- ggplot(data, aes(x = x, y = y, fill = x)) +
    stat_summary(fun = mean, geom = "bar", position = "dodge", width = 0.6, 
                 lwd = 1, fill = selCol, col = "black")
  
  ## Add bars:
  p <- p + stat_summary(fun.data = mean_cl_normal, geom = "errorbar", lwd = 1, width = 0.2)
  
  ## Add per-subject data points:
  if (addPoint){
    p <- p + geom_point(data = data, aes(x = xj, fill = x), shape = 21, 
                        size = 2, stroke = 1.2, color = "black", alpha = colAlpha)
  }
  
  ## Set y-axis:
  if (!(is.null(yLim))){
    p <- p + coord_cartesian(ylim = yLim)
  }
  
  ## Add color scale:
  p <- p + scale_color_manual(values = selCol)
  p <- p + scale_fill_manual(values = selCol)
  
  ## Add axis labels:
  p <- p + labs(x = xLab, y = yLab)
  
  ## Add title:
  p <- p + ggtitle(main)
  
  ## Add theme:
  p <- p + theme_classic()
  
  ## Add font sizes:
  p <- p + theme(axis.text.x = element_text(size = FTSxAxis),
                 axis.title.x = element_text(size = FTS), 
                 axis.text.y = element_text(size = FTSyAxis),
                 axis.title.y = element_text(size = FTS), 
                 # axis.text.x = element_blank(), # remove x-axis labels
                 # axis.ticks.x = element_blank(), # remove x-axis labels
                 title = element_text(size = FTS),
                 legend.position = "none",
                 axis.line = element_line(colour = 'black')) #
  
  ## Adjust angle of x-axis labels:
  if (xAngle != 0){
    p <- p + theme(axis.text.x = element_text(angle = xAngle, vjust = 0.5, hjust = 1))
  }
  
  ## Print plot:
  print(p)
  return(p)
}

# ============================================================================ #
#### Barplot 1 IV: Aggregate per condition per subject, plot (1 IV on x-axis): ####

custom_barplot1 <- function(data, xVar = NULL, yVar = NULL, subVar = "subject_n", 
                            xLab = NULL, yLab = NULL, main = NULL, selCol = NULL,
                            isPoint = T, isConnect = T, isMidLine = F, hLine = NULL, isBeeswarm = F, 
                            yLim = NULL, FTS = NULL, LWD = NULL,
                            savePNG = T, saveEPS = F, prefix = NULL, suffix = NULL){
  #' Make bar plot with error bars and individual-subject data points.
  #' @param data data frame, trial-by-trial data.
  #' @param xVar string, name of variable that goes on x-axis. If numeric, it will be converted to an (ordered) factor.
  #' @param yVar string, name of variable that goes on y-axis. Needs to be numeric.
  #' @param subVar string, name of variable containing subject identifier (default: subject).
  #' @param xLab string, label for x-axis (default: retrieve appropriate name with substitute_label()).
  #' @param yLab string, label for y-axis (default: retrieve appropriate name with substitute_label()).
  #' @param main string, overall plot label (optional).
  #' @param selCol vector of strings (HEX colors), colors for bars (default: retrieve via retrieve_colour()).
  #' @param isPoint Boolean, plot individual data points per condition as small points (default: FALSE).
  #' @param isMidLine Boolean, add horizontal line at midpoint of y-axis (default: FALSE).
  #' @param yLine scalar numeric, draw horizontal line at given y value (default: NULL).   
  #' @param isConnect Boolean, connect individual data points with grey lines (default: FALSE).
  #' @param isBeewswarm Boolean, plot individual data points per condition as beeswarm densities (default: FALSE).
  #' @param yLim vector of two numbers, y-axis (default: automatically determined by ggplot).
  #' @param FTS scalar integer, font size to use for axis labels and title (optional).
  #' @param LWD scalar interger, line width used for axis and bars in plot (optional). 
  #' @param savePNG Boolean, save as .png file.
  #' @param saveEPS Boolean, save as .eps file.
  #' @param prefix string, string to add at the beginning of plot name (optional).
  #' @param suffix string, string to add at the end of plot name (optional).
  #' @return creates (and saves) plot.
  
  # -------------------------------------------------------------------------- #
  ### Load required packages:
  
  require(ggplot2) # for ggplot
  require(ggthemes) # for theme_classic()
  require(plyr) # for ddply
  require(Rmisc) # for summarySEwithin
  
  # -------------------------------------------------------------------------- #
  ### Close any open plots:
  
  if (length(dev.list() != 0)){dev.off()}
  
  # -------------------------------------------------------------------------- #
  ### Check inputs:
  
  ## Check if input variables included in data set:
  if(!(is.data.frame((data)))){stop("data must be a data frame")}
  if(!(yVar %in% names(data))){stop("yVar not found in data")}
  if(!(xVar %in% names(data))){stop("xVar not found in data")}
  if(!(subVar %in% names(data))){stop("subVar not found in data")}
  
  ## Check variable types:
  if(!(is.numeric(data[, yVar]))){stop("yVar has to be numeric")}
  if(!(is.factor(data[, xVar]))){stop("xVar has to be a factor")}
  
  ## Axis labels:
  if(is.null(xLab)){xLab <- substitute_label(xVar)}
  if(is.null(yLab)){yLab <- substitute_label(yVar)}
  if(!(is.character(xLab))){stop("xLab has to be a character string")}
  if(!(is.character(yLab))){stop("yLab has to be a character string")}
  
  ## Exclude any rows with NAs: 
  rowIdx <- which(complete.cases(data[, xVar]) & complete.cases(data[, yVar]))
  cat(paste0("Excluding rows with NA on xVar or yVar: Retain ", length(rowIdx), " out of ", nrow(data), " rows (excluded ", nrow(data) - length(rowIdx), " rows)\n"))
  data <- droplevels(data[rowIdx, ])
  
  # -------------------------------------------------------------------------- #
  ### Fixed plotting settings:
  
  SEweight <- 1
  
  ## Colours:
  if(is.null(selCol)){
    selCol <- retrieve_colour(xVar)
    selCol <- rep(selCol, length.out = length(unique(data[, xVar])))
  }
  if(length(selCol) != length(unique(data[, xVar]))){
    stop(paste0("Length selCol = ", length(selCol), " while number levels xVar = ", length(unique(data[, xVar])), ", do not match"))
  }
  
  ## Font size:
  if (is.null(FTS)){
    FTS <- retrieve_plot_defaults("FTS") # 30
  }
  FTSxAxis <- FTS
  if (length(unique(data[, xVar])) > 10){FTSxAxis <- FTS/2; cat("Half x-axis font size because many x levels\n")}
  
  ## Line width:
  if(is.null(LWD)){LWD <- retrieve_plot_defaults("LWD")} # 1.3
  LWDBars <- LWD
  if (length(unique(data[, xVar])) > 10){LWDBars <- 1; cat("Set LWD of bars to 1 because many x levels\n")}
  
  ## Dot size:
  dotSize <- retrieve_plot_defaults("dotSize") # 2 0.5
  if (length(unique(data[, xVar])) > 15){dotSize <- dotSize/2; cat("Lower dotSize from 1 to 0.5 because many x/z levels\n")}
  
  ## Length of error bars:
  dodgeVal <- retrieve_plot_defaults("dodgeVal")
  colAlpha <- 0.5 # 1
  
  # -------------------------------------------------------------------------- #
  ### Create variables under standardized names:
  
  cat("Overall condition means (without first aggregating per subject):\n")
  print(tapply(data[, yVar], data[, xVar], mean, na.rm = T))
  
  data$x <- data[, xVar]
  data$y <- data[, yVar]
  data$subject <- data[, subVar]
  
  # -------------------------------------------------------------------------- #
  ### Aggregate data per subject per condition:
  
  aggrData <- ddply(data, .(subject, x), function(x){
    y <- mean(x$y, na.rm = T)
    return(data.frame(y))
    dev.off()})
  cat(paste0("Min = ", round(min(aggrData$y), 3), "; Max = ", round(max(aggrData$y), 3)), "\n")
  
  # -------------------------------------------------------------------------- #
  ### Add jittered x-axis variable for points:
  
  aggrData$xpos <- as.numeric(aggrData$x) # to numeric
  aggrData$xpos <- aggrData$xpos - min(aggrData$xpos) + 1 # plot starts at lowest level of x-variable
  aggrData$j <- jitter(rep(0, nrow(aggrData)), amount = .09) # jitter 0.09
  aggrData$xj <- aggrData$xpos + aggrData$j # add jitter
  
  # -------------------------------------------------------------------------- #
  ### Determine y limits if not given:
  
  if(is.null(yLim)){
    yLim <- find_lim_cases(aggrData)
  }
  if (length(yLim) != 2){stop("yLim must be of length 2")}
  
  # -------------------------------------------------------------------------- #
  ### Aggregate across subjects with Rmisc:
  
  summary_d <- summarySEwithin(aggrData, measurevar = "y", idvar = "subject", na.rm = T,
                               withinvars = c("x"))
  
  # -------------------------------------------------------------------------- #
  ### Select settings for saving:
  
  ## Additions:
  if(is.null(prefix)){prefix <- ""} else {prefix <- paste0(prefix, "_")}
  if(is.null(suffix)){suffix <- ""} else {suffix <- paste0("_", suffix)}
  
  ## Name:
  plotName <- paste0("custombarplot1_", prefix, yVar, "_", xVar)
  if (isPoint){plotName <- paste0(plotName, "_points")} 
  if (isConnect){plotName <- paste0(plotName, "_lines")} 
  plotName <- paste0(plotName, suffix)
  cat(paste0("Start plot ", plotName, "\n"))
  
  # -------------------------------------------------------------------------- #
  ### Create ggplot:
  
  ## Start ggplot:
  p <- ggplot(summary_d, aes(x, y))
  
  ## Bars of means:
  cat("Add group-level bars \n")
  p <- p + stat_summary(fun = mean, geom = "bar", position = "dodge", width = 0.6,
                        lwd = LWD, fill = selCol, color = "black") + 
    
    ## Error bars:
    cat("Add error bars \n")
  p <- p + geom_errorbar(data = summary_d,
                         aes(x = x, y = y, ymin = y - se * SEweight, ymax = y + se * SEweight),
                         position = position_dodge(width = dodgeVal), width = 0.1,
                         lwd = LWDBars, color = "black", alpha = 1)
  
  # -------------------------------------------------------------------------- #
  ### Individual data points:
  
  if (isPoint){
    cat("Add per-subject data points\n")
    ## Colored dots:
    p <- p + geom_point(data = aggrData, aes(x = xj, fill = x), shape = 21, 
                        size = dotSize, stroke = 1.2, # size = 0.6, 
                        color = "black", alpha = colAlpha)
    p <- p + scale_fill_manual(values = selCol, limits = levels(aggrData$x))
    ## Grey dots:
    # p <- p + geom_point(data = aggrData, aes(x = xj), shape = 21, size = 2, fill = NA, stroke = 1.5, # size = 0.6, 
    #                     color = "grey", alpha = colAlpha) # color = black, grey60,
  }
  
  if (isConnect){
    cat("Add line connections to per-subject data points\n")
    ## Connect colored dots:
    
    subVec <- sort(unique(aggrData$subject))
    nSub <- length(subVec)
    for(iSub in 1:nSub){
      subData <- subset(aggrData, subject == subVec[iSub])
      p <- p + geom_path(data = subData, aes(x = xj, y= y), color = 'grey40', # color = 'grey70'
                         alpha = 0.40, lwd = 0.5) # alpha = 0.80, lwd = 1
    }
  }
  
  if (isBeeswarm){
    p <- p + geom_beeswarm(data = aggrData, aes(x = xpos), shape = 1, size = 2, stroke = 1, # size = 0.6, 
                           color = "black", alpha = colAlpha)
  }
  
  # -------------------------------------------------------------------------- #
  ### Add additional settings:
  
  ## Add horizontal line in the middle:
  if (isMidLine){
    yMid <- (yLim[1] + yLim[2])/2
    p <- p + geom_hline(yintercept = yMid, linetype = 2, color = "black", linewidth = 1) # Middle line at 0
  }
  
  ## Add horizontal line at specifed y-axis position:
  if (!(is.null(hLine))){
    p <- p + geom_hline(yintercept = hLine, linetype = 2, color = "black")
  }
  
  ## Add y-axis labels:
  if (yLim[1] == 0 & yLim[2] == 1){
    p <- p + scale_y_continuous(breaks = seq(0, 1, by = 0.5)) # only 0, 0.5, 1 as axis labels
  }
  p <- p + coord_cartesian(ylim = yLim)
  
  ## Add x-axis labels:
  p <- p + labs(x = xLab, y = yLab)
  
  ## Add title:
  if (!(is.null(main))){
    cat("Add title\n")
    p <- p + ggtitle(main)  
  }
  
  ## Add theme:
  require(ggthemes)
  p <- p + theme_classic()
  
  ## Add font sizes:
  p <- p + theme(axis.text.x = element_text(size = FTSxAxis),
                 axis.title.x = element_text(size = FTS), 
                 axis.text.y = element_text(size = FTS),
                 axis.title.y = element_text(size = FTS), 
                 # axis.text.x = element_blank(), # remove x-axis labels
                 # axis.ticks.x = element_blank(), # remove x-axis labels
                 title = element_text(size = 18),
                 legend.position = "none",
                 axis.line = element_line(colour = 'black')) # , linewidth = LWD)) # fixed font sizes
  
  ## Save if requested:
  if (saveEPS){
    plotNameFull <- paste0(dirs$plotDir, plotName, ".eps")
    cat(paste0("Save as ", plotNameFull, "\n"))
    setEPS(); postscript(plotNameFull, width = 480, height = 480)
    print(p)
    dev.off()
  }
  if (savePNG){
    plotNameFull <- paste0(dirs$plotDir, plotName, ".png")
    cat(paste0("Save as ", plotNameFull, "\n"))
    png(plotNameFull, width = 480, height = 480)
    print(p)
    dev.off()
  }
  
  ## Print to console:
  print(p)
  
  ## Return:
  return(p)
  cat("Finished :-)\n")
}

# ================================================================================================================================================ #
#### Barplot 2 IVs: Aggregate per condition per subject, plot (2 IVs, 1 on x-axis, 1 as color): ####

custom_barplot2 <- function(data, xVar, yVar, zVar, subVar = "subject_n", 
                            xLab = NULL, yLab =  NULL, zLab = NULL, main = NULL,
                            selCol = NULL, 
                            isPoint = T, isConnect = F, isBeeswarm = F, addLegend = TRUE,
                            yLim = NULL, FTS = NULL, LWD = NULL, dotSize = NULL,
                            savePNG = T, saveEPS = F, prefix = NULL, suffix = NULL){
  #' Make bar plots with 2 IVs: x-axis and color.
  #' @param data data frame, trial-by-trial data.
  #' @param xVar string, name of variable that goes on x-axis. If numeric, it will be converted to an (ordered) factor.
  #' @param yVar string, name of variable that goes on y-axis. Needs to be numeric.
  #' @param zVar string, name of variable that determines bar coloring. Needs to be a factor.
  #' @param subVar string, name of variable containing subject identifier (default: subject).
  #' @param xLab string, label for x-axis (default: retrieve appropriate name with substitute_label()).
  #' @param yLab string, label for y-axis (default: retrieve appropriate name with substitute_label()).
  #' @param zLab string, label for color legend (default: retrieve appropriate name with substitute_label()).
  #' @param main string, overall plot label (optional).
  #' @param selCol vector of strings (HEX colors), colors for bars (default: retrieve via retrieve_colour()).
  #' @param yLim vector of two numbers, y-axis (default: automatically determined by ggplot).
  #' @param isPoint Boolean, plot individual data points per condition as small points (default: TRUE).
  #' @param isConnect Boolean, connect individual data points with grey lines (default: FALSE).
  #' @param isBeeswarm Boolean, plot individual data points per condition as beeswarm density (default: FALSE).
  #' @param addLegend Boolean, add legend for z-axis (colour) (default: TRUE).
  #' @param FTS scalar integer, font size to use for axis labels and title (optional).
  #' @param LWD scalar interger, line width used for axis and bars in plot (optional). 
  #' @param dotSize scalar integer, size of single-subject dots to use (default: 1).
  #' @param savePNG Boolean, save as .png file.
  #' @param saveEPS Boolean, save as .eps file.
  #' @param prefix string, string to add at the beginning of plot name (optional).
  #' @param suffix string, string to add at the end of plot name (optional).
  #' @return creates (and saves) plot.
  
  # xLab = NULL; yLab = NULL; zLab = NULL; main = NULL; selCol = NULL;
  # isPoint = T; isConnect = T; isBeeswarm = F; addLegend = TRUE; yLim = NULL; FTS = NULL; dotSize = NULL; savePNG = T; saveEPS = F; prefix = NULL; suffix = NULL
  
  # -------------------------------------------------------------------------- #
  ### Load packages:
  
  require(ggplot2) # for ggplot
  require(ggthemes) # for theme_classic()
  require(ggbeeswarm) # for ggbeeswarm
  require(plyr) # for ddply
  require(Rmisc) # for summarySEwithin
  
  # -------------------------------------------------------------------------- #
  ### Close any open plots:
  
  if (length(dev.list() != 0)){dev.off()}
  
  # -------------------------------------------------------------------------- #
  ### Check inputs:
  
  ## Check if input variables included in data set:
  if(!(is.data.frame((data)))){stop("data must be a data frame")}
  if(!(yVar %in% names(data))){stop("yVar not found in data")}
  if(!(xVar %in% names(data))){stop("xVar not found in data")}
  if(!(zVar %in% names(data))){stop("zVar not found in data")}
  if(!(subVar %in% names(data))){stop("subVar not found in data")}
  
  ## Check variable types:
  if(!(is.numeric(data[, yVar]))){stop("yVar has to be numeric")}
  if(!(is.factor(data[, xVar]))){stop("xVar has to be a factor")}
  if(!(is.factor(data[, zVar]))){stop("zVar has to be a factor")}
  
  ## Axis labels:
  if(is.null(xLab)){xLab <- substitute_label(xVar)}
  if(is.null(yLab)){yLab <- substitute_label(yVar)}
  if(is.null(zLab)){zLab <- substitute_label(zVar)}
  zLab <- gsub(" ", " \n", zLab) # replace spaces by new lines in any z-label
  if(!(is.character(xLab))){stop("xLab has to be a character string")}
  if(!(is.character(yLab))){stop("yLab has to be a character string")}
  if(!(is.character(zLab))){stop("zLab has to be a character string")}
  
  ## Exclude any rows with NAs: 
  rowIdx <- which(complete.cases(data[, xVar]) & complete.cases(data[, yVar]) & complete.cases(data[, zVar]))
  cat(paste0("Excluding rows with NA on xVar or yVar or zVar: Retain ", length(rowIdx), " out of ", nrow(data), " rows (excluded ", nrow(data) - length(rowIdx), " rows)\n"))
  data <- droplevels(data[rowIdx, ])
  
  # -------------------------------------------------------------------------- #
  ### Fixed plotting settings:
  
  ## Colours:
  if(is.null(selCol)){
    selCol <- retrieve_colour(zVar)
    selCol <- rep(selCol, length.out = length(unique(data[, zVar])))
  }
  if(length(selCol) != length(unique(data[, zVar]))){
    stop(paste0("Length selCol = ", length(selCol), " while number levels zVar = ", length(unique(data[, zVar])), ", do not match"))
  }
  condCol <- rep(selCol, length(unique(data[, xVar])))
  
  ## Font size:
  if (is.null(FTS)){
    FTS <- retrieve_plot_defaults("FTS") # 30
  }
  FTSxAxis <- FTS
  if (length(unique(data[, xVar])) * length(unique(data[, zVar])) >= 15){FTSxAxis <- FTS/2; cat("Half font size because many x/z levels\n")}
  
  ## Line width:
  if (is.null(LWD)){LWD <- retrieve_plot_defaults("LWD")} # 1.3
  LWDBars <- LWD
  if (length(unique(data[, xVar]))  * length(unique(data[, zVar])) >= 15){LWDBars <- 1; cat("Set LWD of bars to 1 because many x/z levels\n")}
  
  ## Dot size:
  if (is.null(dotSize)){
    dotSize <- retrieve_plot_defaults("dotSize") # 0.5
    if (length(unique(data[, xVar])) * length(unique(data[, zVar])) >= 15){dotSize <- dotSize/2; cat("Lower dotSize from 1 to 0.5 because many x/z levels\n")}
  }
  
  ## Error bar size:
  dodgeVal <- 0.6
  colAlpha <- 1
  
  # -------------------------------------------------------------------------- #
  ### Create variables under standardized names:
  
  cat("Overall condition means (without first aggregating per subject):\n")
  print(tapply(data[, yVar], interaction(data[, zVar], data[, xVar]), mean, na.rm = T))
  
  cat("Create new variables x, y, z, subject based on inputs\n")
  data$x <- data[, xVar]
  data$y <- data[, yVar]
  data$z <- data[, zVar]
  data$subject <- data[, subVar]
  
  # -------------------------------------------------------------------------- #
  ### Aggregate data per subject per condition:
  
  cat("Aggregate data per subject\n")
  aggrData <- ddply(data, .(subject, x, z), function(x){
    y <- mean(x$y, na.rm = T)
    return(data.frame(y))
    dev.off()})
  # Wide format: each subject/condition1/condition2 in one line, variables subject, x, y, z
  
  ## Add condition variable:
  cat("Create condition variable\n")
  nZlevel <- length(unique(data$z))
  posScale <- 0.48 * exp(-0.27 * nZlevel) # negative exponential function of # levels of zVar
  aggrData$cond <- as.numeric(aggrData$x)*nZlevel - nZlevel + as.numeric(aggrData$z) # necessary for proper axis positions
  nCond <- length(unique(aggrData$cond))
  if (length(condCol) < nCond){condCol <- rep(condCol, length.out = nCond)}
  
  ## Add jittered x-axis for points:
  cat("Add jitter for points\n")
  aggrData$j <- jitter(rep(0, nrow(aggrData)), amount = .05) # pure jitter .05
  
  ## X-axis position of each condition given x and y levels:
  zMid <- (min(as.numeric(data$z)) + max(as.numeric(data$z)))/2
  aggrData$xpos <- as.numeric(aggrData$x) - min(as.numeric(aggrData$x)) + 1 + ((as.numeric(aggrData$z) - zMid)) * posScale
  aggrData$xj <- aggrData$xpos + aggrData$j # add jitter to xpos
  
  ## Determine y limits if not given:
  if(is.null(yLim)){
    cat("Automatically determine y-axis limits based on per-subject-per-condition means\n")
    yLim <- find_lim_cases(aggrData)
  }
  if (length(yLim) != 2){stop("yLim must be of length 2")}
  
  # -------------------------------------------------------------------------- #
  ### Aggregate across subjects with Rmisc:
  
  cat("Aggregate data across subjects\n")
  summary_d <- summarySEwithin(aggrData, measurevar = "y", idvar = "subject", na.rm = T,
                               withinvars = c("x", "z"))
  # Aggregated over subjects, one row per condition, variables x, z, N, y, sd, se, ci
  
  # -------------------------------------------------------------------------- #
  ### Select settings for saving:
  
  ## Additions:
  # prefix <- NULL; suffix <- NULL
  if(is.null(prefix)){prefix <- ""} else {prefix <- paste0(prefix, "_")}
  if(is.null(suffix)){suffix <- ""} else {suffix <- paste0("_", suffix)}
  
  ## Name:
  plotName <- paste0("custombarplot2_", prefix, yVar, "_", xVar, "_", zVar)
  if (isPoint){plotName <- paste0(plotName, "_points")} 
  if (isBeeswarm){plotName <- paste0(plotName, "_beeswarm")} 
  plotName <- paste0(plotName, suffix)
  cat(paste0("Start plot ", plotName, "\n"))
  
  # -------------------------------------------------------------------------- #
  ### Create ggplot:
  
  ## Start plot:
  p <- ggplot(summary_d, aes(x, y, fill = z))
  
  ## Add bars of means:
  cat("Add bars\n")
  p <- p + stat_summary(fun = mean, geom = "bar", position = "dodge", width = dodgeVal,
                        lwd = LWDBars, color = "black")
  
  ## Add error bars:
  cat("Add error bars\n")
  p <- p + geom_errorbar(data = summary_d,
                         aes(x = x, y = y, ymin = y - se, ymax = y + se),
                         position = position_dodge(width = dodgeVal), width = 0.2,
                         lwd = LWDBars, color = "black", alpha = 1)
  
  # Add individual data points:
  if (isPoint){
    cat("Start adding per-subject points \n")
    for(iCond in 1:nCond){ # add separately per condition
      p <- p + geom_point(data = aggrData[aggrData$cond == iCond, ],
                          aes(x = xj), # position = "dodge",
                          shape = 21, size = dotSize, stroke = 1.2, color = "black", fill = condCol[iCond],
                          alpha = 0.5) # colAlpha)
    }
  }
  
  ## Connect the colored dots:
  if (isConnect){
    cat("Start connecting per-subject points \n")
    
    subVec <- sort(unique(aggrData$subject))
    nSub <- length(subVec)
    
    for(iSub in 1:nSub){ # iSub <- 1
      subData <- subset(aggrData, subject == subVec[iSub])
      p <- p + geom_path(data = subData, aes(x = xj, y = y, group = 1), color = 'grey40', # color = 'grey70'
                         alpha = 0.50, size = 0.5) # consider alpha = 0.80
    }
  }  
  
  ## Add bee swarm-style plots:
  if (isBeeswarm){
    cat("Start adding beeswarm \n")
    for(iCond in 1:nCond){ # add separately per condition
      p <- p + geom_beeswarm(data = aggrData[aggrData$cond == iCond, ],
                             aes(x = xpos), # position = "dodge",
                             # priority = "ascending",
                             shape = 21, size = 2, stroke = 1.2, color = "black", fill = condCol[iCond],
                             alpha = 0.5) # colAlpha)
    }
  }
  
  ## Add y-axis:
  if (yLim[1] == 0 & yLim[2] == 1){
    cat("Add y-axis ticks for 0, 0.5, 1\n")
    # p <- p + scale_y_break(c(0, 0.5, 1))
    p <- p + scale_y_continuous(breaks = seq(0, 1, by = 0.5)) # only 0, 0.5, 1 as axis labels
  }
  if(!(is.null(yLim))){p <- p + coord_cartesian(ylim = yLim)}
  # if(!(is.null(yLim))){p <- p + scale_y_continuous(limits = yLim, breaks = seq(yLim[1], yLim[-1], (yLim[-1] - yLim[1])/2))}
  
  ## Add axis labels:
  p <- p + labs(x = xLab, y = yLab, fill = zLab)
  
  ## Add title:
  if (!(is.null(main))){
    cat("Add title\n")
    p <- p + ggtitle(main)  
  }
  
  ## Add colour:
  p <- p + scale_fill_manual(values = selCol, limits = levels(summary_d$z))
  
  ## Add theme:
  p <- p + theme_classic()
  
  ## Add font sizes:
  p <- p + theme(axis.text.x = element_text(size = FTSxAxis),
                 axis.title.x = element_text(size = FTS),
                 axis.text.y = element_text(size = FTS),
                 axis.title.y = element_text(size = FTS),
                 plot.title = element_text(size = FTS, hjust = 0.5), 
                 axis.line = element_line(colour = 'black')) # , linewidth = LWD)) # fixed font sizes
  
  ## Add legend:
  if (addLegend){
    p <- p + theme(
      legend.text = element_text(size = FTS),
      legend.title = element_text(size = FTS)
    )
  } else {
    p <- p + theme(
      legend.title = element_blank(), 
      legend.position = "none"
    )
  }
  
  ## Save if requested:
  if (saveEPS){
    plotNameFull <- paste0(dirs$plotDir, plotName, ".eps")
    cat(paste0("Save as ", plotNameFull, "\n"))
    setEPS(); postscript(plotNameFull, width = 480, height = 480)
    print(p)
    dev.off()
  }
  if (savePNG){
    plotNameFull <- paste0(dirs$plotDir, plotName, ".png")
    cat(paste0("Save as ", plotNameFull, "\n"))
    png(plotNameFull, width = 480, height = 480)
    print(p)
    dev.off()
  }
  
  
  ## Print to console:
  print(p)
  
  ## Return:
  cat("Finished :-)\n")
  return(p)
}

# ============================================================================ #
#### Barplot 3 IVs: Aggregate per condition per subject, plot (3 IVs, 1 on x-axis, 1 as color, 1 as facets): ####

custom_barplot3 <- function(data, yVar, xVar, zVar, splitVar, subVar = "subject_n", 
                            yLab = NULL, xLab = NULL, zLab = NULL, main = NULL,
                            xLevels = NULL, zLevels = NULL, splitLevels = NULL,
                            selCol = NULL, isPoint = T, yLim = NULL,  FTS = NULL, LWD = NULL,
                            savePNG = T, saveEPS = F){
  #' Make bar plots with 3 IVs: x-axis and color and facetwrap.
  #' Can add points with geom_point, 
  #' but not beeswarm plots because no position argument (hence no dodge) and manual x-position not compatible with facet_wrap.
  #' Note: In order to get the order of bars (from left to right) correctly, all factors are recoded into values from
  #' (nLevels - 1) to 0 in descending order. Later, factor levels are added again in the correct order.
  #' Check in print out if relabeling is done correctly!
  #' @param data data frame, trial-by-trial data.
  #' @param yVar string, name of variable that goes on y-axis. Needs to be numeric.
  #' @param xVar string, name of variable that goes on x-axis. If numeric, it will be converted to an (ordered) factor.
  #' @param zVar string, name of variable that determines bar coloring. Needs to be a factor.
  #' @param splitVar string, name of variable by which to split plot (facetwrap, optional).
  #' @param subVar string, name of variable containing subject identifier (default: subject).
  #' @param xLab string, label for x-axis (default: retrieve appropriate name with substitute_label()).
  #' @param yLab string, label for y-axis (default: retrieve appropriate name with substitute_label()).
  #' @param zLab string, label for color legend (default: retrieve appropriate name with substitute_label()).
  #' @param main string, title of plot (optional).
  #' @param xLevels string, level names for x-axis (default: retrieve from xVar in alphabetical order).
  #' @param zLevels string, level names for x-axis (default: retrieve from zVar in alphabetical order).
  #' @param splitLevels string, level names for x-axis (default: retrieve from splitVar in alphabetical order).
  #' @param selCol vector of strings (HEX colors), colors for bars (default: retrieve via retrieve_colour()).
  #' @param isPoint Boolean, plot individual data points per condition as small points (default: TRUE).
  #' @param yLim vector of two numbers, y-axis (default: automatically determined by ggplot).
  #' @param FTS scalar integer, font size to use for axis labels and title (optional).
  #' @param LWD scalar interger, line width used for axis and bars in plot (optional). 
  #' @param savePNG Boolean, save as .png file.
  #' @param saveEPS Boolean, save as .eps file.
  #' @return creates (and saves) plot.  #' Make bar plot per subject
  #' @param data data frame, with variables \code{variables}
  
  # yLab = NULL; xLab = NULL; zLab = NULL; main = NULL;
  # xLevels = NULL; zLevels = NULL; splitLevels = NULL;
  # selCol = NULL; isPoint = F; yLim = NULL; savePNG = T; saveEPS = F
  
  # -------------------------------------------------------------------------- #
  ### Load packages:
  
  require(ggplot2) # for ggplot
  require(ggthemes) # for theme_classic()
  require(ggbeeswarm) # for ggbeeswarm
  require(plyr) # for ddply
  require(Rmisc) # for summarySEwithin
  
  # -------------------------------------------------------------------------- #
  ### Close any open plots:
  
  if (length(dev.list() != 0)){dev.off()}
  
  # -------------------------------------------------------------------------- #
  ### Check inputs:
  
  ## Check if input variables included in data set:
  if(!(is.data.frame((data)))){stop("data must be a data frame")}
  if(!(yVar %in% names(data))){stop("yVar not found in data")}
  if(!(xVar %in% names(data))){stop("xVar not found in data")}
  if(!(zVar %in% names(data))){stop("zVar not found in data")}
  if(!(splitVar %in% names(data))){stop("splitVar not found in data")}
  if(!(subVar %in% names(data))){stop("subVar not found in data")}
  
  ## Check variable types:
  if(!(is.numeric(data[, yVar]))){stop("yVar has to be numeric")}
  if(!(is.factor(data[, xVar]))){stop("xVar has to be a factor")}
  if(!(is.factor(data[, zVar]))){stop("zVar has to be a factor")}
  if(!(is.factor(data[, splitVar]))){stop("splitVar has to be a factor")}
  
  ## Retrieve axis labels:
  if(is.null(xLab)){xLab <- substitute_label(xVar)}
  if(is.null(yLab)){yLab <- substitute_label(yVar)}
  if(is.null(zLab)){zLab <- substitute_label(zVar)}
  zLab <- gsub(" ", " \n", zLab) # add newline to any z-label
  if(!(is.character(xLab))){stop("xLab has to be a character string")}
  if(!(is.character(yLab))){stop("yLab has to be a character string")}
  if(!(is.character(zLab))){stop("zLab has to be a character string")}
  
  ## Exclude any rows with NAs: 
  rowIdx <- which(complete.cases(data[, xVar]) & complete.cases(data[, yVar]) & complete.cases(data[, zVar]) & complete.cases(data[, splitVar]))
  cat(paste0("Excluding rows with NA on xVar or yVar or zVar or splitVar: Retain ", length(rowIdx), " out of ", nrow(data), " rows (excluded ", nrow(data) - length(rowIdx), " rows)\n"))
  data <- droplevels(data[rowIdx, ])
  
  # -------------------------------------------------------------------------- #
  ### General plotting settings:
  
  ## Colours (must be determined after NA exclusion):
  if(is.null(selCol)){
    selCol <- retrieve_colour(zVar)
    selCol <- rep(selCol, length.out = length(unique(data[, zVar]))) # copy as often as necessary
  }
  if(length(selCol) != length(unique(data[, zVar]))){
    stop(paste0("Length selCol = ", length(selCol), " while number levels zVar = ", length(unique(data[, zVar])), ", do not match"))
  }
  condCol <- rep(selCol, length(unique(data[, xVar])))
  
  ## Font size: 
  if(is.null(FTS)){FTS <- retrieve_plot_defaults("FTS")} # 15 # 30
  
  ## Line width:
  if(is.null(LWD)){LWD <- retrieve_plot_defaults("LWD")} # 1.3 # 1.5
  # if (savePNG | saveEPS){FTS <- retrieve_plot_defaults("FTS")} else {FTS <- 15} # 30
  
  ## Other settings:
  dodgeVal <- 0.6 # displacement of dots
  barWidth <- 0.15 # width of error bars
  colAlpha <- 0.6 # transparency of dots
  
  SEweight <- 1.96 # weight for error bars
  isPrint <- T # print data sets to check proper recoding
  
  # -------------------------------------------------------------------------- #
  ### Create variables under standardized names:
  
  cat("Create new numerical variables x, y, z, subject based on inputs\n")
  data$y <- data[, yVar]
  data$x <- as.numeric(data[, xVar]) # convert to numeric
  data$z <- as.numeric(data[, zVar]) # convert to numeric
  if (!is.null(splitVar)){data$split <- as.numeric(data[, splitVar])} # convert to numeric
  data$subject <- data[, subVar]
  
  ## Determine level names if not set as input:
  if(is.null(xLevels)){xLevels <- as.character(levels(data[, xVar])); cat(paste0("Original levels of xVar are: ", paste0(xLevels, collapse = ", "), "\n"))}
  if(is.null(zLevels)){zLevels <- as.character(levels(data[, zVar])); cat(paste0("Original levels of zVar are: ", paste0(zLevels, collapse = ", "), "\n"))}
  # if (!all(levels(data[, zVar]) == sort(levels(data[, zVar])))){zLevels <- rev(zLevels); cat("Factor levels of zVar not in alphabetical order, invert\n")} # invert levels of z if factor levels not in alphabetical order
  if(is.null(splitLevels)){
    splitLevels <- sort(unique(data[, splitVar]))
    cat(paste0("Original levels of splitVar are: ", paste0(splitVar, collapse = ", "), "\n"))
    # if (!all(levels(data[, splitVar]) == sort(levels(data[, splitVar])))){splitLevels <- rev(splitLevels); cat("Factor levels of splitVar not in alphabetical order, invert\n")} # invert levels of split if factor levels not in alphabetical order
  }
  
  # -------------------------------------------------------------------------- #
  ### Recode to 0 until (nLevels - 1) for combining into condition variable:
  
  ## X-axis levels:
  xLevelVec <- sort(unique(data$x)) # levels
  nXlevels <- length(xLevelVec) # number of levels
  if (isPoint & nXlevels > 3){stop("Points not implemented for x variable with > 3 levels")}
  
  ## Z-axis levels:
  zLevelVec <- sort(unique(data$z)) # levels
  nZlevels <- length(zLevelVec) # number of levels
  if (isPoint & nZlevels > 2){stop("Points not implemented for z variable with > 2 levels")}
  
  ## Recode variables to go from (nLevels - 1) till 0 in descending order:
  data$x <- nXlevels - data$x # recode to (nXlevels - 1) to 0 in descending order
  data$z <- nZlevels - data$z # recode to (nXlevels - 1) to 0 in descending order
  xLevelVec <- rev(sort(unique(data$x))) # update, descending order
  zLevelVec <- rev(sort(unique(data$z))) # update, descending order
  
  if (!is.null(splitVar)){
    sLevelVec <- sort(unique(data$split))
    nSlevels <- length(sLevelVec)
    if (isPoint & nSlevels > 3){stop("Points not implemented for split variable with > 2 levels")}
    data$split <- max(data$split) - data$split # recode to (nXlevels - 1) to 0 in reverse order
    sLevelVec <- rev(sort(unique(data$split))) # update, descending order
  } # to 0 - 1
  
  ## Print recoding of factor levels into descending numbers to console: 
  if(isPrint){cat(paste0("\nMapping of original variable ", xVar, " (rows) on numeric variable x (columns; from 0 - (nLevels - 1)):\n")); print(table(data[, xVar], data$x))} # 1-N becomes (N-1)-0
  if(isPrint){cat(paste0("\nMapping of original variable ", zVar, " (rows) on numeric variable z (columns; from 0 - (nLevels - 1)):\n")); print(table(data[, zVar], data$z))} # 1-N becomes (N-1)-0
  if(isPrint & !is.null(splitVar)){cat(paste0("\nMapping of original variable ", splitVar, " (rows) on numeric variable split (columns; from 0 - (nLevels - 1)):\n")); print(table(data[, splitVar], data$split))} # # 1-N becomes (N-1)-0
  
  # -------------------------------------------------------------------------- #
  ### Combine into single condition variable:
  
  if (!is.null(splitVar)){ # if splitVar: 8 conditions
    # z is fastest changing variable, then x, then split
    data$condition <- 1 + data$split*nXlevels*nZlevels + data$x*nZlevels + data$z
  } else { # No splitVar: 4 conditions
    data$condition <- 1 + data$x*nZlevels + data$z
  }
  nCondExp <- nXlevels * nZlevels * nSlevels
  cat(paste0("Expected ", nCondExp, " levels of condition variable (namely ", nXlevels, " levels for xVar x ", nZlevels, " levels for zVar x ", nSlevels, " levels for splitVar)\n"))
  condVec <- sort(unique(data$condition))
  nCond <- length(condVec)
  if(nCondExp != nCond){cat(paste0("Found only ", nCond, " conditions: ", paste0(condVec, collapse = ", "), "\n"))}
  # stopifnot(min(data$condition) == 1)
  # stopifnot(max(data$condition) == nCond)
  
  # sort(unique(data$condition)) # from 1 to nCond
  # table(data$condition, data$split) # slowest factor, everything nXlevels*nZlevels times, from 0-nSlevels in ascending order
  # table(data$condition, data[, splitVar]) # slowest factor, everything nXlevels*nZlevels times, from nSlevels-0 in descending order
  # table(data$condition, data$x) # middle factor, everything nZlevels times, from 0-nXlevels in ascending order
  # table(data$condition, data[, xVar]) # middle factor, everything nZlevels times, from nXlevels-0 in descending order
  # table(data$condition, data$z) # fastest factor, odd and even, from 0-nZlevels in ascending order
  # table(data$condition, data[, zVar]) # fastest factor, odd and even, from nZlevels-0 in descending order
  
  # -------------------------------------------------------------------------- #
  ## Aggregate data per subject per condition:
  
  cat("Aggregate data per subject\n")
  aggrData <- ddply(data, .(subject, condition), function(x){
    y <- mean(x$y, na.rm = T)
    return(data.frame(y))
    dev.off()})
  
  ## Recover original variables:
  aggrData$z <- (aggrData$condition - 1) %% nZlevels # fastest factor, odd or even
  aggrData$x <- (ceiling(aggrData$condition/nZlevels) - 1) %% nXlevels # middle factor, everything nZlevels times 
  if (!is.null(splitVar)){
    aggrData$split <- ceiling(aggrData$condition/(nXlevels*nZlevels) - 1)
  }
  
  # print(head(aggrData, n = nCond)) # needs to match condition meaning in data object above
  # table(aggrData$condition, aggrData$split) # slowest factor, everything nXlevels*nZlevels times, from 0-nSlevels in ascending order
  # table(aggrData$condition, aggrData$x) # middle factor, everything nZlevels times, from 0-nXlevels in ascending order
  # table(aggrData$condition, aggrData$z) # fastest factor, odd and even, from 0-nZlevels in ascending order
  
  ### Create factors:
  ## Reverse above inversion: xLevelVec is inverted, but xLevels the right way around
  aggrData$x_f <- factor(aggrData$x, levels = xLevelVec, labels = xLevels) # assign factor levels to numerics (in descending order) 
  aggrData$z_f <- factor(aggrData$z, levels = zLevelVec, labels = zLevels) # assign factor levels to numerics (in descending order)
  if (!is.null(splitVar)){aggrData$split_f <- factor(aggrData$split, levels = sLevelVec, labels = splitLevels)} # assign factor levels to numerics (in descending order)
  
  cat(paste0("Assume new factor levels ", paste0(xLevelVec, collapse = ", "), " corresponds to original factor levels ", paste0(xLevels, collapse = ", "), "\n"))
  cat(paste0("Assume new factor levels ", paste0(zLevelVec, collapse = ", "), " corresponds to original factor levels ", paste0(zLevels, collapse = ", "), "\n"))
  if (!is.null(splitVar)){cat(paste0("Assume new factor levels ", paste0(sLevelVec, collapse = ", "), " corresponds to original factor levels ", paste0(splitLevels, collapse = ", "), "\n"))}
  
  if(isPrint){
    cat("\nHead of data file with condition mean per subject; check correspondence of condition to x, z, split:\n")
    print(head(aggrData[, c("subject", "condition", "y", "x", "x_f", "z", "z_f", "split", "split_f")], n = nCond))
  } # needs to match condition meaning in data object above
  
  # -------------------------------------------------------------------------- #
  ### Determine y limits if not given:
  
  if(is.null(yLim)){
    cat("Automatically y-axis limits based on per-subject-per-condition means\n")
    yLim <- find_lim_cases(aggrData)
  }
  if (length(yLim) != 2){stop("yLim must be of length 2")}
  
  # -------------------------------------------------------------------------- #
  ### Aggregate across subjects with Rmisc:
  cat("Aggregate data across subjects\n")
  
  d <- summarySEwithin(aggrData, measurevar = "y", withinvar = "condition", idvar = "subject", na.rm = T)
  d$condition <- condVec # condition will be from 1 to nCond found; instead overwrite with empirical conditions from previous data set
  # d$condition <- as.numeric(as.factor(d$condition)) # condition back to numeric
  # condVec <- sort(unique(d$condition))
  # nCond <- length(unique(condVec))
  # cat(paste0("Expected ", nCondExp, " condition levels; found ", nCond, " condition levels, namely ", paste0(condVec, collapse = ", "), "\n"))
  
  ## Recover independent variables from condition:
  d$z <- (d$condition - 1) %% nZlevels # fastest variable
  d$x <- ceiling(d$condition/nZlevels - 1) %% nXlevels # intermediate variable
  if (!is.null(splitVar)){
    d$split <- ceiling(d$condition/(nXlevels*nZlevels) - 1) # slowest variable
  }
  
  # print(d)
  
  ## Create factors:
  d$x_f <- factor(d$x, levels = xLevelVec, labels = xLevels)
  d$z_f <- factor(d$z, levels = zLevelVec, labels = zLevels)
  if (!is.null(splitVar)){d$split_f <- factor(d$split, levels = sLevelVec, labels = splitLevels)}
  
  if(isPrint){
    cat("\nAggregated data file, check correspondence of x, z, split to factor labels\n")
    print(d[, c("condition", "N", "y", "sd", "se", "ci", "x", "x_f", "z", "z_f", "split", "split_f")])
  }
  
  ## Check if y +/- se within ylim:
  if (any(d$y - d$se < yLim[1], na.rm = T)){warning("Lower error bars will exceed y-axis limit")}
  if (any(d$y + d$se > yLim[2], na.rm = T)){warning("Upper error bars will exceed y-axis limit")}
  
  # -------------------------------------------------------------------------- #
  # -------------------------------------------------------------------------- #
  # -------------------------------------------------------------------------- #
  ## Start plot:
  
  ## Name:
  plotName <- paste0("custombarplot3_",  yVar, "~", xVar, "_", zVar, "_")
  if (!(is.null(splitVar))){plotName <- paste0(plotName, splitVar)}
  if (isPoint){plotName <- paste0(plotName, "_points")} 
  cat(paste0("Start plot ", plotName, "\n"))
  
  ## Initialize ggplot object:
  p <- ggplot(d, aes(x = x_f, y = y, fill = z_f))
  
  ## Add bars of means:
  cat("Add bars\n")
  p <- p + geom_bar(position = "dodge", stat = "summary", fun = "identity",
                    color = "black", width = dodgeVal, lwd = LWD)
  
  ## Add error bars:
  cat("Add error bars\n")
  p <- p + geom_errorbar(data = d,
                         aes(x = x_f, y = y, ymin = y - se, ymax = y + se),
                         position = position_dodge(width = dodgeVal), width = barWidth,
                         lwd = LWD, color = "black", alpha = 1)
  
  ## Add individual data points:
  if (isPoint){
    cat("Start adding per-subject points \n")
    p <- p + geom_point(data = aggrData,
                        position = position_dodge(width = dodgeVal),
                        shape = 21, size = 2, stroke = 1.2, color = "black",
                        alpha = 0.5) 
  }
  
  ## Add facet wrap:
  if (!is.null(splitVar)){
    cat("Start adding facet_wrap\n")
    p <- p + facet_wrap(vars(split_f))
  }
  
  ## Add y-axis limits:
  cat("Start adding y-axis ticks\n")
  p <- p + scale_y_continuous(limits = yLim, breaks = seq(yLim[1], yLim[-1], (yLim[-1] - yLim[1])/2)) 
  
  ## Add labels:
  cat("Start labels\n")
  p <- p + labs(x = xLab, fill = zLab, y = yLab)
  
  ## Add color:
  cat("Start colors for fill\n")
  p <- p + scale_fill_manual(values = rep(selCol, 4), limits = levels(d$z_f))
  
  ## Add theme:
  cat("Start theme\n")
  p <- p + theme_classic()
  
  # Add title:
  if (!(is.null(main))){
    cat("Add title\n")
    p <- p + ggtitle(main)  
  }
  
  ## Add font sizes:
  cat("Start line width and font size \n")
  p <- p + theme(axis.line = element_line(colour = 'black'), # linewidth = LWD),
                 axis.text = element_text(size = FTS),
                 axis.title = element_text(size = FTS), 
                 plot.title = element_text(size = FTS, hjust = 0.5), # center title 
                 legend.title = element_text(size = FTS),
                 strip.text.x = element_text(size = FTS), # facetwrap FTS
                 legend.text = element_text(size = FTS))
  
  ## Save if requested:
  if (savePNG){
    plotNameFull <- paste0(dirs$plotDir, plotName, ".png")
    cat(paste0("Save as ", plotNameFull, "\n"))
    png(plotNameFull, width = 480, height = 480)
    print(p)
    dev.off()
  }  
  ## Save if requested:
  if (saveEPS){
    plotNameFull <- paste0(dirs$plotDir, plotName, ".eps")
    cat(paste0("Save as ", plotNameFull, "\n"))
    setEPS(); postscript(plotNameFull, width = 480, height = 480)
    print(p)
    dev.off()
  }
  
  ## Print to console:
  print(p)
  
  ## Return:
  cat("Finished :-)\n")
  return(p)
  
} # end of function

# ============================================================================ #
#### Lineplot 1 IV: Aggregate per time point per condition per subject, plot (1 IV for any condition, time on x-axis): ####

custom_lineplot <- function(data, xVar, yVar, zVar, subVar = "subject_n", 
                            xLab = NULL, yLab = NULL, main = NULL, addLegend = T,
                            selCol = NULL, selLineType = NULL, FTS = NULL, LWD = NULL,
                            SEweight = 1, yLim = NULL, savePNG = F, saveEPS = F){
  #' Make line plot with group-level and individual lines using base package.
  #' @param data data frame, trial-by-trial data.
  #' @param xVar string, name of variable that goes on x-axis. Variable needs to be numeric.
  #' @param yVar string, name of variable that goes on y-axis. Variable needs to be numeric.
  #' @param zVar string, name of variable that determines bar coloring. Variable needs to be a factor.
  #' @param subVar string, name of variable containing subject identifier (default: subject).
  #' @param xLab string, label for x-axis (default: retrieve appropriate name with substitute_label()).
  #' @param yLab string, label for y-axis (default: retrieve appropriate name with substitute_label()).
  #' @param main string, overall plot label (optional).
  #' @param addLegend, Boolean, add legend of zVar (colours) or not (default: TRUE).
  #' @param selCol vector of strings (HEX colors), colors for input levels of zVar (default: retrieve via retrieve_colours()).
  #' @param selLineType vector of numerics, line types to use (default: all 1).
  #' @param FTS scalar integer, font size to use for axis labels and title (optional).
  #' @param LWD scalar interger, line width used for axis and bars in plot (optional). 
  #' @param SEweight scalar, weight to use for error shades (how many times SE; default: 1).
  #' @param yLim vector of two numbers, y-axis limits (default: NULL).
  #' @param savePNG Boolean, save as .png file.
  #' @param saveEPS Boolean, save as .eps file.
  #' @return creates (and saves) plot.
  
  # -------------------------------------------------------------------------- #
  ### Load packages:
  
  require(plyr) # for ddply
  require(Rmisc) # for summarySEwithin
  
  # -------------------------------------------------------------------------- #
  ### Close any open plots:
  
  if (length(dev.list() != 0)){dev.off()}
  
  ## -------------------------------------------------------------------------- #
  ## Check if input variables included in data set:
  
  if(!(is.data.frame((data)))){stop("data must be a data frame")}
  if(!(yVar %in% names(data))){stop("yVar not found in data")}
  if(!(xVar %in% names(data))){stop("xVar not found in data")}
  if(!(zVar %in% names(data))){stop("zVar not found in data")}
  if(!(subVar %in% names(data))){stop("subVar not found in data")}
  
  ## Check variable types:
  if(!(is.numeric(data[, yVar]))){stop("yVar has to be numeric")}
  if(!(is.numeric(data[, xVar]))){stop("xVar has to be numeric")}
  if(!(is.factor(data[, zVar]))){stop("zVar has to be a factor")}
  
  ## Axis labels:
  if(is.null(xLab)){xLab <- substitute_label(xVar)}
  if(is.null(yLab)){yLab <- substitute_label(yVar)}
  if(!(is.character(xLab))){stop("xLab has to be a character string")}
  if(!(is.character(yLab))){stop("yLab has to be a character string")}
  
  ## Exclude any rows with NAs: 
  rowIdx <- which(complete.cases(data[, xVar]) & complete.cases(data[, yVar]) & complete.cases(data[, zVar]))
  cat(paste0("Excluding rows with NA on xVar or yVar or zVar: Retain ", length(rowIdx), " out of ", nrow(data), " rows (excluded ", nrow(data) - length(rowIdx), " rows)\n"))
  data <- droplevels(data[rowIdx, ])
  
  # -------------------------------------------------------------------------- #
  ## Fixed plotting settings:
  
  ## Colour:
  if(is.null(selCol)){
    selCol <- retrieve_colour(zVar)
    selCol <- rep(selCol, length.out = length(unique(data[, zVar])))
  }
  if(length(selCol) != length(unique(data[, zVar]))){
    stop(paste0("Length selCol = ", length(selCol), " while number levels zVar = ", length(unique(data[, zVar])), ", do not match"))
  }
  
  ## Line type:
  if (is.null(selLineType)){selLineType <- rep(1, length(unique(data[, zVar])))}
  
  ## Font size: 
  if(is.null(FTS)){FTS <- retrieve_plot_defaults("FTS")} # 20
  
  ## Line width:
  if(is.null(LWD)){LWD <- 3} # 3 set by hand
  # if(is.null(LWD)){LWD <- retrieve_plot_defaults("LWD")} # 3
  # if (savePNG | saveEPS){FTS <- retrieve_plot_defaults("FTS")} else {FTS <- 15} # 30
  
  ## Other settings:
  CEX <- 1.5 # axes ticks and labels
  
  # -------------------------------------------------------------------------- #
  ### Create variables under standardized names:
  
  data$x <- data[, xVar]
  data$y <- data[, yVar]
  data$z <- data[, zVar]
  data$subject <- data[, subVar]
  
  # -------------------------------------------------------------------------- #
  ### Aggregate data per subject per condition:
  
  cat("Aggregate data per conditions x/z per subject\n")
  aggrData <- ddply(data, .(subject, x, z), function(x){
    y <- mean(x$y, na.rm = T)
    return(data.frame(y))
    dev.off()
  })
  # Wide format: each subject/x condition/z condition in one line, variables subject, x, y, z
  
  ## Drop NAs:
  rowIdx <- which(complete.cases(aggrData$x) & complete.cases(aggrData$z) & complete.cases(aggrData$y))
  aggrData <- droplevels(aggrData[rowIdx, ])
  
  # -------------------------------------------------------------------------- #
  ### Aggregate across subjects with Rmisc:
  
  cat("Aggregate data across subjects\n")
  summary_d <- summarySEwithin(aggrData, measurevar = "y", idvar = "subject", na.rm = T,
                               withinvars = c("x", "z"))
  # Aggregated over subjects, one row per condition, variables x, z, N, y, sd, se, ci
  
  ## Determine y limits if not given:
  if(is.null(yLim)){
    yLim <- find_lim_cases(summary_d)
  }
  if (length(yLim) != 2){stop("yLim must be of length 2")}
  
  # -------------------------------------------------------------------------- #
  ### Retrieve data dimensions:
  
  xVec <- unique(sort(as.numeric(summary_d$x)))
  xLim <- c(min(xVec), max(xVec))
  condNames <- unique(summary_d$z)
  nCond <- length(unique(summary_d$z))
  
  # -------------------------------------------------------------------------- #
  ### Create plot name:
  
  plotName <- paste0("lineplot_", yVar, "_", xVar, "_",zVar)
  
  # -------------------------------------------------------------------------- #
  ### Start saving (base R, thus no assignment to object p):
  
  if (saveEPS){
    plotNameFull <- paste0(dirs$plot, plotName, ".eps")
    cat(paste0("Save as ", plotNameFull, "\n"))
    setEPS()
    postscript(plotNameFull, width = 480, height = 480)
  }
  if (savePNG){
    plotNameFull <- paste0(dirs$plot, plotName, ".png")
    cat(paste0("Save as ", plotNameFull, "\n"))
    png(plotNameFull, width = 480, height = 480)
  }
  
  # -------------------------------------------------------------------------- #
  ### Start plot:
  
  par(mar = c(5.1, 5.1, 4.1, 2.1)) # bottom, left, top, right
  
  ## Create empty plot:
  cat("Start plot ...\n")
  plot(NA, 
       axes = FALSE, xaxt = "n", yaxt = "n", bty = "n", frame.plot = F,
       xlim = xLim, ylim = yLim,
       lwd = LWD, cex.lab = CEX, cex.axis = CEX, cex.main = CEX,
       xlab = xLab, ylab = yLab, main = main)
  
  ## Add axes:
  axis(side = 1, lwd = LWD, cex.axis = CEX, at = seq(xLim[1], xLim[2], find_step(xLim)), line = 0)
  axis(side = 2, lwd = LWD, cex.axis = CEX, at = seq(yLim[1], yLim[2], find_step(yLim)), line = 0)
  
  ## Loop over conditions:
  for (iCond in 1:nCond){ # iCond <- 1
    
    cat(paste0("Plot condition ", iCond, "\n"))
    
    ## Extract mean and SE for line for this condition:
    condName <- condNames[iCond] # name of condition
    condIdx <- which(summary_d$z == condName)
    xVec <- summary_d$x[condIdx] # x-variable
    yVec <- summary_d$y[condIdx] # y-variable
    seVec <- summary_d$se[condIdx] # se variable
    
    ## Add line:
    lines(xVec, yVec, col = selCol[iCond], lwd = LWD, lty = selLineType[iCond])
    
    ## Add error bars:
    polygon(c(xVec, rev(xVec)),
            c(yVec - SEweight*seVec, rev(yVec + SEweight*seVec)), 
            col = alpha(selCol[iCond], 0.2), border = F)
    par(new = TRUE)
  }
  
  ## Add legend:
  if (addLegend){
    legend("top", legend = condNames,
           col = selCol, lty = selLineType, border = 0, 
           lwd = LWD*2, cex = CEX/2, horiz = TRUE, bty = "n") # double the LWD, half the CEX
  }
  
  ## Stop saving if requested:
  if(savePNG | saveEPS){dev.off()}
  
  ## Return margins to default:
  par(mar = c(5.1, 4.1, 4.1, 2.1)) # bottom, left, top, right
  cat("Finished :-)\n")
}

# ================================================================================================================================================ #
#### Lineplot 1 IV with ggplot: Aggregate per time point per condition per subject, plot (1 IV for any condition, time on x-axis): ####

custom_lineplot_gg <- function(data, xVar, yVar, zVar, subVar = "subject_n", 
                               xLab = NULL, yLab = NULL, zLab = NULL, main = NULL, addLegend = T,
                               selCol = NULL, selLineType = NULL, FTS = NULL, LWD = NULL, 
                               SEweight = 1, yLim = NULL, savePNG = F, saveEPS = F){
  #' Make line plot with group-level lines plus shades in ggplot.
  #' @param data data frame, trial-by-trial data.
  #' @param xVar string, name of variable that goes on x-axis. Variable needs to be numeric.
  #' @param yVar string, name of variable that goes on y-axis. Variable needs to be numeric.
  #' @param zVar string, name of variable that determines bar coloring. Variable needs to be a factor.
  #' @param subVar string, name of variable containing subject identifier (default: subject).
  #' @param xLab string, label for x-axis (default: retrieve appropriate name with substitute_label()).
  #' @param yLab string, label for y-axis (default: retrieve appropriate name with substitute_label()).
  #' @param zLab string, label for z-axis (colour; default: retrieve appropriate name with substitute_label()).
  #' @param main string, overall plot label (optional).
  #' @param addLegend, Boolean, add legend of zVar (colours) or not (default: TRUE).
  #' @param selCol vector of strings (HEX colors), colors for input levels of zVar (default: retrieve via retrieve_colours()).
  #' @param selLineType vector of numerics, line types to use (default: all 1).
  #' @param FTS scalar integer, font size to use for axis labels and title (optional).
  #' @param LWD scalar interger, line width used for axis and bars in plot (optional). 
  #' @param SEweight scalar, weight to use for error shades (how many times SE; default: 1).
  #' @param yLim vector of two numbers, y-axis (default: NULL).
  #' @param savePNG Boolean, save as .png file.
  #' @param saveEPS Boolean, save as .eps file.
  #' @return creates (and saves) plot.
  
  # -------------------------------------------------------------------------- #
  ### Load required packages:
  
  require(ggplot2)
  require(ggthemes)
  require(plyr) # for ddply
  require(Rmisc) # for summarySEwithin
  
  # -------------------------------------------------------------------------- #
  ### Check if input variables included in data set:
  
  if(!(is.data.frame((data)))){stop("data must be a data frame")}
  if(!(yVar %in% names(data))){stop("yVar not found in data")}
  if(!(xVar %in% names(data))){stop("xVar not found in data")}
  if(!(zVar %in% names(data))){stop("zVar not found in data")}
  if(!(subVar %in% names(data))){stop("subVar not found in data")}
  
  ## Check variable types:
  if(!(is.numeric(data[, yVar]))){stop("yVar has to be numeric")}
  if(!(is.numeric(data[, xVar]))){stop("xVar has to be numeric")}
  if(!(is.factor(data[, zVar]))){stop("zVar has to be a factor")}
  
  ## Axis labels:
  if(is.null(xLab)){xLab <- substitute_label(xVar)}
  if(is.null(yLab)){yLab <- substitute_label(yVar)}
  if(is.null(zLab)){zLab <- substitute_label(zVar)}
  zLab <- gsub(" ", " \n", zLab) # replace spaces by new lines in any z-label
  if(!(is.character(xLab))){stop("xLab has to be a character string")}
  if(!(is.character(yLab))){stop("yLab has to be a character string")}
  if(!(is.character(zLab))){stop("zLab has to be a character string")}
  
  ## Exclude any rows with NAs: 
  rowIdx <- which(complete.cases(data[, xVar]) & complete.cases(data[, yVar]) & complete.cases(data[, zVar]))
  cat(paste0("Excluding rows with NA on xVar or yVar or zVar: Retain ", length(rowIdx), " out of ", nrow(data), " rows (excluded ", nrow(data) - length(rowIdx), " rows)\n"))
  data <- droplevels(data[rowIdx, ])
  
  # -------------------------------------------------------------------------- #
  ## Fixed plotting settings:
  
  ## Colour:
  if(is.null(selCol)){
    selCol <- retrieve_colour(zVar)
    selCol <- rep(selCol, length.out = length(unique(data[, zVar])))
  }
  if(length(selCol) != length(unique(data[, zVar]))){
    stop(paste0("Length selCol = ", length(selCol), " while number levels zVar = ", length(unique(data[, zVar])), ", do not match"))
  }
  
  ## Font size: 
  if(is.null(FTS)){FTS <- retrieve_plot_defaults("FTS")} # 30
  
  ## Line width:
  if(is.null(LWD)){LWD <- retrieve_plot_defaults("LWD")} # 1.3  
  
  ## Line type:
  if (is.null(selLineType)){selLineType <- rep(1, length(unique(data[, zVar])))}
  
  colAlpha <- 1
  
  # -------------------------------------------------------------------------- #
  ## Create variables under standardized names:
  
  data$x <- data[, xVar]
  data$y <- data[, yVar]
  data$z <- data[, zVar]
  data$subject <- data[, subVar]
  
  # -------------------------------------------------------------------------- #
  ## Aggregate data per condition per subject:
  
  cat("Aggregate data per x/ z condition per subject\n")
  aggrData <- ddply(data, .(subject, x, z), function(x){
    y <- mean(x$y, na.rm = T)
    return(data.frame(y))
    dev.off()})
  # Wide format: each subject/x condition/z condition in one line, variables subject, x, y, z
  
  # -------------------------------------------------------------------------- #
  ## Aggregate across subjects with Rmisc:
  
  cat("Aggregate data across subjects\n")
  summary_d <- summarySEwithin(aggrData, measurevar = "y", idvar = "subject", na.rm = T,
                               withinvars = c("x", "z"))
  summary_d$x <- as.numeric(summary_d$x) # back to numeric to get continuous x-axis
  # Aggregated over subjects, one row per condition, variables x, z, N, y, sd, se, ci
  
  ## Determine y limits if not given:
  if(is.null(yLim)){
    yLim <- find_lim_cases(summary_d)
  }
  if (length(yLim) != 2){stop("yLim must be of length 2")}
  
  ## Data dimensions:
  xVec <- unique(sort(as.numeric(summary_d$x)))
  xLim <- c(min(xVec), max(xVec))
  condNames <- unique(summary_d$z)
  nCond <- length(unique(summary_d$z))
  
  # -------------------------------------------------------------------------- #
  ## Create ggplot:
  
  # Name:
  plotName <- paste0("lineplot_gg_", yVar, "_", xVar, "_", zVar)
  
  # Start plot:
  # par(mar = c(5.1, 5.1, 4.1, 2.1)) # bottom, left, top, right
  cat("Start plot ...\n")
  p <- ggplot(data = summary_d, aes(x, y, fill = z))
  
  for (iCond in 1:nCond){ # iCond <- 1
    condData <- subset(summary_d, z == condNames[iCond]) # select data for this condition
    condData$ymin <- condData$y - SEweight * condData$se # lower edge of shade
    condData$ymax <- condData$y + SEweight * condData$se # upper edge of shade
    ## Shade:
    p <- p + geom_ribbon(data = condData, aes(x = x, y = y, ymin = ymin, ymax = ymax, group = 1),
                         fill = alpha(selCol[iCond], 0.2))
    ## Line:
    p <- p + geom_path(data = condData, aes(x = x, y = y, group = 1),
                       col = selCol[iCond], linetype = selLineType[iCond], linewidth = LWD, show.legend = T)
    
  }
  
  ## Add x-axis:
  p <- p + scale_x_continuous(limits = xLim, breaks = seq(xLim[1], xLim[2], 5))
  
  ## Add y-axis:
  if (yLim[1] == 0 & yLim[2] == 1){ # if probability
    p <- p + scale_y_continuous(breaks = seq(0, 1, by = 0.5)) # only 0, 0.5, 1 as axis labels
  }
  if(!(is.null(yLim))){p <- p + coord_cartesian(ylim = yLim)}
  
  # Add axus labels:
  p <- p + labs(x = xLab, y = yLab, fill = zLab)
  
  # Add title:
  if (!(is.null(main))){
    p <- p + ggtitle(main)  
  }
  
  ## Add theme:
  p <- p + theme_classic()
  
  ## Add font sizes:
  p <- p + theme(
    axis.text = element_text(size = FTS),
    axis.title = element_text(size = FTS), 
    plot.title = element_text(size = FTS, hjust = 0.5)
  )
  
  ## Add legend or not:
  if (addLegend){
    cat("Add legend\n")
    p <- p + guides(fill = guide_legend(override.aes = list(col = selCol, breaks = levels(data[, zVar])))) # overwrite colours
    p <- p + theme(
      legend.text = element_text(size = FTS/2),
      legend.title = element_text(size = FTS/2),
      legend.position = "top"
    )
  } else {
    p <- p + theme(
      legend.title = element_blank(), 
      legend.position = "none"
    )
  }
  
  ## Save if requested:
  if (saveEPS){
    plotNameFull <- paste0(dirs$plot, plotName, ".eps")
    cat(paste0("Save as ", plotNameFull, "\n"))
    setEPS()
    postscript(plotNameFull, width = 480, height = 480)
    print(p)
    dev.off()
  }
  if (savePNG){
    plotNameFull <- paste0(dirs$plot, plotName, ".png")
    cat(paste0("Save as ", plotNameFull, "\n"))
    png(plotNameFull, width = 480, height = 480)
    print(p)
    dev.off()
  }
  
  ## Print to console:  
  print(p)
  cat("Finished :-)\n")
  
  return(p)
  
}

# ============================================================================ #
#### Line plot with 1 IV with base R: grand mean: ####

custom_lineplot_grandmean1 <- function(data, xVar, yVar, type = "b",
                                       xLab = NULL, yLab = NULL, xLim = NULL, yLim = NULL,
                                       selCol = "black", LWD = 3, CEX = 1.5){
  #' Make line plot for single IV (x-axis) using base package.
  #' @param data data frame, trial-by-trial data.
  #' @param xVar string, name of variable that goes on x-axis. Variable needs to be numeric.
  #' @param yVar string, name of variable that goes on y-axis. Variable needs to be numeric.
  #' @param xLab string, label for x-axis (default: retrieve appropriate name with substitute_label()).
  #' @param yLab string, label for y-axis (default: retrieve appropriate name with substitute_label()).
  #' @param xLim vector of two numbers,x-axis limits (default: NULL).
  #' @param yLim vector of two numbers, y-axis limits (default: NULL).
  #' @param selCol vector of strings (HEX colors), colors for input levels of zVar (default: retrieve via retrieve_colours()).
  #' @param LWD scalar numeric, line width used for axis and bars in plot (default: 3). 
  #' @param CEX scalar numeric, font size for axes (default: 1.5). 
  #' @return creates (and saves) plot.
  
  # -------------------------------------------------------------------------- #
  ### Check if input variables included in data set:
  
  if(!(is.data.frame((data)))){stop("data must be a data frame")}
  if(!(yVar %in% names(data))){stop("yVar not found in data")}
  if(!(xVar %in% names(data))){stop("xVar not found in data")}
  
  ## Check variable types:
  if(!(is.numeric(data[, yVar]))){stop("yVar has to be numeric")}
  if(!(is.numeric(data[, xVar]))){stop("xVar has to be numeric")}
  
  ## Axis labels:
  if(is.null(xLab)){xLab <- substitute_label(xVar)}
  if(is.null(yLab)){yLab <- substitute_label(yVar)}
  if(!(is.character(xLab))){stop("xLab has to be a character string")}
  if(!(is.character(yLab))){stop("yLab has to be a character string")}
  
  ## Exclude any rows with NAs: 
  rowIdx <- which(complete.cases(data[, xVar]) & complete.cases(data[, yVar]))
  cat(paste0("Excluding rows with NA on xVar or yVar: Retain ", length(rowIdx), " out of ", nrow(data), " rows (excluded ", nrow(data) - length(rowIdx), " rows)\n"))
  plotData <- droplevels(data[rowIdx, ])
  
  # -------------------------------------------------------------------------- #
  ## Create vectors:
  
  xLevel <- sort(unique(plotData[, xVar]))
  xVec <- as.numeric(xLevel)
  yVec <- as.numeric(tapply(plotData[, yVar], plotData[, xVar], mean, na.rm = T))
  yVec <- yVec[complete.cases(yVec)]
  
  ## Limits:
  if(is.null(xLim)){xLim <- c(floor(min(xVec, na.rm = T)), ceiling(max(xVec, na.rm = T)))}
  if(is.null(yLim)){yLim <- c(floor(min(yVec, na.rm = T)), ceiling(max(yVec, na.rm = T)))}
  if (length(xLim) != 2){stop("xLim must be of length 2")}
  if (length(yLim) != 2){stop("yLim must be of length 2")}
  
  ## Widths:
  if(length(xVec) > 40){
    pLWD  <- 1
    pCEX <- 1
  } else {
    pLWD  <- LWD
    pCEX <- CEX
  }
  
  # -------------------------------------------------------------------------- #
  ## Initialize empty plot:
  
  plot(NA, axes = FALSE, bty = "n",
       xlim = xLim, ylim = yLim,
       xlab = xLab, ylab = yLab, main = paste0(yLab, " per \n", tolower(xLab)),
       cex.lab = CEX, cex.axis = CEX, cex.main = CEX, cex.sub = CEX)
  
  ## Add axes:
  if(is.factor(xLevel)){
    axis(side = 1, at = xVec, labels = xLevel, lwd = LWD, cex.axis = CEX)
  } else {
    xLim <- c(min(xVec), max(xVec))
    axis(side = 1, at = seq(xLim[1], xLim[2], find_step(xLim)), line = 0, labels = T, lwd = LWD, cex.axis = CEX)
  }
  axis(side = 2, labels = T, lwd = LWD, cex.axis = CEX)
  
  ## Add points:
  if(type %in% c("p", "b")){
    points(xVec, yVec, col = selCol, lwd = pLWD, cex = pCEX)
  }
  
  ## Add lines:
  if(type %in% c("l", "b")){
    lines(xVec, yVec, col = selCol, lwd = pLWD)
  }
  cat("Finished :-)\n")
  
}

# ============================================================================ #
#### Line plot with 2 IVs with base R: grand mean: ####

custom_lineplot_grandmean2 <- function(data, xVar, yVar, zVar, type = "b", 
                                       xLab = NULL, yLab = NULL, zLab = NULL, 
                                       xLim = NULL, yLim = NULL,
                                       addLegend = T, selCol = NULL, selLineType = NULL, 
                                       LWD = 3, CEX = 1.5){
  #' Make line plot for two IV (x-axis and colour) using base package.
  #' @param data data frame, trial-by-trial data.
  #' @param xVar string, name of variable that goes on x-axis. Variable needs to be numeric.
  #' @param yVar string, name of variable that goes on y-axis. Variable needs to be numeric.
  #' @param zVar string, name of variable that determines bar coloring. Variable needs to be a factor.
  #' @param xLab string, label for x-axis (default: retrieve appropriate name with substitute_label()).
  #' @param yLab string, label for y-axis (default: retrieve appropriate name with substitute_label()).
  #' @param xLim vector of two numbers,x-axis limits (default: NULL).
  #' @param yLim vector of two numbers, y-axis limits (default: NULL).
  #' @param addLegend, Boolean, add legend of zVar (colours) or not (default: TRUE).
  #' @param selCol vector of strings (HEX colors), colors for input levels of zVar (default: retrieve via retrieve_colours()).
  #' @param selLineType vector of numerics, line types to use (default: all 1).
  #' @param LWD scalar numeric, line width used for axis and bars in plot (default: 3). 
  #' @param CEX scalar numeric, font size for axes (default: 1.5). 
  #' @return creates (and saves) plot.
  
  # -------------------------------------------------------------------------- #
  ### Check if input variables included in data set:
  
  ## Check inputs:
  if(!(is.data.frame((data)))){stop("data must be a data frame")}
  if(!(yVar %in% names(data))){stop("yVar not found in data")}
  if(!(xVar %in% names(data))){stop("xVar not found in data")}
  if(!(zVar %in% names(data))){stop("zVar not found in data")}
  
  ## Check variable types:
  if(!(is.numeric(data[, yVar]))){stop("yVar has to be numeric")}
  if(!(is.numeric(data[, xVar]))){stop("xVar has to be numeric")}
  if(!(is.factor(data[, zVar]))){stop("zVar has to be a factor")}
  
  ## Label:
  if(is.null(xLab)){xLab <- substitute_label(xVar)}
  if(is.null(yLab)){yLab <- substitute_label(yVar)}
  if(is.null(zLab)){zLab <- substitute_label(zVar)}
  
  ## Exclude any rows with NAs: 
  rowIdx <- which(complete.cases(data[, xVar]) & complete.cases(data[, yVar]) & complete.cases(data[, zVar]))
  cat(paste0("Excluding rows with NA on xVar or yVar or zVar: Retain ", length(rowIdx), " out of ", nrow(data), " rows (excluded ", nrow(data) - length(rowIdx), " rows)\n"))
  plotData <- droplevels(data[rowIdx, ])
  
  # -------------------------------------------------------------------------- #
  ### Plotting settings:
  
  ## Colour:
  if(is.null(selCol)){
    selCol <- retrieve_colour(zVar)
    selCol <- rep(selCol, length.out = length(unique(data[, zVar])))
  }
  if(length(selCol) != length(unique(data[, zVar]))){
    stop(paste0("Length selCol = ", length(selCol), " while number levels zVar = ", length(unique(data[, zVar])), ", do not match"))
  }
  
  ## Line type:
  if (is.null(selLineType)){selLineType <- rep(1, length(unique(data[, zVar])))}
  
  # -------------------------------------------------------------------------- #
  ### Count levels of z:
  
  zLevelVec <- levels(plotData[, zVar])
  nLevelZ <- length(zLevelVec)
  
  # -------------------------------------------------------------------------- #
  ### Extract x and y per level of z:
  
  xList <- list()
  yList <- list()
  for (iLevel in 1:nLevelZ){
    zData <- plotData[plotData[, zVar] == zLevelVec[iLevel], ]
    xList[[iLevel]] <- sort(as.numeric(unique(zData[, xVar])))
    yList[[iLevel]] <- as.numeric(tapply(zData[, yVar], zData[, xVar], mean, na.rm = T))
  }
  
  ## Limits:
  if(is.null(xLim)){xLim <- c(floor(min(unlist(xList), na.rm = T)), ceiling(max(unlist(xList), na.rm = T)))}
  if(is.null(yLim)){yLim <- c(floor(min(unlist(yList), na.rm = T)), ceiling(max(unlist(yList), na.rm = T)))}
  if (length(xLim) != 2){stop("xLim must be of length 2")}
  if (length(yLim) != 2){stop("yLim must be of length 2")}
  
  ## X-axis positions and labels:
  xLevelVec <- sort(unique(plotData[, xVar]))
  xPosVec <- as.numeric(xLevelVec)
  
  ## Widths:
  if(length(unique(unlist(xList))) > 40){
    pLWD  <- 1
    pCEX <- 1
  } else {
    pLWD  <- LWD
    pCEX <- CEX
  }
  
  # -------------------------------------------------------------------------- #
  ## Initialize empty plot:
  
  plot(NA, axes = FALSE, bty = "n",
       xlim = xLim, ylim = yLim,
       xlab = xLab, ylab = yLab, main = paste0(yLab, " per \n", tolower(xLab), ",\n separately per ", zLab),
       cex.lab = CEX, cex.axis = CEX, cex.main = CEX, cex.sub = CEX)
  
  ## Add axes:
  if(is.factor(xLevelVec)){
    axis(side = 1, at = xPosVec, labels = xLevelVec, lwd = LWD, cex.axis = CEX)
  } else {
    axis(side = 1, at = seq(xLim[1], xLim[2], find_step(xLim)), labels = T, lwd = LWD, cex.axis = CEX)
  }
  axis(side = 2, labels = T, lwd = LWD, cex.axis = CEX)
  
  # -------------------------------------------------------------------------- #
  ### Loop over levels of z:
  
  for (iLevel in 1:nLevelZ){
    
    ## Retrieve:
    xVec <- xList[[iLevel]]
    yVec <- yList[[iLevel]]
    yVec <- yVec[complete.cases(yVec)]
    
    ## Add points:
    if(type %in% c("p", "b")){
      points(xVec, yVec, col = selCol[iLevel], lwd = pLWD, cex = pCEX)
    }
    
    ## Add lines:
    if(type %in% c("l", "b")){
      lines(xVec, yVec, col = selCol[iLevel], lty = selLineType[iLevel], lwd = pLWD)
    }
  }
  
  ## Add legend:
  if (addLegend){
    legend("top", legend = zLevelVec,
           col = selCol, lty = selLineType, border = 0, 
           lwd = LWD*2, cex = CEX/2, horiz = TRUE, bty = "n") # double the LWD, half the CEX
  }
  
  cat("Finished :-)\n")
  
}

# ============================================================================ #
#### Bar plot with 1 IV with ggplot: grand mean: ####

custom_barplot_grandmean1_gg <- function(data, xVar, yVar,
                                         xLab = NULL, yLab = NULL, selCol = NULL, yLim = NULL){
  #' Make line plot for single IV (x-axis) using ggplot package.
  #' @param data data frame, trial-by-trial data.
  #' @param xVar string, name of variable that goes on x-axis. Variable needs to be numeric.
  #' @param yVar string, name of variable that goes on y-axis. Variable needs to be numeric.
  #' @param xLab string, label for x-axis (default: retrieve appropriate name with substitute_label()).
  #' @param yLab string, label for y-axis (default: retrieve appropriate name with substitute_label()).
  #' @param selCol vector of strings (HEX colors), colors for input levels of zVar (default: retrieve via retrieve_colours()).
  #' @param yLim vector of two numbers, y-axis limits (default: NULL).
  #' @return creates plot.
  
  # -------------------------------------------------------------------------- #
  ### Check if input variables included in data set:
  
  if(!(is.data.frame((data)))){stop("data must be a data frame")}
  if(!(yVar %in% names(data))){stop("yVar not found in data")}
  if(!(xVar %in% names(data))){stop("xVar not found in data")}
  
  ## Check variable types:
  if(!(is.numeric(data[, yVar]))){stop("yVar has to be numeric")}
  if(!(is.factor(data[, xVar]))){stop("xVar has to be a factor")}
  
  ## Axes labels:
  if(is.null(xLab)){xLab <- substitute_label(xVar)}
  if(is.null(yLab)){yLab <- substitute_label(yVar)}
  if(!(is.character(xLab))){stop("xLab has to be a character string")}
  if(!(is.character(yLab))){stop("yLab has to be a character string")}
  
  ## Exclude any rows with NAs: 
  rowIdx <- which(complete.cases(data[, xVar]) & complete.cases(data[, yVar]))
  cat(paste0("Excluding rows with NA on xVar or yVar: Retain ", length(rowIdx), " out of ", nrow(data), " rows (excluded ", nrow(data) - length(rowIdx), " rows)\n"))
  plotData <- droplevels(data[rowIdx, ])
  
  # -------------------------------------------------------------------------- #
  ### Fixed plotting settings:
  
  ## Colour:
  if(is.null(selCol)){
    selCol <- retrieve_colour(xVar)
    selCol <- rep(selCol, length.out = length(unique(data[, xVar])))
  }
  if(length(selCol) != length(unique(data[, xVar]))){
    stop(paste0("Length selCol = ", length(selCol), " while number levels xVar = ", length(unique(data[, xVar])), ", do not match"))
  }
  
  LWD <- 2 # line width
  FTS <- 20 # font size
  
  # -------------------------------------------------------------------------- #
  ### Create ggplot:  
  
  ## Start ggplot:
  p <- ggplot(plotData, aes_string(x = xVar, y = yVar))
  
  ## Add bars:
  p <- p + stat_summary(fun = mean, geom = "bar", position = "dodge", width = 0.6,
                        lwd = LWD, fill = selCol, col = "black")
  
  ## Add y-axis:
  if(!(is.null(yLim))){p <- p + coord_cartesian(ylim = yLim)}
  
  ## Add labels:
  p <- p + xlab(xLab) + ylab(yLab)
  
  ## Add title:
  p <- p + ggtitle(paste0(yLab, " per \n", tolower(xLab)))
  
  ## Add theme:
  p <- p + theme_classic()
  
  ## Add font sizes:
  p <- p + theme(axis.text = element_text(size = FTS), 
                 plot.title = element_text(size = FTS, hjust = 0.5), # center title 
                 axis.title = element_text(size = FTS))
  
  ## Print and return:
  print(p)
  cat("Finished :-)\n")
  return(p)
}

# ============================================================================ #
#### Custom density plot per condition: ####

custom_densityplot <- function(data, xVar, zVar = NULL, 
                               xLim = NULL, yLim = NULL, 
                               xLab = NULL, yLab = NULL, zLab = NULL, main = NULL, 
                               selCol = NULL, selLineType = NULL){
  #' Create densityplot for density of xVar (per level of zVar) with base package.
  #' @param data data frame, trial-by-trial data.
  #' @param xVar string, name of variable that goes on x-axis. Variable needs to be numeric.
  #' @param yVar string, name of variable that goes on y-axis. Variable needs to be numeric.
  #' @param zVar string, name of variable that determines bar coloring. Variable needs to be a factor (optional).
  #' @param xLim vector of two numbers, x-axis limits (optional).
  #' @param yLim vector of two numbers, y-axis limits (optional).
  #' @param xLab string, label for x-axis (default: retrieve appropriate name with substitute_label()).
  #' @param yLab string, label for y-axis (default: retrieve appropriate name with substitute_label()).
  #' @param zLab string, label for z-axis (colour; default: retrieve appropriate name with substitute_label()).
  #' @param main string, overall plot label (optional).
  #' @param selCol vector of strings (HEX colors), colors for input levels of zVar (default: retrieve via retrieve_colours()).
  #' @param selLineType vector of numerics, line types to use (default: all 1).
  #' @return creates plot.
  #' 
  # xLim <- NULL; yLim <- NULL; xLab <- NULL; yLab <- NULL; zLab <- NULL; main <- NULL; selCol <- NULL; ltyVec <- NULL
  # xVar <- "forage_presses_n"
  # zVar <- "BDI_split_f"
  
  # -------------------------------------------------------------------------- #
  ### Check if input variables included in data set:
  
  ## Check inputs:
  if(!(is.data.frame((data)))){stop("data must be a data frame")}
  if(!(yVar %in% names(data))){stop("yVar not found in data")}
  if(!(xVar %in% names(data))){stop("xVar not found in data")}
  
  ## Check variable types:
  if(!(is.numeric(data[, yVar]))){stop("yVar has to be numeric")}
  if(!(is.numeric(data[, xVar]))){stop("xVar has to be numeric")}
  
  # -------------------------------------------------------------------------- #
  ### Create matrix with counts of xVar (at each level of zVar):
  
  if(is.null(zVar)){ # if no zVar provided
    
    cat("No zVar provided\n")
    plotMat <- t(as.matrix(table(data[, xVar])))
    nLevel <- 1
    if (is.null(selCol)){selCol <- "black"}
    
  } else { # if zVar provided
    
    if(!(zVar %in% names(data))){stop("zVar not found in data")}
    if(!(is.factor(data[, zVar]))){stop("zVar has to be a factor")}
    
    ## Count per x/y level:
    plotMat <- as.data.frame.matrix(table(data[, zVar], data[, xVar]))
    nLevel <- length(unique(data[, zVar]))
    cat(paste0("Variable zVar = ", zVar, " has ", nLevel, " levels\n"))
    if (is.null(selCol)){selCol <- retrieve_colour(zVar)}
    
  }
  
  # -------------------------------------------------------------------------- #
  ### Complete plotting settings:
  
  ## Axis limits:
  if (is.null(xLim)){xLim <- c(min(data[, xVar], na.rm = T), max(data[, xVar], na.rm = T))}
  if (is.null(yLim)){
    yMax <- max(plotMat)
    scalingFactor <- 10^floor(log10(yMax))
    yMax <- ceiling(yMax/scalingFactor) * scalingFactor
    yLim <- c(1, yMax)
  }
  if (length(xLim) != 2){stop("xLim must be of length 2")}
  if (length(yLim) != 2){stop("yLim must be of length 2")}
  
  ## Labels:
  if (is.null(xLab)){xLab <- substitute_label(xVar)}
  if (is.null(yLab)){yLab <- "Count"}
  if (is.null(main)){
    if (is.null(zVar)){
      main <- xLab
    } else {
      if (is.null(zLab)){zLab <- substitute_label(zVar)}
      main <- paste0(xLab, " per ", zLab)
    }
  }
  
  # require(RColorBrewer)
  # cmap <- "YlOrRd"
  # selCol <- brewer.pal(nLevel, cmap)
  # selCol <- c("firebrick", "indianred1")
  
  if (is.null(ltyVec)){ltyVec <- rep(1, nLevel)}
  
  ## Fixed plotting settings:
  CEX <- 1.5; LWD <- 3
  pCEX <- 1; pLWD <- 1
  
  ### Start plot:
  
  ## Create empty plot:
  plot(NA, axes = FALSE, bty = "n",
       xlim = xLim, ylim = yLim, xlab = xLab, ylab = yLab,
       main = main,
       cex.lab = CEX, cex.axis = CEX, cex.main = CEX, cex.sub = CEX)
  
  ## Add axes:
  axis(side = 1, labels = T, lwd = LWD, cex.axis = CEX)
  axis(side = 2, labels = T, lwd = LWD, cex.axis = CEX)
  
  ## Add lines and points per level of zVar:  
  xVarVec <- sort(unique(data[, xVar]))
  for (iLevel in 1:nLevel){ # iLevel <- 1
    points(xVarVec, plotMat[iLevel, ], col = selCol[iLevel], lwd = LWD, cex = CEX)
    lines(xVarVec, plotMat[iLevel, ], col = selCol[iLevel], lwd = LWD, cex = CEX)
  }
  
  ## Add legend:
  if (!is.null(zVar)){
    legend("topright", legend = sort(unique(data[, zVar])), # in alphabetical order 
           horiz = FALSE, bty = "n",
           col = selCol, lty = selLineType, lwd = 2, cex = 1.15)
  }
}

# ============================================================================ #
#### Plot correlation (scatterplot and regression line) between two variables: ####

plot_correlation <- function(data, xVar, yVar, zVar = NULL, addLegend = T,
                             isSubLabel = F, subVar = "subject_n", 
                             xLab = NULL, yLab = NULL, zLab = NULL, main = NULL, printCor = T,
                             xLim = NULL, yLim = NULL, FTS = NULL, isSave = F){
  #' Plot correlation (scatterplot and regression line) between two variables in data frame with ggplot.
  #' @param data data frame, trial-by-trial data.
  #' @param xVar string, name of variable that goes on x-axis. Variable needs to be numeric.
  #' @param yVar string, name of variable that goes on y-axis. Variable needs to be numeric.
  #' @param zVar string, name of variable used to colour-code dots; can be numeric or factor (optional).
  #' @param isSubLabel Boolean, whether to add subject IDs as labels next to dots (T) or not (F) (default: F).
  #' @param subVar string, name of subject identifier used for labeling points. Variable needs to be numeric.
  #' @param xLab string, label for x-axis (default: use value of xVar).
  #' @param yLab string, label for y-axis (default: use value of yVar).
  #' @param zLab string, label for colour axis (default: NULL; use value of zVar).
  #' @param main string, title (default: none).
  #' @param printCor Boolean, if no main provided, print correlation or not (default: TRUE).
  #' @param xLim vector of two numerics, x-axis limits (optional).
  #' @param yLim vector of two numerics, y-axis limits (optional).
  #' @param FTS numeric, font size of axes (optional).
  #' @param isSave Boolean, save as .png file (T) or not (F; default: F).
  #' @return creates (and saves) plot.
  
  # isSubLabel = F; subVar = "subject_n"; xLab = NULL; yLab = NULL; main = NULL; xLim = NULL; yLim = NULL; FTS = NULL; isSave = F
  
  # -------------------------------------------------------------------------- #
  ## Check inputs:
  
  if(!(is.data.frame((data)))){stop("data must be a data frame")}
  if(!(yVar %in% names(data))){stop("yVar not found in data")}
  if(!(xVar %in% names(data))){stop("xVar not found in data")}
  if(!(is.null(zVar))){if(!(zVar %in% names(data))){stop("zVar not found in data")}}
  if(isSubLabel){if(!(subVar %in% names(data))){stop("subVar not found in data")}}
  
  ## Check variable types:
  if(!(is.numeric(data[, yVar]))){stop("yVar has to be numeric")}
  if(!(is.numeric(data[, xVar]))){stop("xVar has to be numeric")}
  # zVar can be either factor or numeric
  
  # -------------------------------------------------------------------------- #
  ## Close any open plots:
  
  if (length(dev.list() != 0)){dev.off()}
  
  # -------------------------------------------------------------------------- #
  ### Fixed settings:
  if (is.null(FTS)){
    FTS <- 20 
  }
  LWD <- 1.3
  MKS <- 3
  subFTS <- 6 # 8
  nRound <- 3 # for print xMin/ xMax/ yMin/ yMax to console
  # margin <- c(1.0, 1.5, 1.0, 1.0, "cm") # top, right, bottom, left
  
  nData <- sum(complete.cases(data[, c(xVar, yVar)]))
  cat(paste0("Plot correlation between ", xVar, " and ", yVar, " given ", nData, " data points\n"))
  
  ## Axis labels:
  if (is.null(xLab)){xLab <- xVar}
  if (is.null(yLab)){yLab <- yVar}
  if(!(is.null(zVar))){if (is.null(zLab)){zLab <- zVar}}
  
  ## Print correlation as header:
  if (is.null(main) & printCor){
    r <- as.numeric(cor(data[xVar], data[yVar], use = "complete.obs"))
    df <- nData - 2
    t <- r/sqrt((1 - r^2) / df)
    p <- pt(abs(t), df, lower.tail = F) * 2
    main <- paste0("r(", df, ") = ", round(r, nRound), ", p = ", round(p, nRound))
    cat(paste0(main, "\n"))
  }
  
  # -------------------------------------------------------------------------- #
  ### Name for saving:
  
  if (isSave){
    plotName <- paste0("ggplot_correlation_", xVar, "_", yVar, "_", FTS, ".png")
    cat(paste0("Save plot under ", plotName, "\n"))
    png(paste0(dirs$plot, plotName),
        width = 480, height = 480) # FTS <- 17
    # tiff(paste0(dirs$plot, "ggplot_correlation_", xVar, "_", yVar, "_", FTS, ".tiff"),
    #     width = 480, height = 320) # FTS <- 17
  }
  
  # -------------------------------------------------------------------------- #
  ### Start ggplot:
  
  data$x <- data[, xVar]
  data$y <- data[, yVar]
  
  if(!(is.null(zVar))){
    data$z <- data[, zVar]
  }
  
  ## Start ggplot:
  if (isSubLabel){
    data$subject <- data[, subVar]
    p <- ggplot(data, aes(x = x, y = y, label = subject)) + 
      geom_text(hjust = 0, vjust = 0, size = subFTS) # hjust = -0.5
  } else {
    p <- ggplot(data, aes(x = x, y = y))
  }
  
  ### Add continuous colour scale for points:
  # cat("Check\n")
  if(!(is.null(zVar))){
    cat("Add colour map for points\n")
    p <- p + aes(color = z)
    if (is.numeric(data$z)){
      # p <- p + scale_colour_viridis_c() # continuous scale
      p <- p + scale_colour_viridis_c(option = "inferno") # viridis, plasma, inferno
    } else {
      p <- p + scale_colour_viridis_d() # numeric scale
    }
    p <- p + labs(color = zLab) # adjust name of colour legend
    # p <- p + guides(color = guide_legend(title = zLab)) + theme(legend.margin = margin(-10, -10, -10, -10)) # reduce margin around colour legend
    # p <- p + theme(legend.box.margin = margin(-20, -10, -10, -10)) # reduce margin around colour legend
    # p <- p + theme(legend.spacing.x = unit(-2, "cm"))
  }
  
  ### Points for scatter plot:
  p <- p + geom_point(shape = 19, size = MKS, stroke = 0.8) # , fill = "white")
  
  ### Regression line:
  p <- p + geom_smooth(method = lm, se = T, col = "red", linewidth = 2) # regression line
  
  ### Horizontal and vertical lines:
  p <- p + geom_vline(xintercept = 0, linetype = 2, color = "black", size = 1) # Middle line at 0
  p <- p + geom_hline(yintercept = 0, linetype = 2, color = "black", size = 1) # Middle line at 0
  
  ### Labels:
  p <- p + xlab(xLab) + ylab(yLab) # Labels
  
  ### Title:
  if (!(is.null(main))){
    p <- p + ggtitle(main)
  }
  
  ### Add theme:
  p <- p + theme_classic() + # theme
    theme(plot.margin = unit(c(0.5, 1.0, 0.5, 0.5), "cm"), # top, right, bottom, left
          axis.text = element_text(size = FTS, color = "black"),
          axis.title = element_text(size = FTS, color = "black"), 
          plot.title = element_text(size = FTS, color = "black", hjust = 0.5), # center title 
          legend.text = element_text(size = FTS, color = "black"),
          legend.title = element_text(size = FTS, color = "black"),
          axis.line = element_line(colour = 'black', linewidth = LWD))
  
  ## Remove legend:
  if (addLegend == F){
    cat("Remove colour legend\n")
    p <- p + theme(legend.position = "none")
  }
  
  # -------------------------------------------------------------------------- #
  ### ADd axis limits:
  
  ## Retrieve:
  xMin <- min(data[, xVar], na.rm = T)
  xMax <- max(data[, xVar], na.rm = T)
  yMin <- min(data[, yVar], na.rm = T)
  yMax <- max(data[, yVar], na.rm = T)
  cat(paste0("X ranges from ", round(xMin, nRound), " to ", round(xMax, nRound), "\n"))
  cat(paste0("Y ranges from ", round(yMin, nRound), " to ", round(yMax, nRound), "\n"))
  
  ## Erode:
  # if(xMin > 0){xMin <- xMin * 0.90} else (xMin <- xMin * 1.10)
  # if(xMax > 0){xMax <- xMax * 1.10} else (xMax <- xMax * 0.90)
  # if(yMin > 0){yMin <- yMin * 0.90} else (yMin <- yMin * 1.10)
  # if(yMax > 0){yMax <- yMax * 1.10} else (yMax <- yMax * 0.90)
  
  ## Set limits:
  if(is.null(xLim)){xLim <- c(xMin, xMax); cat("Set xLim automatically\n")} # define x-axis limits
  if(is.null(yLim)){yLim <- c(yMin, yMax); cat("Set yLim automatically\n")} # define x-axis limits
  if (length(xLim) != 2){stop("xLim must be of length 2")}
  if (length(yLim) != 2){stop("yLim must be of length 2")}
  
  ## Add axis limits:
  p <- p + coord_cartesian(xlim = xLim, ylim = yLim)
  
  # if (!is.null(margin)){p <- p + theme(plot.margin = unit(margin, "cm"))}
  
  # p <- p + theme(legend.box.margin = margin(-20, -10, -10, -10))
  # p <- p + theme(legend.box.margin = margin(-1, -1, -1, -1))
  
  # -------------------------------------------------------------------------- #
  ### Close and save:
  
  if (isSave){
    print(p)
    dev.off()
  }
  print(p)
  return(p)
  cat("Finished :-)\n")
}

# ============================================================================ #
#### Permutation p-value for correlation: ####

permute_correlation <- function(data, xVar, yVar, nIter = 10000,
                                isSave = F){
  #' Plot correlation and permutation distribution of correlation as histogram.
  #' @param data data frame, trial-by-trial data.
  #' @param xVar string, name of first variable. Variable needs to be numeric.
  #' @param yVar string, name of second variable. Variable needs to be numeric.
  #' @param nIter integer, number of permutation (default: 10000).
  #' @param isSave Boolean, save as .png file (T) or not (F; default: F).
  #' @return creates (and saves) plot.
  
  cat(paste0("Permute correlation between ", xVar, " and ", yVar, " with ", nIter, " iterations...\n"))
  
  # -------------------------------------------------------------------------- #
  ### Check inputs:
  
  if(!(is.data.frame((data)))){stop("data must be a data frame")}
  if(!(yVar %in% names(data))){stop("yVar not found in data")}
  if(!(xVar %in% names(data))){stop("xVar not found in data")}
  
  if(!(is.numeric(data[, yVar]))){stop("yVar has to be numeric")}
  if(!(is.numeric(data[, xVar]))){stop("xVar has to be numeric")}
  
  # -------------------------------------------------------------------------- #
  ### Close any open plots:
  
  if (length(dev.list() != 0)){dev.off()}
  
  # -------------------------------------------------------------------------- #
  ### Fixed settings:
  
  nRound <- 3
  LWD <- 3 # axes of plot
  CEX <- 1.5 # axes ticks and labels
  FTS <- 20
  
  # -------------------------------------------------------------------------- #
  ### Extract variables:
  
  x <- data[, xVar]
  y <- data[, yVar]
  nSub <- nrow(data)
  
  # -------------------------------------------------------------------------- #
  ### True correlation:
  
  corVal <- cor(x, y)
  
  # -------------------------------------------------------------------------- #
  ### Permutation distribution:
  
  corVec <- rep(NA, nIter) # initialize
  
  for (iIter in 1:nIter){ # loop 
    xBoot <- sample(x, nSub, replace = F) # permute
    yBoot <- sample(x, nSub, replace = F) # permute
    corVec[iIter] <- cor(xBoot, yBoot)
  }
  
  ## Compute p-value:
  pVal <- mean(abs(corVec) > abs(corVal))
  cat(paste0("Two-sided permutation correlation between ", xVar, " and ", yVar, 
             ":\n r = ", corVal, ", p = ", pVal, "\n"))
  
  # -------------------------------------------------------------------------- #
  ### Save:
  
  if (isSave){
    plotName <- paste0("correlation_permutation_", xVar, "_", yVar, "_nIter", nIter, ".png")
    cat(paste0("Save plot under ", plotName, "\n"))
    png(paste0(dirs$plot, plotName), width = 480, height = 480)
  }
  
  # -------------------------------------------------------------------------- #
  ### Plot:
  
  hist(corVec, breaks = seq(-1, 1, 0.01),
       axes = FALSE,
       xlab = "Permutation distribution or r", ylab = "Density",
       main = paste0("Correlation ", xVar, " and ", yVar, 
                     ":\n r = ", round(corVal, nRound), ", p = ", pVal, "\n"),
       cex.lab = CEX, cex.axis = CEX, cex.main = CEX, cex.sub = CEX)
  
  axis(side = 1, labels = T, lwd = LWD, cex.axis = CEX)
  axis(side = 2, labels = T, lwd = LWD, cex.axis = CEX)
  
  abline(v = corVal, lwd = 5, col = "red")
  
  # -------------------------------------------------------------------------- #
  ### Close saving:
  
  if (isSave){
    dev.off()
  }
  
  # -------------------------------------------------------------------------- #
  ### Return p-value:
  
  return(pVal)
  
}

# ============================================================================ #
#### Compute regression coefficient adjusted for other variables: ####

compute_coef_adjusted <- function(rowData, colData, adjustData, adjustVarVec){
  #' Compute standardized linear regression coefficient (like correlation) between
  #' each variable of rowData and each variable of colData, adjusting for variables
  #' in adjustVarVec contained in adjustData.
  #' Note that all variables in all data sets must be numeric.
  #' @param rowData data frame, variables that will be rows of coefficient matrix.
  #' @param colData data frame, variables that will be columns of coefficient matrix.
  #' @param adjustData data frame, contains variables to be adjusted for.
  #' @param adjustVarVec vector of strings, names of variables to be adjusted for. 
  #' @return M matrix with coefficients (like correlations) of each variable of rowData and each variable of colData adjusted for adjustVarVec.
  
  # -------------------------------------------------------------------------- #
  ### Check inputs:
  
  if(!(is.data.frame((rowData)))){stop("rowData must be a data frame")}
  if(!(is.data.frame((colData)))){stop("colData must be a data frame")}
  if(!(is.data.frame((adjustData)))){stop("adjustData must be a data frame")}
  
  ## Check that same number rows:
  if(nrow(rowData) != nrow(colData)){stop("rowData and colData must have the same number of rows")}
  if(nrow(colData) != nrow(adjustData)){stop("colData and adjustData must have the same number of rows")}
  
  ## Check that all variables are numeric:
  for (iCol in ncol(rowData)){
    if(!(is.numeric(rowData[, iCol]))){stop(names(rowData)[iCol], " in rowData has to be numeric")}
  }
  for (iCol in ncol(colData)){
    if(!(is.numeric(colData[, iCol]))){stop(names(colData)[iCol], " in colData has to be numeric")}
  }
  for (iCol in ncol(adjustData)){
    if(!(is.numeric(adjustData[, iCol]))){stop(names(adjustData)[iCol], " in adjustData has to be numeric")}
  }
  
  ## Check that adjustVars included:
  for (iVar in length(adjustVarVec)){
    if(!(adjustVarVec[iVar] %in% names(adjustData))){stop(adjustVarVec[iVar], " not found in adjustData")}
  }
  
  ## Remove rows with NAs only later after vertical concatenation
  
  cat(paste0("Compute regression coefficients adjusted for ", paste0(adjustVarVec, collapse = ", "), "\n"))
  
  # -------------------------------------------------------------------------- #
  ### Pre-process variable names:
  
  ## Remove dots, replace dashes:
  colnames(rowData) <- gsub("\\.", "", colnames(rowData)) # remove dots
  colnames(rowData) <- gsub("-", "_", colnames(rowData)) # replace "-" by "_"
  colnames(colData) <- gsub("\\.", "", colnames(colData)) # remove dots
  colnames(colData) <- gsub("-", "_", colnames(colData)) # replace "-" by "_"
  colnames(adjustData) <- gsub("\\.", "", colnames(adjustData)) # remove dots
  colnames(adjustData) <- gsub("-", "_", colnames(adjustData)) # replace "-" by "_"
  adjustVarVec <- gsub("\\.", "", adjustVarVec) # remove dots
  adjustVarVec <- gsub("-", "_", adjustVarVec) # replace "-" by "_"
  
  # -------------------------------------------------------------------------- #
  ### Pre-process data:
  
  ## Combine data:
  cat("Combine all data sets\n")
  totalData <- cbind(rowData, colData, adjustData)
  
  ## Z-standardize data (so regression coefficients are standardized betas, like correlations):
  ## Also drop rows with NAs:
  cat("Z-standardize all columns\n")
  totalData <- as.data.frame(scale(totalData))
  # totalData <- as.data.frame(scale(totalData[complete.cases(totalData), ]))
  # cat(paste0("Original data had ", nrow(rowData), " rows; maintain ", nrow(totalData), " rows after excluding NAs\n"))
  
  ## Initialize matrix:
  M <- matrix(NA, ncol(rowData), ncol(colData))
  rownames(M) <- names(rowData)
  colnames(M) <- names(colData)
  cat(paste0("Compute matrix with ", ncol(rowData), " rows and ", ncol(colData), " columns\n"))
  
  # -------------------------------------------------------------------------- #
  ### Loop over variables, compute linear regression adjusted for adjustVarVec:
  
  for (iRow in 1:ncol(rowData)){ # iRow <- 1
    for (iCol in 1:ncol(colData)){ # iCol <- 1
      
      ## Create formula:
      formula <- paste0(names(rowData)[iRow], " ~ ", names(colData)[iCol], " + ", paste0(adjustVarVec, collapse = " + "))
      cat(paste0("Fit a linear regression model with formula ", formula, "\n"))
      
      ## Fit model:
      mod <- lm(formula = formula, data = totalData)
      # print(summary(mod))
      M[iRow, iCol] <- as.numeric(coef(mod)[2]) # extract 2nd coefficient (1st predictor, omit intercept) and save in matrix
      
    }
  }
  
  cat("Finished :-)\n")  
  return(M)
  
}

# ============================================================================ #
#### Plot per-subject effect separately per schedule: ####

custom_gg_effect_per_schedule <- function(mod, iCoef, cmap = NULL){
  #' Plots per-subject effect in lme4 model per subject colored per schedule.
  #' Assumes that scheduleVec and formula are global variables.
  #' @param mod model object fitted with ggplot.
  #' @param iCoef scalar integer, name of effect to plot. 
  #' @return creates and return plot made with ggplot.
  
  # -------------------------------------------------------------------------- #
  ### Check inputs:
  
  ## Check model and effects:
  validClassVec <- c("glmerMod", "lmerMod", "lmerTest", "lmerModLmerTest")
  if(!(class(mod) %in% validClassVec)){stop("mod needs to have one of a the following classes: ", paste0(validClassVec, collapse = ", "))}
  
  if(iCoef > ncol(coef(mod)[[1]])){stop("iCoef too high; coef has only ", ncol(coef(mod)[[1]]), " coefficients")}
  
  # -------------------------------------------------------------------------- #
  ### Close any open plots:
  
  if (length(dev.list() != 0)){dev.off()}
  
  # -------------------------------------------------------------------------- #
  ### Fixed settings:
  
  FTS <- 20
  nSchedule <- 10
  
  if(is.null(cmap)){
    # cmap <- jet.colors(nSchedule) # from jet
    cmap <- turbo(nSchedule) # from viridis
  } else {
    if (length(cmap) != nSchedule){error("Length of cmap does not match number of schedules")}
  }
  
  # -------------------------------------------------------------------------- #
  ### Extract coefficients and schedules:
  
  coefMat <- as.data.frame(coef(mod)[[1]]) # extract coefficients per subject
  coefMat$subject <- 1:nSub # add subjectID at the end
  coefMat$schedule <- factor(scheduleVec) # add schedule at the end
  
  # -------------------------------------------------------------------------- #
  ## Select coefficient of interest:
  
  coefMat$selEffect <- coefMat[, iCoef]
  yMax <- ceiling(max(abs(coefMat$selEffect))*10)/10
  
  # -------------------------------------------------------------------------- #
  ## Mean effect per schedule:
  
  mean4Sched <- data.frame(sort(unique(coefMat$schedule)),
                           tapply(coefMat$selEffect, coefMat$schedule, mean))
  names(mean4Sched) <- c("schedule", "mean")
  
  # -------------------------------------------------------------------------- #
  ## R base Plot:
  # LWD <- 5 # axes of plot
  # CEX <- 1.5 # axes ticks and labels
  # plot(NULL, axes = F,
  #      xlim = c(0, nSub), ylim = c(-1*yMax, yMax),
  #      cex.lab = CEX, cex.axis = CEX, cex.main = CEX,
  #      xlab = "Subject ID", ylab = paste0("Effect of ", names(coefMat)[iCoef], " per subject"),
  #      main = paste0(formula, ":\nEffect of ", names(coefMat)[iCoef], " per subject"))
  # axis(side = 1, lwd = LWD, cex.axis = CEX) #at = seq(0, 35, 5), line = 0)
  # axis(side = 2, lwd = LWD, cex.axis = CEX) # at = seq(0, 12000, 2000))
  # points(coefMat$subject, coefMat$selEffect, col = coefMat$schedule, lwd = 3)
  # abline(h = 0, lty = 2, lwd = 2)
  # legend("top", legend = unique(scheduleVec), col = c(cmap), horiz = T,
  #        lwd = LWD, cex = CEX, bg = NULL, bty = "n")
  
  # -------------------------------------------------------------------------- #
  ## Plot with ggplot:
  
  p <- ggplot(coefMat, aes(x = subject, y = selEffect, col = schedule)) +
    geom_point(size = 5) + 
    geom_hline(yintercept = 0, linetype = "dashed", colour = "black", lwd = 1) +  # dashed line at zero
    geom_hline(data = mean4Sched, aes(yintercept = mean, col = schedule), linetype = "dashed", lwd = 2) + 
    scale_color_manual(values = cmap) + # color bar
    coord_cartesian(ylim = c(-1 * yMax, 1 * yMax)) +  # symmetric y-axis
    labs(x = "Subject ID", y = paste0("Effect of ", names(coefMat)[iCoef], " per subject")) + 
    ggtitle(paste0(paste(strwrap(gsub("_", " ", formula2handle(formula)), width = 40), collapse = "\n"), 
                   ":\nEffect of ", names(coefMat)[iCoef], " per subject")) +
    theme_classic() + # themetheme(axis.text = element_text(size = FTS),
    theme(axis.text = element_text(size = FTS),
          axis.title = element_text(size = FTS),
          plot.title = element_text(size = FTS, hjust = 0.5), # center title
          legend.text = element_text(size = FTS), legend.title = element_text(size = FTS))
  
  print(p)
  return(p)
}

# ============================================================================ #
#### Make a pretty plot in base R: ####

custom_baseR_prettyplot <- function(x, y, type = "b",
                                    xLab = "X", yLab = "Y", cmap = NULL, varNames = NULL){
  #' Plots x/y plot in R using base package, but pretty.
  #' @param x vector of numerics, data on x-axis.
  #' @param y vector of numerics, data on y-axis.
  #' @param type scalar string, type of plot, either "p" (points), "l" (line), "b" (both points and line; default).
  #' @param xLab scalar string, x-axis label (default: "X").
  #' @param yLab scalar string, y-axis label (default: "Y").
  #' @param colMap scalar string, colour map to colour dots as a function of x-axis value, optional.
  #' @param varNames vector of strings,
  #' @return creates and return plot made with base package.
  
  if(!(is.numeric(y))){stop("y has to be numeric")}
  if(!(is.numeric(x))){stop("x has to be numeric")}
  
  # -------------------------------------------------------------------------- #
  ### Plot settings:
  
  ## Transpose y if row vector:
  if (nrow(y) == 1 & ncol(y) > 1){
    cat("y seems to be a row vector, transpose to column vector\n")
    y <- t(y)
  }
  
  ## Plot dimensions:
  xLim <- c(min(x), max(x))
  nOrdMag <- 1 # mean(y)
  yMax <- ceiling(nOrdMag * max(abs(y)))/nOrdMag
  yLim <- c(min(x), max(x))
  
  ## Main title:
  if (is.null(mainTitle)){
    mainTitle <- paste0(yLab, " per \n", xLab)
  }
  
  ## Color map:
  if (is.null(colMap)){
    colMap <- rep("blue", ncol(y))
  }
  if (length(colMap) != ncol(y)){error("Length of colMap does not match # columns of y")}
  
  ## Variable names for legend:  
  if (is.null(varNames) & ncol(y) > 1){
    varNames <- as.character(1:ncol(y))
  }
  if (length(varNames) != ncol(y)){error("Length of varNames does not match # columns of y")}
  
  # -------------------------------------------------------------------------- #
  ### Make empty plot:
  
  plot(NULL, type = "b", axes = F, bty = "n",
       cex.lab = CEX, cex.axis = CEX, cex.main = CEX,
       xlim = xLim,
       ylim = yLim,
       xlab = xLab, ylab = yLab,
       main = mainTitle)
  
  ## Add axes:
  axis(side = 1, lwd = LWD, cex.axis = CEX) #at = seq(0, 35, 5), line = 0)
  axis(side = 2, lwd = LWD, cex.axis = CEX) # at = seq(0, 12000, 2000))
  
  # -------------------------------------------------------------------------- #
  ### Fill plot:
  
  for (iCol in 1:ncol(y)){
    
    ## Add points:
    if (type %in% c("b", "p")){
      points(x, y[, iCol], lwd = LWD, col = colMap[iCol])
    }
    
    ## Add lines:
    if (type %in% c("b", "p")){
      lines(x, y[, iCol], lwd = LWD, col = colMap[iCol])
    }
    
  }
  
  ## Add legend:
  if (ncol(y) > 1){
    legend("top", legend = varNames, col = c(colMap), horiz = T,
           lwd = LWD, cex = CEX, bg = NULL, bty = "n")
  }
  
  cat("Finished :-)\n")
  
}


# ============================================================================ #
#### Make vertical plots of factor scores given data: ####

custom_ggplot_factorScores <- function(input, nFactor = NULL, rotation = "promax", 
                                       dataName = "Dataset", highlightVal = 0.4, highlightCol = "orange"){
  #' Perform factor analysis (either promax or oblimin rotation) given number of desired factors,
  #' plot loadings of each item on each factor.
  #' @param input either data frame with variables in columns or result from factor analysis.
  #' @param nFactor scalar integer, number of factors to extract, default: determined by fa.parallel with scree plot
  #' @param rotation scalar string, type of rotation to use in factor analysis, either "promax" (default) or "varimax" or "oblimin".
  #' @param dataName scalar string, name of data set to be used in title of plot. 
  #' @param highlightVal scalar numeric, name above which to highlight factor loading with color (default: 0.4).
  #' @param highlightCol scalar string, color to use to highlight loadings above cutoff (default: "orange").
  #' @return creates and return plot made with ggplot.
  
  require(psych)
  require(data.table)
  require(ggplot2)
  
  # -------------------------------------------------------------------------- #
  ### Fixed settings for plotting:
  
  FTS <- 12 # default
  xFTS <- 8 # y-axis (item names)
  yFTS <- FTS # x-axis (factor loadings)
  if (is.list(input) && nrow(input$loadings) > 50){yFTS <- 10}
  if (is.list(input) && nrow(input$loadings) > 100){yFTS <- 5}
  if (is.data.frame(input) && ncol(input) > 20){yFTS <- 10}
  if (is.data.frame(input) && ncol(input) > 50){yFTS <- 5}
  if (is.data.frame(input) && ncol(input) > 100){yFTS <- 5}
  if (exists("inputs$loadings")){if(nrow(input$loadings) > 20){yFTS <- 10}}
  
  # -------------------------------------------------------------------------- #
  ### Run factor analysis or just extract loadings:
  
  if (is.data.frame(input)){ # if data frame as input
    
    # ------------------------------------------------------------------------ #
    ### Determine number of factors: run parallel test
    
    if (is.null(nFactor)){ # if not # factors provided as input: determine # factors using parallel analysis
      ## Detect number of desired factors:
      mod <- psych::fa.parallel(input) # suggests number of factors and components
      nFactor <- mod$nfact # retrieve from mod object fitted with psych
      cat(paste("No nFactor provided as input, has no determined to use ", nFactor, " factors using scree plots\n"))
    }
    if (nFactor > 5){xFTS <- 10}
    
    cat(paste0("Compute factor solution of ", nFactor, " factors with ", rotation, " rotation and visualize:\n"))
    
    # ------------------------------------------------------------------------ #
    ### Perform factor analysis:
    
    if (rotation == "promax"){
      facts <- stats::factanal(input, nFactor, rotation = "promax")
      # facts <- psych::fa(input, nFactor, rotate = "promax") #, fm = "ml", rotate = "none" or "promax"
    } else if (rotation == "varimax"){
      facts <- stats::factanal(input, nFactor, rotation = "varimax")
    } else if (rotation == "oblimin"){
      facts <- psych::fa(input, nFactor, rotate = "oblimin") #, fm = "ml", rotate = "none" or "promax"
    } else {
      stop("Unknown rotation type")
    }
    
    # ------------------------------------------------------------------------ #
    ### Extract factor scores:
    
    loadings_wide <- data.frame(unclass(facts$loadings)) # extract factors loadings into data frame
    loadings_wide <- data.table(loadings_wide, keep.rownames = TRUE) # add row names
    loadings_wide$rn <- factor(loadings_wide$rn, levels = colnames(input)) # correct for original colum names in data frame
    loadings_long <- melt.data.table(loadings_wide, id.vars = "rn", variable.name = "factors", value.name = "loadings") # into long format with factors stacked
    
    
  } else if (typeof(input) == "list") { # if factor input provided
    
    loadings_wide <- data.frame(unclass(input$loadings))
    cat(paste0("Visualize factor solution with ", ncol(loadings_wide), " factors:\n"))
    loadings_wide <- data.table(loadings_wide, keep.rownames = TRUE) # add row names
    loadings_wide$rn <- factor(loadings_wide$rn, levels = loadings_wide$rn) # correct for original colum names in data frame
    loadings_long <- melt.data.table(loadings_wide, id.vars = "rn", variable.name = "factors", value.name = "loadings") # into long format with factors stacked
    if("call" %in% names(input)){rotation <- sub('.*rotation\\s*=\\s*"([^"]*)".*', '\\1', deparse(input$call))}
    if("Call" %in% names(input)){rotation <- sub('.*rotate\\s*=\\s*"([^"]*)".*', '\\1', deparse(input$Call))}
    if (length(rotation) == 0){rotation <- "given"}
    
  } else {
    stop(paste0("Unknown class of input object: ", typeof(input), "\n"))
  }
  
  # -------------------------------------------------------------------------- #
  ### Evaluate which inputs to exclude based on low loadings/ cross-loadings:
  
  excludeVec <- c() # initialize
  for (iRow in 1:nrow(loadings_wide)){ # iRow <- 1
    thresh <- 0.4
    rowName <- as.character(loadings_wide$rn[iRow])
    if(all(abs(loadings_wide[iRow, 2:ncol(loadings_wide)]) < thresh)){
      cat(paste0(rowName, ": All absolute loadings < ", thresh, "\n"))
      excludeVec <- c(excludeVec, rowName)
    }
    if(sum(abs(loadings_wide[iRow, 2:ncol(loadings_wide)]) > thresh) > 1){
      cat(paste0(rowName, ": Multiple loadings > ", thresh, "\n"))
      excludeVec <- c(excludeVec, rowName)
    }
  }
  cat(paste0("Recommend to exclude the following inputs: ", paste0(excludeVec, collapse = ", "), "\n"))
  
  # -------------------------------------------------------------------------- #
  ### Plot as horizontal bars (factors as columns), fill for loadings > highlightVal:
  
  p <- ggplot(loadings_long, aes(rn, (loadings), fill = loadings)) +
    geom_bar(stat = "identity", aes(fill = abs(loadings) > highlightVal)) + # bars, fill high loadings
    facet_wrap(~ factors, nrow = 1) + # split by factors
    scale_fill_manual("legend", values = c("TRUE" = highlightCol, "FALSE" = "grey")) + # select colors
    coord_flip() + # vertical factor columns so questionnaire labels readable
    # scale_x_reverse() +
    scale_x_discrete(limits = rev) + 
    xlab("Items") +
    ylab("Loading strength") + 
    ggtitle(paste0(dataName, ":\n Factor analysis with ", rotation, " rotation")) + 
    theme_minimal(base_size = FTS) +
    theme(legend.position = "none", 
          axis.text.x = element_text(size = xFTS), 
          axis.text.y = element_text(size = yFTS), 
          text = element_text(size = FTS),
          strip.text = element_text(size = FTS, face = "bold"))
  
  print(p)
  return(p)
  
}

# ============================================================================ #
#### Select patches with certain behaviour on first x trials: ####

select_data_given_past <- function(data, selVar = "outcome_n", selVec){
  #' Select sub-data set based on past values of given variable.
  #' @param data data frame, trial-by-trial data.
  #' @param selVar string, name of variable by whose past values to select data (default: "outcome_n").
  #' @param selVec numerical vector, past variable values to use to select data subset.
  #' @return data frame with subset of trials.
  
  # -------------------------------------------------------------------------- #
  ### Check inputs:
  
  if(!(is.data.frame((data)))){stop("data must be a data frame")}
  
  # -------------------------------------------------------------------------- #
  ### Fixed settings:
  
  subVar <- "subject_n"
  patchVar <- "subject_round_patch_n"
  trialVar <- "trialnr_patch_n"
  IDvar <- "subject_round_patch_n"
  
  cat(paste0("Select data based on ", selVar, " = [", paste0(selVec, collapse = ", "), "]\n"))
  
  # -------------------------------------------------------------------------- #
  ### Create unique identifier for each patch for each subject:
  
  data[, IDvar] <- data[, subVar] * 1000 + data[, patchVar]
  
  # -------------------------------------------------------------------------- #
  ### Select data:
  # selVec <- c(0, 0)
  nSel <- length(selVec)
  
  ## Select patches based on selVar values on first nSel trials:
  selData <- data # copy over
  for (iSel in 1:nSel){ # iSel <- 1
    selTrialIdx <- which(selData[, trialVar] == iSel & selData[, selVar] == selVec[iSel]) # trials that fulfill criterion
    selpatchVec <- unique(selData[selTrialIdx, IDvar]) # patch IDs of these trials
    selpatchIdx <- which(selData[, IDvar] %in% selpatchVec) # row indices of entire patches
    selData <- selData[selpatchIdx, ] # subselect data
  }
  
  ## Select only directly following trial: 
  cat(paste0("Past values are [", paste0(selVec, collapse = ", "), "], select trials at position ", nSel + 1, "\n"))
  selTrialIdx <- which(selData[, trialVar] == nSel + 1) # only directly following trial
  selData <- selData[selTrialIdx, ] # subselect data
  
  ## Print how many trials left:
  nPatch <- length(unique(selData[, IDvar]))
  nSub <- length(unique(selData[, subVar]))
  cat(paste0("Identified ", nPatch, " patches from ", nSub, " subjects (", round(nPatch/nSub, 2), " per subject)\n"))
  
  ## Inspect:
  # table(selData[, trialVar])
  # selData[1:20, c(trialVar, selVar)]
  
  return(selData)
  
}

# ============================================================================ #
#### Plot effect for selected patches with certain behaviour on first X trials: ####

plot_effect_given_past <- function(data, yVar = "choice_n", xVar = "BRR_f", selVar = "outcome_n", selVec, 
                                   xLabel = NULL, yLabel = NULL, selLabel = NULL,
                                   selCol = colours$bluered){
  #' Plot effect of X on Ysub-data set selected based on past values of given variable.
  #' @param data data frame, trial-by-trial data.
  #' @param yVar string, name of variable for which to compute effect (default: "choice_n").
  #' @param xVar string, name of variable whose effect to compute (default: "BRR_f").
  #' @param selVar string, name of variable by whose past values to select data (default: "outcome_n").
  #' @param selVec numerical vector, past variable values to use to select data subset.
  #' @param xLabel string, label for x-axis (optional).
  #' @param yLabel string, label for y-axis (optional).
  #' @param selLabel string, label of variable used for selecting data (optional).
  #' @param selCol vector of strings, HEX codes of colors to use for lines (default: redblue).
  #' @return creates bar plot.
  
  # -------------------------------------------------------------------------- #
  ### Check inputs:
  
  if(!(is.data.frame((data)))){stop("data must be a data frame")}
  
  # -------------------------------------------------------------------------- #
  ### Fixed plotting settings:
  
  subVar <- "subject_n"
  IDvar <- "subject_round_patch_n"
  
  LWD <- 1.5
  FTS <- 24
  yLim <- c(0, 1)
  
  ## Check labels: 
  if (is.null(xLabel)){xLabel <- xVar}
  if (is.null(yLabel)){yLabel <- yVar}
  if (is.null(selLabel)){selLabel <- selVar}
  
  cat(paste0("Plot effect of ", xVar, " on ", yVar, " given that past values of ", selVar, " were [", paste0(selVec, collapse = ", "), "]\n"))
  
  # -------------------------------------------------------------------------- #
  ### Select data given vector of past trials:
  
  selData <- select_data_given_past(data, selVar = "outcome_n", selVec = selVec)
  nPatch <- length(unique(selData[, IDvar]))
  
  # -------------------------------------------------------------------------- #
  ### Compute p(leave) per environment per subject:
  
  selData$subject <- selData[, subVar]
  selData$x <- selData[, xVar]
  selData$y <- selData[, yVar]
  
  # cat(paste0("Effect of ", xVar, " on ", yVar, " on trial level:\n"))
  # cat(round(tapply(selData$y, selData$x, mean, na.rm = T), 2))
  # cat("\n")
  
  ## Aggregate per subject per condition:
  subCondData <- ddply(selData, .(subject, x), function(x){
    y <- mean(x$y)
    return(data.frame(y))
    dev.off()})
  # cat(paste0("Effect of ", xVar, " on ", yVar, " aggregated per subject:\n"))
  # cat(round(tapply(subCondData$y, subCondData$x, mean, na.rm = T), 2))
  # cat("\n")
  
  ## Add numeric x-axis positions and jitter:
  subCondData$xpos <- as.numeric(subCondData$x)
  subCondData$j <- jitter(rep(0, nrow(subCondData)), amount = .05) # pure jitter .05
  subCondData$xj <- subCondData$xpos + subCondData$j # add jitter to xpos
  
  # -------------------------------------------------------------------------- #
  ### Overall mean per condition:
  
  summary_d <- summarySEwithin(subCondData, measurevar = "y", withinvar = "x", idvar = "subject", na.rm = T)
  condVec <- unique(summary_d$x)
  nCond <- length(condVec)
  
  # -------------------------------------------------------------------------- #
  # -------------------------------------------------------------------------- #
  ### Plot bar plot:
  
  ## Start ggplot: 
  p <- ggplot(data = summary_d, aes(y = y)) # initialize
  
  # -------------------------------------------------------------------------- #
  ## Bars:
  
  p <- p + geom_bar(data = summary_d, aes(x = x, y = y), stat = "identity",
                    fill = selCol, col = "black", width = 0.4, lwd = LWD, alpha = 0.3)
  p <- p + geom_errorbar(data = summary_d, 
                         aes(x = x, y = y, ymin = y - ci, ymax = y + ci),
                         color = "black", width = 0.10, lwd = LWD, alpha = 0.6) 
  
  
  # -------------------------------------------------------------------------- #
  ## Individual data points:
  
  for(iCond in 1:nCond){
    ## Add points:
    p <- p + geom_point(data = subset(subCondData, x == condVec[iCond]), aes(x = xj, y = y), color = selCol[iCond], size = 1.5, 
                        alpha = .35) # alpha = .50
  }
  
  ## Add lines to combine points:
  p <- p + geom_line(data = subCondData, aes(x = xj, y = y, group = subject), 
                     size = 1.0, color = 'grey40', alpha = 0.35) # lightgray
  
  
  # -------------------------------------------------------------------------- #
  cat("Adjust axes, labels\n")
  
  # Y-axis:
  p <- p + coord_cartesian(ylim = yLim) 
  
  # Labels:
  p <- p + scale_x_discrete(labels = condVec) +
    xlab(xLabel) + ylab(yLabel) + theme_classic()
  
  # Other settings:
  main <- paste0(yLabel, " per ", xLabel, " given \npast vector of ", selLabel, " = ", paste(selVec, collapse = ", "), "\n (", nPatch, " trials)")
  if (!(is.null(main))){
    cat("Add title\n")
    p <- p + ggtitle(main) # title off for printing for poster
  }  
  
  # p + theme_classic()
  
  ## Font sizes:
  p <- p + theme(axis.text = element_text(size = FTS),
                 axis.title = element_text(size = FTS), 
                 plot.title = element_text(size = FTS, hjust = 0.5), # center title 
                 legend.text = element_text(size = FTS))
  
  # Print plot in the end:
  print(p)
  cat("Finished :-)\n")
  return(p)
  
}

# ============================================================================ #
#### Plot effect per subject: ####

effect_per_subject <- function(data, yVar = "choice_n", xVar, correctByFRR = FALSE){
  #' Aggregate trial-by-trial data per condition per subject, compute effect of
  #' xVar on yVar.
  #' @param data data frame, trial-by-trial data.
  #' @param yVar string, name of variable for which to compute effect (default: "choice_n").
  #' @param xVar string, name of variable whose effect to compute.
  #' @param correctByFRR boolean, aggregate separately per FRR level, then average (default: FALSE).
  #' @return data frame with variables "subject" (subject ID) and "effect" (effect of X on Y). 
  
  require(plyr)
  
  # -------------------------------------------------------------------------- #
  ### Check inputs:
  
  if(!(is.data.frame((data)))){stop("data must be a data frame")}
  if(!(yVar %in% names(data))){stop("yVar not found in data")}
  if(!(xVar %in% names(data))){stop("xVar not found in data")}
  
  ## Check variable types:
  if(!(is.numeric(data[, yVar]))){stop("yVar has to be numeric")}
  if(!(is.numeric(data[, xVar]))){stop("xVar has to be numeric")}
  
  # -------------------------------------------------------------------------- #
  ### Fixed settings:
  
  FRRvar <- "FRR_f"
  subVar <- "subject_f"
  if(!(FRRvar %in% names(data))){stop("FRRvar not found in data")}
  if(!(subVar %in% names(data))){stop("subVar not found in data")}
  
  cat(paste0("Compute effect of ", xVar, " on ", yVar, " per subject\n"))
  
  # -------------------------------------------------------------------------- #
  ### Enforce default variable structure:
  
  data$subject <- data[, subVar]
  data$x <- data[, xVar]
  data$z <- data[, FRRvar]
  
  # -------------------------------------------------------------------------- #
  ### Aggregate into long format:
  
  if (correctByFRR){
    ### Alternative A: Aggregate by selected variable and FRR:
    cat(paste0("Correct for FRR\n"))
    aggrData_long <- ddply(data, .(subject, x, z), function(x){
      y <- mean(x[, yVar], na.rm = T)
      return(data.frame(y))
      dev.off()})
    aggrData_long$condition_f <- paste0("x", aggrData_long$x, "_", "FRR", aggrData_long$z) # only FRR
  } else {
    ### Alternative B: Aggregate only by selected variable:
    aggrData_long <- ddply(data, .(subject, x), function(x){
      y <- mean(x[, yVar], na.rm = T)
      return(data.frame(y))
      dev.off()})
    aggrData_long$condition_f <- paste0("x", aggrData_long$x) # only BRR
  }
  
  ## Determine levels:   
  levelVec <- levels(aggrData_long$x)
  
  ## Subselect variables:
  aggrData_long <- aggrData_long[, c("subject", "y", "condition_f")]
  
  # -------------------------------------------------------------------------- #
  ### Reshape to wide format:
  
  aggrData_wide <- reshape(aggrData_long, direction = "wide",
                           idvar = "subject", v.names = "y", timevar = "condition_f")
  
  # -------------------------------------------------------------------------- #
  ### Compute BRR effect independently of FRR:
  
  idx1 <- grep(paste0("x", levelVec[1]), names(aggrData_wide), fixed = T)
  idx2 <- grep(paste0("x", levelVec[2]), names(aggrData_wide), fixed = T)
  if (length(idx2) == 1){
    aggrData_wide$effect <- as.numeric(aggrData_wide[, idx2] - aggrData_wide[, idx1])  
  } else {
    aggrData_wide$effect <- as.numeric(rowSums(aggrData_wide[, idx2]) - rowSums(aggrData_wide[, idx1])) 
  }
  
  outputData <- aggrData_wide[, c("subject", "effect")]
  return(outputData)
  
}

# ============================================================================ #
#### Correlate BRR effect on p(leave) for two different vectors of past outcomes: ####

correlate_effects_given_past <- function(data, yVar = "choice_n", xVar = "BRR_f", 
                                         selVar = "outcome_n", selVec1, selVec2, correctByFRR = FALSE){
  #' Plot correlation between effects of X on Y for two separate sub-data sets 
  #' selected based on past values of given variable.
  #' @param data data frame, trial-by-trial data.
  #' @param yVar string, name of variable for which to compute effect (default: "choice_n").
  #' @param xVar string, name of variable whose effect to compute (default: "BRR_f").
  #' @param selVar string, name of variable by whose past values to select data (default: "outcome_n").
  #' @param selVec1 numerical vector, past variable values to use to select first data subset
  #' @param selVec2 numerical vector, past variable values to use toselect second data subset
  #' @param correctByFRR boolean, aggregate separately per FRR level, then average (default: FALSE).
  #' @return creates correlation plot.
  
  # -------------------------------------------------------------------------- #
  ### Check inputs:
  
  if(!(is.data.frame((data)))){stop("data must be a data frame")}
  if(!(yVar %in% names(data))){stop("yVar not found in data")}
  if(!(xVar %in% names(data))){stop("xVar not found in data")}
  if(!(selVar %in% names(data))){stop("selVar not found in data")}
  
  ## Check variable types:
  if(!(is.numeric(data[, yVar]))){stop("yVar has to be numeric")}
  if(!(is.numeric(data[, xVar]))){stop("xVar has to be numeric")}
  
  ## Select based on 1st vector, compute BRR effect per subject:
  selData1 <- select_data_given_past(data, selVar = selVar, selVec = selVec1)
  effData1 <- effect_per_subject(selData1, yVar = yVar, xVar = xVar, correctByFRR = correctByFRR)
  
  ## Select based on 2nd vector, compute BRR effect per subject:
  selData2 <- select_data_given_past(data, selVar = selVar, selVec = selVec2)
  effData2 <- effect_per_subject(selData2, yVar = yVar, xVar = xVar, correctByFRR = correctByFRR)
  
  ## Concatenate into data frame:
  plotData <- merge(effData1, effData2, by = "subject")
  NAidx <- is.na(plotData$effect.x) | is.na(plotData$effect.y)
  cat(paste0("Drop ", sum(NAidx), " subjects with NAs\n"))
  plotData <- plotData[!(NAidx), ]
  nSub <- nrow(plotData)
  cat(paste0("Correlation is r = ", round(cor(plotData$effect.x, plotData$effect.y), 2), "\n"))
  
  ## Plot correlation:
  p <- plot_correlation(data = plotData, xVar = "effect.x", yVar = "effect.y",
                        xLab = paste0(xVar, " effect given ", paste(selVec1, collapse = ", ")), 
                        yLab = paste0(xVar, " effect given ", paste(selVec2, collapse = ", ")),
                        main = paste0("[", paste(selVec1, collapse = ", "), "] & [", paste(selVec2, collapse = ", "), "]: ", 
                                      nSub, " subjects, r = ", round(cor(plotData$effect.x, plotData$effect.y), 2)))
  
  print(p)
  return(p)  
}

# ============================================================================ #
#### Row-wise variance in data frame: ####

RowVar <- function(x, ...) {
  # https://stackoverflow.com/questions/25099825/row-wise-variance-of-a-matrix-in-r
  rowSums((x - rowMeans(x, ...))^2, ...)/(dim(x)[2] - 1)
}

# ============================================================================ #
#### Compute difference between questionnaire items in matrix, count how often certain absolute value achieved: ####

detect_diagonal <- function(data, target = 1){
  #' Compute difference between adjacent columns (e.g. questionnaire items), 
  #' count how often absolute value of this difference is equal to target.
  #' @param data data frame, any kind of data with columns representing adjacent responses (e.g. items in questionnaire).
  #' @param target numeric integer, expected difference between adjacent columns to count.
  #' @return targetVec numeric vector, number of differences equal to target per row.
  
  ## Check if input variables included in data set:
  
  if(!(is.data.frame((data)))){stop("data must be a data frame")}
  
  diffMat <- t(apply(data, 1, diff, na.rm = T)) # compute absolute difference between columns
  if (ncol(data) > 2){
    targetVec <- rowSums(abs(diffMat) == target) # count how often absolute difference is target
    nCol <- ncol(diffMat)
  } else {
    targetVec <- as.numeric(abs(diffMat) == target)
    nCol <- 1
  }
  
  ## Print subjects with 100% target hit rate to console:
  alwaysTargetVec <- targetVec == nCol
  if (any(alwaysTargetVec, na.rm = T)){
    cat(paste0("Found ", sum(alwaysTargetVec, na.rm = T), " subjects with 100% diagonal of ", target, ":\n"))
    cat(paste0(paste(which(alwaysTargetVec), collapse = ", "), "\n"))
    print(data[which(targetVec == nCol), ])
  } else {
    cat(paste0("No subjects with 100% target value detected; maximum is ", max(targetVec, na.rm = T), " out of ", nCol, " differences\n"))
  }
  
  return(targetVec)
  
}

# END OF FILE.