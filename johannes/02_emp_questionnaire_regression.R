#### Scripts Johannes: 02_emp_questionnaire_regression.R ####

rm(list = ls())

# file:///C:/Users/johan/OneDrive/Documents/AI_EEGManyPipelines/notebooks/elena/r_markdown_AIC_h1.html

# ============================================================================ #
#### Set directories, load packages and custom functions: ####

## Set rootDir:
rootDir <- "C:/Users/johan/OneDrive/Documents/AI_EEGManyPipelines/"
helperDir <- paste0(rootDir, "code/Main_Paper/johannes/helpers/")

## Load directories:
source(paste0(helperDir, "set_dirs.R")) # Load packages and options settings
dirs <- set_dirs(rootDir)

## Load colours:
source(paste0(helperDir, "load_colours.R")) # Load functions
colours <- load_colours()

## Load packages:
source(paste0(helperDir, "package_manager.R")) # Load packages and options settings

# ------------------------------------------------- #
## Load custom functions:

source(paste0(helperDir, "00_emp_functions_regression.R")) # Load functions

# ============================================================================ #
#### Load in data: #####

library(readxl)
h1_data <- data.frame(read_excel(paste0(dirs$rawDataDir, "all_AQ_variables_for_h1.xlsx")))
# h1_data <- read.csv(paste0(dirs$rawDataDir, "selected_variables_for_models_h1.csv"))

## Inspect:
dim(h1_data) # 168 28
head(h1_data)
str(h1_data)

# ============================================================================ #
#### Pre-process data: ####

names(h1_data)

## Correct high-pass filter:
h1_data$ans_hf_type[h1_data$ans_hf_type == ""] <- "unknown"
h1_data$ans_hf_direction[h1_data$ans_hf_direction == ""] <- "unknown"


## 02) Software:
# h1_data$software_cleaned_c <- h1_data$software # create new variable
# h1_data$software_cleaned_c[h1_data$software_cleaned_c == "eeglab_erplab"] <- "eeglab"
# h1_data$software_cleaned_c[h1_data$software_cleaned_c == "eeglab_limo"] <- "eeglab"
h1_data$software[h1_data$software == "eeglab_erplab"] <- "eeglab"
h1_data$software[h1_data$software == "eeglab_limo"] <- "eeglab"
h1_data$software_f <- factor(h1_data$software)

## 03) Software host:
table(h1_data$ans_software_host)
h1_data$ans_software_host_f <- factor(gsub("^(\\S+).*", "\\1", h1_data$ans_software_host)) # remove version

## 04) High-pass cut-off:
table(h1_data$hf_cutoff)
h1_data$hf_cutoff_n <- h1_data$hf_cutoff # keep numeric

## 05) High-pass filter type:
table(h1_data$ans_hf_type)
h1_data$ans_hf_type_f <- factor(h1_data$ans_hf_type)

## 06) High-pass filter direction:
table(h1_data$ans_hf_direction)

## 07) :
table(h1_data$ans_hf_direction)

## 08) :

## 09) :

## 10) :

## 11) :

## 12) :

## 13) :

## 14) :

## 15) :

## 16) :

## 17) :

## 18) :

## 19) :

## 20) :

## 21) :

## 22) :

## 23) :

## 24) :

## 25) :

## 26) p-value:
h1_data$pVal_n <- h1_data$pval

## 27) Decision:
h1_data$result_h1_n <- ifelse(h1_data$result_h1 == T, 1, 0)

names(h1_data)

# ---------------------------------------------------------------------------- #
### Select regressors:

regNames <- names(h1_data)[2:26]; regNames

# ============================================================================ #
#### 02) Plots: ####

plotData <- h1_data

source(paste0(helperDir, "00_emp_functions_regression.R")) # Load functions
custom_bar_between(data = plotData, yVar = "result_h1_n", xVar = "software_f", xAngle = 90, yLim = c(0, 1), addPoint = F)

# ============================================================================ #
#### 03) Regression including ALL regressors: ####


# ============================================================================ #
#### 03a) Logistic regression on yes/no response including ALL regressors: ####

modData <- h1_data

table(h1_data$result_h1)

### Logistic regression:
formula <- paste0("result_h1 ~ ", paste0(regNames, collapse = " + "))

mod <- glm(formula = formula, data = modData, family = "binomial")


# ============================================================================ #
#### 04) Regression including SINGLE regressor: ####

# ============================================================================ #
#### 04a) Logistic regression on yes/no response including SINGLE regressors: ####

nReg <- length(regNames)

## Initialize outcomes:
aicVec <- rep(NA, nReg)
bicVec <- rep(NA, nReg)
pValvec <- rep(NA, nReg)

for (iVar in 1:nReg){ # iVar <- 1
  
  formula <- paste0("result_h1 ~ ", regNames[iVar])
  cat(paste0("Fit model ", formula, ":\n"))
  mod <- glm(formula = formula, data = modData, family = "binomial")
  # summary(mod)
  anova(mod)
  aicVec[iVar] <- AIC(mod)
  bicVec[iVar] <- BIC(mod)
  # r2Vec[iVar] <- summary(mod)$r.squared # does not work for logistic regression
  # summary(mod)$coefficients[, 4] 
}

# ============================================================================ #
# ============================================================================ #
# ============================================================================ #
# ============================================================================ #
#### 05) Regression including random subset of predictors: ####

# see http://www.nature.com/articles/s41467-020-16278-6, section "MEG topography contributing to classification accuracy."

# ---------------------------------------- #
### Settings:

## Variables available:
nReg <- length(regNames)

## Number of variables to be selected in regression:
nSel <- 5 # Rollwage: 30 out of 273 channels

## Iterations:
nIter <- 1000 # Rollwage: 2500

# ---------------------------------------- #
### Fit model, compute AIC/BIC for random subset of variables:

## Initialize outcomes:
selVarMat <- matrix(NA, nIter, nSel)
aicIterVec <- rep(NA, nIter)
bicIterVec <- rep(NA, nIter)

## Loop over iterations:
set.seed(123)
for (iIter in 1:nIter){ # iVar <- 1
  
  selIdx <- sample(1:nReg, 5, replace = F)
  selVarMat[iIter, ] <- selIdx
  formula <- paste0("result_h1 ~ ", paste0(regNames[selIdx], collapse = " + "))
  cat(paste0("Iteration ", iIter, ": Fit model ", formula, ":\n"))
  mod <- glm(formula = formula, data = modData, family = "binomial")
  # summary(mod)
  aicIterVec[iIter] <- AIC(mod)
  bicIterVec[iIter] <- BIC(mod)
  if (iIter == nIter){cat("Finished loop :-)\n")}
}

# ---------------------------------------- #
### Compute mean AIC/ BIC per variable:

## Initialize outcomes:
aicRegVec <- rep(NA, nReg)
bicRegVec <- rep(NA, nReg)

## Loop over variables:
for (iReg in 1:nReg){ # iReg <- 1
  
  cat(paste0("Identify and aggregate rows for regressor ", iReg, " (", regNames[iReg], ")\n"))
  
  ## Identify rows with this variable:
  selRowIdx <- which(selVarMat == iReg, arr.ind = TRUE)[, 1]
  # selVarMat[selRowIdx, ]
  
  ## Compute average AIC/ BIC per variable:
  aicRegVec[iReg] <- mean(aicIterVec[selRowIdx], na.rm = T)
  bicRegVec[iReg] <- mean(bicIterVec[selRowIdx], na.rm = T)
  if (iReg == nReg){cat("Finished loop :-)\n")}
}

## Plot:
par(mar = c(9.1, 4.1, 4.1, 2.1)) # bottom, left, top, right
plot(aicRegVec, type = "b", xaxt = "n", xlab = "")
axis(1, at = 1:length(regNames), labels = regNames, las = 2)
par(mar = c(5.1, 4.1, 4.1, 2.1)) # bottom, left, top, right

## Sort 
sortIdx <- sort.int(aicRegVec, decreasing = F, index.return = T)
regNames[sortIdx$ix]

## For each variable, identify rows it contains, compute mean AIC/ BIC: