## set_dirs.R
# Fish Task DISCOVERY scripts.
# Johannes Algermissen, 2024.


set_dirs <- function(rootDir = NULL){
  
  ## Root directory:
  
  dirs <- list()
  
  if (is.null(rootDir)){
    rootDir <- "C:/Users/johan/OneDrive/Documents/AI_EEGManyPipelines/"
  }
  
  dirs$rootDir    <- rootDir

  # -------------------------------------------------------------------------- #
  ### Code:
  
  dirs$codeDir    <- paste0(dirs$rootDir, "code/Main_Paper/johannes/")
  dirs$helperDir    <- paste0(dirs$codeDir, "helpers/")
  
  # -------------------------------------------------------------------------- #
  ### Where to save data:
  
  ## Data:
  dirs$dataDir <- paste0(dirs$rootDir, "data/")
  
  ## Raw data:
  dirs$rawDataDir <- paste0(dirs$dataDir, "rawData/")

  # -------------------------------------------------------------------------- #
  ### Processed data:
  
  dirs$processedDataDir <- paste0(dirs$dataDir, "processedData/")
  dir.create(dirs$processedDataDir, recursive = TRUE, showWarnings = FALSE) # recursive = TRUE)

  ## Newly created data sets:
  dirs$dataSetDir <- paste0(dirs$processedDataDir, "dataSets/")
  dir.create(dirs$dataSetDir, showWarnings = FALSE)
  
  # -------------------------------------------------------------------------- #
  ### Results:
  
  dirs$resultDir <- paste0(dirs$rootDir, "results/")
  dir.create(dirs$resultDir, showWarnings = FALSE)
  
  ## Models:
  dirs$modelDir <- paste0(dirs$resultDir, "models/")
  dir.create(dirs$modelDir, showWarnings = FALSE)
  
  ## Plots:
  dirs$plotDir <- paste0(dirs$resultDir, "plots/")
  dir.create(dirs$plotDir, showWarnings = FALSE)
  
  return(dirs)
  
}