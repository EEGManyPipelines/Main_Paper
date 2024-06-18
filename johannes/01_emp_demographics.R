#### Scripts Johannes: 01_emp_demographics.R ####
# 01_emp_belief_prior_posterior.R
rm(list = ls())

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

# subFolder <- "beliefs_prior_posterior/"
subFolder <- "demographics/"

## Team ID as number:
# demoData <- read.csv(paste0(dirs$rawDataDir, subFolder, "demographic_cleaned_07092022.csv")); data <- demoData
## Team ID as string:
demoData <- read.csv(paste0(dirs$rawDataDir, subFolder, "demographic_final_sample_teamID.csv")); data <- demoData
## Expertise:
expertiseData <- data.frame(read_excel(paste0(dirs$rawDataDir, subFolder, "EEG_expertise_final_sample_385_updated.xlsx"))); data <- expertiseData

## Inspect:
dim(data)
names(data)
str(data)
head(data)

# ============================================================================ #
#### Inspect demographics: #####

names(demoData)

## Number analysts:
nrow(demoData) # 397 analysts

## Number teams:
table(demoData$team_identifier)
length(unique(demoData$team_identifier)) # 168 teams

## Age:
round(t(stat.desc(demoData$age)), 2)
densityplot(demoData$age)

## Gender:
table(demoData$gender)

## Country of origin:
table(demoData$country)

## Job/position:
table(demoData$job_position) # free text entry, diverse descriptions --> recode?

## Years in job:
round(t(stat.desc(demoData$job_years)), 2)
densityplot(demoData$job_years)

## Highest degree:
table(demoData$highest_degree) # free text entry, diverse descriptions  --> recode?

## Years since highest degree:
round(t(stat.desc(demoData$highest_degree_years)), 2)
sort(demoData$highest_degree_years) # one outlier at 2011, next highest is 29
densityplot(demoData$highest_degree_years)
densityplot(demoData$highest_degree_years, xlim = c(0, 30))
# one outlier of 2011

## EEG field:
table(demoData$eeg_field) # free text entry, diverse descriptions --> recode?

## EEG topic:
t(table(demoData$eeg_topics)) # free text entry, diverse descriptions --> recode?

## Years of EEG experience:
round(t(stat.desc(demoData$eeg_years)), 2)
densityplot(demoData$eeg_years)

## EEG data sets analysed:
round(t(stat.desc(demoData$eeg_datasets)), 2)
sort(demoData$eeg_datasets) # highest at 3000, then 1000, then 500, several > 100, mostly < 100
densityplot(demoData$eeg_datasets)
densityplot(demoData$eeg_datasets, xlim = c(0, 1000))
densityplot(demoData$eeg_datasets, xlim = c(0, 500))
densityplot(demoData$eeg_datasets, xlim = c(0, 100))

## EEG papers published:
round(t(stat.desc(demoData$eeg_papers)), 2)
sort(demoData$eeg_papers) # highest at 150, then 89, mostly below 50
densityplot(demoData$eeg_papers)
densityplot(demoData$eeg_papers, xlim = c(0, 100))
densityplot(demoData$eeg_papers, xlim = c(0, 50))

# ============================================================================ #
#### Inspect expertise: #####

dim(expertiseData)
names(expertiseData)
str(expertiseData)
head(expertiseData)

densityplot(expertiseData$expertise_eeg_pre)
densityplot(expertiseData$expertise_eeg_erp)
densityplot(expertiseData$expertise_eeg_tf)
densityplot(expertiseData$expertise_eeg_stats)
densityplot(expertiseData$expertise_eeg_memory)
densityplot(expertiseData$expertise_eeg_lt)
densityplot(expertiseData$expertise_cogneuro)
densityplot(expertiseData$expertise_n1)
densityplot(expertiseData$expertise_theta)
densityplot(expertiseData$expertise_alpha)


# END