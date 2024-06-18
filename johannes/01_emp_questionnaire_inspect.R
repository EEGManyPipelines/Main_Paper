#### Scripts Johannes: 01_emp_questionnaire_inspect.R ####

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
h1_data <- data.frame(read_excel(paste0(dirs$rawDataDir, "h1/all_AQ_variables_for_h1.xlsx")))
# h1_data <- read.csv(paste0(dirs$rawDataDir, "h1/selected_variables_for_models_h1.csv"))

## Inspect:
dim(h1_data) # 168 28
names(h1_data)
str(h1_data)
head(h1_data)

## Summary statistics:
round(t(stat.desc(h1_data)), 2)
#                            nbr.val nbr.null nbr.na min     max   range      sum median   mean SE.mean CI.mean.0.95       var std.dev coef.var
# Team                           168        0      0   1  168.00  167.00 14196.00  84.50  84.50    3.75         7.41   2366.00   48.64     0.58
# software                        NA       NA     NA  NA      NA      NA       NA     NA     NA      NA           NA        NA      NA       NA
# ans_software_host               NA       NA     NA  NA      NA      NA       NA     NA     NA      NA           NA        NA      NA       NA
# hf_cutoff                      168       11      0   0    3.00    3.00    60.14   0.10   0.36    0.03         0.06      0.16    0.40     1.13
# ans_hf_type                     NA       NA     NA  NA      NA      NA       NA     NA     NA      NA           NA        NA      NA       NA
# ans_hf_direction                NA       NA     NA  NA      NA      NA       NA     NA     NA      NA           NA        NA      NA       NA
# reref                           NA       NA     NA  NA      NA      NA       NA     NA     NA      NA           NA        NA      NA       NA
# ds_fs                          168        0      0 100  512.00  412.00 69747.00 512.00 415.16   10.44        20.61  18308.59  135.31     0.33
# subj_excluded                  168      120      0   0   15.00   15.00   171.00   0.00   1.02    0.19         0.37      5.79    2.41     2.36
# topo_region_h1                  NA       NA     NA  NA      NA      NA       NA     NA     NA      NA           NA        NA      NA       NA
# nr_chan_h1                     168        0      0   1   64.00   63.00  1781.77  10.61  10.61    0.80         1.59    108.40   10.41     0.98
# ans_exclusion_criteria_seg      NA       NA     NA  NA      NA      NA       NA     NA     NA      NA           NA        NA      NA       NA
# mc_method_h1                    NA       NA     NA  NA      NA      NA       NA     NA     NA      NA           NA        NA      NA       NA
# stat_method_h1                  NA       NA     NA  NA      NA      NA       NA     NA     NA      NA           NA        NA      NA       NA
# ans_spa_roi_avg_h1              NA       NA     NA  NA      NA      NA       NA     NA     NA      NA           NA        NA      NA       NA
# ans_mt_h1                       NA       NA     NA  NA      NA      NA       NA     NA     NA      NA           NA        NA      NA       NA
# ans_time_w_start_h1             NA       NA     NA  NA      NA      NA       NA     NA     NA      NA           NA        NA      NA       NA
# ans_time_w_end_h1               NA       NA     NA  NA      NA      NA       NA     NA     NA      NA           NA        NA      NA       NA
# time_w_length_h1               166        0      2   2 3000.00 2998.00 35133.00  70.00 211.64   31.01        61.23 159643.52  399.55     1.89
# ans_temp_roi_avg_h1             NA       NA     NA  NA      NA      NA       NA     NA     NA      NA           NA        NA      NA       NA
# ans_ica_algo                    NA       NA     NA  NA      NA      NA       NA     NA     NA      NA           NA        NA      NA       NA
# ans_bad_comp_sel_visual        168       77      0   0    1.00    1.00    91.00   1.00   0.54    0.04         0.08      0.25    0.50     0.92
# ans_bad_comp_sel_plugin        168       99      0   0    1.00    1.00    69.00   0.00   0.41    0.04         0.08      0.24    0.49     1.20
# ans_baseline_start              NA       NA     NA  NA      NA      NA       NA     NA     NA      NA           NA        NA      NA       NA
# ans_baseline_stop               NA       NA     NA  NA      NA      NA       NA     NA     NA      NA           NA        NA      NA       NA
# bs_window_length               168       17      0   0 1500.00 1500.00 38259.00 200.00 227.73   15.52        30.64  40453.47  201.13     0.88
# result_h1                       NA       NA     NA  NA      NA      NA       NA     NA     NA      NA           NA        NA      NA       NA
# pval                           154        0     14   0    0.99    0.99    15.04   0.00   0.10    0.02         0.04      0.06    0.23     2.40

# ============================================================================ #
# ============================================================================ #
# ============================================================================ #
# ============================================================================ #
#### Clean up variables: ####

## Software:
h1_data$software_cleaned_c <- h1_data$software
h1_data$software_cleaned_c[h1_data$software_cleaned_c == "eeglab_erplab"] <- "eeglab"
h1_data$software_cleaned_c[h1_data$software_cleaned_c == "eeglab_limo"] <- "eeglab"

## High-pass filter:
h1_data$ans_hf_type[h1_data$ans_hf_type == ""] <- "unknown"
h1_data$ans_hf_direction[h1_data$ans_hf_direction == ""] <- "unknown"

# ============================================================================ #
# ============================================================================ #
# ============================================================================ #
# ============================================================================ #
#### Inspect regressors: ####

names(h1_data)
# [1] "Team"                       "software"                   "ans_software_host"          "hf_cutoff"                  "ans_hf_type"               
# [6] "ans_hf_direction"           "reref"                      "ds_fs"                      "subj_excluded"              "topo_region_h1"            
# [11] "nr_chan_h1"                 "ans_exclusion_criteria_seg" "mc_method_h1"               "stat_method_h1"             "ans_spa_roi_avg_h1"        
# [16] "ans_mt_h1"                  "ans_time_w_start_h1"        "ans_time_w_end_h1"          "time_w_length_h1"           "ans_temp_roi_avg_h1"       
# [21] "ans_ica_algo"               "ans_bad_comp_sel_visual"    "ans_bad_comp_sel_plugin"    "ans_baseline_start"         "ans_baseline_stop"         
# [26] "bs_window_length"           "result_h1"                  "pval"

# ---------------------------------------------------------------------------- #
#### 01) Team: ####

nrow(h1_data)
length(unique(h1_data$Team)) # 168
h1_data$Team # numbers from 1 to 168
table(h1_data$Team)

## Missing values:
sum(is.na(h1_data$Team)) # 0

# ---------------------------------------------------------------------------- #
#### 02) Software: ####

sum(is.na(h1_data$software))
table(h1_data$software)
# besa    brainstorm   brainvision        custom        eeglab eeglab_erplab   eeglab_limo     fieldtrip     mnepython         other             R 
#    1             5            12             3            44            13             1            29            43            13             1 
#  spm 
#    3 

## 1 x Besa, 1 x erplab, 1 x limo, 1 x R
# 3 x SPM, 3 x custom

## Missing values:
sum(is.na(h1_data$software)) # 0

## Clean:
h1_data$software_cleaned_c <- h1_data$software
h1_data$software_cleaned_c[h1_data$software_cleaned_c == "eeglab_erplab"] <- "eeglab"
h1_data$software_cleaned_c[h1_data$software_cleaned_c == "eeglab_limo"] <- "eeglab"

# ---------------------------------------------------------------------------- #
#### 03) Software host: ####

h1_data$ans_software_host
table(h1_data$ans_software_host)
# BESA Research 7.1 Brain Vision Analyzer             Julia 1.7                MATLAB        MATLAB  R2016b         MATLAB R2015b         MATLAB R2016b 
#                 1                     3                     1                     2                     1                     2                     6 
#     MATLAB R2017b         MATLAB R2018a         MATLAB R2018b         MATLAB R2018B         MATLAB R2019a         MATLAB R2019b         MATLAB R2020a 
#                 6                     1                     4                     1                    12                     7                     7 
#     MATLAB R2020b         MATLAB R2021a         MATLAB R2021b         MATLAB R2022a         Python 3.10.2         Python 3.10.4           Python 3.11 
#                15                    26                    17                     5                     1                     4                     1 
#        Python 3.6          Python 3.6.9            Python 3.7         Python 3.7.10         Python 3.7.13            Python 3.8         Python 3.8.10 
#                 2                     2                     1                     1                     1                     2                     1 
#     Python 3.8.12          Python 3.8.2          Python 3.8.5          Python 3.8.8            Python 3.9         Python 3.9.10         Python 3.9.12 
#                 1                     1                     1                     3                     9                     2                     2 
#      Python 3.9.5          Python 3.9.6          Python 3.9.7          Python 3.9.9        Python v3.8.13               R 3.6.1               R 4.1.3 
#                 1                     1                     6                     1                     1                     1                     1 

## Missing values:
sum(is.na(h1_data$ans_software_host)) # 4
h1_data$software[is.na(h1_data$ans_software_host)] # all brainvision
# "brainvision" "brainvision" "brainvision" "brainvision"

# ---------------------------------------------------------------------------- #
#### 04) High-pass filter cut-off: ####

h1_data$hf_cutoff
table(h1_data$hf_cutoff)
 #                 0              0.01              0.05               0.1               0.2              0.25               0.3              0.35               0.5 
 #                11                 1                 5                74                 7                 2                 4                 1                31 
 # 0.530516476972984                 1                 3 
 #                 1                30                 1 

hist(h1_data$hf_cutoff)
densityplot(h1_data$hf_cutoff)

## Missing values:
sum(is.na(h1_data$hf_cutoff)) # 0

# ---------------------------------------------------------------------------- #
#### 05) High-pass filter type: ####

sum(is.na(h1_data$ans_hf_type))
table(h1_data$ans_hf_type)
sum(is.na(h1_data$ans_hf_type)) # 14
# hf_fir  hf_iir     IIR   other unknown 
#     94      44       2       2      12 

## Missing values:
sum(is.na(h1_data$ans_hf_type)) # 14

## Fill in empty responses:
h1_data$ans_hf_type[h1_data$ans_hf_type == ""] <- "unknown"

# ---------------------------------------------------------------------------- #
#### 05/06) High-pass filter direction: ####

sum(is.na(h1_data$ans_hf_direction))
table(h1_data$ans_hf_direction)
# fwd fwd_bwd fwr_bwd fwr_bwr   other unknown 
#  16      46       1       1       3      87

## Missing values:
sum(is.na(h1_data$ans_hf_direction)) # 14

## Fill in empty responses:
h1_data$ans_hf_direction[h1_data$ans_hf_direction == ""] <- "unknown"

# ---------------------------------------------------------------------------- #
#### 07) Re-referencing: ####

h1_data$reref
table(h1_data$reref)
# avg  mastoid original    other  unknown 
#  85       46       28        7        2

## Missing values:
sum(is.na(h1_data$reref)) # 0

# ---------------------------------------------------------------------------- #
#### 08) Down-sampling frequency: ####

h1_data$ds_fs
table(h1_data$ds_fs)
# 100 128 200 250 256 265 300 400 500 512 
#   1   8   1  17  29   1   1   1   2 107

## Missing values:
sum(is.na(h1_data$ds_fs)) # 0

# ---------------------------------------------------------------------------- #
#### 09) Subjects excluded: ####

h1_data$subj_excluded
table(h1_data$subj_excluded)
#   0   1   2   3   4   5   6   7  11  13  15 
# 120  14  11   8   2   5   2   2   1   2   1 

t(stat.desc(h1_data$subj_excluded))
# nbr.val nbr.null nbr.na min max range sum median     mean   SE.mean CI.mean.0.95      var  std.dev coef.var
#     [1,]     168      120      0   0  15    15 171      0 1.017857 0.1856472    0.3665178 5.790098 2.406262 2.364047
densityplot(h1_data$subj_excluded)

# --> From 0 to 15, mean 1; most from 0 - 5

## Missing values:
sum(is.na(h1_data$subj_excluded)) # 0

# ---------------------------------------------------------------------------- #
#### 10) Topographies: ####

h1_data$topo_region_h1
table(h1_data$topo_region_h1)
#            all                    central central-parietal-occipital            centro-parietal                    frontal          frontal-occipital 
#             11                          2                          1                          1                          6                          2 
# fronto-central                  occipital          occipito-parietal                   parietal                  posterior                    unknown 
#             10                          8                         55                          7                          1                         64 

## Missing values:
sum(is.na(h1_data$topo_region_h1)) # 0

# ---------------------------------------------------------------------------- #
#### 11) Number channels included: ####

h1_data$nr_chan_h1
t(stat.desc(h1_data$nr_chan_h1))
densityplot(h1_data$nr_chan_h1)
densityplot(h1_data$nr_chan_h1, xlim = c(0, 20))

## Missing values:
sum(is.na(h1_data$nr_chan_h1)) # 0

# ---------------------------------------------------------------------------- #
#### 12) Exclusion criteria for time segments: ####

h1_data$ans_exclusion_criteria_seg
table(h1_data$ans_exclusion_criteria_seg)
# automatic mul_thresh      other     plugin     thresh     visual 
#         3         38         16         24         34         41 

## automatic:
h1_data$software[h1_data$ans_exclusion_criteria_seg == "automatic"]
table(h1_data$software[h1_data$ans_exclusion_criteria_seg == "automatic"]) # none, custom, eeglab
# custom eeglab 
#      1      2 

## plugin:
h1_data$software[h1_data$ans_exclusion_criteria_seg == "plugin"]
table(h1_data$software[h1_data$ans_exclusion_criteria_seg == "plugin"]) # brainvision, eeglab, eeglab_erplab, mnepythong, other
# brainvision        eeglab eeglab_erplab     mnepython         other 
#           1             7             3             9             4 

## Visual:
h1_data$software[h1_data$ans_exclusion_criteria_seg == "visual"]
table(h1_data$software[h1_data$ans_exclusion_criteria_seg == "visual"])
# besa    brainstorm   brainvision        eeglab eeglab_erplab     fieldtrip     mnepython         other 
#    1             4             2            12             1            13             5             3 

## Missing:
h1_data$software[is.na(h1_data$ans_exclusion_criteria_seg)]
table(h1_data$software[is.na(h1_data$ans_exclusion_criteria_seg)])
# brainstorm   brainvision        eeglab eeglab_erplab     fieldtrip     mnepython             R 
#          1             2             4             1             2             1             1 

## Missing values:
sum(is.na(h1_data$ans_exclusion_criteria_seg)) # 12

# ---------------------------------------------------------------------------- #
#### 13) Multiple comparison method: ####

h1_data$mc_method_h1
table(h1_data$mc_method_h1)
# bhfdr      bonferroni holm-bonferroni            none           other     permutation             rft         unknown 
#    14              13               1              28               5              46               3              58

## Missing values:
sum(is.na(h1_data$mc_method_h1)) # 0

# ---------------------------------------------------------------------------- #
#### 14) Statistical method: ####

h1_data$stat_method_h1
table(h1_data$stat_method_h1)
# cluster one_sample_ttest            other       pair_ttest         rm_anova 
#      44                8               22               73               20 

## Other methods:
h1_data$software[h1_data$stat_method_h1 == "other"]
table(h1_data$software[h1_data$stat_method_h1 == "other"])
# brainstorm   brainvision        eeglab eeglab_erplab     fieldtrip     mnepython         other             R 
#          1             1             5             2             2             7             3             1 

## Missing:
h1_data$software[is.na(h1_data$stat_method_h1)]
# "eeglab"
h1_data[is.na(h1_data$stat_method_h1), ]

## Missing values:
sum(is.na(h1_data$stat_method_h1)) # 1

# ---------------------------------------------------------------------------- #
#### 15) Averaged within spatial ROI or not: ####

h1_data$ans_spa_roi_avg_h1
table(h1_data$ans_spa_roi_avg_h1)
#  0  1 
# 95 73 

## Missing values:
sum(is.na(h1_data$ans_spa_roi_avg_h1)) # 0

# ---------------------------------------------------------------------------- #
#### 16) Correction for multiple comparisons yes/no: ####

h1_data$ans_mt_h1
table(h1_data$ans_mt_h1)
#  0  1 
# 87 80 

## Missing values:
sum(is.na(h1_data$ans_mt_h1)) # 1
h1_data[is.na(h1_data$ans_mt_h1), ]

## Combine with MC method:
h1_data[1:20, c("ans_mt_h1", "mc_method_h1")]
table(h1_data$ans_mt_h1, h1_data$mc_method_h1)
#   bhfdr bonferroni holm-bonferroni none other permutation rft unknown
# 0     0          0               0   27     1           3   0      56
# 1    14         13               1    1     4          43   3       1
## --> none: both 0 and 1
## --> permutation: both 0 and 1
## --> unknown: both 0 and 1

## No:
h1_data$mc_method_h1[h1_data$ans_mt_h1 == "0"]
table(h1_data$mc_method_h1[h1_data$ans_mt_h1 == "0"])
# none       other permutation     unknown 
#   27           1           3          56 

## Yes:
h1_data$mc_method_h1[h1_data$ans_mt_h1 == "1"]
table(h1_data$mc_method_h1[h1_data$ans_mt_h1 == "1"])
# bhfdr      bonferroni holm-bonferroni            none           other     permutation             rft         unknown 
#    14              13               1               1               4              43               3               1 

# ---------------------------------------------------------------------------- #
#### 17) Start temporal ROI window: ####

h1_data$ans_time_w_start_h1
table(h1_data$ans_time_w_start_h1)

t(stat.desc(as.numeric(h1_data$ans_time_w_start_h1)))
densityplot(as.numeric(h1_data$ans_time_w_start_h1))
densityplot(as.numeric(h1_data$ans_time_w_start_h1), xlim = c(-500, 500))

## Missing values:
sum(is.na(h1_data$ans_time_w_start_h1)) # 1

# ---------------------------------------------------------------------------- #
#### 18) End temporal ROI window: ####

h1_data$ans_time_w_end_h1
table(h1_data$ans_time_w_end_h1)

t(stat.desc(as.numeric(h1_data$ans_time_w_end_h1)))
densityplot(as.numeric(h1_data$ans_time_w_end_h1))
densityplot(as.numeric(h1_data$ans_time_w_end_h1), xlim = c(0, 700))

## Missing values:
sum(is.na(h1_data$ans_time_w_end_h1)) # 2

# ---------------------------------------------------------------------------- #
#### 19) Duration temporal ROI window: ####

h1_data$time_w_length_h1
table(h1_data$time_w_length_h1)

t(stat.desc(as.numeric(h1_data$time_w_length_h1)))
densityplot(as.numeric(h1_data$time_w_length_h1))
densityplot(as.numeric(h1_data$time_w_length_h1), xlim = c(0, 700))

## Missing values:
sum(is.na(h1_data$time_w_length_h1)) # 2

# ---------------------------------------------------------------------------- #
#### 20) Average within temporal ROI yes/no: ####

h1_data$ans_temp_roi_avg_h1
table(h1_data$ans_temp_roi_avg_h1)
#  0  1 
# 73 95

## Missing values:
sum(is.na(h1_data$ans_temp_roi_avg_h1)) # 0

# ---------------------------------------------------------------------------- #
#### 21) ICA algorithm: ####

h1_data$ans_ica_algo
table(h1_data$ans_ica_algo)
# amica ex_infomax    fastICA    infomax      other     picard     runica    unknown 
#     6         30         35         49          3         11          1          6 

## Missing values:
sum(is.na(h1_data$ans_ica_algo)) # 27

# ---------------------------------------------------------------------------- #
#### 22) Visual selection of bad components yes/no: ####

h1_data$ans_bad_comp_sel_visual
table(h1_data$ans_bad_comp_sel_visual)
#  0  1 
# 77 91

## Missing values:
sum(is.na(h1_data$ans_bad_comp_sel_visual)) # 0

# ---------------------------------------------------------------------------- #
#### 23) Automatic selection via plug-in of bad components yes/no: ####

h1_data$ans_bad_comp_sel_plugin
table(h1_data$ans_bad_comp_sel_plugin)
#  0  1 
# 99 69 

## Missing values:
sum(is.na(h1_data$ans_bad_comp_sel_plugin)) # 0

# ---------------------------------------------------------------------------- #
#### 24) Baseline start time: ####

h1_data$ans_baseline_start
table(h1_data$ans_baseline_start) # still contains ms in some responses

## Delete "ms" in answers:
h1_data$ans_baseline_start_n <- as.numeric(gsub("ms", "", h1_data$ans_baseline_start))
h1_data[is.na(h1_data$ans_baseline_start_n), c("ans_baseline_start", "ans_baseline_start_n")] # all NAs in new variable also in old variable

t(stat.desc(h1_data$ans_baseline_start_n))
densityplot(h1_data$ans_baseline_start_n)
densityplot(h1_data$ans_baseline_start_n, xlim = c(-600, 0))

sum(h1_data$ans_baseline_start_n > -600, na.rm = T) # 144
mean(h1_data$ans_baseline_start_n > -600, na.rm = T) # 0.966443

sum(h1_data$ans_baseline_start_n < -600, na.rm = T) # 3
mean(h1_data$ans_baseline_start_n < -600, na.rm = T) # 0.03355705

## Missing values:
sum(is.na(h1_data$ans_baseline_start_n)) # 17

# ---------------------------------------------------------------------------- #
#### 25) Baseline stop: ####

h1_data$ans_baseline_stop
table(h1_data$ans_baseline_stop)

## Delete "ms" in answers:
h1_data$ans_baseline_stop_n <- as.numeric(gsub("ms", "", h1_data$ans_baseline_stop))
h1_data[is.na(h1_data$ans_baseline_start_n), c("ans_baseline_stop", "ans_baseline_stop_n")] # all NAs in new variable also in old variable

t(stat.desc(h1_data$ans_baseline_stop_n))
densityplot(h1_data$ans_baseline_stop_n)
densityplot(h1_data$ans_baseline_stop_n, xlim = c(-500, 500))

## Positive:
sum((h1_data$ans_baseline_stop_n > 0), na.rm = T) # 2
mean((h1_data$ans_baseline_stop_n > 0), na.rm = T) # 0.01324503

## Exactly zero:
sum((h1_data$ans_baseline_stop_n == 0), na.rm = T) # 132
mean((h1_data$ans_baseline_stop_n == 0), na.rm = T) # 0.8741722

## Negative:
sum((h1_data$ans_baseline_stop_n < 0), na.rm = T) # 17
mean((h1_data$ans_baseline_stop_n < 0), na.rm = T) # 0.1125828

## Missing values:
sum(is.na(h1_data$ans_baseline_stop)) # 17

# -------------------------------- #
### Compare both:

h1_data[, c("ans_baseline_start", "ans_baseline_start_n", "ans_baseline_stop", "ans_baseline_stop_n")]
h1_data[is.na(h1_data$ans_baseline_start), c("ans_baseline_start", "ans_baseline_stop", "ans_baseline_start_n", "ans_baseline_stop_n")]
h1_data[is.na(h1_data$ans_baseline_stop), c("ans_baseline_start", "ans_baseline_stop", "ans_baseline_start_n", "ans_baseline_stop_n")]

h1_data$ans_baseline_duration_n <- h1_data$ans_baseline_stop_n - h1_data$ans_baseline_start_n
densityplot(h1_data$ans_baseline_duration_n)
densityplot(h1_data$ans_baseline_duration_n/1000)
t(stat.desc(h1_data$ans_baseline_duration_n))

# ---------------------------------------------------------------------------- #
#### 26) Baseline window length: ####

h1_data$bs_window_length
table(h1_data$time_w_length_h1)

t(stat.desc(as.numeric(h1_data$time_w_length_h1)))
densityplot(as.numeric(h1_data$time_w_length_h1)) # all positive, mostly 200 ms long, sometimes up to 1000
densityplot(as.numeric(h1_data$time_w_length_h1), xlim = c(0, 1000))

## Up to < 200 ms:
sum((as.numeric(h1_data$time_w_length_h1) <= 200), na.rm = T) # 139
mean((as.numeric(h1_data$time_w_length_h1) <= 200), na.rm = T) # 0.8373494

## Missing values:
sum(is.na(h1_data$time_w_length_h1)) # 2
h1_data[is.na(h1_data$time_w_length_h1), c("ans_baseline_start", "ans_baseline_stop", "time_w_length_h1")] # --> seems computable?
h1_data[is.na(h1_data$ans_baseline_start), c("ans_baseline_start", "ans_baseline_stop", "time_w_length_h1")] # length provided, but not start/ stop
h1_data[is.na(h1_data$ans_baseline_stop), c("ans_baseline_start", "ans_baseline_stop", "time_w_length_h1")] # --> length provided, but not start/ stop

# ============================================================================ #
# ============================================================================ #
#### 27) Hypothesis confirmed: ####

h1_data$result_h1
table(h1_data$result_h1)
# FALSE  TRUE 
#    34   134

## Missing values:
sum(is.na(h1_data$result_h1)) # 0

## Plot p-values as function of result confirmed/ disconfirmed:
h1_data$result_h1_f <- factor(ifelse(h1_data$result_h1 == TRUE, "confirmed", "not confirmed"))
custom_bar_between(h1_data, xVar = "result_h1_f", yVar = "pval", selCol = colours$greenpink1) # , yLim = c(0, 0.5))

# -------------------------------- #
## Inspect confirmed:
t(stat.desc(h1_data$pval[h1_data$result_h1 == TRUE]))
mean(h1_data$pval[h1_data$result_h1 == TRUE] <= 0.05, na.rm = T) # all <= .05

## P-value of exactly 0.05:
sum(h1_data$pval == 0.05, na.rm = T) # 2 exactly 0.05
h1_data[which(h1_data$pval == 0.05), ] # 1 x mc_method "other", 1 x mc_method "bhfdr" with "pair_ttest" 

# -------------------------------- #
## Inspect NOT confirmed:
t(stat.desc(h1_data$pval[h1_data$result_h1 == FALSE]))
sum(h1_data$pval[h1_data$result_h1 == FALSE] <= 0.05, na.rm = T) # still 1 x < 0.05
h1_data[which(h1_data$result_h1 == FALSE & h1_data$pval <= 0.05), ] # eeglab, permutation, pair_ttest, p = .025 (two-tailed test???)

# ============================================================================ #
# ============================================================================ #
#### 28) Inspect p-values: ####

h1_data$pval

## Descriptives:
stat.desc(h1_data$pval)
round(t(stat.desc(h1_data$pval)), 3)

## Plot:
plot(h1_data$pval)
plot(sort(h1_data$pval))
plot(sort(h1_data$pval), ylim = c(0, 0.01))
plot(sort(h1_data$pval), ylim = c(0, 0.001))
plot(sort(h1_data$pval), ylim = c(0, 0.00001))

## Densityplot:
densityplot(h1_data$pval)
densityplot(h1_data$pval, xlim = c(0, 0.10)) # zoom in
# no accumulation just above 0.05
# but strangely also none between 0.05 and 0.085

# ------------------------------------------- #
### Convert to z-value using qnorm:
# z-to-p: https://www.r-bloggers.com/2022/05/calculate-the-p-value-from-z-score-in-r/


# Have people working on meta-analysis found a solution for this?
# https://www.statalist.org/forums/forum/general-stata-discussion/general/1558609-calculate-z-scores-from-p-values-to-get-ses 

## Dry test of function: z-to-p
zval <- seq(0, 10, 0.1)
# b <- 1 - pnorm(a)
pval <- (pnorm(zval, lower.tail = F)) * 2
plot(pval, zval, xlim = c(0, 1), ylim = c(0, 3))
abline(h = 1.96)
(pnorm(1.96, lower.tail = F)) * 2
(pnorm(1.64, lower.tail = F)) * 2

##
pval <- seq(0, 1, 0.01) # p-values
zval <- qnorm(pval/2, lower.tail = F) 
plot(pval, zval, xlim = c(0, 1), ylim = c(0, 3))
abline(h = 1.96)

plot(b, pval)

## Dry test of function: p-to-z
a <- seq(0, 1, 0.001)
b <- qnorm(a, lower.tail = T)
b <- qnorm(log(a), lower.tail = T, log.p = T)
plot(a, b)

## Implement:
# h1_data$zval <- qnorm(1 - data$pval)
h1_data$zval <- qnorm(h1_data$pval/2, lower.tail = F) 
plot(h1_data$pval, h1_data$zval)

stat.desc(h1_data$zval)
plot(h1_data$zval)
plot(sort(h1_data$zval))
densityplot(h1_data$zval)


## Expect to find: multiple comparisons applied --> smaller p-values
## Reason: because of cluster statistics

## Expect to find: baseline onset/ offset --> p-values

# END OF FILE.