#### All packages and settings for analysis ####

# =========================================================================================== #
#### Packages: ####

cat("Start loading packages\n")

# ----------------------------------------- #
## Read xlsx files:

library(readxl) # for read_excel

# ----------------------------------------- #
## Descriptives and plots:

require(car)
require(Rmisc) # for summarySEwithin
require(ggplot2)
require(lattice)
require(ltm)
require(pastecs) # for stat.desc
require(psych)
require(effsize) # for Cohen.d

require(stringr) # for str_pad etc.

# ----------------------------------------- #
## For reading Matlab files:

require(rmatio) # read.mat
require(data.table) # rbindlist

# ----------------------------------------- #
## RM-ANOVA:

require(ez)

# ----------------------------------------- #
## Tidyverse:

require(plyr)
require(dplyr)
require(magrittr)

# ----------------------------------------- #
# Linear mixed effects models:

require(lme4)
require(afex)
require(effects)
require(emmeans)

# Bayesian:
#require(rethinking)
# require(brms)
# Generalized additive mixed models:
require(mgcv)
require(itsadug)

# ----------------------------------------- #
## For corrplots:

library(corrplot) # for corrplots
library(synthesisr) # for line breaks in title

# ----------------------------------------- #
## Color bars:

require(sommer)
require(viridis)
require(RColorBrewer)

# ----------------------------------------- #
# For raincloud plots:

require(readr)
require(tidyr)
require(ggplot2)
require(Hmisc)
require(plyr)
require(RColorBrewer)
require(reshape2)
require(ggstatsplot) # for geom_flat_violin
require(gghalves) # for half plots

# ----------------------------------------- #
## Facilitate detecting when model finished:

require(beepr)

# ============================================================================ #
#### General settings: #####

cat("Set scipen to 20\n")
options(scipen = 20)

cat("Set contrasts to sum-to-zero coding\n")
options(contrasts = c("contr.sum", "contr.poly"))

# ============================================================================ #
#### Set seed: #####

mySeed <- 70
cat(paste0("Set seed to ", mySeed, "\n"))
set.seed(mySeed)

# cat("Delete all objects")
# rm(list=ls())
# END