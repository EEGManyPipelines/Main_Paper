rm(list = ls())

library(R.matlab)
library(tidyverse)

# Mean difference waves split by a pipeline choice, standardized over reference schemes.
# For 'cor_rej', 'bad_cmp' and 'hf': group means are computed WITHIN each reference scheme and
# then averaged across schemes with the SAME weights for both groups
# Only schemes containing both groups are used.
# For 'ref': plain mean difference wave per reference scheme (no adjustment).

# Which variables to plot? any of 'ref', 'cor_rej', 'bad_cmp', 'hf'
color_values <- c('ref')

stim_y_coord <- c(0.3,-0.25,-0.5,-0.5)  # for reference channel: c(0.8,-0.5,-1, -1)
hyp_name <- c("H1", "H2a", "H3a_Fz", "H3a_Pz")

#Load data
# set paths
inDir <- '/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/data/for plotting/FINAL 3 H difference wave plotting/'
outDir <- "/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/figures/"

all_amp <- c('H1_amplitude_v2_2.mat','H2a_amplitude_v2_2.mat','H3a_amplitude_Fz_2.mat','H3a_amplitude_Pz_2.mat')
all_time <- c('H1_time_xaxis_2.mat','H2a_time_xaxis.mat','H3a_time_xaxis.mat','H3a_time_xaxis.mat')
all_IDs <- c('IDs_h1_diff_wave_2.mat','IDs_h2a_diff_wave_2.mat', 'IDs_h3a_diff_wave_2.mat', 'IDs_h3a_diff_wave_2.mat')

in_amp_name <- c('amp.h1','amp.h2a','amp.h3a','amp.h3a')
in_time_name <- c('time.h1','time.h2a', 'time.h3a', 'time.h3a')
in_ID_name <- c('IDs','IDs.h2a','IDs.h3a','IDs.h3a')

xmin_rect <- c(80, 300, 300, 300)
xmax_rect <- c(120, 500, 500, 500)

# team-level info (kept untouched; subsets are made inside the loop)
AQ_data <- read.csv('/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/data/Main Analysis/all_var_AQ_h1_corrected.csv')
AQ_ID <- read.csv("/Users/ecesnaite/Desktop/BuschLab/EEGManyPipelines/data/Main Analysis/IDs_linear_models.csv")

ref_all     <- as.character(AQ_data$reref)
ref_all[ref_all=="original"] <- "POz"
cor_rej_all <- ifelse(as.logical(AQ_data$CorrectionBeforeRejection), "yes", "no")
bad_cmp_all <- ifelse(as.logical(AQ_data$ans_bad_comp_sel_plugin), "yes", "no")
hf_all      <- ifelse(as.numeric(AQ_data$hf_cutoff) <= 0.1, "<=0.1", ">0.1")

ref_schemes <- c("avg", "POz", "mastoid")

# settings per variable: grouping vector, legend title, plot title, group levels and colours
settings <- list(
  cor_rej = list(group = cor_rej_all, legend = "corr.bef.rej.", title = "Correction before rejection",
                 levels = c("no", "yes"), pal = c(no = "#0072B2", yes = "#D55E00")),
  bad_cmp = list(group = bad_cmp_all, legend = "plugin use", title = "Component plugin use",
                 levels = c("no", "yes"), pal = c(no = "#7876B1", yes = "#20854E")),
  hf      = list(group = hf_all, legend = "hp.cutoff", title = "High-pass filter cutoff",
                 levels = c("<=0.1", ">0.1"), pal = c("<=0.1" = "#E41A1C", ">0.1" = "#377EB8")),
  ref     = list(group = ref_all, legend = "reference", title = "Reference choice",
                 levels = ref_schemes, pal = c(avg = "#E69F00", POz = "#56B4E9", mastoid = "#009E73"))
)

# team names that differ between the .mat files and the ID file
id_fix <- c("TheCodeMechanics"    = "The Code Mechanics",
            "19068f1fe266c5e1_1"  = "19068f1fe266c5e1",
            "356c77bfd2662b9a_H1" = "356c77bfd2662b9a",
            "ChileMaule"          = "Chile Maule",
            "CognitiveSystems_KU" = "CognitiveSystems-KU",
            "TMS_EEG_DREAM"       = "TMS-EEG-DREAM",
            "TheHanncanny"        = "The Hanncanny",
            "KaiserSoze"          = "Kaiser Soze")

# mean and squared SE across teams (columns) for every time point
group_stats <- function(sub) {
  data.frame(m   = rowMeans(sub, na.rm = TRUE),
             se2 = apply(sub, 1, var, na.rm = TRUE) / rowSums(!is.na(sub)))  # NA if only 1 team
}

for (i in c(1:4)) {
  
  # read in our data
  amp <- readMat(paste0(inDir,all_amp[i]))
  amp <- as.matrix(amp[[in_amp_name[i]]])   # rows are time points and columns are teams
  
  time_vec <- as.numeric(unlist(readMat(paste0(inDir,all_time[i]))[[in_time_name[i]]]))
  
  ID_diff <- unlist(readMat(paste0(inDir,all_IDs[i]))[[in_ID_name[i]]])
  to_fix <- ID_diff %in% names(id_fix)
  ID_diff[to_fix] <- id_fix[ID_diff[to_fix]]
  
  # Match ID
  loc <- match(ID_diff, AQ_ID$teamID, nomatch = 0)
  if (any(loc == 0)) stop(paste("IDs are not matching:", paste(ID_diff[loc == 0], collapse = ", ")))
  
  ref_i <- ref_all[loc]
  
  for (color_value in color_values) {
    
    s <- settings[[color_value]]
    grp_i <- s$group[loc]
    
    # keep only teams with a known reference scheme and a known group
    keep <- ref_i %in% ref_schemes & grp_i %in% s$levels
    amp_k <- amp[, keep, drop = FALSE]
    ref_vec <- ref_i[keep]
    grp_vec <- grp_i[keep]
    
    if (color_value == 'ref') {
      
      # plain mean difference wave per reference scheme
      joined_plotting <- do.call(rbind, lapply(s$levels, function(g) {
        st <- group_stats(amp_k[, grp_vec == g, drop = FALSE])
        data.frame(group = g, time = time_vec, amp_value = st$m, se_value = sqrt(st$se2))
      }))
      subtitle <- NULL
      
    } else {
      
      # crosstab: which schemes contain both groups?
      tab <- table(factor(ref_vec, levels = ref_schemes), factor(grp_vec, levels = s$levels))
      cat("\n", hyp_name[i], "-", color_value, "\n"); print(tab)
      shared <- rownames(tab)[apply(tab > 0, 1, all)]
      if (length(shared) == 0) {
        warning(paste(hyp_name[i], color_value, ": no reference scheme contains both groups - skipped"))
        next
      }
      
      # weights: total number of teams per scheme (same weights for both groups)
      n_ref <- rowSums(tab)[shared]
      w <- n_ref / sum(n_ref)
      
      # group mean and squared SE within each scheme
      strat <- do.call(rbind, lapply(shared, function(r) {
        do.call(rbind, lapply(s$levels, function(g) {
          st <- group_stats(amp_k[, ref_vec == r & grp_vec == g, drop = FALSE])
          data.frame(reference = r, group = g, time = time_vec,
                     m = st$m, se2 = st$se2, w = as.numeric(w[r]))
        }))
      }))
      
      # standardized group waves
      joined_plotting <- strat %>%
        group_by(group, time) %>%
        summarise(amp_value = sum(w * m),
                  se_value  = sqrt(sum(w^2 * se2)),
                  .groups = "drop")
      subtitle <- paste0("standardized over reference")
    }
    
    joined_plotting$group <- factor(joined_plotting$group, levels = s$levels)
    
    # Plot difference wave
    p1 <- ggplot(joined_plotting, aes(x = time, y = amp_value, color = group, group = group)) +
      geom_ribbon(aes(ymin = amp_value - se_value,      # Shaded SE area
                      ymax = amp_value + se_value,
                      fill = group), alpha = 0.3, color = NA) +
      geom_line(linewidth = 1) +
      scale_color_manual(values = s$pal) +
      scale_fill_manual(values = s$pal) +
      labs(x = "Time (ms)", y = "Amplitude (µV)", title = s$title, subtitle = subtitle,
           color = s$legend, fill = s$legend) +
      theme_bw() +
      theme(text = element_text(size = 20), legend.text = element_text(size = 15),
            legend.title = element_text(size = 15, margin = margin(b = 10)),
            plot.subtitle = element_text(size = 12, colour = "grey30")) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "grey", linewidth = 1) +
      geom_hline(yintercept = 0, color = "grey", linewidth = 0.5) +
      annotate(geom = "text", x = -30, y = stim_y_coord[i],
               label = "stimulus", color = "#696969", angle = 90, size = 5) +
      annotate("rect", xmin = xmin_rect[i], xmax = xmax_rect[i], ymin = -Inf, ymax = Inf, alpha = .1)
    
    print(p1)
    ggsave(p1, file = paste0(outDir, "avg_diff_wave_", hyp_name[i], "_", color_value,
                             ifelse(color_value == 'ref', "", "_refadj"), ".png"),
           width = 6.5, height = 3.8, units = "in")
  }
}