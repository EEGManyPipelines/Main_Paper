# Regression analysis across time x channels

Load the standardised data with all team x subj x channel x time data and do regression analysis across all channels and time. Overall idea: have analysis steps as fixed effects and include team and subject as random effects.  

## First version 
First purpose was to quantify variation due to team and subj (based on comments from advisory board). This pipeline use a mix of MATLAB to arrange and plot data and R for the regression analysis. Limitation is that significance is done one a timep- x channel-point basis, which mean that the proper p-values should be Bonferroni corrected, thus very very conservative.

Order of steps:
1) Arange the standardised ERP data with `mcv_arrangeERPdat.m` - this step might be overlapping with what is already done with other analyses in EMP.
Also need to make sure that the channels order is correct per file. Input is folder with the saved standardised data. Output is a time x chan x team x subj matrix.
2) Run the mixed-model regression models with `fixed_effect_analysis.R`. Input is the output from #1. General form of analysis is y ~ X + reref + (1|subj) + (1|group).
This gives a model that estimated the "effect" of the variable X (add reref, but that is mostly there because a) rereferincing offest is trivial and b) it makes the model perform better). The random effects gives direct estimated os the variability of subject and team rrespectivly.
Currently there are several analyses within the same script - consider split so each analysis = one script. Exports a matrix with the beta values and the theta (random effect variance).
3) `mcv_plot_var.m` Takes the output of step 2 and create plots. Currently only imagesc plots for inspection. Should be made better with proper topoplots.

There are here also example scripts used to test out the methods. There were used intitialyy as a sandbox to test if the method would work (it does!):
* `var_over_time_example.R`: example data on simulated data. Demonstrate that the method works.
* `var_over_time_ERPs.R`: random effects only on real data. 

##  Second version
Second version is (an attempt) at implementing the same analysis as the first attempt, but in a more cohernt and easier to use way, using the *Unfold Julia* toolbox (www.unfoldtoolbox.org). This version make use of a version of Unfold that is still in development, which has the option to do cluster-based statistics, which is better for statistical inference on channel x times data - i.e., A solution to the multiple comparison problem in the first version.

Order of steps:
1) So far everythin is in the `mixed_models.jl` script.