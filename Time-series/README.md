# Regression analtsis across time x channels

Order of steps:
1) Arange the ordered ERP data with `mcv_arrangeERPdat.m` - this step might be overlapping with what is already done with other pipelines. 
Also need to make sure that the channels order is correct per file. Input is folder with the saved standardised data. Output is a time x chan x team x subj matrix.
2) Run the mixed-model regression models with `fixed_effect_analysis.R`. Input is the output from #1. General form of analysis is y ~ X + reref + (1|subj) + (1|group).
This gives a model that estimated the "effect" of the variable X (amd reref, but hta is mostly there because a) rereferincing offest is trivial and b) it makes the model perform better). The random effects gives direct estimated os the variability of subject and team rrespectivly.
Currently there are several analyses within the same script - consider split so each analysis = one script. Exports a matrix with the beta values and the theta (random effect variance).
3) `mcv_plot_var.m` Takes the output of #2 and create plots. Currently only imagesc plots. Should be made better with proper topoplots.

There are alos some example scripts used to test out the methods.
* `var_over_time_example.R`: example data on simulated data. Demonstrate that the method works.
* `var_over_time_ERPs.R`: random effects only on real data. 

