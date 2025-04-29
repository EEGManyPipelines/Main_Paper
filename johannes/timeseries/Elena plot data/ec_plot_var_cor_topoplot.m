
clear all, close all
addpath C:\Users\ecesnait\Desktop\EEGManyPipelines\Matlab Scripts\toolboxes\eeglab_current\eeglab2022.0
eeglab

% Load data
coef = readtable("C:\Users\ecesnait\Desktop\EEGManyPipelines\git\EEGManyPipes org\Main_Paper\johannes\timeseries\Elena plot data\chan_var_preproc_GA_ERP_med_cor.csv")
pval = readtable("C:\Users\ecesnait\Desktop\EEGManyPipelines\git\EEGManyPipes org\Main_Paper\johannes\timeseries\Elena plot data\chan_var_preproc_GA_ERP_med_pval.csv")
load('C:\Users\ecesnait\Desktop\EEGManyPipelines\git\EEGManyPipes org\Main_Paper\johannes\timeseries\chan_labels_for_plotting')

% Get channel coordinates
chan_names = struct('labels', chan_labels); % use BESA file int he drop down window

chan_locs = pop_chanedit(chan_names)

chan_locs_T = struct2table(chan_locs)

% Plot the topography for processing steps
figurepath = 'C:\Users\ecesnait\Desktop\EEGManyPipelines\Figures\'
fig=figure
topoplot(coef.x, chan_locs, 'electrodes', 'labels', 'maplimits', 'maxmin'), 

h=colorbar;
yt=get(h,'XTick');
set(h,'XTickLabel',yt)
h.FontSize = 15
colormap winter

saveas(fig, [figurepath, 'var_pipeline_erp_dist_cor_topoplot.jpg']); % this line saves the last figure

% Plot the topography for the order of steps
% Load data
coef = readtable("C:\Users\ecesnait\Desktop\EEGManyPipelines\git\EEGManyPipes org\Main_Paper\johannes\timeseries\Elena plot data\chan_var_order_GA_ERP_med_cor.csv")
pval = readtable("C:\Users\ecesnait\Desktop\EEGManyPipelines\git\EEGManyPipes org\Main_Paper\johannes\timeseries\Elena plot data\chan_var_order_GA_ERP_med_pval.csv")

figurepath = 'C:\Users\ecesnait\Desktop\EEGManyPipelines\Figures\'
fig=figure
topoplot(coef.x, chan_locs, 'electrodes', 'labels', 'maplimits', 'maxmin'), 

h=colorbar;
yt=get(h,'XTick');
set(h,'XTickLabel',yt)
h.FontSize = 15
colormap winter

saveas(fig, [figurepath, 'var_order_erp_dist_cor_topoplot.jpg']); % this line saves the last figure

