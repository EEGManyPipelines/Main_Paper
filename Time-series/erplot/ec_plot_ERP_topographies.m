% Plot the topographic map for ERP amplitudes between 100 and 120ms post
% stimulus
clear all, close all
addpath 'C:\Users\ecesnait\Desktop\EEGManyPipelines\Matlab Scripts\toolboxes\eeglab_current\eeglab2022.0'
eeglab
%Load data
load('C:\Users\ecesnait\Desktop\EEGManyPipelines\git\EEGManyPipes org\Main_Paper\Time-series\erplot\N100\n1_amplitude_120.mat')

%Load channel locations of the dataset
load('chan_locs_CORENATS')
chan_locs = chan_locs(ismember({chan_locs.labels}, n1_amp.chan)) % take only the 64 scalp electrodes

% check if there are teams that have a amplitude scale in 10^-6
colum_mean = nanmean(abs(table2array(n1_amp(:,2:end))))
teams_low_scale = find(colum_mean<10^-5)%find teams that have this scale
n1_data_normalized = table2array(n1_amp(:,2:end));
n1_data_normalized(:,teams_low_scale) = n1_data_normalized(:,teams_low_scale)*10^6;

avg_n1_all_teams = nanmedian(n1_data_normalized, 2); % median across teams

fig=figure, 
topoplot(avg_n1_all_teams, chan_locs, 'electrodes', 'labels', 'maplimits', 'maxmin'), title('All teams', 'FontSize',17),
colorbar,clim([-3.5,3.5]),colormap("parula"),
c = colorbar
c.Label.String = 'uV'
c.FontSize = 17
saveas(fig,'GA_N1_topoplot_113.png')

%% Based on reference choice %%

AQ = readtable('C:\Users\ecesnait\Desktop\EEGManyPipelines\Data\Analysis questionnaire\Analysis questionnaire final sample 168 corrected.xlsx');
ref = AQ.ans_reref_method
ID = AQ.teamID
ref(cellfun(@isempty,ref)) = {'orig'}
num_cats = countcats(categorical(ref))
%[categories(categorical(ref)) num_cats] % most common categories avg, mastoid and orig sums up to 148 of cases

ID_avg = ID(ismember(ref, 'avg'))
ID_mastoid = ID(ismember(ref, 'mastoid'))
ID_orig = ID(ismember(ref, 'orig'))

IDs_ERP = n1_amp.Properties.VariableNames(2:end)

teams_n1_topo = n1_data_normalized% rows are channels, columns are teams


GGA_topo_avg_ref = teams_n1_topo(:,ismember(IDs_ERP,ID_avg))
avg_n1_avg = nanmedian(GGA_topo_avg_ref, 2)
fig=figure, topoplot(avg_n1_avg, chan_locs, 'electrodes', 'labels', 'maplimits', 'maxmin'), title('Average reference', 'FontSize',17),
colorbar,clim([-3.5,3.5]),colormap("parula"),
c = colorbar
c.Label.String = 'uV'
c.FontSize = 17
saveas(fig,['GA_N1_topoplot_avg_ref_',num2str(size(GGA_topo_avg_ref,2)) ,'sc.png'])

GGA_topo_mastoid_ref = teams_n1_topo(:,ismember(IDs_ERP,ID_mastoid))
avg_n1_mastoid = nanmedian(GGA_topo_mastoid_ref, 2)
fig=figure, topoplot(avg_n1_mastoid, chan_locs, 'electrodes', 'labels', 'maplimits', 'maxmin'), title('Mastoid reference', 'FontSize',17)
colorbar,clim([-3.5,3.5]),colormap("parula"),
c = colorbar
c.Label.String = 'uV'
c.FontSize = 17
saveas(fig,['GA_N1_topoplot_mastoid_ref_',num2str(size(GGA_topo_mastoid_ref,2)),'sc.png'])

GGA_topo_orig_ref = teams_n1_topo(:,ismember(IDs_ERP,ID_orig))
avg_n1_orig = nanmedian(GGA_topo_orig_ref, 2)
fig=figure, topoplot(avg_n1_orig, chan_locs, 'electrodes', 'labels', 'maplimits', 'maxmin'), title('Original reference', 'FontSize',17)
colorbar,clim([-3.5,3.5]),colormap("parula"),
c = colorbar
c.Label.String = 'uV'
c.FontSize = 17
saveas(fig,['GA_N1_topoplot_orig_ref_',num2str(size(GGA_topo_orig_ref,2)),'sc.png'])


