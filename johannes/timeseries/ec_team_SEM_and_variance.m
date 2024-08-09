clear all, close all
addpath 'C:\Users\ecesnait\Desktop\EEGManyPipelines\Matlab Scripts\toolboxes\fieldtrip-master'
ft_defaults;

dataDir = 'M:\EMP\EEGManyPipelines\EMP time series exp\TimelockAVG\';
data = dir([dataDir,'*.mat']);

violin = nan(64,length(data));%chan x mean variance across time
scatter = nan(length(data),205);% mean variance across channels x time
% load([dataDir,data(1).name])
% chan_labels = alldatavg{1}.label
% save('chan_labels_for_plotting','chan_labels')
load('chan_labels_for_plotting')
allgrpdat = {}
number_participants=[]
for i = 1:length(data)
    %% Load data %%
    load([dataDir,data(i).name])

    %% Violin plots: mean across time across subjects to see team distribution across channels %%
    % re-order channels
    indx_empty = cellfun(@isempty,alldatavg(1,:))
    if any(indx_empty)
        number_participants(i) = sum(~indx_empty);
    else
        number_participants(i) = 33;
    end
    alldatavg(:,indx_empty) = [];
    for s = 1:length(alldatavg)
        %         if isempty(alldatavg{1,s}); alldatavg(:,s) = []; continue; end
        if ~isequal(alldatavg{1,s}.label,chan_labels)
            [indx_mchan pl_mchan] = ismember(chan_labels,alldatavg{1,s}.label);
            chan_full = cell(1,length(chan_labels));
            chan_full(indx_mchan)=alldatavg{1,s}.label(pl_mchan(find(pl_mchan)));
            %   isequal(chan_full,chan_labels)
            copy_data = alldatavg{1,s}.var;
            order_data = copy_data(pl_mchan(find(pl_mchan)),:);
            alldatavg{1,s}.var = order_data;
        end
    end
    %run timelock grand average
    allgrpdat{i}= ft_timelockgrandaverage([], alldatavg{1,:})
    % median across subjects
    violin(:,i) = nanmedian(allgrpdat{i}.var,2);

    %     %% Scatter plots: mean across channels to see team distributions across time %%
    %     scatter(i,:) = nanmedian(allgrpdat{}.var);
    clear alldatavg indx_mchan pl_mchan chan_full copy_data order_data
end

%% Remove outliers %%
out_indx = [29, 33]
allgrpdat(out_indx)=[]
number_participants(out_indx)=[]

%% Standard error of measurement (SEm) across teams %%
%create a matrix for a single channel
SEM_cpz = nan(55,205)
for s = 1:length(allgrpdat)
    indx_cpz = find(ismember(allgrpdat{s}.label, 'CPz'))
    SEM_cpz(s,:) = abs(sqrt(allgrpdat{s}.var(indx_cpz,:)))/sqrt(number_participants(s))
end
% Count number of NaN per time point. This will be important for the script
% ec_sbj_SEM.m
count_nan=sum(isnan(SEM_cpz));
save('nan_SEM_55teams',"count_nan")
% outliers
outlier_rows = find(isoutlier(SEM_cpz(:,1)))
outlier_rows = [49,41] % after visual inspection
SEM_cpz(outlier_rows,:) = []

T=array2table(SEM_cpz)
writetable(T,'SEM_cpz_team.csv', 'Delimiter',',')

%plot
FS              = 256; % sampling rate in Hz
timeWindow      = [-200 600]; % consistent trial epoching (in ms)
timeVec_standard1 = flip([ 0 : -1/FS*1000 : timeWindow(1) ]);
timeVec_standard2        = [ 0 : 1/FS*1000 : timeWindow(2) ];
timeVec_standard         = [ timeVec_standard1(1:end-1), timeVec_standard2 ];
fig=figure()

plot(timeVec_standard,SEM_cpz,'Color',[0.4, 0.4, 0.4, 0.2]), hold on, plot(timeVec_standard,nanmean(SEM_cpz), 'LineWidth',1.5),...
    xlabel('time(msec)'), ylabel('SEM at CPz(uV)'),fontsize(fig, scale=1.6)  % 120, hold off
saveas(fig,'SEM_55teams.png')

%% Order channels to regions for the violin plot %%
chan_labels
front = [{'Fp1'}    {'AF7'}    {'AF3'}    {'F1'}    {'F3'}    {'F5'}    {'F7'}    {'FT7'}    {'FC5'}    {'FC3'}    {'FC1'} {'Fpz'}  ...
    {'Fp2'}    {'AF8'}    {'AF4'}    {'AFz'}    {'Fz'}    {'F2'}    {'F4'}    {'F6'}    {'F8'} {'FC6'}    {'FC4'} {'FC2'}    {'FCz'} ]
central = [ {'C1'}    {'C3'}    {'C5'} {'CP5'}    {'CP3'}    {'CP1'} {'CPz'} {'Cz'}    {'C2'}    {'C4'}    {'C6'} {'CP6'}    {'CP4'}    {'CP2'}]
parietal = [{'P1'}    {'P3'}    {'P5'}    {'P7'}    {'P9'}    {'PO7'}    {'PO3'} {'POz'} {'Pz'} {'P2'}    {'P4'}    {'P6'}    {'P8'} {'P10'}    {'PO8'}    {'PO4'} ]
occipital = [{'O1'}    {'Iz'}    {'Oz'} {'O2'}]
temporal = [{'T7'} {'TP7'} {'FT8'} {'T8'}    {'TP8'}]

mean_frontal = median(violin( ismember(chan_labels, front) ,:))
mean_central = median(violin( ismember(chan_labels, central) ,:))
mean_parietal = median(violin( ismember(chan_labels, parietal) ,:))
mean_occipital = median(violin( ismember(chan_labels, occipital) ,:))
mean_temporal = median(violin( ismember(chan_labels, temporal) ,:))

T_violin = table(mean_frontal',mean_central', mean_parietal', mean_occipital', mean_temporal')
T_violin.Properties.VariableNames = ["Frontal", "Central", "Parietal", "Occipital", "Temporal"]
writetable(T_violin,"variance_regions.csv", "Delimiter",",")

