clear all, close all
addpath 'C:\Users\ecesnait\Desktop\EEGManyPipelines\Matlab Scripts\toolboxes\fieldtrip-master'
addpath 'C:\Users\ecesnait\Desktop\EEGManyPipelines\git\EEGManyPipes org\Main_Paper\johannes\timeseries'
ft_defaults;

% Load data
load('data_avg_allteam031224.mat') % time lock GA ERP
load('abs_diff_matrix_all_teams.mat')
lv_dist = readtable('C:/Users/ecesnait/Desktop/EEGManyPipelines/Data/Pipeline variability/levenstein_distance_order_steps.csv') %Levenstein distance for the order of steps

%% get IDs of analyst team ERP results %%
dataDir = 'Z:\ecesnait\EMP\EMP time series exp\TimelockAVG\';
data = dir([dataDir,'*.mat']);

ID = extractBetween([data.name],'_','.')
ID(20) = []
%correct IDs
ID{find(strcmp(ID,'19068f1fe266c5e1_1'))} = '19068f1fe266c5e1'
ID{find(strcmp(ID,'356c77bfd2662b9a_H1'))} = '356c77bfd2662b9a'
indx_h3 = find(strcmp(ID,'356c77bfd2662b9a_H3'))
ID(indx_h3) = []
allgrpdat(indx_h3) = [];
ID{find(strcmp(ID,'TheCodeMechanics'))} = 'The Code Mechanics'

%% Match LV distance to the order of ERP IDs %%
indx_missing = ismember(lv_dist.ID_order, ID)
lv_dist(~indx_missing,:) = [] % remove teams with no GA ERP data

[~,pl_lv] = ismember(ID,lv_dist.ID_order)
lv_dist = lv_dist(pl_lv,:)
isequal(lv_dist.ID_order, ID)

%% Exchange GA ERPs with team's distance to the median GA ERP for each channel %%
allgrpdat_diff_med = allgrpdat;

for ii = 1:length(allgrpdat_diff_med)
    if ismember(i,[45,47,57]) % teams with a strange amplitude that have no NaNs in the data only
        continue
    end
    allgrpdat_diff_med{ii}.avg = squeeze(diff_matrix_all_teams(ii,:,:));

    avg_ERP_diff_med_mat(ii,:) =  nanmean(squeeze(diff_matrix_all_teams(ii,:,:))); % average across channels
end

% average over time and correlate
median_erp_diff = nanmedian(diff_matrix_all_teams,3);
% 
% [rho, pval] = corr(test_distance , lv_dist.lv_dist,'rows','pairwise', Type = 'Spearman')
% pval'
% corr(test_distance(:,1) ,lv_dist.lv_dist , 'rows','pairwise', Type = 'Spearman')
% test_distance_no_nan = test_distance
% test_distance_no_nan(find(sum(isnan(test_distance),2)),:) = []
% lv_dist_no_nan = lv_dist.lv_dist 
% lv_dist_no_nan(find(sum(isnan(test_distance),2)),:) = []
% [rho2, pval2] = corr(test_distance_no_nan,lv_dist_no_nan, 'rows','complete', Type = 'Spearman')
% [rho, rho2]
% 
% hist(test_distance(:,44))
%% Initialize job:

% Initialize neighbors (needed only of not averaged over channels):

% Retrieve neighbours:
cfg_neighb          = [];
cfg_neighb.method   = 'distance';
cfg_neighb.elecfile = 'easycap-M1.txt';
cfg.neighbours      = ft_prepare_neighbours(cfg_neighb);
%cfg.channel         = job.channels; 

% Do not average over channels or timebins:
cfg.avgoverchan     = 'no';
cfg.avgovertime     = 'no';

%% Permutation test:

% Other settings:
cfg.parameter                       = 'avg';
cfg.method                          = 'montecarlo';
cfg.statistic                       = 'ft_statfun_correlationT';
cfg.type                            =  'Spearman';
cfg.alpha                           = 0.05;
cfg.correctm                        = 'cluster';
cfg.correcttail                     = 'prob';
cfg.numrandomization                = 1000;
n1 = 111
design(1,1:n1)       = lv_dist.lv_dist; %here we insert our independent variable (behavioral data) in the cfg.design matrix.
cfg.design           = design;
cfg.ivar             = 1;

[stat]                  = ft_timelockstatistics(cfg, allgrpdat{:});


%% 1 negative cluster with p=0.06
indx_first_clust = stat.negclusterslabelmat==1;

% time
first_clust_time = find(sum(indx_first_clust)) 
stat.time(first_clust_time) % between 164ms to 600ms 

first_clust_cahn = find(sum(indx_first_clust,2))
stat.label(first_clust_cahn) % 63 channels


figure,
plot(allgrpdat_diff_med{1}.time, avg_ERP_diff_med_mat),colororder(my_color_ordered), xlabel('time'), ylabel('uV'), title("Distance to median ERP(LV dist coloured)"),...
    fontsize(16,"points"),xregion([164, 600])

saveas(gcf,'cluster_stat_abs_unusualERP_lv_dist_col.png')


%% 1 positive cluster in ERP waveform
indx_first_clust = stat.posclusterslabelmat==1;

first_clust_time = find(sum(indx_first_clust)) 
stat.time(first_clust_time) % between 218ms to 503ms 

first_clust_cahn = find(sum(indx_first_clust,2))
stat.label(first_clust_cahn) % 32 channels, central parietal

%% Plot these two clusters in time %%
dist = lv_dist.lv_dist
dist = dist +1 % to avoid 0 for plotting colors
all_col = parula(10)
max(dist)
my_color_ordered = all_col(dist,:)

load('GA_ERP_matrix')
med_GA_ERP = squeeze(nanmedian(GA_ERP_matrix(:,first_clust_cahn,:),2));
figure,
plot(allgrpdat{1}.time, med_GA_ERP),colororder(my_color_ordered), xlabel('time'), ylabel('uV'), title("Median ERP(LV dist coloured)"),...
    fontsize(16,"points"),xregion([218, 503])

saveas(gcf,'cluster_stat_unusualERP_lv_dist_col.png')

