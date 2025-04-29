clear all, close all
addpath 'C:\Users\ecesnait\Desktop\EEGManyPipelines\Matlab Scripts\toolboxes\fieldtrip-master'
addpath 'C:\Users\ecesnait\Desktop\EEGManyPipelines\git\EEGManyPipes org\Main_Paper\johannes\timeseries'
ft_defaults;

dataDir = 'Z:\ecesnait\EMP\EMP time series exp\TimelockAVG\';
data = dir([dataDir,'*.mat']);

ID = extractBetween([data.name],'_','.')
load('chan_labels_for_plotting')

bsl_correct = 1
bstart = -200;
bstop  = 0;

allgrpdat = {}

for i = 1:length(data)%something went wrong with subject 20
    if i==20
        continue
    end
    %% Load data %%
    load([dataDir,data(i).name])

    % Remove empty cells (if a tream excluded a subject)
    indx_empty = cellfun(@isempty,alldatavg(1,:))
    alldatavg(:,indx_empty) = [];

    % channels should be in the same order
    for s = 1:length(alldatavg)
        %         if isempty(alldatavg{1,s}); alldatavg(:,s) = []; continue; end
        if ~isequal(alldatavg{1,s}.label,chan_labels)
            error('Elena: Inspect channel order')
        end
    end

    if bsl_correct
        for ss = 1:length(alldatavg)
            xstart = dsearchn(alldatavg{1,ss}.time', bstart);
            xstop  = dsearchn(alldatavg{1,ss}.time', bstop);

            bsl = nanmean(alldatavg{1,ss}.avg(:,xstart:xstop),2); % mean for each channel over the time window between -100 and 0
            bsl = repmat(bsl, [1, length(alldatavg{1,ss}.time)]);
            bsl_cor_data = alldatavg{1,ss}.avg - bsl;
            alldatavg{1,ss}.avg = bsl_cor_data
            clear bsl_cor_data bsl
        end
    end
    %% Run timelock grand average ERPs for each team %%
    %
    allgrpdat{i}= ft_timelockgrandaverage([], alldatavg{1,:})

    clear alldatavg
end

%% Remove outliers and save%%
% out_indx = [34]
% ID(out_indx)=[]
% figure, plot(allgrpdat{35}.time,allgrpdat{34}.avg)
% allgrpdat(out_indx)=[]

allgrpdat(20)=[]
ID(20) = []
save('data_avg_allteam031224', 'allgrpdat', '-v7.3')

%% Plot data and create the matrix for all GA ERPs
%allgrpdat{32}.avg = allgrpdat{32}.avg*10^6;
GA_ERP_matrix = nan(length(allgrpdat),64,205);
fig=figure
for i=1:length(allgrpdat) % 32,33,37-not sure about the scale, it is in 100s and -70uV
    if ismember(i,[45,47,57])
        continue
    end
        indx_cpz =find(ismember(allgrpdat{i}.label, 'CPz'))
        label{i} = allgrpdat{i}.label(indx_cpz) % just a sanity check
    if abs(nanmean(allgrpdat{i}.avg(indx_cpz,:)))>5*10
        error('inspect')
        figure, plot(allgrpdat{i}.time,allgrpdat{i}.avg(indx_cpz,:))
    elseif abs(nanmean(allgrpdat{i}.avg(indx_cpz,:))) < 10^-3
        allgrpdat{i}.avg = allgrpdat{i}.avg * 10^6  % bring back to uV
        %error('check data')
    end
    plot(allgrpdat{i}.time,allgrpdat{i}.avg(indx_cpz,:),'Color',[0.4, 0.4, 0.4, 0.2]),hold on
    % matrix for team x channel x time
    GA_ERP_matrix(i,:,:) = allgrpdat{i}.avg;
end
save('GA_ERP_matrix','GA_ERP_matrix')
% calculate the median GA ERP across teams for each time point and distance
% to it
medianERP = nan(64,205) %channels x time
%distance_across_time = nan(111,64) % teams x channels
diff_matrix_all_teams = nan(size(GA_ERP_matrix,1),64,205);

for ch = 1:64
    medianERP(ch,:) = nanmedian(squeeze(GA_ERP_matrix(:,ch,:)))

    % the distance to the median GA ERP for each team
    diff_matrix_all_teams(:,ch,:) = abs(squeeze(GA_ERP_matrix(:,ch,:)) - medianERP(ch,:));% for each channel time point
    distance_across_time(:,ch) = nanmedian(squeeze(diff_matrix_all_teams(:,ch,:)),2); % median across time
    %     figure, plot(allgrpdat{i}.time,medianERP)
end

figure, plot(allgrpdat{i}.time, squeeze(diff_matrix_all_teams(:,18,:))) % median ERP for all channels. Plot in R  ?
save('abs_diff_matrix_all_teams','diff_matrix_all_teams')


GA_ch_ERP=array2table(medianERP)
writetable(GA_ch_ERP, 'channel_GA_ERP_112_teams.csv', 'Delimiter',',')
writetable(array2table(ID),'GA_ERP_112_ID.csv', 'Delimiter',',')

writetable(table(distance_across_time),'channel_distance_to_median_112.csv','Delimiter',',')
test_distance = readtable('channel_distance_to_median_112.csv','Delimiter',',')
test_distance = table2array(test_distance)
test_distance(indx_h3,:) = []
% calculate the mean GA ERP across teams for each time point
% medianERP = nanmedian(GA_ERP_matrix)
% GAERPT=array2table(GA_ERP_matrix)
% writetable(GAERPT, 'GA_ERP_74.csv', 'Delimiter',',')
% writetable(array2table(ID),'GA_ERP_74_ID.csv', 'Delimiter',',')
% plot(allgrpdat{i}.time,medianERP,'Color','blue','LineWidth',1.5)
% saveas(fig, 'GA ERPs and median 74.png')

% figure, plot(allgrpdat{i}.time,abs(GA_ERP_matrix),'Color',[0.4, 0.4, 0.4, 0.5]), hold on
% plot(allgrpdat{i}.time,abs(medianERP))

% Calculate the distance to the median GA ERP for each team
diff_matrix = abs(GA_ERP_matrix-medianERP)%
med_all1=nanmedian(diff_matrix,2) % median across time
writetable(table(med_all1),'distance_to_median_77.csv','Delimiter',',')

% add team info
teamIDs_full = extractBetween({data.name},'_','.mat')
%teamIDs_full(29) = []
distance_med_GA_ERP = table(teamIDs_full',med_all1)
writetable(distance_med_GA_ERP,'distance_med_GA_ERP_74.csv','Delimiter',',')
ID(20) = []
writetable(table(ID),'distance_med_GA_ERP_ID.csv')

% med_all = nanmedian(abs(medianERP))
% med_team = nan(length(allgrpdat),1)
% for t=1:length(allgrpdat)
%      if i==32
%         continue
%      end
%      med_team(t) = med_all - nanmedian(abs(GA_ERP_matrix(t,:)));
% end
