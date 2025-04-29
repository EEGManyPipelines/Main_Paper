%restoredefaultpath;matlabrc
clear all, close all

% <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
% data in the standardized (ftp) format
% downloaded from sciebo:
% https://gast.sciebo.de/index.php/apps/files?dir=/EEGManyPipelines/EMP%20time%20series%20exp/TimelockAVG
project_path = 'Z:\ecesnait\EMP\EMP time series exp\'
data_dir = fullfile(project_path, "TimelockAVG")


%cd(data_dir)
team_list = dir(fullfile(data_dir,'sbjavg*.mat'))  % 113 teams
load('chan_labels_for_plotting')
chan_cell = chan_labels

%chan_cell = alldatavg{1,1}.label

FS              = 256; % sampling rate in Hz
timeWindow      = [-200 600]; % consistent trial epoching (in ms)
timeVec_standard1 = flip([ 0 : -1/FS*1000 : timeWindow(1) ]);
timeVec_standard2        = [ 0 : 1/FS*1000 : timeWindow(2) ];
time_vector         = [ timeVec_standard1(1:end-1), timeVec_standard2 ];
disp(time_vector(1:length(time_vector)-1:end))

%time_vector = alldatavg{1,1}.time

n_chan = length(chan_cell);
n_sample = length(time_vector);
n_subj = 33;

data_avg_allteam =[];
data_median_allteam =[];
data_var_allteam =[];

i_chan = find(ismember(chan_cell,'CPz')) %CPz

%% GA ERP across all epochs for each team %%
% Not used for correlations later on
% cycle across all teams
for i_team=1:length(team_list)
    %ii=71
    team_name = team_list(i_team).name(1:end-4)
    load(fullfile(data_dir, team_list(i_team).name));

    % initialize 3d arrays with NaN
    data_team_allsubj = NaN(n_subj, n_chan, n_sample);
    data_var_allsubj = NaN(n_subj, n_chan, n_sample);
    position_tmp_orig =[];

    for i_subj = 1:size(alldatavg,2)

        if ~isempty(alldatavg{1,i_subj})

            % find corresponding channel position:
            position_tmp =[];
            [~, position_tmp] = ismember(chan_cell, alldatavg{1,i_subj}.label);
            %chan_cell{i_chan}
            alldatavg{1,i_subj}.label(position_tmp(i_chan))
            if i_subj ==1; position_tmp_orig = position_tmp; end

            if isequal(position_tmp_orig, position_tmp)
                if length(position_tmp) == n_chan
                    %if isequal(alldatavg{1,i_subj}.label, chan_cell)
                    data_avg_tmp =[];
                    data_avg_tmp = alldatavg{1,i_subj}.avg;
                    % re-order the data_avg 2d (chan x timepoint)
                    % with a common channel vector
                    data_team_allsubj(i_subj,:,:) = data_avg_tmp(position_tmp,:);

                    data_var_tmp =[];
                    data_var_tmp = alldatavg{1,i_subj}.var;
                    data_var_allsubj(i_subj,:,:) = data_var_tmp(position_tmp,:);

                else
                    disp([ '!!! problem with CHAN in: ' team_name ' subj: ' num2str(i_subj)])
                end
            end

        else
            disp(['!!! subj ' num2str(i_subj) ' in ' team_name ' is empty'])
        end
    end

    data_avg_allteam(i_team,:,:) = squeeze(nanmean(data_team_allsubj,1));
    data_median_allteam(i_team,:,:) = squeeze(nanmedian(data_team_allsubj,1));

    data_var_allteam(i_team,:,:) = squeeze(nanmedian(data_var_allsubj,1));
    clearvars alldatavg data_avg_tmp

end

%% Find NaN
xxx = isnan(data_avg_allteam(:,1,1))
indx_nan = find(xxx)
{team_list(indx_nan).name}'
to_keep = [66,28] % NaNs only in time or channel domain
indx_nan = setdiff(indx_nan, to_keep)
data_avg_allteam(indx_nan,:,:) = []; % remove teams that have nan values, resulting N=106
team_list(indx_nan) = []


%% FIND ALL PEAKS AND DROPS

% addpath(genpath('C:\Users\ecesnait\Desktop\EEGManyPipelines\git\Robust-Correlations-2\Robust-Correlations-2'))

% get peaks and drops for wach channnel, each subject, each team
basl_ref_cor = 1
bstart = -200
bstop = 0
for i_team =1:length(team_list)

    team_name = extractBetween( team_list(i_team).name, '_','.');

    disp([strcat("## Loading team ", team_name, ", number ", string(i_team), ' ###')])

    load(fullfile(data_dir, team_list(i_team).name));  %-> alldataavg

    for i_subj = 1:n_subj
        if ~isempty(alldatavg{1,i_subj}) % if subject data exists

            data_avg_tmp =[];
            data_avg_tmp = alldatavg{1,i_subj}.avg; %Averaged ERPs of one subject in each channel

            % apply average reference
            if basl_ref_cor
                new_avg_ref = data_avg_tmp - nanmean(data_avg_tmp,1);

                % baseline correct
                xstart = dsearchn(alldatavg{1,i_subj}.time', bstart);
                xstop  = dsearchn(alldatavg{1,i_subj}.time', bstop);

                bsl = nanmean(new_avg_ref(:,xstart:xstop),2); % the shift we saw before was because we averaged data with the old reference but applied it to avg ref
                bsl = repmat(bsl, [1, length(alldatavg{1,i_subj}.time)]);
                new_ref_bsl = new_avg_ref - bsl;

                data_avg_tmp = new_ref_bsl;
            end

            if abs(nanmean(max(data_avg_tmp))) < 10^-4 % check if the data is in V and bring it back to uV
                data_avg_tmp = data_avg_tmp .* 10^6;
            elseif abs(nanmean(max(data_avg_tmp))) > 50 % unexplicable scales
                continue
            end

            % find maxima of the peaks
            for i_chan = 1:n_chan
                time_sbj = alldatavg{1,i_subj}.time;

                [TF P] = islocalmax(data_avg_tmp(i_chan,:),'MinProminence',0.5) ;% second argument is min prominence required in uV
                peak_prominence{i_team,i_subj,i_chan} = P(TF);
                peak_latency_ms{i_team,i_subj,i_chan} = time_sbj(TF);
                max_time{i_chan} = TF;

                [TF2 P2] = islocalmin(data_avg_tmp(i_chan,:),'MinProminence',0.5);
                drop_prominence{i_team,i_subj,i_chan} = P2(TF2);
                drop_latency_ms{i_team,i_subj,i_chan} = time_sbj(TF2);
                min_time{i_chan} = TF2;
                clearvars TF TF2 P P2
            end
            %pick a random channel and plot it for sanity check
            % rand_ch = randi([1,64],1);
            %
            % fig = figure('visible','off'),plot(time_sbj, data_avg_tmp(rand_ch,:),time_sbj(max_time{rand_ch}),data_avg_tmp(rand_ch,max_time{rand_ch}),'b*'), hold on,
            % plot(time_sbj, data_avg_tmp(rand_ch,:),time_sbj(min_time{rand_ch}),data_avg_tmp(rand_ch,min_time{rand_ch}),'r*'), title(strcat(team_name,", team =", string(i_team)))
            % exportgraphics(fig, 'GA_ERP_min_max_peaks.pdf','Append',true)
            % close(fig)
            % clearvars max_time min_time rand_ch
        else
            disp([ '!!! problem with CHAN in: ' team_name ' subj: ' num2str(i_subj)])
            continue
        end; %end

    end


end
%
% save('C:\Users\ecesnait\Desktop\EEGManyPipelines\git\EEGManyPipes org\Main_Paper\johannes\timeseries\GA ERP peak params\peak_prominence.mat','peak_prominence')
% save('C:\Users\ecesnait\Desktop\EEGManyPipelines\git\EEGManyPipes org\Main_Paper\johannes\timeseries\GA ERP peak params\peak_latency_ms.mat','peak_latency_ms')
% save('C:\Users\ecesnait\Desktop\EEGManyPipelines\git\EEGManyPipes org\Main_Paper\johannes\timeseries\GA ERP peak params\drop_prominence.mat','drop_prominence')
% save('C:\Users\ecesnait\Desktop\EEGManyPipelines\git\EEGManyPipes org\Main_Paper\johannes\timeseries\GA ERP peak params\drop_latency_ms.mat','drop_latency_ms')
team_IDs = extractBetween( {team_list.name}, '_','.')
% save('C:\Users\ecesnait\Desktop\EEGManyPipelines\git\EEGManyPipes org\Main_Paper\johannes\timeseries\GA ERP peak params\team_IDs.mat','team_IDs')


exclude = 25 % team has uploaded data for H1 and H3. H3 is now team 25
peak_prominence(exclude,:,:) = [];
peak_latency_ms(exclude,:,:) = [];
drop_prominence(exclude,:,:) = [];
drop_latency_ms(exclude,:,:) = [];
team_IDs(exclude) = [];
% get peak prominense only for a negaive peak around 80-120ms and positive peak
% just after it

%% TIME WINDOW OF INTEREST for the negative and positive peak
% finding the max of the peak in a pre-defined time window
% in msec - - - - - - - -

neg_start = 80 %in msec: first NEGative peak
neg_end = 150 %170 %160 %in msec: first POSitive peak
pos_start = 120 %msec around the peak
pos_end = 200

for ss= 1:33

    % team x subject x channels
    one_sbj_all_drop = squeeze(drop_latency_ms(:,ss,:)); % teams are rows, channels columns
    one_sbj_all_drop_prom = squeeze(drop_prominence(:,ss,:)); % teams are rows, channels columns

    one_sbj_all_peak =     squeeze(peak_latency_ms(:,ss,:));
    one_sbj_all_peak_prom = squeeze(peak_prominence(:,ss,:)); % teams are rows, channels columns

    for tt = 1:length(one_sbj_all_drop)
        for ch = 1:64

            % LOOK FOR THE BIGGEST NEGATIVE DEPLETION IN A TIME WINDOW
            % BETWEEN 80 AND 150MS
            one_t_ch = one_sbj_all_drop{tt,ch}; % one team one channel for negative depl
            indx_neg =  find(ismember(one_t_ch, one_t_ch(one_t_ch>= neg_start & one_t_ch <=neg_end)));

            one_t_ch_prom = one_sbj_all_drop_prom{tt,ch}; % prominence of one channel one team
            indx_most_prom_neg = find(one_t_ch_prom(indx_neg)==max(one_t_ch_prom(indx_neg))); % take the wave with a more negative peak
            neg_latency_ms{tt,ch} = one_t_ch(indx_neg(indx_most_prom_neg));
            neg_prom{tt,ch} = one_t_ch_prom(indx_neg(indx_most_prom_neg));

            % LOOK FOR THE BIGGEST peak IN A TIME WINDOW
            % BETWEEN 150 and 200MS
            one_t_ch_peak_ms = one_sbj_all_peak{tt,ch}; % one team one channel for negative depl
            indx_pos =  find(ismember(one_t_ch_peak_ms, one_t_ch_peak_ms(one_t_ch_peak_ms>= pos_start & one_t_ch_peak_ms <=pos_end)));

            one_t_ch_peak_prom = one_sbj_all_peak_prom{tt,ch}; % one team one channel for peaks
            indx_most_prom_peak = find(one_t_ch_peak_prom(indx_pos)==max(one_t_ch_peak_prom(indx_pos))); % take the wave with a more negative peak
            pos_latency_ms{tt,ch} = one_t_ch_peak_ms(indx_pos(indx_most_prom_peak));
            pos_prom{tt,ch} = one_t_ch_peak_prom(indx_pos(indx_most_prom_peak));
            clearvars one_t_ch_peak_ms  indx_pos one_t_ch_peak_prom indx_most_prom_peak indx_neg one_t_ch_prom one_t_ch indx_most_prom_neg
        end
    end
    subj_peaks{ss}.latency_ms = pos_latency_ms;
    subj_peaks{ss}.prominence = pos_prom;

    subj_drops{ss}.latency_ms = neg_latency_ms;
    subj_drops{ss}.prominence = neg_prom;

    clearvars pos_latency_ms pos_prom neg_latency_ms neg_prom

end

% #################################################
% compute (ROBUST) CORRELATION (for a single channel)
% at the peaks of interest
% #################################################
%% LOAD pipeline distance matrics
%distance_dir = fullfile(project_path, '_corr_ERP_DISTANCE_graph')
% step_struct

load("distance_from_graph_path_ref_BCFHMN.mat")

% Match IDs
ID_match_step = cellstr(step_struct.ID)

team_IDs(ismember(team_IDs, 'TheCodeMechanics')) = {'The Code Mechanics'}
team_IDs(ismember(team_IDs, '356c77bfd2662b9a_H1')) = {'356c77bfd2662b9a'}
team_IDs(ismember(team_IDs, '19068f1fe266c5e1_1')) = {'19068f1fe266c5e1'}

indx_ID = ismember(team_IDs', ID_match_step)
ID_match_team = team_IDs(indx_ID)
[~,pl_ID] = ismember(ID_match_team, ID_match_step)

[ID_match_team', ID_match_step(pl_ID)]
isequal(ID_match_team', ID_match_step(pl_ID))

ID_step_matched = ID_match_step(pl_ID)
ID_match_team = ID_match_team'


order_steps = step_struct.distance_allteam'
order_steps = order_steps(pl_ID)

fig_flag = 0;

%% Correlate unusualness of the order of steps to the rpominence of the largest negative depletion around 100ms %%
% Per subject
effect_negative_drop = nan(33,64)
signific_negative_drop = nan(33,64);

effect_negative_drop_lat = nan(33,64);
signific_negative_drop_lat = nan(33,64);

for subj = 1:33
    for chan = 1:64
        
        one_chan_neg_prom = subj_drops{1,subj}.prominence(:,chan);
        one_chan_neg_latency = subj_drops{1,subj}.latency_ms(:,chan);

        indx_nan = find(cellfun(@isempty, one_chan_neg_prom));
        if numel(indx_nan) > 60 %more than half people do not have it
            sum_nan_ch(subj,chan) = numel(indx_nan)
            continue
        end
        order_steps_tmp = order_steps;
        order_steps_tmp(indx_nan) = []; % exchange NaN prominence with 0

        % remove outliers
        erp_prominence_one_ch = [subj_drops{1,subj}.prominence{:,chan}]';
        erp_latency_one_ch = [subj_drops{1,subj}.latency_ms{:,chan}]';

        indx_out = find(erp_prominence_one_ch >= nanmean(erp_prominence_one_ch)+std(erp_prominence_one_ch)*3); % outlier is every data point that is 3sd away from the mean

        erp_prominence_one_ch(indx_out) = [];
        erp_latency_one_ch(indx_out) = [];

        order_steps_tmp(indx_out) = [] ;
        corr_value_array_prom = [ erp_prominence_one_ch , order_steps_tmp ];
        corr_value_array_lat = [ erp_latency_one_ch , order_steps_tmp ];

        % remove NaN
        corr_value_array_prom(any(isnan(corr_value_array_prom), 2), :) = [];
        corr_value_array_lat(any(isnan(corr_value_array_lat), 2), :) = [];

        [r_prom,p_prom] = corr(corr_value_array_prom(:,1), corr_value_array_prom(:,2));
         [r_lat,p_lat] = corr(corr_value_array_lat(:,1), corr_value_array_lat(:,2));

        %figure,scatter(corr_value_array_prom(:,1),corr_value_array_prom(:,2) ), xlabel('prominence'), ylabel('order of steps')

        effect_negative_drop(subj,chan) = r_prom;
        signific_negative_drop(subj,chan) = p_prom;

        effect_negative_drop_lat(subj,chan) = r_lat;
        signific_negative_drop_lat(subj,chan) = p_lat;
        clearvars  indx_nan corr_value_array erp_latency_one_ch erp_prominence_one_ch indx_out one_chan_neg_prom one_chan_neg_latency order_steps_tmp
    end
end

%mask matrix for prominence correlations
indx_sig = signific_negative_drop < 0.05
mask_significance  = zeros(33,64)
mask_significance(indx_sig) = signific_negative_drop(indx_sig)
mask_effect = zeros(33,64)
mask_effect(indx_sig) = effect_negative_drop(indx_sig)

%mask matrix for latency correlations
indx_sig_lat = signific_negative_drop_lat < 0.05
mask_significance_lat  = zeros(33,64)
mask_significance_lat(indx_sig_lat) = signific_negative_drop_lat(indx_sig_lat)
mask_effect_lat = zeros(33,64)
mask_effect_lat(indx_sig_lat) = effect_negative_drop_lat(indx_sig_lat)

sum(indx_sig)

% Unusualness matrix
erp_teams_prom = cell(length(team_IDs),33,64)
erp_teams_lat = cell(length(team_IDs),33,64)

for subj = 1:33
    sbj_erp_teams_prom = cell(length(team_IDs),64);
    sbj_erp_teams_lat = cell(length(team_IDs),64);
    for chan = 1:64
        
        one_chan_neg_prom = subj_drops{1,subj}.prominence(:,chan);
        one_chan_neg_latency = subj_drops{1,subj}.latency_ms(:,chan);

        sbj_erp_teams_prom(:,chan) = one_chan_neg_prom; % single sibject data teams x channels
        sbj_erp_teams_lat(:,chan) =one_chan_neg_latency; 
    end
    erp_teams_prom(:,subj,:) = sbj_erp_teams_prom; % 3D stucture with all teams, all subjects, all channels. I'll take a mean over subjects later
    erp_teams_lat(:,subj,:) = sbj_erp_teams_lat; % 3D stucture with all teams, all subjects, all channels. I'll take a mean over subjects later

    clearvars  sbj_erp_teams_prom sbj_erp_teams_lat

end

%mean over subjects
one_team_mean_sbj = nan(length(team_IDs),64)

data = erp_teams_lat % erp_teams_lat OR erp_teams_prom
for i_team = 1 : 112
    one_team_data_cell = squeeze(data(i_team,:,:));
    one_team_data_nan = str2double(squeeze(data(i_team,:,:)));

    indx_empty = cellfun(@isempty, squeeze(data(i_team,:,:)));
    one_team_data_nan(~indx_empty) = cell2mat(one_team_data_cell(~indx_empty));

    one_team_mean_sbj(i_team,:) = nanmean(one_team_data_nan,1);
    clearvars indx_empty one_team_data_nan one_team_data_cell
end

one_team_mean_sbj_table = array2table(one_team_mean_sbj)
one_team_mean_sbj_table.Properties.VariableNames = chan_cell
one_team_mean_sbj_table = [team_IDs', one_team_mean_sbj_table]

writetable(one_team_mean_sbj_table, 'mean_neg_peak_latency_channel_all_teams.csv', 'Delimiter',',')

% distance matrix 
column_mean = nanmean(one_team_mean_sbj)
dist_mean = abs(one_team_mean_sbj - column_mean)

for p = 1:64
    cor_dist = [order_steps,dist_mean(:,p)]
    
    % remove NaN
    cor_dist(any(isnan(cor_dist), 2), :) = [];

            indx_out = find(cor_dist(:,2) >= nanmean(cor_dist(:,2))+std(cor_dist(:,2))*3); % outlier is every data point that is 3sd away from the mean
cor_dist(indx_out,:) = []

    [r_dist,p_dist] = corr(cor_dist(:,1),cor_dist(:,2))
    all_p(p) = p_dist
    all_r(p)=r_dist
end

%% CHECK RESULTS 55
% The slope of the line can be set to the correlation value
% For simplicity, let's plot a line with the slope equal to r
%y = r * x; % Line equation y = r * x

ERP_pval = NaN([64,2])

for i_chan = 1:n_chan

    peak_vector_tmp =[];
    peak_vector_tmp = effect_negative_drop(:,i_chan) %squeeze(peak_vector(i_chan,:,:));

    if nanmedian(peak_vector_tmp) > 0
        [ ~, ERP_pval(i_chan,1) ] = ttest(peak_vector_tmp, 0);
    else
        ERP_pval(i_chan,1) = NaN;
    end

    % if nanmedian(peak_vector_tmp(subj_vector, 9)) < 0
    %     %[h, p] = ttest(peak_vector_tmp(subj_vector, 9), 0)
    %     [ ~, ERP_pval(i_chan,2) ] = ttest(peak_vector_tmp(subj_vector, 9), 0);
    % else
    %     ERP_pval(i_chan,2) = NaN;
    % end

end

% - - - - - - - --
% add eeglab
%load('EMP01_clean.mat')
cd('C:\Users\ecesnait\Desktop\EEGManyPipelines\Matlab Scripts\toolboxes\eeglab_current\eeglab2022.0')
eeglab
%EEG = data; eeglab redraw

load('C:\Users\ecesnait\Desktop\BUSCHLAB\CORENATS\Matlab codes\MasterThesisElisabeth\EEG analyze\chan_locs.mat');

figure; colormap(hot)
subplot 121;
% topoplot(ERP_pval(:,1), chan_locs, 'maplimits', [min(ERP_pval(:,1)) max(ERP_pval(:,1))], 'electrodes', 'labels', 'colormap', flipud(hot) ); colorbar
title('100 ms (pos corr)')
subplot 122
topoplot(ERP_pval(:,2), chan_locs, 'maplimits', [0 0.1], 'electrodes', 'labels', 'colormap', flipud(hot) ); colorbar
title('160 ms (neg corr)')