close all, clear all

project_path = 'Z:\ecesnait\EMP\EMP time series exp\'
data_dir = fullfile(project_path, "TimelockAVG")
cd(data_dir)
team_list = dir('sbjavg*.mat')  % 113 teams

cd('C:\Users\ecesnait\Desktop\EEGManyPipelines\git\EEGManyPipes org\Main_Paper\johannes\timeseries')
load('chan_labels_for_plotting')
chan_cell = chan_labels

%chan_cell = alldatavg{1,1}.label

FS              = 256; % sampling rate in Hz
timeWindow      = [-200 600]; % consistent trial epoching (in ms)
timeVec_standard1 = flip([ 0 : -1/FS*1000 : timeWindow(1) ]);
timeVec_standard2        = [ 0 : 1/FS*1000 : timeWindow(2) ];
time_vector         = [ timeVec_standard1(1:end-1), timeVec_standard2 ];
disp(time_vector(1:length(time_vector)-1:end))

n_chan = length(chan_cell);
n_sample = length(time_vector);
n_subj = 33;

data_avg_allteam =[];
data_median_allteam =[];
data_var_allteam =[];
%data_var2_allteam =[]; % nanstd
%data_var3_allteam =[]; % nanmad


% <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
% plot team-ERP for a specific channel

i_chan = find(ismember(chan_cell,'CPz')) %CPz

load("distance_from_graph_path_ref_BCFHMN.mat")

% - - - - - -  - - --  - - -
% extract distance value for each team in the team_list
distance_vector = NaN([length(team_list), 1]);
distance_norm_vector = NaN([length(team_list), 1]);
distance_step_vector = NaN([length(team_list), 1]);

step_struct_ID = cellstr(step_struct.ID);
n=1
for i_team =1:length(team_list)
    team_name = team_list(i_team).name(8:end-4)

    for kk=1:length(step_struct.ID)
        %kk=2
        if contains(step_struct_ID{kk,1}, team_name)
            disp(step_struct_ID{kk,1})

            distance_vector(i_team,1) = step_struct.distance_allteam(kk);
            distance_norm_vector(i_team,1) = step_struct.distance_norm_allteam(kk);
            distance_step_vector(i_team,1) = step_struct.n_step(kk);
            %index = strcmp({step_struct.ID}, team_name)
            Step_Ids(n) = step_struct_ID(kk,1)
            n=n+1
        end
    end
end
Step_Ids = Step_Ids'
teamsIDs = {team_list.name}'
% finding the max of the peak in a pre-defined time window
% in msec - - - - - - - -
%ERP_toi0 = 60 %in msec
ERP_toi1 = 90 %100 %90 %in msec: first NEGative peak
ERP_toi2 = 160 %170 %160 %in msec: first POSitive peak
%ERP_toi3 = 250 %in msec:
%ERP_toi4 = 350 %in msec:
%ERP_toi6 = 550 %in msec

delta_toi = 30 %msec

% convert into sample: - - - - - - - - - -
%[ ~, ERP_soi0 ] = min(abs(time_vector - ERP_toi0))
[ ~, ERP_soi1 ] = min(abs(time_vector - ERP_toi1))
[ ~, ERP_soi2 ] = min(abs(time_vector - ERP_toi2))
%[ ~, ERP_soi3 ] = min(abs(time_vector - ERP_toi3))
%[ ~, ERP_soi4 ] = min(abs(time_vector - ERP_toi4))

delta_soi = floor(delta_toi / (1/250*1000)) % considering a 30ms time window


addpath(genpath('C:\Users\ecesnait\Desktop\EEGManyPipelines\git\Robust-Correlations-2\Robust-Correlations-2'))

n_subj = 33
n_chan = length(chan_cell);
n_sample = length(time_vector);
n_team = length(team_list)

peak_vector =NaN([n_chan, n_subj, 10]);  %for storing all the results
%peak_vector =[]  %for storing all the results

%figure; hold on

for i_subj = 1:n_subj
    if any(i_subj == [16,19,20,23,28,30])
        continue
    end
    %for i_subj = 2:n_subj
    %for i_subj=3

    data_avg_singlesubj = NaN(n_team, n_chan, n_sample);
    data_var_singlesubj = NaN(n_team, n_chan, n_sample);

    for i_team=1:length(team_list)
        %ii=71
        team_name = team_list(i_team).name(1:end-4)
        load(fullfile(data_dir, team_list(i_team).name));  %-> alldataavg

        position_tmp_orig =[];

        if ~isempty(alldatavg{1,i_subj})

            % find corresponding channel position:
            position_tmp =[];
            [~, position_tmp] = ismember(chan_cell, alldatavg{1,i_subj}.label);
            %chan_cell{i_chan}
            alldatavg{1,i_subj}.label(position_tmp(i_chan))
            if i_subj ==1; position_tmp_orig = position_tmp; end

            %if isequal(position_tmp_orig, position_tmp)
            if length(position_tmp) == n_chan
                %if isequal(alldatavg{1,i_subj}.label, chan_cell)
                data_avg_tmp =[];
                data_avg_tmp = alldatavg{1,i_subj}.avg;

                if abs(nanmean(max(data_avg_tmp))) < 10^-4 % check if the data is in V and bring it back to uV
                    data_avg_tmp = data_avg_tmp .* 10^6;
                elseif abs(nanmean(max(data_avg_tmp))) > 50 % unexplicable scales
                    continue
                end

                data_avg_singlesubj(i_team,:,:) = data_avg_tmp(position_tmp,:);

                % figure(40); subplot(5,7,i_subj); hold on
                % chan_tmp = 32
                %plot(time_vector, squeeze(data_avg_singlesubj(i_team,chan_tmp,:)))
                % ylim([-20 20])
                %figure; imagesc(data_avg_tmp)
                %figure; imagesc(squeeze(data_avg_singlesubj(i_team-1,:,:)))

                % if i_team == n_team
                %     plot(time_vector, nanmedian(squeeze(data_avg_singlesubj(:,chan_tmp,:)),1), 'k-', 'LineWidth', 3)
                % end
                data_var_tmp =[];
                data_var_tmp = alldatavg{1,i_subj}.var;
                data_var_singlesubj(i_team,:,:) = data_var_tmp(position_tmp,:);

            else
                disp([ '!!! problem with CHAN in: ' team_name ' subj: ' num2str(i_subj)])
            end; %end

        else
            disp(['!!! subj ' num2str(i_subj) ' in ' team_name ' = NaN'])
        end
    end


    for i_chan = 1:n_chan
        %i_chan = 32

        % #################################################
        % find PEAKs of interest (for a single channel)
        % help imregionalmax
        % #################################################
        data_median_singlesubj = nanmedian(squeeze(data_avg_singlesubj(:,i_chan,:)),1);
        % Compute derivative with respect to x
        dy = [ 0 diff(data_median_singlesubj) ./ diff(1:length(time_vector))];
        % if i_chan ==32
        %     % figure(50); subplot(5,7,i_subj); hold on
        %     %plot(dy, '--'); hold on; plot(data_median_singlesubj)
        %     %figure; plot(time_vector, dy); hold on; plot(time_vector, data_median_singlesubj)
        % end

        %find 1° NEGative peak close to ERP_toi1
        % with derivative ~ 0
        soi_tmp = ERP_soi1-delta_soi : ERP_soi1+delta_soi;
        idx_tmp = imregionalmin(data_median_singlesubj(soi_tmp)); % find peak with a highest absolute value in a median waveform of all teams in one subject pone channel
        % if there are more than one peak
        % find the peak with highest abs value
        if length(soi_tmp(idx_tmp))>1
            [~, idx_tmp2] = min(data_median_singlesubj(soi_tmp(idx_tmp)));
            val_tmp = soi_tmp(idx_tmp);
            peak_vector(i_chan,i_subj,1) = val_tmp(idx_tmp2);
        else
            peak_vector(i_chan,i_subj,1) = soi_tmp(idx_tmp);
        end
        peak_vector(i_chan,i_subj,2) = data_median_singlesubj(peak_vector(i_chan,i_subj,1));
        peak_vector(i_chan,i_subj,3) = dy(peak_vector(i_chan,i_subj,1));% derivative. a sanity check


        %find 1° POSitive peak close to ERP_toi2
        soi_tmp = ERP_soi2-delta_soi : ERP_soi2+delta_soi;
        idx_tmp = imregionalmax(data_median_singlesubj(soi_tmp));
        if length(soi_tmp(idx_tmp))>1
            [~, idx_tmp2] = max(data_median_singlesubj(soi_tmp(idx_tmp)));
            val_tmp = soi_tmp(idx_tmp);
            peak_vector(i_chan,i_subj,6) = val_tmp(idx_tmp2);
        else
            peak_vector(i_chan,i_subj,6) = soi_tmp(idx_tmp);
        end
        peak_vector(i_chan,i_subj,7) = data_median_singlesubj(peak_vector(i_chan,i_subj,6));
        peak_vector(i_chan,i_subj,8) = dy(peak_vector(i_chan,i_subj,6));

        if i_chan ==32
            % figure(50); subplot(5,7,i_subj); hold on
            %scatter(peak_vector(i_chan,i_subj,1), peak_vector(i_chan,i_subj,2), 25, 'r', 'filled')
            %scatter(peak_vector(i_chan,i_subj,6), peak_vector(i_chan,i_subj,7), 25, 'r', 'filled')
        end

        % #################################################
        % compute (ROBUST) CORRELATION (for a single channel)
        % at the peaks of interest
        % #################################################
        fig_flag = 0;

        % 1° NEGative peak
        data_avg_tmp =[];
        data_avg_tmp = data_avg_singlesubj(:,i_chan, peak_vector(i_chan,i_subj,1));

        data_var_tmp = data_var_singlesubj(:,i_chan, peak_vector(i_chan,i_subj,1));

        %data_mean_tmp = nanmean(data_avg_tmp);
        %data_dist_fromavg = data_avg_tmp - data_mean_tmp;
        %data_absdist_fromavg = abs(data_avg_tmp - data_mean_tmp);
        %data_dist_frommedian = data_avg_tmp - nanmedian(data_avg_tmp);;

        %         [corr_value_tmp, p_value_tmp]= corrcoef(data_avg_tmp, distance_vector, 'Rows', 'complete');
        %         peak_vector(i_chan,i_subj,4) = corr_value_tmp(1,2);
        %         peak_vector(i_chan,i_subj,5) = p_value_tmp(1,2);

        corr_value_array = [ data_avg_tmp, distance_vector ];
        % remove NaN
        corr_value_array(any(isnan(corr_value_array), 2), :) = [];
        fig_flag = 0;
        [r,t,p] = Spearman(corr_value_array(:,1), corr_value_array(:,2), fig_flag,0.05);
        peak_vector(i_chan,i_subj,4) = r;
        peak_vector(i_chan,i_subj,5) = p;


        % also for VARIANCE ??


        % - - - - - - - -  -- - - - - - -
        % 1° POSitive peak
        data_avg_tmp =[];  data_var_tmp =[];
        data_avg_tmp = data_avg_singlesubj(:,i_chan, peak_vector(i_chan,i_subj,6));
        %data_avg_tmp =  data_avg_singlesubj(:,i_chan, 98)

        data_var_tmp = data_var_singlesubj(:,i_chan, peak_vector(i_chan,i_subj,6));

        %         [corr_value_tmp, p_value_tmp]= corrcoef(data_avg_tmp, distance_vector, 'Rows', 'complete');
        %         peak_vector(i_chan,i_subj,9) = corr_value_tmp(1,2);
        %         peak_vector(i_chan,i_subj,10) = p_value_tmp(1,2);

        corr_value_array = [ data_avg_tmp, distance_vector ];
        % remove NaN
        corr_value_array(any(isnan(corr_value_array), 2), :) = [];
        %fig_flag = 1;
        [r,t,p] = Spearman(corr_value_array(:,1), corr_value_array(:,2), fig_flag,0.05);
        peak_vector(i_chan,i_subj,9) = r;
        peak_vector(i_chan,i_subj,10) = p;

    end
end
save('pos_neg_peaks_median_based','peak_vector')

test_pos_cor = peak_vector(:,:,5)';
test_pos_cor([16,19,20,23,28,30],:) = []
mask_pos = nan(27,64)
indx_pos = test_pos_cor <0.05
mask_pos(indx_pos) = test_pos_cor(indx_pos)
sum(indx_pos)


test_neg_cor = peak_vector(:,:,10)';
test_neg_cor([16,19,20,23,28,30],:) = []
mask_neg = nan(27,64)
indx_neg = test_neg_cor <0.05
mask_neg(indx_neg) = test_neg_cor(indx_neg)
sum(indx_neg)

subj_vector = 1:33
% !!!!!!!
% subjects to remove (for now)  since the peak detection didn't work
subj_vector([16,19,20,23,28,30]) = [];

ERP_pval = NaN([n_chan,2])

for i_chan = 1:n_chan
    %i_chan = 32
    peak_vector_tmp =[];
    peak_vector_tmp = squeeze(peak_vector(i_chan,:,:));

    if median(peak_vector_tmp(subj_vector, 4)) > 0
        [ ~, ERP_pval(i_chan,1) ] = ttest(peak_vector_tmp(subj_vector, 4), 0);
    else
        ERP_pval(i_chan,1) = NaN;
    end

    if median(peak_vector_tmp(subj_vector, 9)) < 0
        %[h, p] = ttest(peak_vector_tmp(subj_vector, 9), 0)
        [ ~, ERP_pval(i_chan,2) ] = ttest(peak_vector_tmp(subj_vector, 9), 0);
    else
        ERP_pval(i_chan,2) = NaN;
    end


    if i_chan ==32
        figure(100);
        for i_subj = subj_vector
            disp(i_subj)

            slope_tmp = peak_vector_tmp(i_subj, 4);
            subplot(121); hold on
            xx = 1:3;
            plot(slope_tmp*xx)

            slope_tmp = peak_vector_tmp(i_subj, 9);
            %             if median(slope_tmp) > 0
            %                 [ ~, ERP_1_pval(i_chan) ] = ttest(slope_tmp, 0);
            %             else
            %                 ERP_1_pval(i_chan) = NaN;
            %             end
            subplot(122); hold on
            xx = 4:6
            plot(slope_tmp*xx)
        end
    end
end

% - - - - - - - --
% add eeglab
%load('EMP01_clean.mat')
%cd('D:\_TOOLBOX_EEG\eeglab_20201226')
eeglab
%EEG = data; eeglab redraw
chan_locs = EEG.chanlocs;
load('C:\Users\ecesnait\Desktop\BUSCHLAB\CORENATS\Matlab codes\MasterThesisElisabeth\EEG analyze\chan_locs.mat');

figure; colormap(hot)
subplot 121;
topoplot(ERP_pval(:,1), chan_locs, 'maplimits', [0 0.5], 'electrodes', 'labels', 'colormap', flipud(hot) ); colorbar %,'emarker2' , {find(sum(indx_pos)>1),'*','g'}
title('90 ms (pos corr)')
subplot 122
topoplot(ERP_pval(:,2), chan_locs, 'maplimits', [0 0.5], 'electrodes', 'labels', 'colormap', flipud(hot) ); colorbar %,'emarker2' , {find(sum(indx_neg)>1),'*','g'}
title('160 ms (neg corr)')
