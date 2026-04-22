% This code runs the data for the last 42 teams who could not be correctly 
% processed in the first code. 
% 
% The code loads each team's data and processes all participants
% individually. For each participant, a difference wave is created
% by subtracting GA ERPs of natural images from man-made images. The
% resulting difference wave
% for each team, each participant is then stored in a 51 x 33 x 205 matrix
% (teams x participants x time points). 
% We combine data files from the first and the second code at the end of
% the loop which results in a 99 x 33 x 205 matrix. Two outliers are
% identified and removed.
% We baseline-correct the data for further analyses and average it across 
% teams and across participants.
% 
clear all, close all

load('C:\Users\ecesnait\Desktop\EEGManyPipelines\git\EEGManyPipes org\Main_Paper\Time-series\erplot\ERP based on condition\all_sbj_team_diff_waves_H1_inspect_cases.mat')

team_ID = unique(inspect_cases)

% Paths
datapath = 'Y:\aebuschgold\ecesnait\EMP\EMP time series exp\Standardized\'%'M:\EMP\EEGManyPipelines\EMP time series exp\EEGLAB all teams\';
addpath('C:\Users\ecesnait\Desktop\EEGManyPipelines\git\EEGManyPipes org\Main_Paper\Time-series\erplot\')
addpath('C:\Users\ecesnait\Desktop\EEGManyPipelines\Matlab Scripts\toolboxes\fieldtrip-master\')

ft_defaults;

grps = team_ID


%% Collect data from all groups.

all_sbj_diff_waves = nan(length(grps),33,205);%teams, subjects, time

no_trialinfo=0

inspect_cases = {}

for gg = 1:length(grps)% 8 - wrong indexing, 9,14 - empty trial info, 10- no information on event codes

    grp = grps{gg};

    disp(['Processing participant... ',num2str(gg), ', team ID - ', grp])

    % Teams with no trial info or trial codes that should be skipped
    if ismember(grp,{'standart_344dd59ded90cb34.mat','standart_8559e4d7314e45ec.mat','standart_a25b8419335d2131.mat','standart_c577d3cdf78548ce.mat',...
            'standart_e146a94b29a41713.mat','standart_e72d90a6ff4b5108.mat','standart_ee8c062e3dc35b1d.mat'})
        continue
    end
    % A special case - when a single team has more than 1 file, skip the
    % second file
    if endsWith(grp, '_H3.mat')
        continue
    end

    % Load data
    groupdat = load(fullfile(datapath, grp));
    groupdat = groupdat.(string(fieldnames(groupdat))) % un-nest

    %Loop over participants
    for pp = 1: size(groupdat,2)
        if isempty(groupdat{1,pp})
            continue
        end

        % Split based on condition: man-made vs natural for H1
        if isequal(class(groupdat{1,pp}.trialinfo),'double')
            char_type = num2str(groupdat{1,pp}.trialinfo(:,1))
            indx_manmade = find(char_type(:,1)=='1');
            indx_natural = find(char_type(:,1)=='2');

        elseif isfield(groupdat{1,pp}.trialinfo,'events_id')
            if ismember(grp,{'standart_0bc9ee704db74104.mat','standart_48e64dc185199502.mat','standart_77fddd91c557626d.mat','standart_aa6aa366e9788967.mat',...
                             'standart_c91e489c4acd0bf4.mat','standart_e13e7e07b99d853b.mat'})
                    char_type = extractBefore(groupdat{1,pp}.trialinfo.events_id.OriginalVariableNames,'/')
                    indx_manmade_v1 = strcmp(char_type,'man-made');
                    if sum(indx_manmade_v1)==0;  indx_manmade_v1 =  strcmp(char_type,'manmade'); end

                char_man_made = groupdat{1,pp}.trialinfo.events_id.Var1(indx_manmade_v1)

                indx_natural_v1 =  strcmp(char_type,'natural');
                if sum(indx_natural_v1)==0;  indx_natural_v1 =  strcmp(char_type,'nature'); end

                char_natural = groupdat{1,pp}.trialinfo.events_id.Var1(indx_natural_v1)
                indx_manmade = ismember(double(groupdat{1,pp}.trialinfo.events(:,3)), double(char_man_made))
                indx_natural = ismember(double(groupdat{1,pp}.trialinfo.events(:,3)), double(char_natural))
                %Alternative for the few teams is below (to do: find team
                %identifiers for which this solution works):
            else
                char_type = extractAfter(groupdat{1,pp}.trialinfo.events_id.OriginalVariableNames,'/') ;
                char_type = char(char_type);
                indx_manmade_v1 = find(char_type(:,1)=='1');
                char_man_made = groupdat{1,pp}.trialinfo.events_id.Var1(indx_manmade_v1);
                indx_natural_v1 = find(char_type(:,1)=='2');
                char_natural = groupdat{1,pp}.trialinfo.events_id.Var1(indx_natural_v1);
                indx_manmade = ismember(double(groupdat{1,pp}.trialinfo.events(:,3)), double(char_man_made));
                indx_natural = ismember(double(groupdat{1,pp}.trialinfo.events(:,3)), double(char_natural));
            end

        elseif isequal(class(groupdat{1,pp}.trialinfo),'cell')
            if strcmp(grp,'standart_26ab3771b0c195f5.mat')
                indx_manmade = strcmp(groupdat{1,pp}.trialinfo, 'man_made');
                indx_natural = strcmp(groupdat{1,pp}.trialinfo, 'natural');
            elseif strcmp(grp,'standart_Chile Maule.mat')
                indx_manmade = strcmp(groupdat{1,pp}.trialinfo(:,4), 'manmade');
                indx_natural = strcmp(groupdat{1,pp}.trialinfo(:,4), 'natural');
            else
                for q=1:length(groupdat{1,pp}.trialinfo)
                    indx_manmade(q) = strcmp(groupdat{1,pp}.trialinfo{q,1}.image,'man_made');
                    indx_natural(q) = strcmp(groupdat{1,pp}.trialinfo{q,1}.image,'natural');
                end
            end
        elseif isfield(groupdat{1,pp}.trialinfo,'eventtype')
            if isa(groupdat{1,pp}.trialinfo(1).eventtype,'char')
                char_type = char({groupdat{1,pp}.trialinfo.eventtype});
                indx_manmade = find(char_type(:,1)=='1');
                indx_natural = find(char_type(:,1)=='2');

            elseif isa(groupdat{1,pp}.trialinfo(1).eventtype,'double')
                char_type = num2str([groupdat{1,pp}.trialinfo.eventtype]')
                indx_manmade = find(char_type(:,1)=='1');
                indx_natural = find(char_type(:,1)=='2');
            end
        elseif isfield(groupdat{1,pp}.trialinfo,'type')
            char_type = char({groupdat{1,pp}.trialinfo.type});

            indx_manmade = find(char_type(:,1)=='1');
            indx_natural = find(char_type(:,1)=='2');
        elseif isfield(groupdat{1,pp}.trialinfo,'events')
            char_type = char(convertStringsToChars(string(groupdat{1,pp}.trialinfo.events(:,3))));
            indx_manmade = find(char_type(:,1)=='1');
            indx_natural = find(char_type(:,1)=='2');
        end

        if ismember(grp,{'standart_3c8c07696fbd0314.mat'})
            char_type = char(groupdat{1,pp}.trialinfo.value) ;
            indx_manmade = find(char_type(:,1)=='1');
            indx_natural = find(char_type(:,1)=='2');
        end

        if ismember(grp,{'standart_628a18bc8a3d36dd.mat'})
            char_type = char(convertStringsToChars(string(groupdat{1,pp}.trialinfo.events))) ;
            indx_manmade = find(char_type(:,1)=='1');
            indx_natural = find(char_type(:,1)=='2');
        end

        % if no trials were found - store team id and inspect
        if ~exist('indx_manmade','var')
            error('wrong indexing')
        end

        % for man-made epochs
        manmade_sbj_epochs = groupdat{1,pp}.trial(indx_manmade);
        manmade_sbj_epochs = cat(3,manmade_sbj_epochs{:});% channels, times, epochs

        indx_cpz = find(ismember(groupdat{1,pp}.label, 'CPz')); % get channel indx

        indx_subj = cellfun(@(x) str2double(regexp(x, '\d+', 'match', 'once')),groupdat(2,pp)); % get the participant number

        % for natural epochs
        natural_sbj_epochs = groupdat{1,pp}.trial(indx_natural);
        natural_sbj_epochs = cat(3,natural_sbj_epochs{:});% channels, times, epochs

        sbj_diff_wave = nanmean(manmade_sbj_epochs(indx_cpz,:,:),3) -  nanmean(natural_sbj_epochs(indx_cpz,:,:),3);%the output is mean ampltidue in channel by time

        all_sbj_diff_waves(gg,indx_subj,:) = sbj_diff_wave;%teams, subjects, time

        clear indx_manmade indx_natural char_type manmade_sbj_epochs natural_sbj_epochs sbj_diff_wave

    end

    disp('done');
    clear groupdat
    %

end
save('all_sbj_team_diff_waves_H1_all.mat', 'all_sbj_diff_waves')

clear all, close all

%% Merge it with another batch of teams that were processed before
first_batch = load('all_sbj_team_diff_waves_H1_231025.mat')
second_batch = load('all_sbj_team_diff_waves_H1_all.mat')

% remove empty rows
indx_empty = find(isnan(mean(squeeze(mean(first_batch.all_sbj_diff_waves,2,'omitnan')),2,'omitnan')))
first_batch = first_batch.all_sbj_diff_waves;
first_batch(indx_empty,:,:) = [];

second_batch = second_batch.all_sbj_diff_waves;
indx_empty2 = find(isnan(mean(squeeze(mean(second_batch,2,'omitnan')),2,'omitnan')));
second_batch(indx_empty2,:,:) = [];

% join matrices
all_sbj_diff_waves = [first_batch; second_batch];

%% Find outliers based on IQR

data = all_sbj_diff_waves;  % take original matrix we created in the loop

mean_team_time = nanmean(squeeze(nanmean(data, 2)),2);  % average over 2nd dimension (participants)

Q1  = quantile(mean_team_time, 0.25);
Q3  = quantile(mean_team_time, 0.75);
IQR = Q3 - Q1;

outlier_indx = find(mean_team_time < Q1 - 3*IQR | mean_team_time > Q3 + 3*IQR);

data(outlier_indx,:,:) = []; % remove two teams as having usually large amplitudes


%% Baseline correct and average across every team every participant
baseline = squeeze(nanmean(data(:,:,1:52),3));
data_bs_cor = data - baseline;

subject_bs_cor = squeeze(nanmean(data_bs_cor, 1));     % average over 1st dimension (teams)

team_bs_cor = squeeze(nanmean(data_bs_cor, 2));  % average over 2nd dimension (participants)

% save tables for plotting
across_teams = array2table(subject_bs_cor);
across_subjects = array2table(team_bs_cor);

writetable(across_teams,'difference_wave_H1_across_teams.csv')
writetable(across_subjects,'difference_wave_H1_across_subjects.csv')

