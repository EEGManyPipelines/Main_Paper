% This code runs the data for the first 57 teams. It uses standardized data
% that has been brought to the same epoch length, order of channels, and
% scale (V to uV).
% The code loads each team's data and processes all participants
% individually. For each participant, a difference wave is created
% by subtracting GA ERPs of natural images from man-made images. The
% resulting difference wave
% for each team, each participant is then stored in a 57 x 33 x 205 matrix
% (teams x participants x time points). 
%
clear all, close all

%% SETUP
% Paths
datapath = 'Z:\aebuschgold\ecesnait\EMP\EMP time series exp\Standardized\'%'M:\EMP\EEGManyPipelines\EMP time series exp\EEGLAB all teams\';
addpath('C:\Users\ecesnait\Desktop\EEGManyPipelines\git\EEGManyPipes org\Main_Paper\Time-series\erplot\')
addpath('C:\Users\ecesnait\Desktop\EEGManyPipelines\Matlab Scripts\toolboxes\fieldtrip-master\')

ft_defaults;
grps = dir([datapath, '*.mat'])
grps = {grps.name}


%% Collect data from all groups.

all_sbj_diff_waves = nan(length(grps),33,205);%teams, participants, time

no_trialinfo=0

inspect_cases = {}

% Loop over groups. No averaging is done here, just storing information for
% every group, every participant, one channel
for gg = 1:length(grps)%team 21 has only the marker for natural but not man-made stimuli, 44,45,54,62,63,68,79,92,95,107,114 - mixed up event structures where one trial has several event markers
    
if ismember(gg,[21,44,45,54,62,63,68,79,92,95,107,114]) % skip teams that lack info for further processing
    continue
end
    grp = grps{gg};

    disp(['Processing participant... ',num2str(gg), ', team ID - ', grp])

    % A special case - when a single team has more than 1 file, skip the
    % second file
    if endsWith(grp, '_H3.mat')
        continue
    end

    % Load data
    groupdat = load(fullfile(datapath, grp));
    groupdat = groupdat.(string(fieldnames(groupdat))) % un-nest

    %Loop over participants (N=33)
    for pp =1: size(groupdat,2)
        if isempty(groupdat{1,pp})
           
            continue
        end

        % Split based on condition: man-made vs natural for H1
        if isfield(groupdat{1,pp}.trialinfo,'eventscene_category')
            indx_manmade = find(strcmp([groupdat{1,pp}.trialinfo.eventscene_category], 'manmade'));
            indx_natural = find(strcmp([groupdat{1,pp}.trialinfo.eventscene_category], 'natural'));
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
        end
        
       % team-specific event information extraction that couldn't be
       % generalised to other teams.
        if ismember(grp, {'standart_129c99e90c45f38c.mat','standart_1559fd3bafe5582c.mat', 'standart_2e6f06d6e89db2ca.mat','standart_356c77bfd2662b9a_H1.mat',...
                'standart_8107271d26b4e6ce.mat','standart_9c490673610ca32b.mat','standart_CognitiveSystems-KU.mat','standart_The Hanncanny.mat',...
                'standart_a28bd183f8dca47f.mat','standart_93af9133e6fa6cf5.mat','standart_d5c8ed05b7af02a3.mat','standart_d9a070789fe1b133.mat','standart_gNeC.mat'})
            [indx_manmade,indx_natural] = fun_ec_special_cases(grp,groupdat,pp);
        end

    % if no trials were found - store team id and inspect
        if ~exist('indx_manmade','var')
            inspect_cases{length(inspect_cases)+1} = grps{gg};
            clear indx_manmade indx_natural char_type manmade_sbj_epochs natural_sbj_epochs sbj_diff_wave
            continue
        end

        % take man-made epochs
        manmade_sbj_epochs = groupdat{1,pp}.trial(indx_manmade);
        manmade_sbj_epochs = cat(3,manmade_sbj_epochs{:});% channels, times, epochs

        indx_cpz = find(ismember(groupdat{1,pp}.label, 'CPz')); % get channel indx
        
        indx_subj = cellfun(@(x) str2double(regexp(x, '\d+', 'match', 'once')),groupdat(2,pp)); % get the participant number

        % take natural epochs
        natural_sbj_epochs = groupdat{1,pp}.trial(indx_natural);
        natural_sbj_epochs = cat(3,natural_sbj_epochs{:});% channels, times, epochs

        sbj_diff_wave = nanmean(manmade_sbj_epochs(indx_cpz,:,:),3) -  nanmean(natural_sbj_epochs(indx_cpz,:,:),3);%the output is a mean amplitude at CPz channe

        all_sbj_diff_waves(gg,indx_subj,:) = sbj_diff_wave;%dimensions are teams, subjects, time

        clear indx_manmade indx_natural char_type manmade_sbj_epochs natural_sbj_epochs sbj_diff_wave

    end

    disp('done');
    clear groupdat
    % 

end
save('all_sbj_team_diff_waves_H1_231025.mat', 'all_sbj_diff_waves')
save('all_sbj_team_diff_waves_H1_inspect_cases.mat', 'inspect_cases')
