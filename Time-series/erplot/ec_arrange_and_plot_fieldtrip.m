%% Import and plot EMP ERP data
%
% For each group:
%   1) do subj avg
%   2) do grp grand avg
% Then collect all data and do a great grand average. Plot and some misch
% stats (just for fun for now).
%

%% SETUP
clear all, close all
% Paths
user_name = getenv('username');

if isequal(user_name, 'ecesnait')
    datapath = 'N:\EMP\EEGManyPipelines\EMP time series exp\_fieldtrip_data3d\';
    addpath('C:\Users\ecesnait\Desktop\EEGManyPipelines\git\EEGManyPipes org\Main_Paper\Time-series\erplot\')
    addpath('C:\Users\ecesnait\Desktop\EEGManyPipelines\Matlab Scripts\toolboxes\fieldtrip-master\')
elseif isequal(user_name,'mikkelcv')
    datapath = '/home/mikkelcv/emp/main/Time-series/erplot/nobackup/data';
    addpath('/home/mikkelcv/emp/main/Time-series/erplot')
    addpath('/home/mikkelcv/fieldtrip/')
end

ft_defaults;

% Find group folders
d = dir([datapath, '*_EEG3d_struct*']);
grps = {d.name};

%% Collect data from all groups.
% Average withing subject within group, then average all subjects within
% group (grand average). Collect grand averages for comparison across
% groups.

% Loop over groups
allgrpdat = {};
for gg = 2:length(grps) %3 out of memory
    %     gg=1;
    grp = grps{gg};
    disp(['Processing participant... ',num2str(gg)])

    teamID = extractBefore(grp,'_')

    % Load data
    %chan_time_epoch = load(fullfile(datapath,[teamID,'_BAD_chan_trial.mat']));
    EEG = load(fullfile(datapath,[teamID,'_EEG3d_struct.mat']));

    % Check if data is epoched. If not, skip this subject
    if numel(size(EEG.team_eeg(1).eeg3d)) < 3
        continue
    else
        allsubjdat = []
        groupdat = EEG.team_eeg; % un-nest

        %Loop over participants
        while size(groupdat,2) > 0 % saving memory in the workspace
            % check for excluded channels
            if ~isempty(groupdat(1).excluded_sensor)
                excl_chan = regexp(string(groupdat(1).excluded_sensor), '\s','split');
                indx_excl_chan = find(ismember(groupdat(1).chan_label, excl_chan));
                groupdat(1).eeg3d(indx_excl_chan,:,:)=[];
                groupdat(1).chan_label(indx_excl_chan)=[];
            end
            % Average all trials (not careing about conditions for now!)
            sbj_avg_epoch = double(mean(groupdat(1).eeg3d, 3));      % dim 3 = trials

            % Find CPz channel
            %find_cpz = find(ismember(groupdat(ss).chan, 'CPz'));
            if ~isempty(sbj_avg_epoch)
                fttmp = [];
                fttmp.avg       = sbj_avg_epoch;%sbj_avg_epoch(find_cpz,:)
                fttmp.time      = groupdat(1).time_msec;
                fttmp.label     = groupdat(1).chan_label;
                fttmp.dimord    = 'chan_time';
                groupdat(1) = [];
                allsubjdat{size(allsubjdat,2)+1} = fttmp; % Avoids empty entries
                clear sbj_avg_epoch fttmp
            else
                groupdat(1) = [];
            end
        end
        disp('done');
        clear groupdat infile
    end

    % Special cases when a single team has more than 1 file
    if endsWith(grp, '_27.mat')
        % Load data
        grp = grps{gg+1}
        teamID = extractBefore(grp,'_')

        % Load data
        EEG = load(fullfile(datapath,[teamID,'_EEG3d_struct_28_33.mat']));

        % Check if data is epoched. If not, skip this subject
        if numel(size(EEG.team_eeg(1).eeg3d)) < 3
            continue
        else
            allsubjdat = []
            groupdat = EEG.team_eeg; % un-nest

            %Loop over participants
            while size(groupdat,2) > 0 % saving memory in the workspace
                % check for excluded channels
                if ~isempty(groupdat(1).excluded_sensor)
                    excl_chan = regexp(string(groupdat(1).excluded_sensor), '\s','split');
                    indx_excl_chan = find(ismember(groupdat(1).chan_label, excl_chan));
                    groupdat(1).eeg3d(indx_excl_chan,:,:)=[];
                    groupdat(1).chan_label(indx_excl_chan)=[];
                end
                % Average all trials (not careing about conditions for now!)
                sbj_avg_epoch = double(mean(groupdat(1).eeg3d, 3));      % dim 3 = trials

                % Find CPz channel
                %find_cpz = find(ismember(groupdat(ss).chan, 'CPz'));
                if ~isempty(sbj_avg_epoch)
                    fttmp = [];
                    fttmp.avg       = sbj_avg_epoch;%sbj_avg_epoch(find_cpz,:)
                    fttmp.time      = groupdat(1).time_msec;
                    fttmp.label     = groupdat(1).chan_label;
                    fttmp.dimord    = 'chan_time';
                    groupdat(1) = [];
                    allsubjdat{size(allsubjdat,2)+1} = fttmp; % Avoids empty entries
                    clear sbj_avg_epoch fttmp
                else
                    groupdat(1) = [];
                end
            end
            disp('done');
            clear groupdat infile

        end
    end

    % Make grand avg
    allgrpdat{gg} = ft_timelockgrandaverage([], allsubjdat{:});
    clear allsubjdat EEG
end

save('allgrpdat_127_150.mat', 'allgrpdat')
