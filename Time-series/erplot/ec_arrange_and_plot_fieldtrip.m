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
for gg = 22:length(grps) %4 and 5 (same group),20 could not open,9 and 10 on epoched, 11,15,21 out of memory,13 processed but had different dimensions
    %     gg=1;
    grp = grps{gg};
    disp(['Processing participant... ',num2str(gg)])

    teamID = extractBefore(grp,'_')

    % Load data
    if strcmp(teamID, '1497d4b19bba4f30')% could not open
        continue
    else
        EEG = load(fullfile(datapath,[teamID,'_EEG3d_struct.mat']));
    end

    % Check if data is epoched. If not, skip this subject
    if numel(size(EEG.team_eeg(1).eeg3d)) < 3
        continue
    else
        allsubjdat = []
        groupdat = EEG.team_eeg; % un-nest

        %Loop over participants
        while size(groupdat,2) > 0 % saving memory in the workspace
            % check for excluded channels
%             if ~isempty(groupdat(1).excluded_sensor)
%                 if isfield(groupdat(1).excluded_sensor,'channels_rejected') %|| sum(strcmp(groupdat(1).excluded_sensor.Properties.VariableNames, 'channels_rejected'))
%                     excl_chan = regexp(string(groupdat(1).excluded_sensor.channels_rejected), '\s','split');
%                     indx_excl_chan = find(ismember(groupdat(1).chan_label, [excl_chan{:}]));
% %                 elseif sum(strcmp(groupdat(1).excluded_sensor.Properties.VariableNames, 'Afp9_reject'))
% %                     excl_chan= extractBefore(groupdat(1).excluded_sensor.Afp9_reject,',')
% %                     indx_excl_chan = find(ismember(groupdat(1).chan_label, excl_chan));
%                 elseif isnumeric(groupdat(1).excluded_sensor)
%                     indx_excl_chan = groupdat(1).excluded_sensor;
%                 else
%                     excl_chan = regexp(string(groupdat(1).excluded_sensor), '/s','split');
%                     indx_excl_chan = find(ismember(groupdat(1).chan_label, excl_chan));
%                 end
%                   if isempty(excl_chan)
%                         error('did not catch channels')
%                         excl_chan = regexp(string(groupdat(1).excluded_sensor), ',','split');
%                   end
%                 groupdat(1).eeg3d(indx_excl_chan,:,:)=[];
%                 groupdat(1).chan_label(indx_excl_chan)=[];
%             end
            % Average all trials (not careing about conditions for now!)
            sbj_avg_epoch = double(mean(groupdat(1).eeg3d, 3));      % dim 3 = trials

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
