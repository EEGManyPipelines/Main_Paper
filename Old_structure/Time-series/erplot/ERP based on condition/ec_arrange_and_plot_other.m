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


datapath = 'M:\EMP\EEGManyPipelines\EMP time series exp\'
addpath('C:\Users\ecesnait\Desktop\EEGManyPipelines\Matlab Scripts\toolboxes\fieldtrip-master\')

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
num_trials = struct()
for gg = 4:length(grps) %4 and 5 (same group),20 could not open,9 and 10 on epoched, 11,15,21 out of memory,13 processed but had different dimensions
    %     gg=1;
    grp = grps{gg};
    disp(['Processing participant... ',num2str(gg)])

    teamID = extractBefore(grp,'_')

    % Load data
    if any(strcmp(teamID, {'1497d4b19bba4f30','0c87f7355ce7ec6d'}))% could not open
        continue
    else
        EEG = load(fullfile(datapath,[teamID,'_EEG3d_struct.mat']));
    end

    % Check if data is epoched. If not, skip this subject
    if numel(size(EEG.team_eeg(1).eeg3d)) < 3
        continue
    else
        allsubjdat_manmade = []
        allsubjdat_natural = []

         num_trials(gg).Team_Id = grp;% add team ID to count the number of epochs
        groupdat = EEG.team_eeg; % un-nest

        %% Loop over participants
        while size(groupdat,2) > 0 % saving memory in the workspace
            if isempty(groupdat(1).epoch)%if epoch structure is empty -> skip the team
                groupdat(1) = [];
                continue
            end

            % remove excluded channels
            if ~isempty(groupdat(1).excluded_sensor)
                if isa(groupdat(1).excluded_sensor, 'table')
                    indx_excl_ch = find(ismember(groupdat(1).chan_label,groupdat(1).excluded_sensor.channels_rejected))
                else
                    indx_excl_ch = find(ismember(groupdat(1).chan_label,split(groupdat(1).excluded_sensor)))
                end
                % check if the dimensions are correct in the EEG struct and
                % remove
                if size(groupdat(1).eeg3d,1) == length(groupdat(1).chan_label)
                    groupdat(1).eeg3d(indx_excl_ch,:,:) = [];
                    groupdat(1).chan_label(indx_excl_ch)=[]; % remove it also from the channel label struct
                else
                    error('check dimensions')
                end
            end

            % epochs have already been deleted - the dataset has all epochs
            % and additional info at the excluded_epoch column
            if size(groupdat(1).eeg3d,3) == 1200 && ~isempty(groupdat(1).excluded_epoch)
                    error('epochs have not been removed?')
            end

           % Split based on condition: man-made vs natural for H1
            char_type = num2str(groupdat(1).epoch)
            indx_manmade = find(char_type(:,1)=='1'); %all markers for man-made stimuli start with 1
            indx_natural = find(char_type(:,1)=='2');%all markers for natural stimuli start with 2
            clear char_type
%                 indx_manmade = find(strcmp([groupdat(1).epoch], 'manmade'));
%                 indx_natural = find(strcmp([groupdat(1).epoch.eventscene_category], 'natural'));
            
            % Average across epochs based on condition
             manmade_avg_epoch = double(mean(groupdat(1).eeg3d(:,:,indx_manmade), 3));
            natural_avg_epoch = double(mean(groupdat(1).eeg3d(:,:,indx_natural), 3));

            % count the number of epochs
            num_trials(gg).manmade_natural(34-size(groupdat,2),1) = length(indx_manmade);
            num_trials(gg).manmade_natural(34-size(groupdat,2),2) = length(indx_natural);

            if ~isempty(manmade_avg_epoch) % prepare the fttmp struct for man-made and natural data separately
                [fttmp_manmade, fttmp_natural] = ec_ssubj_erp_condition(groupdat, manmade_avg_epoch, natural_avg_epoch)

                %delete entry
                groupdat(1) = [];
                allsubjdat_manmade{size(allsubjdat_manmade,2)+1} = fttmp_manmade; % Avoids empty entries
                allsubjdat_natural{size(allsubjdat_natural,2)+1} = fttmp_natural; % Avoids empty entries

                clear manmade_avg_epoch natural_avg_epoch fttmp_natural fttmp_manmade indx_manmade indx_natural
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

        %the code has to be adjusted here for both conditions separately
        error('check this specific case')
       
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
    allgrpdat_manmade{gg} = ft_timelockgrandaverage([], allsubjdat_manmade{:});
    allgrpdat_natural{gg} = ft_timelockgrandaverage([], allsubjdat_natural{:});

    clear allsubjdat_natural allsubjdat_manmade fig
end

save('allgrpdat_manmade_condition_fieldtrip.mat', 'allgrpdat_manmade')
save('allgrpdat_natural_condition_fieldtrip.mat', 'allgrpdat_natural')
save('numer_trials_fieldtrip_conditions', 'num_trials')

