%% Import and plot EMP ERP data
%
% For each group:
%   1) do subj avg
%   2) do grp grand avg
% Then collect all data and do a great grand average. Plot and some misch
% stats (just for fun for now).
%
clear all, close all
%% SETUP
% Paths
user_name = getenv('username');

if isequal(user_name, 'ecesnait')
    datapath = 'M:\EMP\EEGManyPipelines\EMP time series exp\EEGLAB all teams\';
    addpath('C:\Users\ecesnait\Desktop\EEGManyPipelines\git\EEGManyPipes org\Main_Paper\Time-series\erplot\')
    addpath('C:\Users\ecesnait\Desktop\EEGManyPipelines\Matlab Scripts\toolboxes\fieldtrip-master\')

    ft_defaults;
    grps = dir([datapath, '*.mat'])
    grps = {grps.name}
else
    datapath = '/home/mikkelcv/emp/main/Time-series/erplot/nobackup/data';
    addpath('/home/mikkelcv/emp/main/Time-series/erplot')
    addpath('/home/mikkelcv/fieldtrip/')

    ft_defaults;

    % Find group folders
    d = dir(datapath);
    d = d([d.isdir]);
    d = {d.name};
    grps = d(~(strcmp('.',d)|strcmp('..',d)));     % Remove dots

    % List of individual datasets
    subjects = {'sub-001','sub-002','sub-003'};   % use these for now
end

%% Collect data from all groups.
% Average withing subject within group, then average all subjects within
% group (grand average). Collect grand averages for comparison across
% groups.

% Store the number of trials for Man-made and Natural stimuli
num_trials = struct()

% Loop over groups
for gg = 87:length(grps) % File 12 (same epoch struct for all subjects),16 (epochs uploaded only for natural stimuli), 17 has no epoch info,
    % 19 data for h3 testing, 23 is continuous data, 39,40 and 88 has more epochs than data,74 has no time info
    % 21,24-26, 69, 89 could not be loaded due to memory problems. Files 65 and 66 (same group) did not provide channel and time info, file 86 has no time info

    if ismember(gg,[12,16,17,19,23:26,39,40,69,89,65,66,74,86,88])
        continue
    end
    disp(['Processing participant... ',num2str(gg)])

    grp = grps{gg};

    % A special case - when a single team has more than 1 file, skip the
    % second file
    if endsWith(grp, '_2.mat')
        continue
    end

    % Load data
    groupdat = load(fullfile(datapath, grp));
    groupdat = groupdat.(string(fieldnames(groupdat))) % un-nest

    % If data is not epoched - skip it. TO DO: epoch non-epoched data
    if numel(size(groupdat(1).EEGts)) < 3 %check how many dimensions EEGts has. if it's conitnuous data:
        clear groupdat
        continue
    else
        num_trials(gg).Team_Id = grp;

        allsubjdat_manmade = []
        allsubjdat_natural = []

        %Loop over participants
        while size(groupdat,2) > 0 % saving memory in the workspace
            if isempty(groupdat(1).epoch)
                groupdat(1) = [];
                continue
            end
            % Split based on condition: man-made vs natural for H1
            if isfield(groupdat(1).epoch,'eventscene_category')
                indx_manmade = find(strcmp([groupdat(1).epoch.eventscene_category], 'manmade'));
                indx_natural = find(strcmp([groupdat(1).epoch.eventscene_category], 'natural'));
            elseif isfield(groupdat(1).epoch,'eventtype')
                if isa(groupdat(1).epoch(1).eventtype,'char')
                    char_type = char({groupdat(1).epoch.eventtype});
                    indx_manmade = find(char_type(:,1)=='1');
                    indx_natural = find(char_type(:,1)=='2');

                elseif isa(groupdat(1).epoch(1).eventtype,'double')
                    char_type = num2str([groupdat(1).epoch.eventtype]')
                    indx_manmade = find(char_type(:,1)=='1');
                    indx_natural = find(char_type(:,1)=='2');
                end
             elseif isfield(groupdat(1).epoch,'type')
                  char_type = char({groupdat(1).epoch.type});
                    indx_manmade = find(char_type(:,1)=='1');
                    indx_natural = find(char_type(:,1)=='2');
            end

            if ismember(grp, {'129c99e90c45f38c.mat','1559fd3bafe5582c.mat', '2e6f06d6e89db2ca.mat','356c77bfd2662b9a_H1.mat',...
                    '7c793519c38db522.mat','8107271d26b4e6ce.mat','9c490673610ca32b.mat','CognitiveSystems-KU.mat','The Hanncanny.mat',...
                    'a28bd183f8dca47f.mat','d5c8ed05b7af02a3.mat','d9a070789fe1b133.mat','gNeC.mat','fc67513ed5c4b400.mat'})
                [indx_manmade,indx_natural] = ec_special_cases(grp,groupdat);
            end

            manmade_avg_epoch = double(mean(groupdat(1).EEGts(:,:,indx_manmade), 3));
            natural_avg_epoch = double(mean(groupdat(1).EEGts(:,:,indx_natural), 3));

            % count the number of epochs
            num_trials(gg).manmade_natural(34-size(groupdat,2),1) = length(indx_manmade);
            num_trials(gg).manmade_natural(34-size(groupdat,2),2) = length(indx_natural)

            if ~isempty(manmade_avg_epoch)
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

        % if every epoch was empty
        if isempty(allsubjdat_manmade)
            continue
        end

        % Special cases when a single team has more than 1 file 
        if endsWith(grp, '_1.mat')
            % Load data
            grp = grps{gg+1}
            groupdat = load(fullfile(datapath, grp));
            groupdat = groupdat.(string(fieldnames(groupdat)))

            num_trials(gg+1).Team_Id = grp;
            while size(groupdat,2) > 0
                if isempty(groupdat(1).epoch)
                    groupdat(1) = [];
                    continue
                end
                % Split based on condition: man-made vs natural for H1
                if isfield(groupdat(1).epoch,'eventscene_category')
                    indx_manmade = find(strcmp([groupdat(1).epoch.eventscene_category], 'manmade'));
                    indx_natural = find(strcmp([groupdat(1).epoch.eventscene_category], 'natural'));
                elseif isfield(groupdat(1).epoch,'eventtype')
                    char_type = num2str([groupdat(1).epoch.eventtype]')
                    indx_manmade = find(char_type(:,1)=='1');
                    indx_natural = find(char_type(:,1)=='2');
                end

                manmade_avg_epoch = double(mean(groupdat(1).EEGts(:,:,indx_manmade), 3));
                natural_avg_epoch = double(mean(groupdat(1).EEGts(:,:,indx_natural), 3));
                % count the number of epochs
                num_trials(gg+1).manmade_natural(34-size(groupdat,2),1) = length(indx_manmade);
                num_trials(gg+1).manmade_natural(34-size(groupdat,2),2) = length(indx_natural)

                if ~isempty(manmade_avg_epoch)
                    [fttmp_manmade, fttmp_natural] = ec_ssubj_erp_condition(groupdat, manmade_avg_epoch, natural_avg_epoch)

                    %delete entry
                    groupdat(1) = [];

                    allsubjdat_manmade{size(allsubjdat_manmade,2)+1} = fttmp_manmade; % Avoids empty entries
                    allsubjdat_natural{size(allsubjdat_natural,2)+1} = fttmp_natural; % Avoids empty entries

                    clear manmade_avg_epoch natural_avg_epoch fttmp_natural fttmp_manmade
                else
                    groupdat(1) = []
                end
            end
            disp('done');
            clear groupdat infile
        end
    end

    % special cases when some manipulation is needed due to a format
    if isequal(grp,'d9a070789fe1b133.mat')
        for i = 1:length(allsubjdat_manmade)
            allsubjdat_manmade{i}.label = allsubjdat_manmade{i}.label{1:end};
            allsubjdat_natural{i}.label = allsubjdat_natural{i}.label{1:end};
        end
    elseif isequal(grp,'d5c8ed05b7af02a3.mat') % the team did not indicate channel labels but 1 and -1. Restore channel labels using a full examplary dataset
        error('Elena: check the special case for two conditions')
        chan_labels = allgrpdat_manmade{70}.label
        for i=1:length(allsubjdat_manmade)
            curr_lab = allsubjdat_manmade{i}.label;
            curr_lab(curr_lab == -1) = 0;
            allsubjdat_manmade{i}.label = chan_labels(find(curr_lab));
            allsubjdat_natural{i}.label = chan_labels(find(curr_lab));

            allsubjdat_manmade{i}.avg = allsubjdat_manmade{i}.avg(find(curr_lab),:);
            allsubjdat_natural{i}.avg = allsubjdat_natural{i}.avg(find(curr_lab),:);

            clear curr_lab
        end
    end

    % Make grand avg
    allgrpdat_manmade{gg} = ft_timelockgrandaverage([], allsubjdat_manmade{:});
    allgrpdat_natural{gg} = ft_timelockgrandaverage([], allsubjdat_natural{:});

    % Plot
    %ec_plot_gaERP(allgrpdat_manmade{gg},allgrpdat_natural{gg})

    clear allsubjdat_natural allsubjdat_manmade fig
end

% add Team IDs
allgrpdat_manmade = [allgrpdat_manmade; grps]
allgrpdat_natural = [allgrpdat_natural; grps]

save('allgrpdat_condition_manmade_EEGLAB.mat', 'allgrpdat_manmade')
save('allgrpdat_condition_natural_EEGLAB.mat', 'allgrpdat_natural')

save('number_trials_manmade_natural_EEGLAB','num_trials')

