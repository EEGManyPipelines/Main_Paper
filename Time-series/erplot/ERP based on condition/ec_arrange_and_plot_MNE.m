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
load('C:\Users\ecesnait\Desktop\EEGManyPipelines\git\EEGManyPipes org\Main_Paper\Time-series\erplot\ERP based on condition\allgrpdat files\allgrpdat_manmade_condition_MNE.mat')
% Paths
user_name = getenv('username');

if isequal(user_name, 'ecesnait')
    %datapath = 'N:\EMP\EEGManyPipelines\EMP time series exp\_fieldtrip_data3d\';
    datapath = 'M:\EMP\EEGManyPipelines\EMP time series exp\'
    % addpath('C:\Users\ecesnait\Desktop\EEGManyPipelines\git\EEGManyPipes org\Main_Paper\Time-series\erplot\')
    addpath('C:\Users\ecesnait\Desktop\EEGManyPipelines\Matlab Scripts\toolboxes\fieldtrip-master\')
elseif isequal(user_name,'mikkelcv')
    datapath = '/home/mikkelcv/emp/main/Time-series/erplot/nobackup/data';
    addpath('/home/mikkelcv/emp/main/Time-series/erplot')
    addpath('/home/mikkelcv/fieldtrip/')
else
    datapath = '/media/pasca/pasca_media/data/eeg/EMP_data_audit/team/MNE';
    addpath('/home/pasca/Tools/software/fieldtrip')
end

ft_defaults;

% Find group folders
d = dir(datapath);
d = d([d.isdir]);
d = {d.name};
grps = d(~(strcmp('.',d)|strcmp('..',d)));


%% Collect data from all groups.
% Average withing subject within group, then average all subjects within
% group (grand average). Collect grand averages for comparison across
% groups.

% already processed teams
exist_teamID = allgrpdat_manmade(2,:);
clear allgrpdat_manmade
% ids to exclude
excl = {'Continuous_to_epoched','_fieldtrip_data3d', 'EEGLAB all teams'}
indx_excl = ismember(grps,[exist_teamID,excl])
grps = grps(~indx_excl)

% Loop over groups
num_trials = struct();

fields = {'label', 'time'};
for gg = 2:length(grps)
    grp = grps{gg};
    disp(['Processing participant... ',num2str(gg)])

    % Loop over subjects
    subjects = dir([fullfile(datapath,grp, '*.mat')])
    allsubjdat = cell(size(subjects));

    allsubjdat_manmade = [];
    allsubjdat_natural = [];

    for ss = 1:length(subjects)
        % Load data
        subj = subjects(ss).name;

        epo_fpath = fullfile(datapath,grp, subj);
        disp(['LOAD ' epo_fpath])
        epo_data = load(epo_fpath);
        size(epo_data.data)

        if isequal(grp, '2e4c7afeba24d070')
            epo_data = epo_data.data(ss);
            epo_data.chs_name = char(epo_data.chan);
            epo_data.data = epo_data.EEGts;
            epo_data.events(:,3) = epo_data.epoch;
        end

        groupdat(1).time = epo_data.time;
        for nc = 1:size(epo_data.chs_name, 1)
            groupdat(1).chan(nc,:) = {epo_data.chs_name(nc, :)};
        end

        % Check if data is epoched. If not, skip this subject
        if numel(size(epo_data.data)) < 3
            continue
        else
            id_manmade = [];
            id_natural = [];

            events_id = int64(epo_data.events(:, 3));

            indx_manmade = startsWith(string(events_id), "1");  % all markers for man-made stimuli start with 1
            indx_natural = startsWith(string(events_id), "2");  % all markers for natural stimuli start with 2

            % Average across epochs based on condition
            manmade_avg_epoch = double(mean(epo_data.data(:,:,indx_manmade), 3));
            natural_avg_epoch = double(mean(epo_data.data(:,:,indx_natural), 3));

            % count the number of epochs
            num_trials(gg).manmade_natural(ss, 1) = sum(indx_manmade);
            num_trials(gg).manmade_natural(ss, 2) = sum(indx_natural);

            if ~isempty(manmade_avg_epoch) % prepare the fttmp struct for man-made and natural data separately
                [fttmp_manmade, fttmp_natural] = ec_ssubj_erp_condition(groupdat, manmade_avg_epoch, natural_avg_epoch)

                % delete entry
                allsubjdat_manmade{size(allsubjdat_manmade,2)+1} = fttmp_manmade; % Avoids empty entries
                allsubjdat_natural{size(allsubjdat_natural,2)+1} = fttmp_natural; % Avoids empty entries

                clear manmade_avg_epoch natural_avg_epoch fttmp_natural fttmp_manmade indx_manmade indx_natural
    
            end
            groupdat = [];
        end
    end

%     % check if all sbj have the same channels and time
%     for nf=1:numel(fields)
%         is_ok = check_field(allsubjdat_manmade, fields{nf});
%         if is_ok == 0
%             error('something wrong')
%         end
%     end
% 
%     for nf=1:numel(fields)
%         is_ok = check_field(allsubjdat_natural, fields{nf});
%         if is_ok == 0
%             error('something wrong')
%         end
%     end
test = ft_timelockgrandaverage([], allsubjdat_manmade{:});
    % Make grand avg
    allgrpdat_manmade{gg} = ft_timelockgrandaverage([], allsubjdat_manmade{:});
    allgrpdat_natural{gg} = ft_timelockgrandaverage([], allsubjdat_natural{:});
    clear allsubjdat_natural allsubjdat_manmade fig
end

allgrpdat_manmade = [allgrpdat_manmade; grps]
allgrpdat_natural = [allgrpdat_natural; grps]

save('allgrpdat_manmade_condition_MNE.mat', 'allgrpdat_manmade')
save('allgrpdat_natural_condition_MNE.mat', 'allgrpdat_natural')
save('numer_trials_MNE_conditions', 'num_trials')


