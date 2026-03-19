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
datapath = 'M:\EMP\EEGManyPipelines\EMP time series exp\'
addpath('C:\Users\ecesnait\Desktop\EEGManyPipelines\Matlab Scripts\toolboxes\fieldtrip-master\')

ft_defaults;

% Find group folders
d = dir(datapath);
d = d([d.isdir]);
d = {d.name};
grps = d(~(strcmp('.',d)|strcmp('..',d)));

groups_to_inspect = {'93af9133e6fa6cf5','9985e8ae6679b0e2','cba9657d4b55f7ca',...
    'fc67513ed5c4b400', '344dd59ded90cb34','8559e4d7314e45ec','c577d3cdf78548ce','e72d90a6ff4b5108'}

% Loop over groups
num_trials = struct();

fields = {'label', 'time'};
field_names = {'man-made', 'natural'};
for gg = 6:length(groups_to_inspect)

    grp = groups_to_inspect{gg};
    disp(['Processing participant... ',num2str(gg)])

    if strcmp(grp,'93af9133e6fa6cf5')
        EEG = load([fullfile(datapath, 'EEGLAB all teams/93af9133e6fa6cf5.mat')])
        data = EEG.data;
        allsubjdat_manmade = []
        allsubjdat_natural = []
        for i=24:length(data)
            if i<24
                indx_manmade = startsWith({data(i).epoch.eventtype}, "1");  % all marke
                indx_natural = startsWith({data(i).epoch.eventtype}, "2");  % all marke
            else
                indx_manmade = startsWith(string([data(i).epoch.eventtype]), "1");  % all markers for man-made stimuli start with 1
                indx_natural = startsWith(string([data(i).epoch.eventtype]), "2");  % all markers for man-made stimuli start with 1

            end

            % Average across epochs based on condition
            manmade_avg_epoch = double(mean(data(i).EEGts(:,:,indx_manmade), 3));
            natural_avg_epoch = double(mean(data(i).EEGts(:,:,indx_natural), 3));
            groupdat.time = data(i).time;
            groupdat.chan = data(i).chan;
            if ~isempty(manmade_avg_epoch) % prepare the fttmp struct for man-made and natural data separately
                [fttmp_manmade, fttmp_natural] = ec_ssubj_erp_condition(groupdat, manmade_avg_epoch, natural_avg_epoch)

                % delete entry
                allsubjdat_manmade{size(allsubjdat_manmade,2)+1} = fttmp_manmade; % Avoids empty entries
                allsubjdat_natural{size(allsubjdat_natural,2)+1} = fttmp_natural; % Avoids empty entries

                clear manmade_avg_epoch natural_avg_epoch fttmp_natural fttmp_manmade indx_manmade indx_natural EEG

            end
        end
        % Make grand avg
        allgrpdat_manmade{gg} = ft_timelockgrandaverage([], allsubjdat_manmade{:});
        allgrpdat_natural{gg} = ft_timelockgrandaverage([], allsubjdat_natural{:});
        save('allgrpdat_manmade_93af9133e6fa6cf5', 'allgrpdat_manmade')
        save('allgrpdat_natural_93af9133e6fa6cf5', 'allgrpdat_natural')
        clear allsubjdat_natural allsubjdat_manmade fig data

    elseif strcmp(grp,'9985e8ae6679b0e2') % multiple markers
        continue
    elseif strcmp(grp,'cba9657d4b55f7ca')
        continue
    else
        % Loop over subjects
        subjects = dir([fullfile(datapath,grp, '*.mat')])
    end
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
if isequal(grp,'8559e4d7314e45ec')
              n_events = length(events_id);

  indx_manmade = logical(zeros(n_events, 1));
            indx_natural = logical(zeros(n_events, 1));
           
            [ev_id_manmade, ev_id_natural] = get_event_id(epo_data.events_id, field_names);
            for ne=1:length(ev_id_manmade)
              indx_manmade(events_id == ev_id_manmade(ne)) = 1;
            end
            for ne=1:length(ev_id_natural)
              indx_natural(events_id == ev_id_natural(ne)) = 1;
            end

else
            indx_manmade = startsWith(string(events_id), "1");  % all markers for man-made stimuli start with 1
            indx_natural = startsWith(string(events_id), "2");  % all markers for natural stimuli start with 2
end

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

    % check if all sbj have the same channels and time
    for nf=1:numel(fields)
        is_ok = check_field(allsubjdat_manmade, fields{nf});
        if is_ok == 0
            error('something wrong')
        end
    end

    for nf=1:numel(fields)
        is_ok = check_field(allsubjdat_natural, fields{nf});
        if is_ok == 0
            error('something wrong')
        end
    end
    %  test = ft_timelockgrandaverage([], allsubjdat_manmade{:});
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
unique(allsubjdat_manmade{1,1}.label)

