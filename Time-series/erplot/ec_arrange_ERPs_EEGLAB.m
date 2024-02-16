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
    datapath = 'N:\EMP\EEGManyPipelines\EMP time series exp\EEGLAB all teams\';
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

% Loop over groups
allgrpdat = cell(size(grps));
cnt = 0; % variable that storees how many data

for gg = 1:length(grps) % File 24-26, 69, 89 could not be loaded due to memory problems. Files 65 and 66 (same group) did not provide channel and time info, file 86 has no time info

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
        cnt = cnt + 1;
        clear groupdat
        continue
    else
        allsubjdat = []

        %Loop over participants
        while size(groupdat,2) > 0 % saving memory in the workspace
            % Average all trials (not careing about conditions for now!)
            sbj_avg_epoch = double(mean(groupdat(1).EEGts, 3));      % dim 3 = trials

            % Find CPz channel
            %find_cpz = find(ismember(groupdat(ss).chan, 'CPz'));
            if ~isempty(sbj_avg_epoch)
                fttmp = [];
                fttmp.avg       = sbj_avg_epoch;%sbj_avg_epoch(find_cpz,:)
                fttmp.time      = groupdat(1).time;
                fttmp.label     = groupdat(1).chan;
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

        % Special cases when a single team has more than 1 file
        if endsWith(grp, '_1.mat')
            % Load data
            grp = grps{gg+1}
            groupdat = load(fullfile(datapath, grp));
            groupdat = groupdat.(string(fieldnames(groupdat)))

            while size(groupdat,2) > 0
                % Average all trials (not careing about conditions for now!)
                sbj_avg_epoch = double(mean(groupdat(1).EEGts, 3));      % dim 3 = trials

                % Find CPz channel
                %find_cpz = find(ismember(groupdat(ss).chan, 'CPz'));

                if ~isempty(sbj_avg_epoch)
                    fttmp = [];
                    fttmp.avg       = sbj_avg_epoch;%sbj_avg_epoch(find_cpz,:)
                    fttmp.time      = groupdat(1).time;
                    fttmp.label     = groupdat(1).chan;
                    fttmp.dimord    = 'chan_time';
                    groupdat(1) = []
                    allsubjdat{size(allsubjdat,2)+1} = fttmp;
                    clear sbj_avg_epoch fttmp
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
        for i = 1:33
            allsubjdat{i}.label = allsubjdat{i}.label{1:end};
        end
    elseif isequal(grp,'d5c8ed05b7af02a3.mat')
        chan_labels = allgrpdat{74}.label
        for i=1:33
            curr_lab = allsubjdat{i}.label;
            curr_lab(curr_lab == -1) = 0;
            allsubjdat{i}.label = chan_labels(find(curr_lab));
            allsubjdat{i}.avg = allsubjdat{i}.avg(find(curr_lab),:);
            clear curr_lab
        end
    end

    % Make grand avg
    allgrpdat{gg} = ft_timelockgrandaverage([], allsubjdat{:});
    clear allsubjdat
end
save('allgrpdat_61_90.mat', 'allgrpdat')

