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
        allsubjdat_manmade = []
        allsubjdat_natural = []

        %Loop over participants
        while size(groupdat,2) > 0 % saving memory in the workspace
            % Average all trials (not careing about conditions for now!)
            %sbj_avg_epoch = double(mean(groupdat(1).EEGts, 3));      % dim 3 = trials

            % Split based on condition: man-made vs natural for H1
            % events fro man-made stimuli starts with 1 and natural starts with 2 
            indx_manmade = find(strcmp([groupdat(1).epoch.eventscene_category], 'manmade'));
            indx_natural = find(strcmp([groupdat(1).epoch.eventscene_category], 'natural'));

            manmade_avg_epoch = double(mean(groupdat(1).EEGts(:,:,indx_manmade), 3));
            natural_avg_epoch = double(mean(groupdat(1).EEGts(:,:,indx_natural), 3));

            if ~isempty(manmade_avg_epoch)
                fttmp_manmade = [];
                fttmp_manmade.avg       = manmade_avg_epoch;%sbj_avg_epoch(find_cpz,:)
                fttmp_manmade.time      = groupdat(1).time;
                fttmp_manmade.label     = groupdat(1).chan;
                fttmp_manmade.dimord    = 'chan_time';

                fttmp_natural = [];
                fttmp_natural.avg       = natural_avg_epoch;%sbj_avg_epoch(find_cpz,:)
                fttmp_natural.time      = groupdat(1).time;
                fttmp_natural.label     = groupdat(1).chan;
                fttmp_natural.dimord    = 'chan_time';

                %delete entry
                groupdat(1) = [];
                allsubjdat_manmade{size(allsubjdat_manmade,2)+1} = fttmp_manmade; % Avoids empty entries
                allsubjdat_natural{size(allsubjdat_natural,2)+1} = fttmp_natural; % Avoids empty entries

                clear manmade_avg_epoch natural_avg_epoch fttmp_natural fttmp_manmade
            else
                groupdat(1) = [];
            end
        end
        disp('done');
        clear groupdat infile

        % Special cases when a single team has more than 1 file - STILL
        % NEEDS UPDATE  
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
    
    %plot
    figure,
            plot(groupdat(1).time, manmade_avg_epoch(32,:),'LineWidth',1), hold on
            plot(groupdat(1).time,natural_avg_epoch(32,:),'LineWidth',1), legend({'man made', 'natural'})
            fig = gcf
            exportgraphics(fig, 'ERPs_condition_EEGLAB.pdf','Append',true)
            close(fig)
    clear allsubjdat
end
save('allgrpdat_61_90.mat', 'allgrpdat')

