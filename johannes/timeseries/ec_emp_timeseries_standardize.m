% emp_timeseries_02_load_team.m

clear all; close all; clc

% ----------------------------------------------------------------------- %
%% Set up directories:
dirs = [];
dirs.root       = 'C:\Users\ecesnait\Desktop\EEGManyPipelines\git\EEGManyPipes org\Main_Paper\';
dirs.scripts    = fullfile(dirs.root, 'johannes\timeseries');
dirs.data       = fullfile('M:\EMP\EEGManyPipelines\EMP time series exp\EEGLAB all teams');
dirs.saveDir = 'M:\EMP\EEGManyPipelines\EMP time series exp\Standardized\'
%dirs.n100       = fullfile(dirs.data, 'N100 amplitudes');
%dirs.EEG3D      = fullfile(dirs.data, 'EEG3D');

% ----------------------------------------------------------------------- %
%% Add fieldtrip toolbox:

dirs.fieldtrip = 'C:\Users\ecesnait\Desktop\EEGManyPipelines\Matlab Scripts\toolboxes\fieldtrip-master\';
if ~contains(lower(path), lower(fullfile(dirs.fieldtrip)))
    fprintf('Add Fieldtrip\n');
    addpath(dirs.fieldtrip);
end

cd(dirs.fieldtrip)

ft_defaults;
global ft_default;
ft_default.showlogo = 'no';

% ----------------------------------------------------------------------- %
%% Set input settings:

% Desired time settings:
FS              = 256; % sampling rate in Hz
timeWindow      = [-200 600]; % consistent trial epoching (in ms)
timeVec_standard1 = flip([ 0 : -1/FS*1000 : timeWindow(1) ]);
timeVec_standard2        = [ 0 : 1/FS*1000 : timeWindow(2) ];
timeVec_standard         = [ timeVec_standard1(1:end-1), timeVec_standard2 ];
%timeVecDes      = timeWindow(1):((1/FS)*1000):timeWindow(end); % desired time bins after resampling (in ms)
nTimeDes        = length(timeVec_standard); % number desired time bins

% Expected channels:
% sort(rawData(1).chan_label)'
allChanVec      = {...
    'AF3', 'AF4', 'AF7', 'AF8', 'AFz', 'Afp10', 'Afp9', 'C1', 'C2', 'C3', 'C4', 'C5', 'C6', ...
    'CP1', 'CP2', 'CP3', 'CP4', 'CP5', 'CP6', 'CPz', 'Cz', 'F1', 'F2', 'F3', 'F4', 'F5', 'F6', ...
    'F7', 'F8', 'FC1', 'FC2', 'FC3', 'FC4', 'FC5', 'FC6', 'FCz', 'FT7', 'FT8', 'Fp1', 'Fp2', ...
    'Fpz', 'Fz', 'HEOG', 'IO1', 'IO2', 'Iz', 'M1', 'M2', 'O1', 'O2', 'Oz', 'P1', 'P10', 'P2', ...
    'P3', 'P4', 'P5', 'P6', 'P7', 'P8', 'P9', 'PO3', 'PO4', 'PO7', 'PO8', 'POz', 'Pz', 'T7', ...
    'T8', 'TP7', 'TP8', 'VEOG' ...
    }; % extract original channels, sorted alphabetically
nonEEGChanVec   = {'Afp10', 'Afp9', 'HEOG', 'IO1', 'IO2', 'M1', 'M2', 'VEOG'}; % channels 65-72 in dataset documentation
eegChanVec      = allChanVec(~ismember(allChanVec, nonEEGChanVec)); % EEG channels
nChan           = length(eegChanVec);

% Expected number subjects:
nSub            = 33;

% ----------------------------------------------------------------------- %
%% Detect input data, set settings:

% Detect all files:
selPattern  = '*.mat';
fprintf('*** Search for files with pattern %s ... ***\n', selPattern);
tmp         = dir(fullfile(dirs.data, selPattern));
teamList    = {tmp.name};
nTeam       = length(teamList);
fprintf('*** Found %d files:\n%s ***\n', nTeam, strjoin(teamList, ', '));

% ----------------------------------------------------------------------- %
%% Loop over teams:

for iTeam = 1:nTeam % 15 problemw it resampling (interpolation problem) - perhaps the format, 21,24 out of memory
    if ismember(iTeam,[1,15,21,24]) % skip temporarily
        continue
    end

    alldatstand = cell(1,nSub)
    if endsWith(teamList{iTeam}, '_2.mat')
        continue
    end
    % Load data:
    fprintf('*** ========================================================== ***\n');
    fprintf('*** TEAM %s: Start loading data ...  ***\n', teamList{iTeam});
    rawData         = load(fullfile(dirs.data, teamList{iTeam}));
    rawData         = rawData.data;
    fprintf('*** ... finished :-) ***\n');

    nSubFound       = size(rawData, 2);
    fprintf('*** Team %s: Found data from %02d subjects ***\n', teamList{iTeam}, nSubFound);

    % If data is not epoched - skip it.
    if numel(size(rawData(1).EEGts)) < 3 %check how many dimensions EEGts has. if it's conitnuous data:
        fprintf('*** Team %s: the EEG data is not epoched', teamList{iTeam});
        clear rawData
        continue
    end

    % A few special cases to deal with
    if isequal(teamList{iTeam},'d9a070789fe1b133.mat')
        error('Elena: inspect')
        for i = 1:33
            allsubjdat{i}.label = allsubjdat{i}.label{1:end};
        end
    elseif isequal(teamList{iTeam},'d5c8ed05b7af02a3.mat')
        error('Elena: inspect')
        chan_labels = allgrpdat{74}.label
        for i=1:33
            curr_lab = allsubjdat{i}.label;
            curr_lab(curr_lab == -1) = 0;
            allsubjdat{i}.label = chan_labels(find(curr_lab));
            allsubjdat{i}.avg = allsubjdat{i}.avg(find(curr_lab),:);
            clear curr_lab
        end
    end

    % Loop over found subjects:
    for iSub = 1:nSubFound % iSub = 1;

        fprintf('*** --------------------------------------------- ***\n');
        fprintf('*** Subject %02d: START ***\n', iSub);

        % Extract data dimensions:
        nChanFound = []
        nChanFound          = size(rawData(iSub).EEGts, 1);
        nTimeFound          = size(rawData(iSub).EEGts, 2);
        nTrialFound         = size(rawData(iSub).EEGts, 3);
        fprintf('*** Subject %02d: Found data from %d channels, %d time points, %d trials ***\n', ...
            iSub, nChanFound, nTimeFound, nTrialFound);

        %% Keep only the selected channels
        indx_chan = ismember(rawData(iSub).chan, eegChanVec)
        rawData(iSub).chan = rawData(iSub).chan(indx_chan)
        if size(rawData(iSub).EEGts,1) <=72
           rawData(iSub).EEGts = rawData(iSub).EEGts(indx_chan,:,:)
        else
            error('Elena: isnpect dimentions')
        end

        % --------------------------------------------------------------- %
        %% Create Fieldtrip structure:
        % https://github.com/fieldtrip/fieldtrip/blob/release/utilities/ft_datatype_raw.m

        % Check if data exists (or subject excluded), if not, then skip:
        if isempty(rawData(iSub).EEGts)

            fprintf('*** Subject %02d: Data is empty, skip subject ***\n', iSub);
            continue

        else
            fprintf('*** Subject %02d: Cast into Fieldtrip structure ***\n', iSub);
            data1                   = []; % initialize
            data1.dimord            = 'chan_time'; % dimension order for each trial
            timeVecFound            = rawData(iSub).time; % extract time bin labels
            if (timeVecFound(end) - timeVecFound(1)) < 10; timeVecFound = timeVecFound * 1000; end % convert from sec to ms
            data1.time              = repmat({timeVecFound}, 1, nTrialFound); % time bin labels
            data1.label             = rawData(iSub).chan; % channel labels
            data1.trial             = squeeze(mat2cell(rawData(iSub).EEGts, nChanFound, nTimeFound, ones(1, nTrialFound)))'; % data
            if isfield(rawData(iSub).epoch, 'value')
                data1.trialinfo         = rawData(iSub).epoch.value'; % trigger values per trial (as single row, thus transposed)
            end
            % --------------------------------------------------------------- %
            %% Re-sample to consistent time bins:

            % Limit new time vector to boundaries of data available:
            fprintf('*** Subject %02d: Resample to new time bins ***\n', iSub);
            timeVecNew     = timeVec_standard(timeVec_standard >= rawData(iSub).time(1) & timeVec_standard <= rawData(iSub).time(end));

            cfg                     = [];
            cfg.time                = repmat({timeVecNew}, 1, nTrialFound); % timeVecNew;
            cfg.demean              = 'no';
            cfg.detrend             = 'no';
            data3                   = ft_resampledata(cfg, data1);
            % data3.time{1}

            % --------------------------------------------------------------- %
            %% Epoch data to time window of interest:

            fprintf('*** Subject %02d: Select data in time window of %03d - +%03d ms***\n', ...
                iSub, timeWindow(1), timeWindow(end));
            cfg                     = [];
            cfg.latency             = timeWindow;
            cfg.avgovertime         = 'no';
            cfg.nanmean             = 'yes';
            data4 = []
            data4                   = ft_selectdata(cfg, data3);
            data4.event = rawData(iSub).epoch;

            % --------------------------------------------------------------- %
            %             %% Compute summary statistic (mean, variance, dof) across trials:
            %
            %             fprintf('*** Subject %02d: Compute summary statistics across trials ***\n', iSub);
            %             cfg                     = [];
            %             data5                   = ft_timelockanalysis(cfg, data4);
            %
            % --------------------------------------------------------------- %
            %% Insert NaN for empty time bins:
            % https://www.fieldtriptoolbox.org/faq/how_can_i_interpret_the_different_types_of_padding_that_i_find_when_dealing_with_artifacts/
            %
            %             fprintf('*** Subject %02d: Insert NaN for non-existing time bins ***\n', iSub);
            %             timeIdx                 = find(ismember(timeVecDes, data4.time{1})); % selection of desired time bins present
            %             data6                   = data5; % copy over
            %             data6.avg               = nan(nChanFound, nTimeDes); % initialize
            %             data6.var               = nan(nChanFound, nTimeDes); % initialize
            %             data6.dof               = nan(nChanFound, nTimeDes); % initialize
            %             data6.avg(:, timeIdx)   = data5.avg; % copy over for available time bins
            %             data6.var(:, timeIdx)   = data5.var; % copy over for available time bins
            %             data6.dof(:, timeIdx)   = data5.dof; % copy over for available time bins
            %
            % --------------------------------------------------------------- %
            %% Insert NaN for missing channels, sort channels:

            %             fprintf('*** Subject %02d: Insert NaN for non-existing channels ***\n', iSub);
            %
            %             % Loop over all theoretically existing channels, fill if existing:
            %             data7                   = data6; % copy over
            %             data7.label             = eegChanVec; % fill in names of all EEG channels
            %             data7.avg               = nan(nChan, nTimeDes); % initialize
            %             data7.var               = nan(nChan, nTimeDes); % initialize
            %             data7.dof               = nan(nChan, nTimeDes); % initialize
            %
            % Loop over all possibly available channels (alphabetically):
            %             for iChan = 1:nChan % iChan = 1;
            %                 if ismember(data7.label{iChan}, data6.label) % check if channel exists
            %                     chanIdx     = find(strcmp(data7.label{iChan}, data6.label)); % index of this channel in previous data
            %                     data7.avg(iChan, :) = data6.avg(chanIdx, :); % copy over
            %                     data7.var(iChan, :) = data6.var(chanIdx, :); % copy over
            %                     data7.dof(iChan, :) = data6.dof(chanIdx, :); % copy over
            %                 end
            %             end
            %
            % --------------------------------------------------------------- %
            %% Save chan x time matrix in teams x subjects cell:

            subID = str2double(extract(rawData(iSub).subID,digitsPattern));%str2double(cell2mat(extractBetween(rawData(iSub).subID, 5, 6)));
            fprintf('*** Subject %02d: Save under subject ID %02d ***\n', iSub, subID);

            % Save in cell:
            alldatstand{1,subID} = data4;
            alldatstand{2,subID} = ['Subj-',num2str(subID)];

        end % if data is empty
        fprintf('*** Subject %02d: FINISHED ***\n', iSub);
        clear data1 data3 data5 data6 data7 timeIdx cfg timeVecNew timeVecFound  nTimeFound nTrialFound

    end % end iSub
    fprintf('*** TEAM %s: FINISHED  ***\n', teamList{iTeam});

    %% A special case - when a single team has more than 1 file, skip the
    % second file
    if endsWith(teamList{iTeam}, '_1.mat')
        %         error('Eelena: inspect both files')
        clear rawData data4
        rawData         = load(fullfile(dirs.data, [extractBefore(teamList{iTeam}, '_1'),'_2.mat']));
        rawData         = rawData.data;
        nSubFound       = sum(~cellfun(@isempty,{rawData.subID}));
        indx_sub = find(~cellfun(@isempty,{rawData.subID}));

        % Loop over found subjects:
        for iSub = 1:nSubFound % iSub = 1;
            iSub = indx_sub(iSub)
            fprintf('*** --------------------------------------------- ***\n');
            fprintf('*** Subject %02d: START ***\n', iSub);

            % Extract data dimensions:
            nChanFound = []
            nChanFound          = size(rawData(iSub).EEGts, 1);
            nTimeFound          = size(rawData(iSub).EEGts, 2);
            nTrialFound         = size(rawData(iSub).EEGts, 3);
            fprintf('*** Subject %02d: Found data from %d channels, %d time points, %d trials ***\n', ...
                iSub, nChanFound, nTimeFound, nTrialFound);

            % --------------------------------------------------------------- %
            %% Create Fieldtrip structure:
            % Check if data exists (or subject excluded), if not, then skip:
            if isempty(rawData(iSub).EEGts)

                fprintf('*** Subject %02d: Data is empty, skip subject ***\n', iSub);
                continue

            else
                fprintf('*** Subject %02d: Cast into Fieldtrip structure ***\n', iSub);
                data1                   = []; % initialize
                data1.dimord            = 'chan_time'; % dimension order for each trial
                timeVecFound            = rawData(iSub).time; % extract time bin labels
                if (timeVecFound(end) - timeVecFound(1)) < 10; timeVecFound = timeVecFound * 1000; end % convert from sec to ms
                data1.time              = repmat({timeVecFound}, 1, nTrialFound); % time bin labels
                data1.label             = rawData(iSub).chan; % channel labels
                data1.trial             = squeeze(mat2cell(rawData(iSub).EEGts, nChanFound, nTimeFound, ones(1, nTrialFound)))'; % data
                if isfield(rawData(iSub).epoch, 'value')
                    data1.trialinfo         = rawData(iSub).epoch.value'; % trigger values per trial (as single row, thus transposed)
                end
                % --------------------------------------------------------------- %
                %% Re-sample to consistent time bins:

                % Limit new time vector to boundaries of data available:
                fprintf('*** Subject %02d: Resample to new time bins ***\n', iSub);
                timeVecNew     = timeVecDes(timeVecDes >= rawData(iSub).time(1) & timeVecDes <= rawData(iSub).time(end));

                cfg                     = [];
                cfg.time                = repmat({timeVecNew}, 1, nTrialFound); % timeVecNew;
                cfg.demean              = 'no';
                cfg.detrend             = 'no';
                data3                   = ft_resampledata(cfg, data1);
                % data3.time{1}

                % --------------------------------------------------------------- %
                %% Epoch data to time window of interest:

                fprintf('*** Subject %02d: Select data in time window of %03d - +%03d ms***\n', ...
                    iSub, timeWindow(1), timeWindow(end));
                cfg                     = [];
                cfg.latency             = timeWindow;
                cfg.avgovertime         = 'no';
                cfg.nanmean             = 'yes';
                data4 = []
                data4                   = ft_selectdata(cfg, data3);
                data4.event = rawData(iSub).epoch;
                %------------------------------------------------------------- %
                %% Save chan x time matrix in teams x subjects cell:

                subID = str2double(extract(rawData(iSub).subID,digitsPattern));%str2double(cell2mat(extractBetween(rawData(iSub).subID, 5, 6)));
                fprintf('*** Subject %02d: Save under subject ID %02d ***\n', iSub, subID);

                % Save in cell:
                alldatstand{1,subID} = data4;
                alldatstand{2,subID} = ['Subj-',num2str(subID)];

            end % if data is empty
            fprintf('*** Subject %02d: FINISHED ***\n', iSub);
            clear data1 data3 data5 data6 data7 timeIdx cfg timeVecNew timeVecFound  nTimeFound nTrialFound

        end % end iSub
        fprintf('*** TEAM %s: FINISHED  ***\n', teamList{iTeam});
    end

    %save
    save([dirs.saveDir,'standart_',extractBefore(teamList{iTeam},'.mat')], 'alldatstand','-v7.3')

    %create a report

    fileID = fopen([dirs.saveDir, 'standartization_report.txt'],'a+');
    fprintf(fileID,'\n %s',datestr(datetime), ...
        ['Processed team: ', teamList{iTeam}], ['Time window: ', num2str(data4.time{1,1}(1)),' to ', num2str(data4.time{1,1}(end))],...
        ['Number of channels: ', num2str(nChanFound)]);
    fclose(fileID);

    clear  rawData alldatstand data4 nChanFound
end % end iTeam


% END OF FILE.