% emp_timeseries_02_load_team.m

% clear all; close all; clc

% ----------------------------------------------------------------------- %
%% Set up directories:

dirs            = 'C:\Users\ecesnait\Desktop\EEGManyPipelines\git\EEGManyPipes org\Main_Paper\';
dirs.root       = 'M:\EMP\EEGManyPipelines\EMP time series exp\EEGLAB all teams';
dirs.scripts    = fullfile(dirs.root, 'code/Main_Paper/johannes/timeseries');
dirs.data       = fullfile(dirs.root, 'data/processedData/timeseries');
dirs.n100       = fullfile(dirs.data, 'N100 amplitudes');
dirs.EEG3D      = fullfile(dirs.data, 'EEG3D');

% ----------------------------------------------------------------------- %
%% Add fieldtrip toolbox:

dirs.fieldtrip = 'C:/Users/johan/OneDrive/Documents/github-repositories/fieldtrip';
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
FS              = 256;          % sampling rate in Hz
timeWindow      = [-200 500];   % consistent trial epoching (in ms)

timeVecDes      = timeWindow(1):((1/FS)*1000):timeWindow(end); % desired time bins after resampling (in ms)
nTimeDes        = length(timeVecDes); % number desired time bins

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
selPattern  = '*EEG3d_struct.mat';
fprintf('*** Search for files with pattern %s ... ***\n', selPattern);
tmp         = dir(fullfile(dirs.EEG3D, selPattern));
teamList    = {tmp.name};
nTeam       = length(teamList);
fprintf('*** Found %d files:\n%s ***\n', nTeam, strjoin(teamList, ', '));

% Initialize as cells:
allAvgMat       = cell(nTeam, nSub);
allVarMat       = cell(nTeam, nSub);
allDofMat       = cell(nTeam, nSub);

% Or initialize as 4D matrices:
% allAvgMat       = nan(nTeam, nSub, nChan, nTimeDes);
% allVarMat       = nan(nTeam, nSub, nChan, nTimeDes);
% allDofMat       = nan(nTeam, nSub, nChan, nTimeDes);

% ----------------------------------------------------------------------- %
%% Loop over teams:

for iTeam = 1:nTeam % iTeam = 3;

    % Load data:
    fprintf('*** ========================================================== ***\n');
    fprintf('*** TEAM %s: Start loading data ...  ***\n', teamList{iTeam});
    rawData         = load(fullfile(dirs.EEG3D, teamList{iTeam}));
    rawData         = rawData.team_eeg;
    fprintf('*** ... finished :-) ***\n');

    nSubFound       = size(rawData, 2);
    fprintf('*** Team %s: Found data from %02d subjects ***\n', teamList{iTeam}, nSubFound);

    % Loop over found subjects:
    for iSub = 1:nSubFound % iSub = 1;

        fprintf('*** --------------------------------------------- ***\n');
        fprintf('*** Subject %02d: START ***\n', iSub);

        % Extract data dimensions:
        nChanFound          = size(rawData(iSub).eeg3d, 1);
        nTimeFound          = size(rawData(iSub).eeg3d, 2);
        nTrialFound         = size(rawData(iSub).eeg3d, 3);
        fprintf('*** Subject %02d: Found data from %d channels, %d time points, %d trials ***\n', ...
            iSub, nChanFound, nTimeFound, nTrialFound);

        % --------------------------------------------------------------- %
        %% Create Fieldtrip structure:
        % https://github.com/fieldtrip/fieldtrip/blob/release/utilities/ft_datatype_raw.m

        % Check if data exists (or subject excluded), if not, then skip:
        if isempty(rawData(iSub).eeg3d)

            fprintf('*** Subject %02d: Data is empty, skip subject ***\n', iSub);

        else 
            fprintf('*** Subject %02d: Cast into Fieldtrip structure ***\n', iSub);
            data1                   = []; % initialize
            data1.dimord            = 'chan_time'; % dimension order for each trial
            timeVecFound            = rawData(iSub).time_msec; % extract time bin labels
            if (timeVecFound(end) - timeVecFound(1)) < 10; timeVecFound = timeVecFound * 1000; end % convert from sec to ms
            data1.time              = repmat({timeVecFound}, 1, nTrialFound); % time bin labels
            data1.label             = rawData(iSub).chan_label; % channel labels
            data1.trial             = squeeze(mat2cell(rawData(iSub).eeg3d, nChanFound, nTimeFound, ones(1, nTrialFound)))'; % data
            if isfield(rawData(iSub).epoch, 'value')
                data1.trialinfo         = rawData(iSub).epoch.value'; % trigger values per trial (as single row, thus transposed)
            end
            % data1.sampleinfo % can be left out; will be automatically created
            % by ft_preprocessing below
       
            % --------------------------------------------------------------- %
            %% Reference to grand mean:
    
            fprintf('*** Subject %02d: Reference to grand average ***\n', iSub);
            cfg                     = [];
            cfg.reref               = 'yes';
            cfg.refchannel          = data1.label; % use all channels
            cfg.refmethod           = 'avg';
            data2                   = ft_preprocessing(cfg, data1);
    
            % --------------------------------------------------------------- %
            %% Re-sample to consistent time bins:
    
            % Limit new time vector to boundaries of data available:
            fprintf('*** Subject %02d: Resample to new time bins ***\n', iSub);
            timeVecNew     = timeVecDes(timeVecDes >= rawData(iSub).time_msec(1) & timeVecDes <= rawData(iSub).time_msec(end));
        
            cfg                     = [];
            cfg.time                = repmat({timeVecNew}, 1, nTrialFound); % timeVecNew;
            cfg.demean              = 'no';
            cfg.detrend             = 'no';
            data3                   = ft_resampledata(cfg, data2);
            % data3.time{1}
    
            % --------------------------------------------------------------- %
            %% Epoch data to time window of interest:
    
            fprintf('*** Subject %02d: Select data in time window of %03d - +%03d ms***\n', ...
                iSub, timeWindow(1), timeWindow(end));

            cfg                     = [];
            cfg.latency             = timeWindow;
            cfg.avgovertime         = 'no';
            cfg.nanmean             = 'yes';
            data4                   = ft_selectdata(cfg, data3);
    
            % --------------------------------------------------------------- %
            %% Compute summary statistic (mean, variance, dof) across trials:
    
            fprintf('*** Subject %02d: Compute summary statistics across trials ***\n', iSub);
            cfg                     = [];
            data5                   = ft_timelockanalysis(cfg, data4);
    
            % --------------------------------------------------------------- %
            %% Insert NaN for empty time bins:
            % https://www.fieldtriptoolbox.org/faq/how_can_i_interpret_the_different_types_of_padding_that_i_find_when_dealing_with_artifacts/
    
            fprintf('*** Subject %02d: Insert NaN for non-existing time bins ***\n', iSub);
            timeIdx                 = find(ismember(timeVecDes, data5.time)); % selection of desired time bins present 
            data6                   = data5; % copy over
            data6.avg               = nan(nChanFound, nTimeDes); % initialize
            data6.var               = nan(nChanFound, nTimeDes); % initialize
            data6.dof               = nan(nChanFound, nTimeDes); % initialize
            data6.avg(:, timeIdx)   = data5.avg; % copy over for available time bins
            data6.var(:, timeIdx)   = data5.var; % copy over for available time bins
            data6.dof(:, timeIdx)   = data5.dof; % copy over for available time bins
    
            % --------------------------------------------------------------- %
            %% Insert NaN for missing channels, sort channels:
    
            fprintf('*** Subject %02d: Insert NaN for non-existing channels ***\n', iSub);
    
            % Loop over all theoretically existing channels, fill if existing:
            data7                   = data6; % copy over
            data7.label             = eegChanVec; % fill in names of all EEG channels
            data7.avg               = nan(nChan, nTimeDes); % initialize
            data7.var               = nan(nChan, nTimeDes); % initialize
            data7.dof               = nan(nChan, nTimeDes); % initialize
    
            % Loop over all possibly available channels (alphabetically):
            for iChan = 1:nChan % iChan = 1;
                if ismember(data7.label{iChan}, data6.label) % check if channel exists
                    chanIdx     = find(strcmp(data7.label{iChan}, data6.label)); % index of this channel in previous data
                    data7.avg(iChan, :) = data6.avg(chanIdx, :); % copy over
                    data7.var(iChan, :) = data6.var(chanIdx, :); % copy over
                    data7.dof(iChan, :) = data6.dof(chanIdx, :); % copy over
                end
            end
        
            % --------------------------------------------------------------- %
            %% Save chan x time matrix in teams x subjects cell:
    
            subID                       = str2double(cell2mat(extractBetween(rawData(iSub).subjID, 5, 6)));
            fprintf('*** Subject %02d: Save under subject ID %02d ***\n', iSub, subID);
    
            % Save in cell:
            allAvgMat{iTeam, subID}     = data7.avg;
            allVarMat{iTeam, subID}     = data7.var;
            allDofMat{iTeam, subID}     = data7.dof;
    
            % Or save in 4D matrix:
            % allAvgMat(iTeam, subID, :, :)   = data7.avg;
            % allVarMat(iTeam, subID, :, :)   = data7.var;
            % allDofMat(iTeam, subID, :, :)   = data7.dof;

        end % if data is empty
        fprintf('*** Subject %02d: FINISHED ***\n', iSub);
    end % end iSub
    fprintf('*** TEAM %s: FINISHED  ***\n', teamList{iTeam});
end % end iTeam

% Save data matrices:
% save(allAvgMat, fullfile(dirs.save, 'allAvgMat.mat'));
% save(allVarMat, fullfile(dirs.save, 'allVarMat.mat'));
% save(allDofMat, fullfile(dirs.save, 'allDofMat.mat'));

% END OF FILE.