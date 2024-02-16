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
d = dir([datapath, '*_EEG3d_struct.mat']);
grps = {d.name};

%% Collect data from all groups.
% Average withing subject within group, then average all subjects within
% group (grand average). Collect grand averages for comparison across
% groups.

% Loop over groups
allgrpdat = {};
for gg = 1:length(grps)
    %     gg=1;
    grp = grps{gg};
    disp(['Processing participant... ',num2str(gg)])

    teamID = extractBefore(grp,'_')

    % Load data
    %chan_time_epoch = load(fullfile(datapath,[teamID,'_BAD_chan_trial.mat']));
    EEG = load(fullfile(datapath,[teamID,'_EEG3d_struct.mat']));

    % Check if data is epoched. If not, skip this subject
    if numel(size(infile.data)) < 3
        continue
    end

    % Average all trials (not careing about conditions for now!)
    avgdat = double(mean(infile.data, 3));      % dim 3 = trials
    label = cell(length(infile.chs_name),1);
    for x = 1:length(infile.chs_name)
        label{x} = strtrim(infile.chs_name(x,:));
    end


    fttmp = [];
    fttmp.avg       = avgdat;
    fttmp.time      = infile.time;
    fttmp.label     = label;
    fttmp.dimord    = 'chan_time';

    allsubjdat{ss} = fttmp;
    disp(['done ', num2str(ss)]);
    clear subjdat infile

    % Make grand avg
    allgrpdat{gg} = ft_timelockgrandaverage([], allsubjdat{:});
    clear allsubjdat fttmp infile avgdat label

end
save('allgrpdat_91_126.mat', 'allgrpdat')
