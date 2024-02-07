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
    datapath = 'N:\EMP\EEGManyPipelines\EMP data extracted\';
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

for gg = 21:40%length(grps)

    disp(['Processing participant... ',num2str(gg)])

    grp = grps{gg};

    % A special case - when a single team has more than 1 file, skip the
    % second file
    if endsWith(grp, '_2.mat'), continue, end

    % Load data
    groupdat = load(fullfile(datapath, grp));
    groupdat = groupdat.(string(fieldnames(groupdat))) % un-nest

    % If data is not epoched - skip it. TO DO: epoch non-epoched data
    if numel(size(groupdat(1).EEGts)) < 3 %check how many dimensions EEGts has. if it's conitnuous data:
        cnt = cnt + 1;
        continue
    else
        allsubjdat = []

        %Loop over participants
        for ss = 1:size(groupdat,2)
            % Average all trials (not careing about conditions for now!)
            sbj_avg_epoch = double(mean(groupdat(ss).EEGts, 3));      % dim 3 = trials

            % Find CPz channel
            %find_cpz = find(ismember(groupdat(ss).chan, 'CPz'));
            if ~isempty(sbj_avg_epoch)
                fttmp = [];
                fttmp.avg       = sbj_avg_epoch;%sbj_avg_epoch(find_cpz,:)
                fttmp.time      = groupdat(ss).time;
                fttmp.label     = groupdat(ss).chan;
                fttmp.dimord    = 'chan_time';

                allsubjdat{size(allsubjdat,2)+1} = fttmp; % Avoids empty entries
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

            for ss = 1:size(groupdat,2)
                % Average all trials (not careing about conditions for now!)
                sbj_avg_epoch = double(mean(groupdat(ss).EEGts, 3));      % dim 3 = trials

                % Find CPz channel
                %find_cpz = find(ismember(groupdat(ss).chan, 'CPz'));

                if ~isempty(sbj_avg_epoch)
                    fttmp = [];
                    fttmp.avg       = sbj_avg_epoch;%sbj_avg_epoch(find_cpz,:)
                    fttmp.time      = groupdat(ss).time;
                    fttmp.label     = groupdat(ss).chan;
                    fttmp.dimord    = 'chan_time';

                    allsubjdat{size(allsubjdat,2)+1} = fttmp;
                end
            end
            disp('done');
            clear groupdat infile
        end
    end

    % Make grand avg
    allgrpdat{gg} = ft_timelockgrandaverage([], allsubjdat{:});
    clear allsubjdata

end

%% ########################################################################
% End of data collection seteps. Below here are some tests doing plots and
% other on the collected data structure. It might be a good idea to save
% the data structure instead and do these steps in sperate scripts.


% remove empty entries due to continuous or corrupt data files
allgrpdat_full = allgrpdat(~cellfun(@isempty, allgrpdat))

%% Plot
cfg = [];
cfg.layout      = 'elec1010.lay';
cfg.showlabels  = 'yes';
ft_multiplotER(cfg, allgrpdat_full{2:5}) % Expects all datasets to have the same sampling rate!!!

cfg.channel = {'CPz'}
cfg.parameter = 'avg'

ft_singleplotER(cfg, allgrpdat_full{2:5}) % % Expects all datasets to have the same sampling rate!!! Use a simple plotting function below?

fig=figure
for p = 1:length(allgrpdat_full)
    cpz_indx = strcmp(allgrpdat_full{p}.label, 'FCz')
    % if the format is not channels x times, but times x channels
    if size(allgrpdat_full{p}.avg,1) > size(allgrpdat_full{p}.avg,2) 
        y = allgrpdat_full{p}.avg(:,cpz_indx); % CPz
    else
      y = allgrpdat_full{p}.avg(cpz_indx,:); % CPz
    end
    x = allgrpdat_full{p}.time;
    plot(x,y, 'LineWidth',1.2) ,fontsize(fig, 15, "points"), xlim([-100, 500]), title('Great Grand Average (FCz), N=18'),...
    xlabel('Time(ms)'), ylabel('uV'), hold on
end



greatgrandavg = ft_timelockgrandaverage([], allgrpdat_full{:});

cfg = [];
cfg.layout      = 'elec1010.lay';
cfg.showlabels  = 'yes';
cfg.parameter   = 'var';
ft_multiplotER(cfg, greatgrandavg)



%% Dispersion
cfg = [];
cfg.keepindividual = 'yes';
ggdat = ft_timelockgrandaverage(cfg, allgrpdat_full{:});

ampMat = 1.4826*mad(ggdat.individual, 1, 3)';
dispersion = 1.4826*mad(ampMat,1,2) ./ median(ampMat,2);

cfg = [];
cfg.avgovertime = 'yes';
dispdat = ft_selectdata(cfg, greatgrandavg);
dispdat.dispersion = dispersion;

cfg = [];
cfg.layout      = 'elec1010.lay';
cfg.showlabels  = 'no';
cfg.parameter   = 'dispersion';
ft_topoplotER(cfg, dispdat)

%% Alpha
% reshap
X = reshape(ggdat.individual, [3,63*90]);

alf = reliability_analysis(X,'interval');






