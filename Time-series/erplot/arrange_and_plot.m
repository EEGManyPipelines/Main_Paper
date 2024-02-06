%% Import and plot EMP ERP data

% For each group:
%   1) do subj avg
%   2) do grp grand avg
% Then collect all data 
%

% Paths
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

%% Collect data from all groups. Average withing subject within group, then 
% average all subjects within group (grand average). Collect grand averages
% for comparison across groups.

% Loop over groups
allgrpdat = cell(size(grps));
for gg = 1:length(grps)
%     gg=1;
    grp = grps{gg};

    % loop over subjects
    allsubjdat = cell(size(subjects));
    for ss = 1:length(subjects)
%         ss = 1;
        subj = subjects{ss};
        infile = find_files(fullfile(datapath,grp), subj);
        fprintf('Loading subj %s for group %s (%i of %i)...\n', subj, grp, gg, length(grps));
    
        % NEED CHECK TO SEE IF FILE EXIST (in case grp rejected subjs)
        % - are rejected subject data still in folders? If so, we should
        % remove it when doing grand average.

        % Load data
        subjdat = load(fullfile(datapath, grp, infile{:}));
        
        % Average all trials (not careing about conditions for now!)
        subjdat.avgdat = double(mean(subjdat.data, 3));      % dim 3 = trials
        label = cell(length(subjdat.chs_name),1);
        for x = 1:length(subjdat.chs_name)
            label{x} = strtrim(subjdat.chs_name(x,:));
        end
    
        fttmp = [];
        fttmp.avg       = subjdat.avgdat;
        fttmp.time      = subjdat.time;
        fttmp.label     = label;
        fttmp.dimord    = 'chan_time';
    %     ft_datatype(tmp)
    
    %     % Inspect (uncomment)
    %     cfg = [];
    %     cfg.layout      = 'elec1010.lay';
    %     cfg.showlabels  = 'yes'
    %     ft_multiplotER(cfg, tmp)
    
        allsubjdat{ss} = fttmp;
        disp('done');
        clear subjdat infile
    end

    % Make grand avg
    allgrpdat{gg} = ft_timelockgrandaverage([], allsubjdat{:});
    clear allsubjdata

    % Inspect (uncomment)
%     cfg = [];
%     cfg.layout      = 'elec1010.lay';;
%     cfg.showlabels  = 'yes';
%     ft_multiplotER(cfg, allgrpdat{gg} )
end

%% Plot
cfg = [];
cfg.layout      = 'elec1010.lay';
cfg.showlabels  = 'yes';
ft_multiplotER(cfg, allgrpdat{:})

greatgrandavg = ft_timelockgrandaverage([], allgrpdat{:});

cfg = [];
cfg.layout      = 'elec1010.lay';
cfg.showlabels  = 'yes';
cfg.parameter   = 'var';
ft_multiplotER(cfg, greatgrandavg)


%% Dispersion
cfg = [];
cfg.keepindividual = 'yes';
ggdat = ft_timelockgrandaverage(cfg, allgrpdat{:});

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






