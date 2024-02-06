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

% loop over subjects
allsubjdat = cell(size(subjects));
for ss = 1:length(subjects)
%         ss = 1;
    subj = subjects{ss};

    % Loop over groups
    allgrpdat = cell(size(grps));
    for gg = 1:length(grps)
    %     gg=1;
        grp = grps{gg};
        infile = find_files(fullfile(datapath,grp), subj);

        fprintf('Loading subj %s for group %s (%i of %i)...\n', subj, grp, gg, length(grps));
    
        % Load data
        subjdat = load(fullfile(datapath, grp, infile{:}));
        
        % Average all trials (not careing about conditions for now!)
        subjdat.avgdat = double(mean(subjdat.data, 3));      % dim 3 = trials
        label = cell(length(subjdat.chs_name),1);
        for x = 1:length(subjdat.chs_name)
            label{x} = strtrim(subjdat.chs_name(x,:));
        end
    
        tmp = [];
        tmp.avg       = subjdat.avgdat;
        tmp.time      = subjdat.time;
        tmp.label     = label;
        tmp.dimord    = 'chan_time';    

        allgrpdat{gg} = tmp;
        disp('done');
        clear subjdat infile
    end

    % SHOULD PROBABLY SAVE DATA SOMEWHERE AROUND HERE.

    % Calculate great GA
    cfg = [];
    cfg.layout      = 'elec1010.lay';
    cfg.showlabels  = 'yes';
    ft_multiplotER(cfg, allgrpdat{:})
    
    greatgrandavg = ft_timelockgrandaverage([], allgrpdat{:});

    % Plot
    cfg = [];
    cfg.layout      = 'elec1010.lay';
    cfg.showlabels  = 'yes';
    ft_multiplotER(cfg, allgrpdat{gg})
    
end


%END