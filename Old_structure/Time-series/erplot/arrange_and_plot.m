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
    datapath = 'N:\EMP\EEGManyPipelines\EMP time series exp\';
    addpath('C:\Users\ecesnait\Desktop\EEGManyPipelines\git\EEGManyPipes org\Main_Paper\Time-series\erplot\')
    addpath('C:\Users\ecesnait\Desktop\EEGManyPipelines\Matlab Scripts\toolboxes\fieldtrip-master\')
elseif isequal(user_name,'mikkelcv')
    datapath = '/home/mikkelcv/emp/main/Time-series/erplot/nobackup/data';
    addpath('/home/mikkelcv/emp/main/Time-series/erplot')
    addpath('/home/mikkelcv/fieldtrip/')
end

ft_defaults;

% Find group folders
d = dir(datapath);
d = d([d.isdir]);
d = {d.name};
grps = d(~(strcmp('.',d)|strcmp('..',d)));     % Remove dots


% List of individual datasets
subjects = {'sub-001','sub-002','sub-003', 'sub-004'};   % use these for now

%% Run settings
% standard window
winStart    = -0.2;      % Seconds
winEnd      = 0.6;       % Seconds

% Standard sample frequency
fixFS       = 256;       % Hz

% Init.
tAx = winStart:(1/fixFS):winEnd;            % New time axis for resample

%% Collect data from all groups.
% Average withing subject within group, then average all subjects within
% group (grand average). Collect grand averages for comparison across
% groups.

% Loop over groups
allgrpdat = cell(size(grps));
allSemDat = cell(size(grps));
for gg = 1:length(grps)
    gg=1;
    grp = grps{gg};

    % loop over subjects
    allsubjdat = cell(size(subjects));
    for ss = 1:length(subjects)
        %         ss = 1;
        subj = subjects{ss};
        infile = find_files(fullfile(datapath,grp), subj);

        % NEED CHECK TO SEE IF FILE EXIST (in case grp rejected subjs)
        % - are rejected subject data still in folders? If so, we should
        % remove it when doing grand average.
        if isempty(infile)
            warning('Found no subj %s for group %s!\n', subj, grp);
            continue
        else
            fprintf('Loading subj %s for group %s (%i of %i)...\n', subj, grp, gg, length(grps));
        end

        % Load data
        subjdat = load(fullfile(datapath, grp, infile{:}));

        % MOVE Average all trials (not careing about conditions for now!)
        subjdat.avgdat = double(mean(subjdat.data, 3));      % dim 3 = trials
        label = cell(length(subjdat.chs_name),1);
        for x = 1:length(subjdat.chs_name)
            label{x} = strtrim(subjdat.chs_name(x,:));
        end

        %% Single trl data
        fs = 1./diff(subjdat.time);
        fs = fs(1);

        trltmp = [];
        for tt = 1:size(subjdat.data, 3)
            trltmp.trial{tt} = subjdat.data(:,:,tt);
            trltmp.time{tt} = subjdat.time;
        end
        trltmp.trialinfo = subjdat.events;
        trltmp.label     = label;
        trltmp.dimord    = 'chan_time';
        trltmp.fsample   = fs;

        %% crop        
        % Get the actual max/min time in data for later
        maxtim = max(trltmp.time{1});
        mintim = min(trltmp.time{1});

        cfg = [];
        cfg.latency = [winStart, winEnd];
        tst = ft_selectdata(cfg, tst);

        cfg = [];
        cfg.layout      = 'elec1010.lay';
        ft_multiplotER(cfg, tst)

        %% resample
        tAxReal = tAx(tAx >= mintim & tAx <= maxtim);   % Only interpolate on the section that has data. Otherwise it gives nonsense.

        cfg = [];
%         cfg.resamplefs  = fixFS;
        cfg.method      = 'pchip';
        cfg.detrend     = 'no';
        cfg.demean      = 'no';
        cfg.time        = repmat({tAxReal}, length(trltmp.trial), 1);
        trltmp_sam = ft_resampledata(cfg, trltmp);

        %% Add NaN for missing time points (ver 2)

        if maxtim < winEnd || mintim > winStart
            for tt = 1:length(trltmp_sam.trial)
                if maxtim < winEnd
                    afterPts = length(tAx(tAx > maxtim));
                    afterAdd = nan(size(trltmp_sam.trial{tt},1), afterPts);
                else 
                    afterAdd = [];
                end
    
                if mintim > winStart
                    beforePts = length(tAx(tAx < mintim));
                    beforeAdd = nan(size(trltmp_sam.trial{tt},1), beforePts);
                else
                    beforeAdd = [];
                end
                trltmp_sam.time{tt} = tAx;
                trltmp_sam.trial{tt} = [beforeAdd, trltmp_sam.trial{tt}, afterAdd];
            end
        end

%         cfg = [];
%         cfg.layout      = 'elec1010.lay';
%         ft_multiplotER(cfg, trltmp_sam)

        %% rereference and common baseline
        ft_warning off                                  % To avoid anoying NaN message
        cfg = [];
        cfg.reref       = 'yes';
        cfg.refchannel  = 'all';
        cfg.demean      = 'yes';
        cfg.baselinewindow = [-0.2, 0];
        trltmp_ref = ft_preprocessing(cfg, trltmp_sam); ft_warning on;

%         cfg = [];
%         cfg.layout      = 'elec1010.lay';
%         ft_multiplotER(cfg, trltmp_ref, trltmp_sam)


        %% Averaged data
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

    allsubjdat = allsubjdat(~cellfun(@isempty, allsubjdat));

    % Make grand avg
    allgrpdat{gg} = ft_timelockgrandaverage([], allsubjdat{:});

    cfg = [];
    cfg.keepindividual = 'yes';
    tmp = ft_timelockgrandaverage(cfg, allsubjdat{:});
    
    semDat = [];
    semDat.label  = tmp.label;
    semDat.time   = tmp.time;
    semDat.dimord = 'chan_time';
    semDat.sem    = squeeze(std(tmp.individual,1)./sqrt(size(tmp.individual,1)));
    semDat.sem2   = semDat.sem.^2;

    allSemDat{gg} = semDat; 
    clear allsubjdata

    % Inspect (uncomment)
    %     cfg = [];
    %     cfg.layout      = 'elec1010.lay';;
    %     cfg.showlabels  = 'yes';
    %     ft_multiplotER(cfg, allgrpdat{gg} )
end

%% ########################################################################
% End of data collection steps. Below here are some tests doing plots and
% other on the collected data structure. It might be a good idea to save
% the data structure instead and do these steps in sperate scripts.

%% Plot
cfg = [];
cfg.layout      = 'elec1010.lay';
cfg.showlabels  = 'yes';
ft_multiplotER(cfg, allgrpdat{:})

greatgrandavg = ft_timelockgrandaverage([], allgrpdat{:});

cfg = [];
cfg.layout      = 'elec1010.lay';
cfg.showlabels  = 'yes';
cfg.parameter   = 'avg';
ft_multiplotER(cfg, greatgrandavg)

%% Collect data in one big matrix
cfg = [];
cfg.keepindividual = 'yes';
ggdat = ft_timelockgrandaverage(cfg, allgrpdat{:});


%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Plot variability
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Option A: Coef of Var
% Is not valid as it cannot handle data with both positive and negative
% values.

% Coef. of Var.
codat = removefields(ggdat, 'individual');
codat.dimord = 'chan_time';

codat.cvar = squeeze(std(ggdat.individual)./mean(ggdat.individual));        % Normal CoV
codat.rvar = squeeze(mad(ggdat.individual)./median(ggdat.individual));      % Robust CoV

cfg = [];
cfg.layout      = 'elec1010.lay';
cfg.showlabels  = 'yes';
cfg.parameter   = 'cvar';
ft_multiplotER(cfg, codat)

%% Option B: Median Absolute Devidation (MAD)
% Good easy to use and interpret metric.

% Use this to get all GA data in a [ team x channel x time ] matrix
cfg = [];
cfg.keepindividual = 'yes';
ggDat = ft_timelockgrandaverage(cfg, allgrpdat{:});

madDat = [];
madDat.label    = ggDat.label;
madDat.time     = ggDat.time;
madDat.dimord   = 'chan_time';
madDat.mad      = squeeze(mad(ggdat.individual, 1, 1)); 

% Plot
cfg = [];
cfg.layout      = 'elec1010.lay';
cfg.showlabels  = 'no';
cfg.parameter   = 'mad';
ft_multiplotER(cfg, madDat)


% pltmax = max(max(madDat.mad));
% pltmin = 0; % min(min(madDat.mad))

quantile(madDat.mad(:), [.05, .95])

tbins = -0.2:0.1:0.4;
figx = figure();
cfg = [];
cfg.parameter   = 'mad';
cfg.layout      = 'elec1010.lay';
cfg.zlim = quantile(madDat.mad(:), [.05, .95]);  %[min(min(madDat.mad)), max(max(madDat.mad))];
cfg.figure = figx;

for tt = 1:(length(tbins)-1)
    subplot(1, length(tbins)-1, tt)

    cfg.xlim = [tbins(tt), tbins(tt+1)];
    ft_topoplotER(cfg, madDat)

end


%% Standard Measurement Error 
% cf. Luck et al. - in the paper they explicitly mention that this metric
% is illdefined for group level data for some reason? Perhaps we should not
% go for it here?

cfg = [];
cfg.keepindividual  = 'yes';
cfg.parameter       = 'sem2';
tmp = ft_timelockgrandaverage(cfg, allSemDat{:});


MSsme = [];
MSsme.label    = MSsmeDat.label;
MSsme.time     = MSsmeDat.time;
MSsme.dimord   = 'chan_time';
MSsme.MSsem    = squeeze(mean(tmp.individual, 1)); 

% Plot
cfg = [];
cfg.layout      = 'elec1010.lay';
cfg.showlabels  = 'no';
cfg.parameter   = 'mad';
ft_multiplotER(cfg, madDat)


varTotal = squeeze(var(ggdat.individual, 1))
varTrue = varTotal - MSsme.MSsem
% This gave negative values - not sure why.


%% Option C: Dispersion
% Cf. ...

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
% reshape
X = reshape(ggdat.individual, [3,63*90]);

alf = reliability_analysis(X,'interval');






