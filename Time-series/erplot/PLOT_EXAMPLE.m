% Some example for generic way to make topoplolts
clear all; colse all

%% PATHS
if isequal(getenv('username'),'ncb623')
    emp_path = 'C:\Users\ncb623\EMP';
    ftpath = 'C:\Users\ncb623\fieldtrip\fieldtrip';

else
    % ...
end

datapath = fullfile(emp_path, 'data');
outpath  = fullfile(emp_path, 'Main_Paper/data/plots');
datadir  = fullfile(datapath, 'TimelockAVG');

addpath(ftpath); ft_defaults
addpath(datapath)

%% Get metadata
files = find_files(datadir, '.mat', 'H3');
load(fullfile(datadir, files{1}), "alldatavg")

label = alldatavg{1,1}.label;
time  = alldatavg{1,1}.time / 1000;

% clear alldatavg

%% General use example
load(fullfile(datapath, 'alldatmat.mat'))   % data: grp x subj x channel x timepoints
% load('GA_ERP_example.mat') % Not sure where this came from

mdat_grp = squeeze(nanmean(alldatmat, 2));    % Grand average (mean within group)
GGA      = squeeze(median(mdat_grp, 1, "omitnan"));          % Great grand average
GGA_mad  = squeeze(mad(mdat_grp, 1, 1));       % Great median absolute deviation
GGA_std  = squeeze(std(mdat_grp, 0, "omitnan"));

% Make FT datastructure
avgdat = [];
avgdat.time     = time;
avgdat.label    = label;
avgdat.dimord   = 'chan_time';
avgdat.avg      = GGA;
avgdat.mad      = GGA_mad;
avgdat.std      = GGA_std;

ft_checkdata(avgdat)

% Plot
tbins = (-200:100:400) ./ 1000;

util_topoplot(avgdat, tbins, 'Pz', 'avg');

util_topoplot(avgdat, tbins, 'Pz', 'mad');

util_topoplot(avgdat, tbins, 'Pz', 'std');


%% Plot regression coefficients
load('fxmod_software.mat')

% Make dummy data
tooldat = [];
tooldat.time    = time;
tooldat.label   = label;
tooldat.dimord  = 'chan_time';
tooldat.brnvsn  = beta_tool(:,:,1);
tooldat.eeglab  = beta_tool(:,:,2);
tooldat.fldtrp  = beta_tool(:,:,3);
tooldat.other   = beta_tool(:,:,4);
tooldat.spm     = beta_tool(:,:,5);

% Plot
tbins = (-200:100:400) ./ 1000;

util_topoplot(tooldat, tbins, 'Pz', 'brnvsn');
util_topoplot(tooldat, tbins, 'Pz', 'eeglab');
util_topoplot(tooldat, tbins, 'Pz', 'fldtrp');
util_topoplot(tooldat, tbins, 'Pz', 'other');
util_topoplot(tooldat, tbins, 'Pz', 'spm');

%% Test

% Make FT datastructure
avgdat2 = [];
avgdat2.time     = time;
avgdat2.label    = label;
avgdat2.dimord   = 'rpt_chan_time';
avgdat2.avg      = mdat_grp;
avgdat2.mad      = GGA_mad;
avgdat2.std      = GGA_std;
ft_datatype(avgdat2)

group = randi(2, [size(avgdat2.avg, 1), 1]);  % 

util_singleplot(avgdat2, 'Pz', 'avg', group)