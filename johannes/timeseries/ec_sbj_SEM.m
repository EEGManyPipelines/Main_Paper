clear all, close all
addpath 'C:\Users\ecesnait\Desktop\EEGManyPipelines\Matlab Scripts\toolboxes\fieldtrip-master'
ft_defaults;

dataDir = 'M:\EMP\EEGManyPipelines\EMP time series exp\TimelockAVG\';
data = dir([dataDir,'*.mat']);

%% Aggregate all datasets %%
all_data = {};
for t=1:length(data)
    % Load data
    load([dataDir,data(t).name]);
    all_data{t} = alldatavg;
    clear alldatavg
end
%% Remove outliers %%
out_indx = [29, 33]
all_data(out_indx)=[]

%% Mean over channels + SEM %%
load('chan_labels_for_plotting')
load('C:\Users\ecesnait\Desktop\EEGManyPipelines\git\EEGManyPipes org\Main_Paper\johannes\timeseries\Elena plot data\nan_SEM_55teams.mat')

% count number of teams that were averaged for each data point
all_teams = repmat(length(all_data),1,205);
number_teams = all_teams-count_nan;% remove nan values where teams had no data for those time windows

allgrpdat = {};
%create a matrix for a single channel each subject
SEM_cpz_sbj = nan(33,205)

for sbj = 1:33 % for each subject
    sbj_mat = {};
    for t=1:length(all_data) % for each team
        sbj_mat{t} = all_data{t}{1,sbj};% structure for each individual subject across all teams
        if isempty(sbj_mat{t})
            continue
        end
        % This code assumes that every team has 64 channels
        if length(sbj_mat{t}.label)~=64; error('Not a full set of channels?!'); end
        % Now order channels
        if ~isequal(sbj_mat{t}.label,chan_labels)
            [indx_mchan pl_mchan] = ismember(chan_labels,sbj_mat{t}.label);
            chan_full = cell(1,length(chan_labels));
            chan_full(indx_mchan)=sbj_mat{t}.label(pl_mchan(find(pl_mchan)));%order the labels
            if   isequal(chan_full,chan_labels)
                copy_data = sbj_mat{t}.var;
                order_data = copy_data(pl_mchan(find(pl_mchan)),:);% order the data
                sbj_mat{t}.var = order_data;
            else;error('Something went wrong with ordering channels'); end
        end

    end
    % remove empty indices of data (When team did not have subject data)
    indx_empty=find(cellfun(@isempty, sbj_mat))
    if indx_empty
        sbj_mat(indx_empty)=[];
    end
    %run timelock grand average across one subject all teams
    allgrpdat{sbj}= ft_timelockgrandaverage([], sbj_mat{:})

    % calculate SEM
    indx_cpz = find(ismember(chan_labels, 'CPz'))
    if sum(isnan(allgrpdat{sbj}.var(indx_cpz,:)))==205 % if CPz is missing in all time points
        error('Inspect. Missing channel in all of the subejcts??')
    end

    SEM_cpz_sbj(sbj,:) = abs(sqrt(allgrpdat{sbj}.var(indx_cpz,:)))./sqrt(number_teams);

    clear indx_mchan pl_mchan chan_full copy_data order_data number_participants indx_empty
end


%plot
FS              = 256; % sampling rate in Hz
timeWindow      = [-200 600]; % consistent trial epoching (in ms)
timeVec_standard1 = flip([ 0 : -1/FS*1000 : timeWindow(1) ]);
timeVec_standard2        = [ 0 : 1/FS*1000 : timeWindow(2) ];
timeVec_standard         = [ timeVec_standard1(1:end-1), timeVec_standard2 ];

fig=figure()
plot(timeVec_standard,SEM_cpz_sbj,'Color',[0.4, 0.4, 0.4, 0.2]), hold on, plot(timeVec_standard,nanmean(SEM_cpz_sbj), 'LineWidth',1.5),...
    xlabel('time(msec)'), ylabel('SEM at CPz'),fontsize(fig, scale=1.6), hold off

saveas(fig,'SEM_33subj_v2.png')
T=array2table(SEM_cpz_sbj)
T_time=array2table(timeVec_standard)
writetable(T,'SEM_cpz_sbj.csv', 'Delimiter',',')
writetable(T_time,'standard_time.csv', 'Delimiter',',')


