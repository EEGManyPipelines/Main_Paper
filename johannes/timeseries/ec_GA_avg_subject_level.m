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
%re-create time
FS              = 256; % sampling rate in Hz
timeWindow      = [-200 600]; % consistent trial epoching (in ms)
timeVec_standard1 = flip([ 0 : -1/FS*1000 : timeWindow(1) ]);
timeVec_standard2        = [ 0 : 1/FS*1000 : timeWindow(2) ];
timeVec_standard         = [ timeVec_standard1(1:end-1), timeVec_standard2 ];
% figure, plot(timeVec_standard,all_data{1,34}{1,1}.avg*10^-6)

out_indx = [34]
all_data(out_indx)=[]

%% Mean over channels + SEM %%
load('chan_labels_for_plotting')

% count number of teams that were averaged for each data point
all_teams = repmat(length(all_data),1,205);

allgrpdat = {};
%create a matrix for a single channel each subject
GA_cpz_sbj = nan(33,205)

for sbj = 1:33 % for each subject
    sbj_mat = {};
  
    for t = 1:length(all_data) % for each team
        sbj_mat{t} = all_data{t}{1,sbj};% structure for each individual subject across all teams
        if isempty(sbj_mat{t})       
            continue
        elseif abs(max(nanmean(sbj_mat{t}.avg,2))) > 20 || abs(max(nanmean(sbj_mat{t}.avg,2))) < 10^-3 && abs(max(mean(sbj_mat{t}.avg,2))) ~=0
           sbj_mat{t}=[]
            continue
        end

%         fig = gcf
%         exportgraphics(fig, ['ERP_each_team_s17new.pdf'],'Append',true)
%         close(fig)

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

    clear indx_mchan pl_mchan chan_full copy_data order_data number_participants indx_empty sbj_mat
end

%% Remove outliers %%

cpz_GA = nan(33,205)
for i=1:33
    cpz_GA(i,:)=allgrpdat{i}.avg(32,:)
end

%plot
figure, plot(timeVec_standard, cpz_GA)
save('cpz_GA_ERP_33subj', 'cpz_GA')

T=array2table(cpz_GA)
writetable(T,'GA_cpz_sbj.csv', 'Delimiter',',')