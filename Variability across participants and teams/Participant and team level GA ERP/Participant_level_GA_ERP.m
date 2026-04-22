% This code calculates grand-average ERP across participants across all
% trials for every team by using Fieldtrip toolbox *. The code uses the
% data with unmatched scale and brings V to uV.
%
% * Oostenveld, R., Fries, P., Maris, E., Schoffelen, JM (2011). FieldTrip:
% Open Source Software for Advanced Analysis of MEG, EEG, and Invasive Electrophysiological Data.
% Computational Intelligence and Neuroscience, Volume 2011 (2011), Article ID 156869, doi:10.1155/2011/156869

clear all, close all
addpath 'C:\Users\ecesnait\Desktop\EEGManyPipelines\Matlab Scripts\toolboxes\fieldtrip-master'
ft_defaults;

% Non-standardized data location
dataDir = 'Z:\aebuschgold\ecesnait\EMP\EMP time series exp\TimelockAVG\';
data = dir([dataDir,'*.mat']);

%% Aggregate all datasets %%
all_data = {};
for t=1:length(data)
    % Load data
    load([dataDir,data(t).name]);
    all_data{t} = alldatavg;
    clear alldatavg
end

% Re-create time vector
FS              = 256; % sampling rate in Hz
timeWindow      = [-200 600]; % consistent trial epoching (in ms)
timeVec_standard1 = flip([ 0 : -1/FS*1000 : timeWindow(1) ]);
timeVec_standard2        = [ 0 : 1/FS*1000 : timeWindow(2) ];
timeVec_standard         = [ timeVec_standard1(1:end-1), timeVec_standard2 ];

%remove one outlier team
out_indx = [34]
all_data(out_indx)=[]

% Load exemplary order of channels in case some are missing in team's data
load('chan_labels_for_plotting')

% get team IDs
data_ID = extractBetween({data.name}, '_', '.mat')
data_ID(out_indx)=[]

%Team IDs that had data in Volts and should be brought back to microvolts.
volt_ID = {'08de3c5e092173e4', '0ba1c7f1dafc1134', '0bc9ee704db74104', '19e8ad8bf94af489','344dd59ded90cb34', '48e64dc185199502', ...
    'TheCodeMechanics', 'Varuwa','a0cf32754296214f', 'd5c8ed05b7af02a3','bd3077a83b5b16bd', 'c0c75576f9cd0b2a', ...
    'c577d3cdf78548ce','e13e7e07b99d853b', 'e146a94b29a41713', 'e69a83408d1f3811','e72d90a6ff4b5108', 'ee8c062e3dc35b1d',  'c4ed0094fc18efbc',...
    '628a18bc8a3d36dd','77fddd91c557626d','7f3fe2bc79a9d3f9', '8559e4d7314e45ec', '8c587a4cbf53865d','90420442f22fa870',...
    'CIMHPipe','a25b8419335d2131','aa6aa366e9788967','b0edd369b6d8f4f1','c91e489c4acd0bf4','da33cf2264c9baa2','e2d0d90e5cf594ed','ea6b1d1870708b82',...
    'eef1406b3fca3e9c','f92a1d6a49d0d40a','ff8bf48d04d11c84'}

% we will count a number of teams that were averaged for each data point
all_teams = repmat(length(all_data),1,205);

allgrpdat = {};


%create a matrix for a single channel each participant
GA_cpz_sbj = nan(33,205)

for sbj = 1:33 % for each participant
    sbj_mat = {};

    for t = 1:length(all_data) % for each team
        sbj_mat{t} = all_data{t}{1,sbj};% structure for each individual participant across all teams

        %remove empty datasets and outliers
        if isempty(sbj_mat{t})
            continue
            %exclude outliers when the amplitude is larger than 20uV
        elseif abs(max(nanmean(sbj_mat{t}.avg,2))) > 20
            sbj_mat{t}=[]
            continue
        end

        %transform from V to uV
        if ismember(data_ID{t},volt_ID)
            sbj_mat{t}.avg = sbj_mat{t}.avg.*10^6
        end

        % This code assumes that every team has 64 channels
        if length(sbj_mat{t}.label)~=64; error('Not a full set of channels!'); end


    end

    % remove empty indices of data (When team did not have subject data)
    indx_empty=find(cellfun(@isempty, sbj_mat));
    if indx_empty
        sbj_mat(indx_empty)=[];
    end


    %run timelock grand average across one subject all teams
    allgrpdat{sbj}= ft_timelockgrandaverage([], sbj_mat{:});

    clear indx_mchan pl_mchan chan_full copy_data order_data number_participants indx_empty sbj_mat
end

%% Now as the channels have been ordered, take the GA in CPz ans save it for further analyses %%

cpz_GA = nan(33,205)
for i=1:33
    cpz_GA(i,:)=allgrpdat{i}.avg(18,:) ;
end

T=array2table(cpz_GA)
writetable(T,'GA_cpz_sbj_new.csv', 'Delimiter',',')

T_time = array2table(timeVec_standard')
writetable(T_time,'Time_cpz_sbj_new.csv', 'Delimiter',',')


