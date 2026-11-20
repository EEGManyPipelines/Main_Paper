% This code calculates grand-average ERP across participants across all
% trials for every team by using Fieldtrip toolbox *. The code uses the
% data with unmatched scale and brings V to uV.
%
% * Oostenveld, R., Fries, P., Maris, E., Schoffelen, JM (2011). FieldTrip:
% Open Source Software for Advanced Analysis of MEG, EEG, and Invasive Electrophysiological Data.
% Computational Intelligence and Neuroscience, Volume 2011 (2011), Article ID 156869, doi:10.1155/2011/156869

clear all, close all

% Paths

%FieldTrip, change accordingly, or run personal startup.m file
%addpath('C:\Users\ecesnait\Desktop\EEGManyPipelines\Matlab Scripts\toolboxes\fieldtrip-master\')
addpath('Y:\aebusch\ecesnait\Desktop\EEGManyPipelines\Matlab Scripts\toolboxes\fieldtrip-master\')
ft_defaults;

% InOffice Paths
datapath = 'Y:\aebuschgold\ecesnait\EMP\EMP time series exp\Standardized\'; 
savetopath = "Y:\aebusch\ecesnait\Desktop\EEGManyPipelines\git\EEGManyPipes org\Main_Paper\Time-series\erplot\ERP based on condition\FINAL 3 H difference wave plotting\Patrick's script H2a H3a\new_Output";
%cd Y:\nbuschgold\ecesnait\EMP\Patrick\;

% HomeOffice Paths
% datapath = 'X:\ecesnait\EMP\EMP time series exp\Standardized\'; 
% savetopath = 'X:\ecesnait\EMP\EMP time series exp\TimelockAVG_Hyp2a\';

% Get the teams

h1_grps = {'018ab79008f2e342', '02b9138753429402', '03f53ea2afe035fd', '04b28b5356eb17bd',...
    '08de3c5e092173e4', '0a0e092a0c1e6c8f', '0ba1c7f1dafc1134', '0bc9ee704db74104', '0c87f7355ce7ec6d',...
    '1291ad7d784e90e5', '129c99e90c45f38c', '13bef9b0362773b3', '1497d4b19bba4f30', '1559fd3bafe5582c',...
    '19068f1fe266c5e1_1', '19e8ad8bf94af489', '1e41774b7169df7d', '223dfd3c429d0e79', '26ab3771b0c195f5',...
    '27a31ff4f251781a', '2968df3b1e9bd676', '2e4c7afeba24d070', '2e6f06d6e89db2ca', '356c77bfd2662b9a_H1',...
    '37ddc3bdb7342c83', '3b8a90cfec1e6dab', '3c8c07696fbd0314', '3dc2c5916354971b', '48e64dc185199502',...
    '4dc16bcffc6daf9b', '4e65b329e7e27005', '533a774ad52b045', '53e7528dd1a9b39c', '5739286752f82402',...
    '5a5918ffce98de10', '60197350a0c2b888', '628a18bc8a3d36dd', '694028366488e777', '6cfb6838a570c17c',...
    '724143462c318037', '77fddd91c557626d', '7d5750a1b70460c2', '7f3fe2bc79a9d3f9', '80d914dc5270b2e7',...
    '8107271d26b4e6ce', '841f7d890725a452', '861c06b5c483cd72', '8c587a4cbf53865d', '8d9ffe6a6752d2af',...
    '90420442f22fa870', '91ce1477c08b672c', '92eff34d631e5d43', '93af9133e6fa6cf5', '962658984e31030d',...
    '9c490673610ca32b', 'CIMHPipe', 'Chile Maule', 'CognitiveSystems-KU', 'Kaiser Soze', 'TMS-EEG-DREAM',...
    'TeamKeele', 'TheCodeMechanics', 'The Hanncanny', 'Varuwa', 'a0cf32754296214f', 'a28bd183f8dca47f',...
    'aa3529f492e8e3af', 'aa6aa366e9788967', 'aaaaf00cc3912121', 'ac0f676d6813a969', 'ae58d64de88cfd4e',...
    'b0edd369b6d8f4f1', 'b3a91f02d3e5a9b0', 'b47e0e61375f4d31', 'b78245f9464a1398', 'b965a3f37c262164',...
    'ba2732f7a065a4b9', 'baba120af1f1caca', 'bd3077a83b5b16bd', 'c0c75576f9cd0b2a', 'c58937f04a355198',...
    'c58937f04a3551aa', 'c91e489c4acd0bf4', 'd18fa8d3caaf88cf', 'd58e77448897a421', 'd5c8ed05b7af02a3',...
    'd9a070789fe1b133', 'da33cf2264c9baa2', 'db40ed62b3cf9ae9', 'dc50b9e1e4c36653', 'e13e7e07b99d853b',...
    'e2d0d90e5cf594ed', 'e37fd347e252962c', 'e69a83408d1f3811', 'ea6b1d1870708b82', 'eef1406b3fca3e9c',...
    'f2101fb6fdd45635', 'f51bd6c2779b2256', 'f68b7096e0e9c0d2', 'f92a1d6a49d0d40a', 'ff8bf48d04d11c84',...
    'ff8cd5ce3123a097', 'gNeC'};

%Team IDs that had data in Volts and should be brought back to microvolts.
volt_ID =  'd5c8ed05b7af02a3';


bsl_correct = 1
bstart = -200;
bstop  = 0;

% how many teams to process
n_teams = 103

%% Aggregate all datasets %%
all_data = {};

for t = 1:length(short_idx)

    disp(['Processing participant... ',num2str(t)])

    % Load data
    groupdat =load([datapath,['standart_' h1_grps{short_idx(t)} '.mat']]);
    groupdat = groupdat.(string(fieldnames(groupdat))) % un-nest

    % average the trials within each participant
    cfgavg = [];
    cfgavg.keeptrials = 'no';
    cfgavg.covariance = 'no';
    % % avgs = cell(size(groupdat,2));
    % empty_idx = find(cellfun(@isempty, groupdat(2,:)));   % logical array, true where a cell is empty
    % 
    % if ~isempty(empty_idx)
    %     groupdat(:,empty_idx) = [];
    % end
    
    % participant-level average
    for s = 1:size(groupdat,2)
        disp(['processing subject...',num2str(s)])
        if isempty(groupdat{1,s})
            avgs{s} = []
            continue
        else
            avgs{s} = ft_timelockanalysis(cfgavg, groupdat{1,s});
        end
    end
    all_data{short_idx(t)} = avgs;
    clear avgs groupdat empty_idx
end

save('all_sbj_team_avg_data_131026','all_data', '-v7.3')

lens = cellfun(@numel, all_data);        % number of items in each entry
short_idx = find(lens < 33);         % indices of entries with fewer than 33

% Re-create time vector
FS              = 256; % sampling rate in Hz
timeWindow      = [-200 600]; % consistent trial epoching (in ms)
timeVec_standard1 = flip([ 0 : -1/FS*1000 : timeWindow(1) ]);
timeVec_standard2        = [ 0 : 1/FS*1000 : timeWindow(2) ];
timeVec_standard         = [ timeVec_standard1(1:end-1), timeVec_standard2 ];


% we will count a number of teams that were averaged for each data point
all_teams = repmat(n_teams,1,205);

allgrpdat = {};

%create a matrix for a single channel each participant
GA_cpz_sbj = nan(33,205)

for sbj = 1:33 % for each participant

    disp(['processing subject...', num2str(sbj) ])

    sbj_mat = {};

    for t = 1:length(all_data) % for each team
            disp(['processing team...', num2str(t) ])

        sbj_mat{t} = all_data{t}{1,sbj};% structure for each individual participant across all teams

        %remove empty datasets and outliers
        if isempty(sbj_mat{t})
            continue
            %exclude outliers when the amplitude is larger than 20uV
        elseif abs(max(nanmean(sbj_mat{t}.avg,2))) > 10
       
            sbj_mat{t}=[]
            continue
        end

        %transform from V to uV
        if strcmp(h1_grps{t},volt_ID)
            sbj_mat{t}.avg = sbj_mat{t}.avg.*10^6
        end
        % % one team seems to be wrongly alligned to uV group
        % if abs(max(nanmean(sbj_mat{t}.avg,2))) >10
        %     abs(max(nanmean(sbj_mat{t}.avg,2)))
        %     h1_grps{t}
        %     error('check amplitudes 2')
        % end

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
writetable(T,'GA_cpz_sbj_new_103_teams.csv', 'Delimiter',',')

T_time = array2table(timeVec_standard')
writetable(T_time,'Time_cpz_sbj_new.csv', 'Delimiter',',')

