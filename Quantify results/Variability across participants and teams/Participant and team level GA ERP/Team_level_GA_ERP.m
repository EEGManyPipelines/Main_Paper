% This code calculates grand-average ERP across teams across all
% trials for every participant by using Fieldtrip toolbox *. The code uses the 
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

allgrpdat = {}
number_participants=[]

for i = 1:length(h1_grps)

    grp = h1_grps{i};

    disp(['Processing participant... ',num2str(i), ', team ID - ', grp])


    % Load data
    groupdat = load(fullfile(datapath, ['standart_' grp '.mat']));
    groupdat = groupdat.(string(fieldnames(groupdat))) % un-nest

    % average the trials within each participant
    cfgavg = [];
    cfgavg.keeptrials = 'no';
    cfgavg.covariance = 'no';
    % avgs = cell(size(groupdat,2));
    empty_idx = find(cellfun(@isempty, groupdat(2,:)));   % logical array, true where a cell is empty
   
    if ~isempty(empty_idx)
        groupdat(:,empty_idx) = [];
    end
    
    % participant-level average
    for s = 1:size(groupdat,2)
        avgs{s} = ft_timelockanalysis(cfgavg, groupdat{1,s});
    end
    
    % grand average across participants
    cfggrand = [];
    cfggrand.keepindividual = 'no';

    %run timelock grand average
    allgrpdat{i} = ft_timelockgrandaverage(cfggrand, avgs{:});

    % Bring data tot he same scale (from V to uV) 

    if strcmp(grp,volt_ID)
        %error('inspect scale')
       
        allgrpdat{i}.avg = allgrpdat{i}.avg * 10^6

    end

       %% baseline correct
    if bsl_correct
        xstart = dsearchn(allgrpdat{i}.time', bstart);
        xstop  = dsearchn(allgrpdat{i}.time', bstop);

        bsl = nanmean(allgrpdat{i}.avg(:,xstart:xstop),2); % mean for each channel over the time window between -200 and 0
        bsl_cor_data = allgrpdat{i}.avg - bsl;
        allgrpdat{1,ss}.avg = bsl_cor_data;
        clear bsl_cor_data bsl
        
    end
    % median across subjects
   % violin(:,i) = nanmedian(allgrpdat{i}.var,2);

    clear alldatavg indx_mchan pl_mchan chan_full copy_data order_data avgs
end
save('GA_ERP_all_cond_all_ch_103teams','allgrpdat')
%% GA ERP across teams at the channel CPz%%
%create a matrix for a single channel
GA_cpz = nan(length(allgrpdat),205)

for s = 1:length(allgrpdat)
    indx_cpz = find(ismember(allgrpdat{s}.label, 'CPz'))
    GA_cpz(s,:) = allgrpdat{s}.avg(indx_cpz,:);
end

%% save the resulting data to a csv file for further analyses

T_GA=array2table(GA_cpz)
writetable(T_GA,'GA_cpz_team_new_2.csv', 'Delimiter',',')


