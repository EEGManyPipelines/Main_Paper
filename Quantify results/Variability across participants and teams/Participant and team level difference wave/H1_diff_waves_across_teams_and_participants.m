%% Import and plot EMP ERP data
%
% For each group:
%   1) do subj avg
%   2) do grp grand avg
% Then collect all data and subtract one from another for a difference wave across teams and across participants
%
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

Volt_teams = 'd5c8ed05b7af02a3';
indx_id = ismember(h1_grps,Volt_teams);
volt_indx = find(indx_id);

%% Baseline correct
bsl_correct = 1
bstart = -200;
bstop  = 0;

%% Collect data from all groups.
% Average withing subject within group, then average all subjects within
% group (grand average). Collect grand averages for comparison across
% groups.

all_sbj_diff_waves = nan(length(h1_grps),33,205);%teams, subjects, time

no_trialinfo=0

inspect_cases = {}

% Loop over groups
for gg = 1:length(h1_grps)%

    grp = h1_grps{gg};

    disp(['Processing participant... ',num2str(gg), ', team ID - ', grp])


    % Load data
    groupdat = load(fullfile(datapath, ['standart_' grp '.mat']));
    groupdat = groupdat.(string(fieldnames(groupdat))) % un-nest

    
    %Loop over participants
    for pp = 1: size(groupdat,2)
        if isempty(groupdat{1,pp})
           
            continue
        end
    
    allsubjavg_manmade = []; %Array with new averages
    allsubjavg_natural = []; %Array with old averages


    % Split based on condition: man-made vs natural for H1
        %% Now try to get indices with getIndices function
        disp([groupdat{2,pp},':']);       
        [ind_manmade,ind_natural,error_catch] = getIndicesGeneral_h1(groupdat{1,pp}.trialinfo,groupdat{2,pp},grp);
 
        % this formats the indices for fieldtrip
        if size(ind_manmade,2) == 1
            ind_manmade = ind_manmade';
            ind_natural = ind_natural';
        end

        %% Average across the epochs for two different conditions
        % now we calculate the averages for each condition and save them in
        % an array so we can do a grand-average after subject iteration.
        % also we count the number of trials per condition such that we can
        % compare these later for correctness and for reference.

        cfg = []; %condition manmade
        cfg.trials = ind_manmade;
        avg_manmade = ft_timelockanalysis(cfg,groupdat{1,pp});

        cfg = []; %condition natural
        cfg.trials = ind_natural;
        avg_natural = ft_timelockanalysis(cfg,groupdat{1,pp});

            %transform from Volts to uV
        if gg == volt_indx
    
            avg_manmade.avg = avg_manmade.avg * 10^6;
            avg_natural.avg = avg_natural.avg * 10^6;
        end

        indx_cpz = find(ismember(groupdat{1,pp}.label, 'CPz')); % get channel indx
        
        indx_subj = cellfun(@(x) str2double(regexp(x, '\d+', 'match', 'once')),groupdat(2,pp)); % get the subject number

        sbj_diff_wave = avg_manmade.avg(indx_cpz,:) - avg_natural.avg(indx_cpz,:);%the output is mean ampltidue in channel by time
         
        %% baseline correct
            if bsl_correct
                
                xstart = dsearchn(avg_manmade.time', bstart);
                xstop  = dsearchn(avg_manmade.time', bstop);
        
                bsl = nanmean(sbj_diff_wave(xstart:xstop)); % mean for each channel over the time window between -200 and 0
                bsl_cor_data = sbj_diff_wave - bsl;
                sbj_diff_wave = bsl_cor_data
                 clear bsl_cor_data bsl
                
            end

        all_sbj_diff_waves(gg,indx_subj,:) = sbj_diff_wave;%teams, subjects, time

        clear ind_manmade ind_natural avg_manmade avg_natural  indx_subj sbj_diff_wave

    end

    disp('done');
    clear groupdat
    % 

end

save('all_sbj_team_diff_waves_H1_131026.mat', 'all_sbj_diff_waves')
save('all_sbj_team_diff_waves_H1_inspect_cases_131026.mat', 'inspect_cases')


subject_bs_cor = squeeze(nanmean(data, 1));     % average over 1st dimension (teams)

team_bs_cor = squeeze(nanmean(data, 2));  % average over 2nd dimension (subjects)

% save tables for plotting
across_teams = array2table(subject_bs_cor);
across_subjects = array2table(team_bs_cor);

writetable(across_teams,'difference_wave_H1_across_teams_131026.csv')
writetable(across_subjects,'difference_wave_H1_across_subjects_131026.csv')
