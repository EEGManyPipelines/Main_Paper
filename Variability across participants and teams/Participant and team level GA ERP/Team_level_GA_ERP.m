% This code calculates grand-average ERP across teams across all
% trials for every participant by using Fieldtrip toolbox *. The code uses the 
% data with unmatched scale and brings V to uV. 
%
% * Oostenveld, R., Fries, P., Maris, E., Schoffelen, JM (2011). FieldTrip:
% Open Source Software for Advanced Analysis of MEG, EEG, and Invasive Electrophysiological Data. 
% Computational Intelligence and Neuroscience, Volume 2011 (2011), Article ID 156869, doi:10.1155/2011/156869

clear all, close all
addpath '/Volumes/aebusch/aebusch/ecesnait/Desktop/EEGManyPipelines/Matlab Scripts/toolboxes/fieldtrip-master'
ft_defaults;

dataDir = '/Volumes/aebusch/aebuschgold/ecesnait/EMP/EMP time series exp/TimelockAVG/';
data = dir([dataDir,'*.mat']);
data_ID = extractBetween({data.name}, '_', '.mat')

%Team IDs that had data in Volts and should be brought back to microvolts.
%Excluded:'73473e3e1798d1d2',,'9985e8ae6679b0e2'
volt_ID = {'08de3c5e092173e4', '0ba1c7f1dafc1134', '0bc9ee704db74104', '19e8ad8bf94af489','344dd59ded90cb34', '48e64dc185199502', ...
    'TheCodeMechanics', 'Varuwa','a0cf32754296214f', 'd5c8ed05b7af02a3','bd3077a83b5b16bd', 'c0c75576f9cd0b2a', ...
    'c577d3cdf78548ce','e13e7e07b99d853b', 'e146a94b29a41713', 'e69a83408d1f3811','e72d90a6ff4b5108', 'ee8c062e3dc35b1d',  'c4ed0094fc18efbc',...
    '628a18bc8a3d36dd','77fddd91c557626d','7f3fe2bc79a9d3f9', '8559e4d7314e45ec', '8c587a4cbf53865d','90420442f22fa870',...
'CIMHPipe','a25b8419335d2131','aa6aa366e9788967','b0edd369b6d8f4f1','c91e489c4acd0bf4','da33cf2264c9baa2','e2d0d90e5cf594ed','ea6b1d1870708b82',...
'eef1406b3fca3e9c','f92a1d6a49d0d40a','ff8bf48d04d11c84'}

% Load the full set of channel labels
load('chan_labels_for_plotting')

bsl_correct = 1
bstart = -200;
bstop  = 0;

allgrpdat = {}
number_participants=[]
for i = 1:length(data)
    if ismember(i,[20,47,49,59]) % 20 -team data is epmty. Need to inspect previous steps; 47 - unusual scale
        continue
    end
    %
    %% Load data %%
    load([dataDir,data(i).name])

    %% get number of participants 
    indx_empty = cellfun(@isempty,alldatavg(1,:))
    if any(indx_empty)
        number_participants(i) = sum(~indx_empty);
    else
        number_participants(i) = 33;
    end

    %% remove empty entries and outliers above 20 uV
    alldatavg(:,indx_empty) = [];

    if abs(max(nanmean(alldatavg{1,1}.avg,2))) > 20
            continue
    end

    %% baseline correct
    if bsl_correct
        for ss = 1:length(alldatavg)
            xstart = dsearchn(alldatavg{1,ss}.time', bstart);
            xstop  = dsearchn(alldatavg{1,ss}.time', bstop);

            bsl = nanmean(alldatavg{1,ss}.avg(:,xstart:xstop),2); % mean for each channel over the time window between -200 and 0
            bsl = repmat(bsl, [1, length(alldatavg{1,ss}.time)]);
            bsl_cor_data = alldatavg{1,ss}.avg - bsl;
            alldatavg{1,ss}.avg = bsl_cor_data
            clear bsl_cor_data bsl
        end
    end

    %run timelock grand average
    allgrpdat{i}= ft_timelockgrandaverage([], alldatavg{1,:})

    % Bring data tot he same scale (from V to uV) 

    if ismember(data_ID{i},volt_ID)
        %error('inspect scale')
        allgrpdat{i}.avg = allgrpdat{i}.avg * 10^6
        allgrpdat{i}.var = allgrpdat{i}.var * 10^12

    end
    % median across subjects
   % violin(:,i) = nanmedian(allgrpdat{i}.var,2);

    clear alldatavg indx_mchan pl_mchan chan_full copy_data order_data
end

%% Remove empty indices %%
out_indx = [20,47,49,59] %empty indices
allgrpdat(out_indx)=[]
number_participants(out_indx)=[]
data_ID(out_indx)=[]

%% GA ERP across teams at the channel CPz%%
%create a matrix for a single channel
GA_cpz = nan(length(allgrpdat),205)

for s = 1:length(allgrpdat)
    indx_cpz = find(ismember(allgrpdat{s}.label, 'CPz'))
    GA_cpz(s,:) = allgrpdat{s}.avg(indx_cpz,:);
end

%% save the resulting data to a csv file for further analyses

T_GA=array2table(GA_cpz)
writetable(T_GA,'GA_cpz_team_new.csv', 'Delimiter',',')


