% This code loads grand average ERPs for two conditions - man made and
% natural stimuli - that were used to answer hypothesis 1. It then
% subtracts the GA ERP of natural stimuli from GA ERP of man made stimuli. 


close all, clear all

% Load data
load('GA_ERPs_all_teams_manmade.mat')
load('GA_ERPs_all_teams_natural.mat')

% one team was processed separately for handling large data files. Add it to the structure.
one_team_manmade = load('allgrpdat_manmade_93af9133e6fa6cf5.mat')
one_team_natural = load('allgrpdat_natural_93af9133e6fa6cf5.mat')
all_manmade{1,119} = one_team_manmade.allgrpdat_manmade{1,1}
all_natural{1,119} = one_team_natural.allgrpdat_natural{1,1}
all_manmade{2,119} = '93af9133e6fa6cf5'
all_natural{2,119} = '93af9133e6fa6cf5'


% Find and delete empty indices. This might happen if there was a problem with man-made and
% natural condition event markers.
indx_empty = cellfun(@isempty, all_manmade(1,:));
all_manmade(:,indx_empty) = [];
all_natural(:,indx_empty) = []; 
%115 is empty
%all_manmade(:,115) = [];
%all_natural(:,115) = [];


%% Calculate the difference wave (manmade - natural)
difference_wave = {}
for d = 1:length(all_manmade)

    difference_wave{d}.id = all_manmade{2,d};%team ID
    difference_wave{d}.data = all_manmade{1,d}.avg - all_natural{1,d}.avg;%time series
    difference_wave{d}.label = all_manmade{1,d}.label; %channel label

    % Some teams submitted their data in seconds while most teams have it
    % milliseconds. Find the teams and convert the time scale.

    if max(all_manmade{1,d}.time)<10 % the epoch length is up to 3s or 3000 ms
        difference_wave{d}.time = all_manmade{1,d}.time*1000;% convert from sec to msec
    else
        difference_wave{d}.time = all_manmade{1,d}.time;
    end

    % Inspect zeroes and NaN and zero values. 
    % Some NaN values are expected due to
    % different epoch lengths that teams cut data into. Here we visually
    % inspected data that had more than 30 NaN values. This was done to
    % control for super short data length and data that only contained NaN
    % values.
    % 
    if sum(isnan(mean(difference_wave{d}.data,2)))>30
         mean(difference_wave{d}.data,2)
         all_manmade{2,d}
         error('inspect nan')
        %difference_wave{d} = [] 
    elseif sum(mean(difference_wave{d}.data,2)) == 0 
         error('inspect zeroes')
        %difference_wave{d} = []
    end
    %
end

% When plotting the data we noticed that one team had their epochs shifted by 1000ms (the 0 that marks the presentation of a stimulus was shifted).
% Correct time for this team.
figure, plot(difference_wave{12}.time,difference_wave{12}.data)

difference_wave{12}.time = difference_wave{12}.time - 1000 %
all_natural{1,12}.time = all_natural{1,12}.time - 1000 %
all_manmade{1,12}.time = all_manmade{1,12}.time - 1000 %

% Remove zeros and NaN
indx_empty2 = cellfun(@isempty, difference_wave)
difference_wave(indx_empty2) = []
save('difference_wave_orig_109_clean.mat','difference_wave')


