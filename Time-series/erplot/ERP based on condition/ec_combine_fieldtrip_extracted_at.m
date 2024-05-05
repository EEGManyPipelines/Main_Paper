clear all, close all

%call fieldtrip
addpath('C:\Users\ecesnait\Desktop\EEGManyPipelines\Matlab Scripts\toolboxes\fieldtrip-master\')
ft_defaults

data_path = 'C:\Users\ecesnait\Desktop\EEGManyPipelines\git\EEGManyPipes org\Main_Paper\Time-series\erplot\ERP based on condition\_fieldtrip_ERP_H1_Andrea\';
data_files = dir([data_path,'*.mat'])

team_id = {data_files.name} %22 teams

%Load example to create a structure
load('C:\Users\ecesnait\Desktop\EEGManyPipelines\git\EEGManyPipes org\Main_Paper\Time-series\erplot\ERP based on condition\allgrpdat files\allgrpdat_condition_manmade_EEGLAB.mat')
example = allgrpdat_natural;
clear allgrpdat_manmade

for i = 1:length(team_id)
    allsubjdat_manmade = [];
    allsubjdat_natural = [];

    fttmp_manmade = [];
    fttmp_natural = [];
    %load one team's data
    load([data_path, team_id{i}])

    % assign conditions
    % fttmp_manmade is a struct for each subject with fields: avg, time,
    % label, dimord (e.g., 'chan_time')
    for p = 1:size(team_allsubj_cond2,1) % subj x chan x time
        %create the fttmp struct for each subject
        fttmp_manmade.avg = squeeze(team_allsubj_cond2(p,:,:));
        fttmp_manmade.time = time_vector_msec;
        fttmp_manmade.label = chan_name_orig2;
        fttmp_manmade.dimord = 'chan_time' % based on the team_allsubj_cond2 matrix

        allsubjdat_manmade{size(allsubjdat_manmade,2)+1} = fttmp_manmade; % Avoids empty entries
        allsubjdat_natural{size(allsubjdat_natural,2)+1} = fttmp_natural; % Avoids empty entries
    end
    %
end