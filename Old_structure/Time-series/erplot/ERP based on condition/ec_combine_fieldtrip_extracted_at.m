clear all, close all

%call fieldtrip
addpath('C:\Users\ecesnait\Desktop\EEGManyPipelines\Matlab Scripts\toolboxes\fieldtrip-master\')
ft_defaults

data_path = 'C:\Users\ecesnait\Desktop\EEGManyPipelines\git\EEGManyPipes org\Main_Paper\Time-series\erplot\ERP based on condition\_fieldtrip_ERP_H1_Andrea\';
data_files = dir([data_path,'*.mat'])

team_id = {data_files.name} %22 teams
team_id(16) = [] % not a team
%Load example to create a structure
load('C:\Users\ecesnait\Desktop\EEGManyPipelines\git\EEGManyPipes org\Main_Paper\Time-series\erplot\ERP based on condition\allgrpdat files\allgrpdat_condition_manmade_EEGLAB.mat')
example = allgrpdat_manmade;
clear allgrpdat_manmade

for i = 1:length(team_id)
    allsubjdat_manmade = [];
    allsubjdat_natural = [];

    fttmp_manmade = [];
    fttmp_natural = [];
    %load one team's data
    load([data_path, team_id{i}])

     % transform channel labels and delete the missing indices
        empy_indx = cellfun(@isempty, chan_name_orig2);
        chan_name_orig2(empy_indx) = []
        % remove empty channel from the time series data
        team_allsubj_cond2(:,empy_indx,:) =  [];
        team_allsubj_cond1(:,empy_indx,:) =  [];

    % assign conditions
    % fttmp_manmade is a struct for each subject with fields: avg, time,
    % label, dimord (e.g., 'chan_time')
    for p = 1:size(team_allsubj_cond2,1) % subj x chan x time
       
        %create the fttmp struct for each subject
        fttmp_manmade.avg = squeeze(team_allsubj_cond1(p,:,:));
        fttmp_manmade.time = time_vector_msec;
        fttmp_manmade.label =  chan_name_orig2;
        fttmp_manmade.dimord = 'chan_time' % based on the team_allsubj_cond2 matrix

        fttmp_natural.avg = squeeze(team_allsubj_cond2(p,:,:));
        fttmp_natural.time = time_vector_msec;
        fttmp_natural.label = chan_name_orig2;
        fttmp_natural.dimord = 'chan_time' % based on the team_allsubj_cond2 matrix

        allsubjdat_manmade{size(allsubjdat_manmade,2)+1} = fttmp_manmade; % Avoids empty entries
        allsubjdat_natural{size(allsubjdat_natural,2)+1} = fttmp_natural; % Avoids empty entries
    end
    % Calculate GA ERPs per team
    allgrpdat_manmade{1,i} = ft_timelockgrandaverage([], allsubjdat_manmade{:});
    allgrpdat_natural{1,i} = ft_timelockgrandaverage([], allsubjdat_natural{:});

    %Assign team IDs to the struct
    allgrpdat_natural{2,i} = extractBefore(team_id{i},'_')
    allgrpdat_manmade{2,i} = extractBefore(team_id{i},'_')

    clearvars -except allgrpdat_manmade allgrpdat_natural team_id data_path
end


%examplary ERPs to visually inspect the output
figure, plot(allgrpdat_natural{1,1}.time, allgrpdat_manmade{1,1}.avg(32,:)), hold on
plot(allgrpdat_natural{1,1}.time, allgrpdat_natural{1,1}.avg(32,:))

save("allgrpdat_condition_manmade_fieldtrip.mat", 'allgrpdat_manmade')
save("allgrpdat_condition_natural_fieldtrip.mat", 'allgrpdat_natural')