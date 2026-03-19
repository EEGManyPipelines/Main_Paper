clear all, close all

%load all files for man-made condition
addpath 'allgrpdat files'\
load('allgrpdat_condition_manmade_EEGLAB.mat')
indx_empty = cellfun(@isempty, allgrpdat_manmade(1,:))
allgrpdat_manmade(:,indx_empty) = []; % delete empty entries of teams we could not process data of
EEGlab_man = allgrpdat_manmade;
clear allgrpdat_manmade

load('allgrpdat_condition_manmade_fieldtrip.mat')
Fieldtrip_man = allgrpdat_manmade;
clear allgrpdat_manmade

load('allgrpdat_manmade_condition_MNE.mat')
MNE_man = allgrpdat_manmade;
clear allgrpdat_manmade

load('allgrpdat_manmade_condition_MNE_ec.mat')
MNE_man2 = allgrpdat_manmade;
clear allgrpdat_manmade

% combine them all

all_manmade = [EEGlab_man, Fieldtrip_man, MNE_man, MNE_man2] % 118 teams

%% SAME FOR NATURAL STIMULI %%

%load all files for man-made condition
addpath 'allgrpdat files'\
load('allgrpdat_condition_natural_EEGLAB.mat')
indx_empty = cellfun(@isempty, allgrpdat_natural(1,:))
allgrpdat_natural(:,indx_empty) = []; % delete empty entries of teams we could not process data of
EEGlab_natural = allgrpdat_natural;
clear allgrpdat_natural indx_empty

load('allgrpdat_condition_natural_fieldtrip.mat')
Fieldtrip_natural = allgrpdat_natural;
clear allgrpdat_natural

load('allgrpdat_natural_condition_MNE.mat')
MNE_natural = allgrpdat_natural;
clear allgrpdat_natural

load('allgrpdat_natural_condition_MNE_ec.mat')
MNE_natural2 = allgrpdat_natural;
clear allgrpdat_natural

% combine them all
all_natural = [EEGlab_natural, Fieldtrip_natural, MNE_natural, MNE_natural2] % 118 teams

save('GA_ERPs_all_teams_manmade','all_manmade')
save('GA_ERPs_all_teams_natural','all_natural')

