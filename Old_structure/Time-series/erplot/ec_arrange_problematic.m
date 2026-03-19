clear all, close all
addpath('C:\Users\ecesnait\Desktop\EEGManyPipelines\Matlab Scripts\toolboxes\fieldtrip-master\')
ft_defaults;


% Load data including empty processed cells
load('C:\Users\ecesnait\Desktop\EEGManyPipelines\git\EEGManyPipes org\Main_Paper\Time-series\erplot\allgrpdat files\allgrpdat_1_147_Andrea_memory.mat')

% indx empty corresponds to different problems
indx_empty = find(cellfun(@isempty,allgrpdat_joined(1,:))) % first 1-90 EEGLAB teams, 91 - 124 - 'other', and 125 - 147 Fieldtrip

% index problems
indx_memory = [24:26, 69, 89, 135, 139,145]
indx_missing_info = [65, 66, 86]
indx_format = [ 128, 129, 144]
indx_other = [10,34,50]% 10 has empty data, 34 and 50 has no epoch info
indx_continuous = setdiff(indx_empty, [indx_memory, indx_missing_info,indx_format,indx_other])

%Load continuous
addpath C:\Users\ecesnait\Desktop\EEGManyPipelines\Matlab Scripts\toolboxes\eeglab_current\eeglab2022.0
eeglab % a phantom EEG struct to fill in with our data
addpath C:\Users\ecesnait\Desktop\BUSCHLAB\CORENATS\data\
corenats = pop_loadset('C:\Users\ecesnait\Desktop\BUSCHLAB\CORENATS\data\CN01_import.set')

% ID_continuous = allgrpdat_joined(2,indx_continuous)' % get their sampling rates in this same order
% ID_continuous_2 = [{'00625232e8a8d010','19068f1fe266c5e1'}'; extractBefore(ID_continuous([3:12]), '.'); {'488abe5e0f5fa0f9', '4f9aeed964836381'}'];

% AQ = readtable('C:\Users\ecesnait\Desktop\EEGManyPipelines\Data\Analysis questionnaire\Analysis questionnaire final sample 168 corrected.xlsx');
% [indx_AQ, pl_AQ] = ismember(ID_continuous_2,AQ.teamID)
% isequal(AQ.teamID(pl_AQ), ID_continuous_2)
% new_fs = AQ.ans_ds_new_fs(pl_AQ)
srates = [512, 512, 512, 500, 512, 512, 512, 125, 256, 512, 256, 512, 512, 256]
stimuli = {'1030','1031','1039','1040','1041','1049','1110','1111','1119','1120','1121','1129','2030','2031','2039','2040','2041','2049',...
    '2091','1091','2191','1191','2090','1090','2190','1190',...
    '2110','2111','2119','2120','2121','2129'}

for i = 7:length(indx_continuous) % 2 was empty, 4 has empty epochs
    if indx_continuous(i) < 91 % EEGLAB
        data = load(['N:\EMP\EEGManyPipelines\EMP time series exp\EEGLAB all teams\', allgrpdat_joined{2,indx_continuous(i)}])
    elseif indx_continuous(i)>90 && indx_continuous(i)<124 % other
        data = load(['N:\EMP\EEGManyPipelines\EMP time series exp\', allgrpdat_joined(2,indx_continuous(i))])
    else % fieldtrip
        data = load(['N:\EMP\EEGManyPipelines\EMP time series exp\_fieldtrip_data3d\', allgrpdat_joined(2,indx_continuous(i))])
    end

    field_name = string(fieldnames(data));
    new_struct = data.(field_name);
    clear data

    %In case the data is epoched
    if numel(size(new_struct(1).EEGts)) > 2
        error('not a continuous dataset')
    end

    % epoch the data for each subject (N ~ 33)
    for pp = 1:length(new_struct)
        EEG.data = new_struct(pp).EEGts;
        EEG.times = new_struct(pp).time;
        EEG.event = new_struct(pp).epoch;
        EEG.pnts = length(new_struct(pp).time);
        EEG.srate = srates(i);

        % add chanlocs from the original dataset
        [indx_ch, pl_ch] = ismember({corenats.chanlocs.labels}, new_struct(pp).chan)
        isequal(pl_ch, [1:length(new_struct(pp).chan)])
        EEG.chanlocs = corenats.chanlocs(pl_ch(indx_ch))

        % epoch the data
        if isequal(class(new_struct(pp).epoch(1).type),'double')
                stim_type = unique([new_struct(pp).epoch.type])
                stim_type = num2cell(stim_type)
        else
             stim_type = stimuli
        end

        EEG_epoched = pop_epoch(EEG,stim_type, [-0.5 1])

        fttmp = [];
        fttmp.avg       = double(mean(EEG_epoched.data, 3));
        fttmp.time      = EEG_epoched.times;
        fttmp.label     = new_struct(pp).chan;
        fttmp.dimord    = 'chan_time';

        allsubjdat{pp} = fttmp;

        % new_struct(pp).epoch = EEG_epoched.event

        EEG = eeg_emptyset()
        clear EEG_epoched stim_type indx_stimulus indx_ch pl_ch fttmp
    end
    % save epoched data - it takes as long as loading it and re-running the
    % whole script
    % save(['N:\EMP\EEGManyPipelines\EMP time series exp\Continuous_to_epoched\',allgrpdat_joined{2,indx_continuous(i)}],'new_struct','-v7.3')

    % RUN Grand average and add it back to the allgrpdat structure
    allgrpdat_joined{1,indx_continuous(i)} = ft_timelockgrandaverage([], allsubjdat{:});

    clear new_struct fttmp allsubjdat

end

save('allgrpdat_1_147_continuous','allgrpdat_joined')

%% Add teams that had memory problems but were uploaded by Andrea %%
clear all
addpath('C:\Users\ecesnait\Desktop\EEGManyPipelines\Matlab Scripts\toolboxes\fieldtrip-master\')
ft_defaults;

load('C:\Users\ecesnait\Desktop\EEGManyPipelines\git\EEGManyPipes org\Main_Paper\Time-series\erplot\allgrpdat files\allgrpdat_1_147.mat')
IDs_stand = [extractBefore(allgrpdat_joined(2,1:90), '.'), allgrpdat_joined(2,91:124), extractBefore(allgrpdat_joined(2,125:end), '_')]

dirAndrea = dir(['M:\EEGManyPipelines\_team_extra\', '*.mat'])

for a = 2:length(dirAndrea) % 2,4 missing time
        load(fullfile(dirAndrea(a).folder, dirAndrea(a).name))

      indx_team = find(ismember(IDs_stand, extractBefore(dirAndrea(a).name,'_')))

      % fit data into a cell array for each participant to calcilate the GA
      %should do this for all teams separately as inputs vary
      for i =1:33
          fttmp = [];
        fttmp.avg       = squeeze(allsubj_avg_ch(i,:,:));
        fttmp.time      = time_vector;
        fttmp.label     = chan_vector;
        fttmp.dimord    = 'chan_time';

        allsubjdat{i} = fttmp;
      end

      % Calculate GA
      grpdat_GA= ft_timelockgrandaverage([], allsubjdat{:});

      if cellfun(@isempty,allgrpdat_joined(1,indx_team))
          allgrpdat_joined{1,indx_team} = grpdat_GA;
      end
    
clearvars -except a IDs_stand dirAndrea allgrpdat_joined
end
save('allgrpdat_joined_Andrea_memory', 'allgrpdat_joined')

%% Look for teams whose data were not exported %%
clear all
load('C:\Users\ecesnait\Desktop\EEGManyPipelines\Matlab Scripts\IDs_complete_168.mat')

load('C:\Users\ecesnait\Desktop\EEGManyPipelines\git\EEGManyPipes org\Main_Paper\Time-series\erplot\allgrpdat files\allgrpdat_1_147.mat')
IDs_exported = [extractBefore(allgrpdat_joined(2,1:90), '.'), allgrpdat_joined(2,91:124), extractBefore(allgrpdat_joined(2,125:end), '_')]
IDs_exported'
indx_underscore = find(~cellfun(@isempty,regexp(IDs_exported,'_', 'match')))
IDs_exported(indx_underscore) = extractBefore(IDs_exported(indx_underscore),'_') % there are some repeated IDs

% now find the missing IDs
[indx_missing, pl_missing] = ismember(complete_sample, IDs_exported)
missing_IDs = complete_sample(~indx_missing) % 1 missing fdt files not uploaded not contacted, 2 - saved only the raw data in pre-processed folder

% extract their data and create a mat file containing all 33 subjects
datadir = 'M:\EEGManyPipelines\_team_extra\EMP extr missing\ed45c2f3b0136a24\Data\'
names_all = dir([datadir, 'S*'])
{names_all.name}
subj_exclude = find(~cellfun(@isempty, regexp({names_all.name}, 'exclude')))
names_include = {names_all.name}
names_include(subj_exclude) = []

for i = 1:length(names_include)
    file = dir(fullfile(datadir,names_include{i},'Pre-processed time series data','*.set'))
        EEG = pop_loadset(file.name,file.folder)


end

% correct time from s to ms
% correct for differet dimensions (chan x time x epoch)
