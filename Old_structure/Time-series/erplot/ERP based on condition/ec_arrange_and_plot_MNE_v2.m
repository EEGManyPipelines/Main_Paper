%% Import and plot EMP ERP data
%
% For each group:
%   1) do subj avg
%   2) do grp grand avg
% Then collect all data and do a great grand average. Plot and some misch
% stats (just for fun for now).
%

%% SETUP
clear all, close all
% Paths
user_name = getenv('username');

if isequal(user_name, 'ecesnait')
    %datapath = 'N:\EMP\EEGManyPipelines\EMP time series exp\_fieldtrip_data3d\';
    datapath = 'M:\EMP\EEGManyPipelines\EMP time series exp\'
    % addpath('C:\Users\ecesnait\Desktop\EEGManyPipelines\git\EEGManyPipes org\Main_Paper\Time-series\erplot\')
    addpath('C:\Users\ecesnait\Desktop\EEGManyPipelines\Matlab Scripts\toolboxes\fieldtrip-master\')
elseif isequal(user_name,'mikkelcv')
    datapath = '/home/mikkelcv/emp/main/Time-series/erplot/nobackup/data';
    addpath('/home/mikkelcv/emp/main/Time-series/erplot')
    addpath('/home/mikkelcv/fieldtrip/')
else
  datapath = '/home/pasca/Science/projects/emp/team/MNE';
%   datapath = '/media/pasca/pasca_media/data/eeg/EMP_data_audit/team/MNE';
  addpath('/home/pasca/Tools/software/fieldtrip')
end

ft_defaults;

% Find group folders
d = dir(datapath);
d = d([d.isdir]);
d = {d.name};
grps = d(~(strcmp('.',d)|strcmp('..',d))); 


%% Collect data from all groups.
% Average withing subject within group, then average all subjects within
% group (grand average). Collect grand averages for comparison across
% groups.

% Check if the data struct exist

allgrpdat_manmade_fname = 'allgrpdat_manmade_condition_MNE.mat';
allgrpdat_natural_fname = 'allgrpdat_natural_condition_MNE.mat';
numer_trials_fname = 'numer_trials_MNE_conditions.mat';

if exist(allgrpdat_manmade_fname, 'file') > 0
  load(allgrpdat_manmade_fname)
  ng = size(allgrpdat_manmade, 2);
else
  allgrpdat_manmade = [];
  ng = 0;
end
if exist(allgrpdat_natural_fname, 'file') > 0
  load(allgrpdat_natural_fname)
else
  allgrpdat_natural = [];
end
if exist(numer_trials_fname, 'file') > 0
  load(numer_trials_fname)
else
  num_trials = struct();
end


%% Loop over groups
% teams for which the idx of events is derived from epo_data.events_id 
teams_events_id = {'b0edd369b6d8f4f1', 'ff8bf48d04d11c84', '48e64dc185199502', ...
  'eef1406b3fca3e9c', 'e13e7e07b99d853b', '08de3c5e092173e4', 'aa6aa366e9788967', ...
  '0ba1c7f1dafc1134', '19e8ad8bf94af489', '77fddd91c557626d', 'Varuwa', ...
  'c0c75576f9cd0b2a', 'bd3077a83b5b16bd', 'c91e489c4acd0bf4', ...
  'e146a94b29a41713', 'e2d0d90e5cf594ed'};
% teams for which sbjs have different number of chs
teams_different_nchs = {'e13e7e07b99d853b', 'Varuwa', '08de3c5e092173e4', ...
  'e2d0d90e5cf594ed'};
% temas for which there are events id different from manmade and natural,
% e.g. 99=noise
teams_other = {'e69a83408d1f3811'};
fields = {'label', 'time'};
field_names = {'man-made', 'natural'};

for gg = 1:1%length(grps)
    grp = grps{gg};
    disp(['Processing participant... ',num2str(gg)])
  
    % Loop over subjects
    subjects = dir([fullfile(datapath,grp, '*.mat')])
    allsubjdat = cell(size(subjects));

    allsubjdat_manmade = [];
    allsubjdat_natural = [];

    for ss = 1:length(subjects)
        % Load data
        subj = subjects(ss).name;

        epo_fpath = fullfile(datapath,grp, subj);
        disp(['LOAD ' epo_fpath])
        epo_data = load(epo_fpath);
        size(epo_data.data)

        groupdat.time = epo_data.time;
        for nc = 1:size(epo_data.chs_name, 1)
          groupdat.chan(nc,:) = {epo_data.chs_name(nc, :)};
        end

        % Check if data is epoched. If not, skip this subject
        if numel(size(epo_data.data)) < 3
            continue
        else
          if length(epo_data.events(:,3)) == size(epo_data.data, 3)
            events_id = int64(epo_data.events(:, 3));
          elseif length(epo_data.events) == size(epo_data.data, 3)
            events_id = int64(epo_data.events);
          end
          n_events = length(events_id);
  
          if isempty(find(cellfun(@(x) isequal(x, grp), teams_events_id))) == 0
            indx_manmade = logical(zeros(n_events, 1));
            indx_natural = logical(zeros(n_events, 1));
           
            [ev_id_manmade, ev_id_natural] = get_event_id(epo_data.events_id, field_names);
            for ne=1:length(ev_id_manmade)
              indx_manmade(events_id == ev_id_manmade(ne)) = 1;
            end
            for ne=1:length(ev_id_natural)
              indx_natural(events_id == ev_id_natural(ne)) = 1;
            end

          else
            indx_manmade = startsWith(string(events_id), "1");  % all markers for man-made stimuli start with 1
            indx_natural = startsWith(string(events_id), "2");  % all markers for natural stimuli start with 2
          end
          % Average across epochs based on condition
          manmade_avg_epoch = double(mean(epo_data.data(:,:,indx_manmade), 3));
          natural_avg_epoch = double(mean(epo_data.data(:,:,indx_natural), 3));

         
%          % count the number of epochs
%           num_trials(ng+gg).manmade_natural(ss, 1) = sum(indx_manmade);
%           num_trials(ng+gg).manmade_natural(ss, 2) = sum(indx_natural);

          if sum(indx_manmade) + sum(indx_natural) ~= size(epo_data.data, 3)
            if isempty(find(cellfun(@(x) isequal(x, grp), teams_other))) == 0
              disp(sum(indx_manmade) + sum(indx_natural))
              indx_other = startsWith(string(events_id), "99");
              disp(sum(indx_manmade) + sum(indx_natural) + sum(indx_other))
              if sum(indx_manmade) + sum(indx_natural) + sum(indx_other) ~= size(epo_data.data, 3)
                error('something wrong')
                return
              end
            else
              error('something wrong')
              return
            end
          end
          if max(indx_manmade + indx_natural) ~= 1
            error('something wrong')
            return
          end

          if ~isempty(manmade_avg_epoch) % prepare the fttmp struct for man-made and natural data separately
            [fttmp_manmade, fttmp_natural] = ec_ssubj_erp_condition(groupdat, manmade_avg_epoch, natural_avg_epoch)

            % delete entry
            allsubjdat_manmade{size(allsubjdat_manmade,2)+1} = fttmp_manmade; % Avoids empty entries
            allsubjdat_natural{size(allsubjdat_natural,2)+1} = fttmp_natural; % Avoids empty entries

            clear manmade_avg_epoch natural_avg_epoch fttmp_natural fttmp_manmade indx_manmade indx_natural epo_data groupdat
          else
              groupdat(1) = [];
          end
        end
    end

    if isempty(find(cellfun(@(x) isequal(x, grp), teams_different_nchs))) == 0
      allsubjdat_manmade = get_common_chs(allsubjdat_manmade);
      allsubjdat_natural = get_common_chs(allsubjdat_natural);
    end

    % check if all sbj have the same channels and time
    for nf=1:numel(fields)
       is_ok = check_field(allsubjdat_manmade, fields{nf});
       if is_ok == 0
         error('something wrong')
       end
    end

    for nf=1:numel(fields)
       is_ok = check_field(allsubjdat_natural, fields{nf});
       if is_ok == 0
         error('something wrong')
       end
    end
    
    % Make grand avg
    allgrpdat_manmade{1, ng+gg} = ft_timelockgrandaverage([], allsubjdat_manmade{:});
    allgrpdat_manmade{2, ng+gg} = grps{gg};

    allgrpdat_natural{1, ng+gg} = ft_timelockgrandaverage([], allsubjdat_natural{:});
    allgrpdat_natural{2, ng+gg} = grps{gg};

    clear allsubjdat_natural allsubjdat_manmade fig
end

%% some plot
for i = size(allgrpdat_manmade, 2)-3:size(allgrpdat_manmade, 2)
  i_ch = find(strcmp(strtrim(allgrpdat_manmade{1,i}.label), 'CPz'))
  figure, hold on
  title(sprintf('%s', allgrpdat_manmade{2,i}))
  plot(allgrpdat_manmade{1,i}.time, allgrpdat_manmade{1,i}.avg(i_ch, :))
  plot(allgrpdat_natural{1,i}.time, allgrpdat_natural{1,i}.avg(i_ch, :))
end

return
% allgrpdat_manmade = [allgrpdat_manmade; grps]
% allgrpdat_natural = [allgrpdat_natural; grps]

save('allgrpdat_manmade_condition_MNE.mat', 'allgrpdat_manmade')
save('allgrpdat_natural_condition_MNE.mat', 'allgrpdat_natural')
save('numer_trials_MNE_conditions', 'num_trials')


teams_id = {};
for i=1:size(allgrpdat_natural, 2)
  teams_id{i} = allgrpdat_natural{2, i};
end
sort(teams_id')
