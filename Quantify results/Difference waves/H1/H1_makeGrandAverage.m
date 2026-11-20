
%% Import and plot EMP ERP data
%
% For each group:
%   1) do subj avg
%   2) do grp grand avg
% Then collect all data and do a great grand average. Plot and some misch
% stats (just for fun for now).
%
clear all, close all
%% SETUP
% Paths
clear;clc;
%% SETUP
% Paths

%FieldTrip, change accordingly, or run personal startup.m file
%addpath('C:\Users\ecesnait\Desktop\EEGManyPipelines\Matlab Scripts\toolboxes\fieldtrip-master\')
addpath('Y:\aebusch\ecesnait\Desktop\EEGManyPipelines\Matlab Scripts\toolboxes\fieldtrip-master\')
ft_defaults;

% InOffice Paths
datapath = 'Y:\aebuschgold\ecesnait\EMP\EMP time series exp\Standardized\'; 
savetopath = "Y:\aebusch\ecesnait\Desktop\EEGManyPipelines\git\EEGManyPipes org\Main_Paper\Time-series\erplot\ERP based on condition\FINAL 3 H difference wave plotting\Patrick's script H2a H3a\new_Output\";
%cd Y:\nbuschgold\ecesnait\EMP\Patrick\;

% HomeOffice Paths
% datapath = 'X:\ecesnait\EMP\EMP time series exp\Standardized\'; 
% savetopath = 'X:\ecesnait\EMP\EMP time series exp\TimelockAVG_Hyp2a\';

% Get the teams
grps = dir([datapath, '*.mat']);
grps = grps(~startsWith({grps.name}, '._'));  % drop AppleDouble files
grps = {grps.name};

missing_teams = {'standart_Chile Maule.mat','standart_3c8c07696fbd0314.mat'}
indx_missing= find(ismember(grps,missing_teams))

% Create subject names
subject_names = {};
for i = 1:33
    subject_names = [subject_names ['Subj-' + string(i)]];
end


%% Collect data from all groups.
% Average withing subject within group, then average all subjects within
% group (grand average). Collect grand averages for comparison across
% groups.

% Loop over groups
for gg = 1 :length(grps) % 

    disp(['Processing participant... ',num2str(gg)])

    grp = grps{gg};

    % Getting team Ids as identifier
     Team_Id = char(extractBetween(grp, '_', '.'));

    disp(['TeamID: ', Team_Id]);


    if strcmp(Team_Id(end-2:end),'_H3')
        continue
    end

    if ismember(Team_Id,{'463594c97c98f36d_1','791ae888506dc374','7c793519c38db522','8b8bb53a403750d9',...
            '9985e8ae6679b0e2', '9ab142ee3786b57b','ENSLab','ab8e0f351c5e3fe3','cba9657d4b55f7ca','d7303b34d38b2a18',...
            'ee3fdc470061d8db', 'fc67513ed5c4b400'})
         disp('Adding team to exclusion list')
        error_catch.errorMSG = 'multiple markers'
         logFile = fullfile(savetopath, 'ExcludedTeamsLog_h1.txt');

         msg = ["Team Number: " + gg
         "Team Id: " + string(Team_Id)
          "Error: " + string(error_catch.errorMSG)
             ""];                       % blank line between entries

        writelines(msg, logFile, WriteMode="append");
        disp('skipping Team...');
        clear groupdat
        continue
    end
    if strcmp(Team_Id,'2f7c7b90372f9c9e')
        disp('Adding team to exclusion list')
        error_catch.errorMSG = 'missing manmade markers'
         logFile = fullfile(savetopath, 'ExcludedTeamsLog_h1.txt');

         msg = ["Team Number: " + gg
         "Team Id: " + string(Team_Id)
          "Error: " + string(error_catch.errorMSG)
             ""];                       % blank line between entries

        writelines(msg, logFile, WriteMode="append");
        disp('skipping Team...');
        clear groupdat

        continue
    end

    if strcmp(Team_Id,'c4ed0094fc18efbc')
        disp('Missing marker info')
        error_catch.errorMSG = 'missing manmade markers'
         logFile = fullfile(savetopath, 'ExcludedTeamsLog_h1.txt');

         msg = ["Team Number: " + gg
         "Team Id: " + string(Team_Id)
          "Error: " + string(error_catch.errorMSG)
             ""];                       % blank line between entries

        writelines(msg, logFile, WriteMode="append");
        disp('skipping Team...');
        clear groupdat

        continue
    end
  %% Preparing empty structures and variables

    error_catch = {}; %This will later include error messages and flag if 
                      % no data or indices could be found for this team

    data = struct(); %Structure to save data later    
    
    allsubjavg_manmade = []; %Array with new averages
    allsubjavg_natural = []; %Array with old averages
    allsubjname    = []; %Array with subject names matching averages


    % Load data
    groupdat = load(fullfile(datapath, grp));
    groupdat = groupdat.(string(fieldnames(groupdat))) % un-nest

      %% Loop over subjects
    % first, we loop over all subjects to get the averages per subject per
    % condition and add them to the prepared arrays
    disp('Processing Subjects');
    tic
    for iSubject = 1:size(groupdat,2)
        %% First, check for empty datasets
        if isempty(groupdat{1,iSubject}) %no Data -> no Average -> next subject
            disp(['Subject ', num2str(iSubject) ,' skipped']);
            continue
        end
        if isempty(groupdat{1,iSubject}.trialinfo) %no Epochs -> no Average -> next subject
            disp(['Subject ', num2str(iSubject) ,' skipped']);
            continue
        end
        %% Now try to get indices with getIndices function
        disp([groupdat{2,iSubject},':']);       
        [ind_manmade,ind_natural,error_catch] = getIndicesGeneral_h1(groupdat{1,iSubject}.trialinfo,groupdat{2,iSubject},Team_Id);

        %% Error handling in subject-loop
        % If getIndices did find some error, we skip the team, i.e. break out
        % of the loop and let the error-handle skip the team later
        if ~isempty(error_catch)
            break
        end
        
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
        avg_manmade = ft_timelockanalysis(cfg,groupdat{1,iSubject});

        cfg = []; %condition natural
        cfg.trials = ind_natural;
        avg_natural = ft_timelockanalysis(cfg,groupdat{1,iSubject});

        % Add subject averages and names to array
        allsubjavg_manmade{size(allsubjavg_manmade,2)+1} = avg_manmade; % Avoids empty entries
        allsubjavg_natural{size(allsubjavg_natural,2)+1} = avg_natural; % Avoids empty entries
        allsubjname{size(allsubjname,2)+1} = groupdat{2,iSubject};

        %%
        clear ind_new ind_old avg_new avg_old cfg 
    end
    t2 = toc;
    disp('Done');
    disp(['Processing Time: ' , num2str(t2), ' seconds.']);
    disp('--------------------')

    %% Error handling
    % if all trialdata is empty and has been skipped, we make a new error
    % message here and skip the team later
    if isempty(allsubjavg_natural) & isempty(error_catch) % all trials empty!
        error_catch.errorMSG = 'All Subject Data Empty';
    end


    % now we skip the team if an error has been found and include the team
    % in an exclusion list for further reference
    if ~isempty(error_catch)
        disp(error_catch.errorMSG);
       disp('Adding team to exclusion list')

         logFile = fullfile(savetopath, 'ExcludedTeamsLog_h1.txt');

         msg = ["Team Number: " + gg
         "Team Id: " + string(Team_Id)
          "Error: " + string(error_catch.errorMSG)
             ""];                       % blank line between entries

        writelines(msg, logFile, WriteMode="append");
        disp('skipping Team...');

        clear allsubjavg_manmade allsubjavg_natural allsubjname  cfg data error_catch groupdat
        continue
    end

    %% Adding Data to Structures
    % here we make the grand-average over all subjects and add all data to
    % our structure to save into our file

    % Add Team_Id & Trials to data
     Team_Id_name = matlab.lang.makeValidName(['sngavg_',Team_Id]); %to make valid name for saving

    data.(Team_Id_name).Team_Id = Team_Id;

    % Make grand avg
    cfg = [];

    data.(Team_Id_name).allgravg_manmade = ft_timelockgrandaverage(cfg, allsubjavg_manmade{:});
    data.(Team_Id_name).allgravg_natural = ft_timelockgrandaverage(cfg, allsubjavg_natural{:});
   
    %% Saving Data
    % Add the data to the one file for our grand-grand-average later
    disp('Saving data...')
    tic
    if exist(strcat(savetopath, 'allgravgdat_h1.mat'),"file") ~= 2
        % creates the file in first loop
        save(strcat(savetopath, 'allgravgdat_h1.mat'),'-struct','data');
    else
        % appends to file after first loop
        save(strcat(savetopath, 'allgravgdat_h1.mat'),'-struct','data','-append');
    end
    t3 = toc;
    disp('Done');
    disp(['Saving time: ' , num2str(t3), ' seconds.']);
    disp('------------------------------------------')

    %% Clear for next Loop
    clear allsubjavg_manmade allsubjavg_natural allsubjname  cfg data error_catch groupdat

end

