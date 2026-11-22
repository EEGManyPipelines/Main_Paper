%% Make Grand Averages of all Teams per Condition
%
% For each group:
%   1) do subj avg per condition (Hit vs Miss)
%   2) do grp grand avg per condition
% Save all grand averages in one file
% Later collect all data and do a great grand average. 
clear;clc;
%% SETUP
% Paths

%FieldTrip, change accordingly, or run personal startup.m file
%addpath('C:\Users\ecesnait\Desktop\EEGManyPipelines\Matlab Scripts\toolboxes\fieldtrip-master\')
addpath('C:\Users\p_agui01\Documents\MATLAB\fieldtrip-master')
addpath 'H:\MATLAB\mne-matlab-master\matlab'; % if needed for mne-teams
ft_defaults;

% InOffice Paths
datapath = 'Z:\aebuschgold\ecesnait\EMP\EMP time series exp\Standardized\'; 
savetopath = 'Z:\nbuschgold\ecesnait\EMP\EMP time series exp\TimelockAVG_Hyp3a\';
cd Z:\aebuschgold\ecesnait\EMP\Patrick\;

% HomeOffice Paths
% datapath = 'X:\ecesnait\EMP\EMP time series exp\Standardized\'; 
% savetopath = 'X:\ecesnait\EMP\EMP time series exp\TimelockAVG_Hyp3a\';

% Get the teams
grps = dir([datapath, '*.mat']);
grps = {grps.name};

% Create subject names
subject_names = {};
for i = 1:33
    subject_names = [subject_names ['Subj-' + string(i)]];
end

%% Collect data from all groups.
% Average withing subject within group, then average all subjects within
% group (grand average). Collect grand averages for comparison across
% groups. Preparing AllInOne Filesave

% -Excluded Groups:
%   Groups witout information: gg 17,20,22,24,25,33,70,114. 8 in total
%   No Raw Event Ids: 23,52,74,89,98,102,105. 7 in total
%   Multiple Trials per Cell: 44,45,54,61,62,67,78,91,104,111. 10 in total
%   Whatever happened here: 51 excluded bc inbalance of trials. 1 in total
%
% -Special Case included Groups:
%   Special cases with extra lines for time-triggercodes: 10,21,75
%   Special cases with trialinfo as table: 28,66
%   Special case with strange trialinfo cells: 63
%   mne-teams: gg=5,7,8,14,23,30,39,43,48,52,55,57,64, ... 
%    71,72,73,74,76,80,87,88,89,90,94,97,98,99,101,102,103,105,106,110,112.
%   Fishy latency cases: 30,43,74,98

for gg = 1:length(grps) %
    %% Preparing empty structures and variables

    error_catch = {}; %This will later include error messages and flag if 
                      % no data or indices could be found for this team

    data = struct(); %Structure to save data later    
    
    % prepare table for number of epochs per condition
    ntrials_hit_mis = table('Size',[34 2],'VariableTypes',{'double', 'double'}, ...
        'VariableNames', {'Hit', 'Miss'}, 'RowNames', [subject_names 'Total']);

    allsubjavg_hit = []; %Array with hit averages
    allsubjavg_mis = []; %Array with miss averages
    allsubjname    = []; %Array with subject names matching averages

    disp(['Processing Team: ',num2str(gg)]);

    % Getting team Ids as identifier
    grp = grps{gg};
    Team_Id = extractBetween(grp, '_', '.');
    Team_Id = Team_Id{1};   %to get proper string
    Team_Id_name = matlab.lang.makeValidName(['sngavg_',Team_Id]); %to make valid name for saving
    disp(['TeamID: ', Team_Id]);

    %% Loading data
    disp('Loading the data...');
    tic
    groupdat = load(fullfile(datapath, grp)); %load the data
    groupdat = groupdat.(string(fieldnames(groupdat))); %un-nest
    t1 = toc;
    disp('Done');
    disp(['Loading time: ' , num2str(t1), ' seconds.']);

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
        [ind_hit,ind_mis,error_catch] = getIndicesGeneral_h3a(groupdat{1,iSubject}.trialinfo,groupdat{2,iSubject},Team_Id);

        %% Error handling in subject-loop
        % If getIndices did find some error, we skip the team, i.e. break out
        % of the loop and let the error-handle skip the team later
        if ~isempty(error_catch)
            break
        end
        
        % this formats the indices for fieldtrip
        if size(ind_hit,2) == 1
            ind_hit = ind_hit';
            ind_mis = ind_mis';
        end

        %% Average across the epochs for two different conditions
        % now we calculate the averages for each condition and save them in
        % an array so we can do a grand-average after subject iteration.
        % also we count the number of trials per condition such that we can
        % compare these later for correctness and for reference.

        cfg = []; %condition hit
        cfg.trials = ind_hit;
        avg_hit = ft_timelockanalysis(cfg,groupdat{1,iSubject});

        cfg = []; %condition miss
        cfg.trials = ind_mis;
        avg_mis = ft_timelockanalysis(cfg,groupdat{1,iSubject});

        % Count the number of epochs for each condition
        ntrials_hit_mis(groupdat{2,iSubject},'Hit') = {length(ind_hit)};
        ntrials_hit_mis(groupdat{2,iSubject},'Miss') = {length(ind_mis)};

        % Add subject averages and names to array
        allsubjavg_hit{size(allsubjavg_hit,2)+1} = avg_hit; % Avoids empty entries
        allsubjavg_mis{size(allsubjavg_mis,2)+1} = avg_mis; % Avoids empty entries
        allsubjname{size(allsubjname,2)+1} = groupdat{2,iSubject};

        %%
        clear ind_hit ind_mis avg_hit avg_mis cfg 
    end
    t2 = toc;
    disp('Done');
    disp(['Processing Time: ' , num2str(t2), ' seconds.']);
    disp('--------------------')

    % then we need to calculate the number of trials to check for unbalanced
    % conditions. If conditions are unbalanced, we make a new error message
    % and skip the team later. Also we want the trialcount stored for
    % reference later
    ntrials_hit_mis('Total','Hit') = sum(ntrials_hit_mis(:,'Hit'));
    ntrials_hit_mis('Total','Miss') = sum(ntrials_hit_mis(:,'Miss'));

    %% Error handling
    % if all trialdata is empty and has been skipped, we make a new error
    % message here and skip the team later
    if isempty(allsubjavg_hit) & isempty(error_catch) % all trials empty!
        error_catch.errorMSG = 'All Subject Data Empty';
    end


    % now we skip the team if an error has been found and include the team
    % in an exclusion list for further reference
    if ~isempty(error_catch)
        disp(error_catch.errorMSG);
        disp('Adding team to exclusion list')
        writelines(['Team Number: ' num2str(gg) ...
            newline 'Team Id: ' Team_Id ...
            newline 'Error: ' error_catch.errorMSG ...
            newline], ...
            [savetopath 'ExcludedTeamsLog_h3a.txt'],WriteMode="append");        
        disp('skipping Team...');
        clear allsubjavg_mis allsubjavg_hit allsubjname ntrials_hit_mis cfg data error_catch
        continue
    end

    %% Adding Data to Structures
    % here we make the grand-average over all subjects and add all data to
    % our structure to save into our file

    % Add Team_Id & Trials to data
    data.(Team_Id_name).Team_Id = Team_Id;

    % Make grand avg
    cfg = [];
    data.(Team_Id_name).allgravg_hit = ft_timelockgrandaverage(cfg, allsubjavg_hit{:});
    data.(Team_Id_name).allgravg_mis = ft_timelockgrandaverage(cfg, allsubjavg_mis{:});
    
    % Add Numbers to structure
    data.(Team_Id_name).num_trials_hit_mis = ntrials_hit_mis;
    %% Make Plots for checking later
    % Save a plot for sanity checking later
    cfg  = [];
    cfg.channel = 'FCz';
    cfg.figure = 'no';

    %fig = figure('Visible', 'off');
    ft_singleplotER(cfg,data.(Team_Id_name).allgravg_hit,data.(Team_Id_name).allgravg_mis);
    legend('Hit', 'Miss');
   % saveas(fig,[savetopath '\Plots\' Team_Id_name '_plot.png']);

    %% Saving Data
    % % Add the data to the one file for our grand-grand-average later
    % disp('Saving data...')
    % tic
    % if exist([savetopath, 'allgravgdat.mat'],"file") ~= 2
    %     % creates the file in first loop
    %     save([savetopath, 'allgravgdat.mat'],'-struct','data');
    % else
    %     % appends to file after first loop
    %     save([savetopath, 'allgravgdat.mat'],'-struct','data','-append');
    % end
    % t3 = toc;
    % disp('Done');
    % disp(['Saving time: ' , num2str(t3), ' seconds.']);
    % disp('------------------------------------------')

    %% Clear for next Loop
    clear allsubjavg_hit allsubjavg_mis allsubjname ntrials_hit_mis cfg data error_catch

end
