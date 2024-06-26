clear all, close all

load('C:\Users\ecesnait\Desktop\EEGManyPipelines\git\EEGManyPipes org\Main_Paper\Time-series\erplot\allgrpdat files\allgrpdat_1_147_Andrea_memory.mat')
IDs_stand = [extractBefore(allgrpdat_joined(2,1:90), '.'), allgrpdat_joined(2,91:124), extractBefore(allgrpdat_joined(2,125:end), '_')]
allgrpdat_joined(2,:) = IDs_stand

% remove empty entries due to continuous or corrupt data files
allgrpdat_full = allgrpdat_joined(:,~cellfun(@isempty, allgrpdat_joined(1,:)))

% get mean varaibility per team per channel

variance = table()
variance.chan = allgrpdat_full{1,1}.label(1:64)

allgrpdat_full{1,2}.label{end} = 'Cz' % a typo

for pp = 4:length(allgrpdat_full)
    
       % if the format is not channels x times, but times x channels
    if size(allgrpdat_full{1,pp}.avg,1) > size(allgrpdat_full{1,pp}.avg,2) 
        allgrpdat_full{1,pp}.avg = allgrpdat_full{1,pp}.avg';
        allgrpdat_full{1,pp}.var = allgrpdat_full{1,pp}.var';
        allgrpdat_full{1,pp}.dof = allgrpdat_full{1,pp}.dof';
        allgrpdat_full{1,pp}.dimord = 'chan_time'
    elseif pp == 114
        allgrpdat_full{1,pp}.avg = squeeze(allgrpdat_full{1,pp}.avg);
        allgrpdat_full{1,pp}.var = squeeze(allgrpdat_full{1,pp}.var);
        allgrpdat_full{1,pp}.dof = squeeze(allgrpdat_full{1,pp}.dof);
        allgrpdat_full{1,pp}.dimord = 'chan_time'
    end
    if ismember(pp, [19, 31, 35, 45, 53,59,110,119])  % p= 19(scale), 31(scale), 35 (scale), 45 (scale),53(scale in some of the channels), ...
        % 59(wrong GA averaging), 95(time corrected),110(only 8 channels and no N1)
        %  111(only 36 channels-returned), 119(empty avg, cross check)
       continue
%        error('check and correct')
%        figure, plot(allgrpdat_full{1,pp}.time,allgrpdat_full{1,pp}.avg),xlim([-100,200])
    end

    team_var = mean(allgrpdat_full{1,pp}.var,2); % average N1 a,plitude in all channels

    [indx_chan, pl_chan] = ismember(variance.chan, allgrpdat_full{1,pp}.label)
    name = allgrpdat_full{2,pp}

    if sum(pl_chan == 0)
        indx_zero = find(pl_chan == 0)
        variance.(name)(indx_zero) = nan()
        variance.(name)(setdiff([1:64]',indx_zero)) = team_var(pl_chan(indx_chan));
        
    else
        variance.(name) = team_var(pl_chan)
    end

    clear team_var indx_chan pl_chan indx_zero
end

save('variance_120','variance')

%% Plot %%
addpath C:\Users\ecesnait\Desktop\EEGManyPipelines\Matlab Scripts\toolboxes\eeglab_current\eeglab2022.0\
eeglab

%Load channel locations of the dataset
load('C:\Users\ecesnait\Desktop\EEGManyPipelines\git\EEGManyPipes org\Main_Paper\Time-series\erplot\N100\chan_locs_CORENATS')
chan_locs = chan_locs(ismember({chan_locs.labels}, variance.chan)) 

% check if there are teams that have a amplitude scale in 10^-6
colum_mean = nanmean(abs(table2array(variance(:,2:end))))
teams_low_scale = find(colum_mean<10^-10)%find teams that have this scale
var_data_normalized = table2array(variance(:,2:end));
var_data_normalized(:,teams_low_scale) = var_data_normalized(:,teams_low_scale)*10^12;
colum_mean = nanmean(abs(var_data_normalized))

avg_var_all_teams = nanmedian(var_data_normalized, 2); % median across teams

fig=figure, topoplot(avg_var_all_teams, chan_locs, 'electrodes', 'labels', 'maplimits', 'maxmin'), title('All teams', 'FontSize',15),
colorbar,
c = colorbar
c.Label.String = 'var'
c.FontSize = 15
saveas(fig,'variance_topoplot_113.png')