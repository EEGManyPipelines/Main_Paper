clear all, close all

%Load difference wave
load('C:\Users\ecesnait\Desktop\EEGManyPipelines\git\EEGManyPipes org\Main_Paper\Time-series\erplot\ERP based on condition\difference_wave_orig_109_clean.mat')

% Load channel labels and locations that will be used for plotting the
% result
channels = readtable('C:\Users\ecesnait\Desktop\EEGManyPipelines\git\EEGManyPipelines-personal\Matlab\Result forms\Data\res_topoplot_h1.csv')

% calculate the mean N1 amplitude in a time window between 100-120ms
n1_amp = table('Size',[109,65],'VariableTypes',["string", repelem("double",1,64)],'VariableNames',[{'ID'}, channels.electrodes'])

no_channel = 0

tstart = 80 % time window for N100 avg
tstop = 120

difference_wave{1,2}.label{end} = 'Cz' % a typo

for pp =1:length(difference_wave)
    xstart = dsearchn(difference_wave{pp}.time', tstart)
    xstop  = dsearchn(difference_wave{pp}.time', tstop)

    % if the format is not channels x times, but times x channels
    if size(difference_wave{pp}.data,1) > size(difference_wave{pp}.data,2)
        difference_wave{pp}.data = difference_wave{pp}.data';
    end

    chan_n1 = mean(difference_wave{pp}.data(:,xstart:xstop),2); % average N1 a,plitude in all channels

    [indx_chan, pl_chan] = ismember(channels.electrodes,difference_wave{pp}.label)

    name = difference_wave{pp}.id

    if sum(indx_chan)==0
        %error('somethign is off')
        cell_Array = regexprep(difference_wave{pp}.label, '\s', '') %delete spaces
        difference_wave{pp}.label = cell_Array
        [indx_chan, pl_chan] = ismember(channels.electrodes,difference_wave{pp}.label)

    elseif ~isequal(channels.electrodes,difference_wave{pp}.label(pl_chan(find(pl_chan)))) && isempty(find(~indx_chan))
        error('ispect order of channels')
        
    else
        if ~isempty(find(~indx_chan)) % if there are channels that
            indx_empty=find(~indx_chan)
            pl_chan(indx_empty) = 1 %replace zeros with a value to be applied
            n1_amp(pp,2:end) = array2table(chan_n1(pl_chan)') ;
            n1_amp(pp,indx_empty+1) = table(NaN) ;%remove that value that was added
        else
            n1_amp(pp,2:end) = array2table(chan_n1(pl_chan)') ;
        end
        n1_amp(pp,1).ID = {name}
    end
    clear chan_n1 indx_chan pl_chan indx_zero xstart xstop name pl_chan indx_empty cell_Array
end

% find outliers
mean_all_team = nanmean(table2array(n1_amp(:,2:end)),2) 

boxplot(mean_all_team) % one clear outlier?
find(mean_all_team==max(mean_all_team))%team 27

n1_amp(27,:) = []

% clean IDs
n1_amp.ID(1:56)=extractBefore(n1_amp.ID(1:56),'.mat')

save('n1_amplitude_v2_all_chan','n1_amp')
writetable(n1_amp,'n1_amplitudes_all_chan.csv', 'Delimiter',',')