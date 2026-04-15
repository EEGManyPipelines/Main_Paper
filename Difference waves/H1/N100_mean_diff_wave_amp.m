% This code loads the difference waves for hypothesis 1 and calculates the mean
% difference wave in the CPz channel in the time window around the N100 peak -
% from 80 to 120ms.

clear all, close all

load('C:\Users\ecesnait\Desktop\EEGManyPipelines\git\EEGManyPipes org\Main_Paper\Time-series\erplot\ERP based on condition\difference_wave_orig_109_clean.mat')

% create an empty table for the mean N1 amplitude in a time window between 80-120ms
n1_amp = table()
n1_amp.id = cell(length(difference_wave),1)
n1_amp.cpz = zeros(length(difference_wave),1)

no_channel = 0 % we will count below how many team had no data in the CPz channel

tstart = 80 % time window for N100
tstop = 120

%difference_wave{1,2}.label{end} = 'Cz' % a typo

for pp = 1:length(difference_wave)
     xstart = dsearchn(difference_wave{pp}.time', tstart);
     xstop  = dsearchn(difference_wave{pp}.time', tstop);

       % if the format is not channels x times, but times x channels
    if size(difference_wave{pp}.data,1) > size(difference_wave{pp}.data,2) 
        difference_wave{pp}.data = difference_wave{pp}.data';

    end

    chan_n1 = mean(difference_wave{pp}.data(:,xstart:xstop),2); % average N1 amplitude in all channels

    [indx_chan, pl_chan] = ismember(difference_wave{pp}.label, 'CPz'); %find CPz
    name = difference_wave{pp}.id; % team ID

    if sum(indx_chan)==0
        no_channel = no_channel + 1;
        continue
%         indx_zero = find(pl_chan == 0)
%         n1_amp.(name)(indx_zero) = nan()
%         n1_amp.(name)(setdiff([1:64]',indx_zero)) = chan_n1(pl_chan(indx_chan));
        
    else
        n1_amp(pp,1).id = {name}
        n1_amp(pp,2).cpz = chan_n1(indx_chan);
    end

    clear chan_n1 indx_chan pl_chan indx_zero xstart xstop
end

% Find empty entries if data didn't exist in the CPz channel
indx_empty = find(cellfun(@isempty,n1_amp.id))
n1_amp(indx_empty,:) = [] % remove teams that used CPz as a reference

% inspect for outliers
idx = find(abs(n1_amp.cpz) > 10);
n1_amp(idx,:) = []

% clean IDs
n1_amp.id(1:55)=extractBefore(n1_amp.id(1:55),'.mat')

save('n1_amplitude_v3','n1_amp')
writetable(n1_amp,'n1_amplitudes_v3.csv', 'Delimiter',',')