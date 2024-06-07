clear all, close all

load('C:\Users\ecesnait\Desktop\EEGManyPipelines\git\EEGManyPipes org\Main_Paper\Time-series\erplot\ERP based on condition\difference_wave_orig_109_clean.mat')

% calculate the mean N1 amplitude in a time window between 100-120ms
n1_amp = table()
n1_amp.id = cell(length(difference_wave),1)
n1_amp.cpz = zeros(length(difference_wave),1)

no_channel = 0
%n1_amp.chan = allgrpdat_full{1,1}.label(1:64)

tstart = 80 % time window for N100 avg
tstop = 120

difference_wave{1,2}.label{end} = 'Cz' % a typo

for pp = 1:length(difference_wave)
     xstart = dsearchn(difference_wave{pp}.time', tstart)
     xstop  = dsearchn(difference_wave{pp}.time', tstop)

       % if the format is not channels x times, but times x channels
    if size(difference_wave{pp}.data,1) > size(difference_wave{pp}.data,2) 
        difference_wave{pp}.data = difference_wave{pp}.data';

%     elseif pp == 114
%         allgrpdat_full{1,pp}.avg = squeeze(allgrpdat_full{1,pp}.avg);
%         allgrpdat_full{1,pp}.var = squeeze(allgrpdat_full{1,pp}.var);
%         allgrpdat_full{1,pp}.dof = squeeze(allgrpdat_full{1,pp}.dof);
%         allgrpdat_full{1,pp}.dimord = 'chan_time'
    end
%     if ismember(pp, [19, 31, 35, 45, 53,59,110,119])  % p= 19(scale), 31(scale), 35 (scale), 45 (scale),53(scale in some of the channels), ...
%         % 59(wrong GA averaging), 95(time corrected),110(only 8 channels and no N1)
%         %  111(only 36 channels-returned), 119(empty avg, cross check)
%        continue
% %        error('check and correct')
% %        figure, plot(allgrpdat_full{1,pp}.time,allgrpdat_full{1,pp}.avg),xlim([-100,200])
%     end

    chan_n1 = mean(difference_wave{pp}.data(:,xstart:xstop),2); % average N1 a,plitude in all channels

    [indx_chan, pl_chan] = ismember(difference_wave{pp}.label, 'CPz')
    name = difference_wave{pp}.id

    if sum(indx_chan)==0
        no_channel = no_channel + 1;% 11 teams to inspect
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


indx_empty = find(cellfun(@isempty,n1_amp.id))
n1_amp(indx_empty,:) = [] % used CPz as a reference
[indx,pl]  = max(n1_amp.cpz) % outlier 47 amp diff
n1_amp(pl,:) = []

[indx,pl]  = max(n1_amp.cpz) % outlier 3 amp diff
n1_amp(pl,:) = []

% clean IDs
n1_amp.id(1:55)=extractBefore(n1_amp.id(1:55),'.mat')
figure,hist(n1_amp.cpz)

save('n1_amplitude_v2','n1_amp')
writetable(n1_amp,'n1_amplitudes_v2.csv', 'Delimiter',',')