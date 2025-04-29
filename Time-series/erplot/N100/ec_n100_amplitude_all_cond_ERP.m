clear all, close all

load('C:\Users\ecesnait\Desktop\EEGManyPipelines\git\EEGManyPipes org\Main_Paper\Time-series\erplot\allgrpdat files\allgrpdat_1_147_Andrea_memory.mat')
IDs_stand = [extractBefore(allgrpdat_joined(2,1:90), '.'), allgrpdat_joined(2,91:124), extractBefore(allgrpdat_joined(2,125:end), '_')]
allgrpdat_joined(2,:) = IDs_stand

% remove empty entries due to continuous or corrupt data files
allgrpdat_full = allgrpdat_joined(:,~cellfun(@isempty, allgrpdat_joined(1,:)))

%correct team IDs

% correct time for some teams
wrong_time = [13, 47, 48, 62, 95]

for i= 1:length(wrong_time)
    if i == 1 % 0 is start of the epoch and 1000 is stimulus presentation
        allgrpdat_full{1,wrong_time(i)}.time = allgrpdat_full{1,wrong_time(i)}.time - 1000 % 
    else
       allgrpdat_full{1,wrong_time(i)}.time = allgrpdat_full{1,wrong_time(i)}.time * 1000 % from seconds to ms
    end
end

% calculate the mean N1 amplitude in a time window between 100-120ms
n1_amp = table()
n1_amp.chan = allgrpdat_full{1,1}.label(1:64)

tstart = 100 % time window for N100 avg
tstop = 120

allgrpdat_full{1,2}.label{end} = 'Cz' % a typo

for pp = 1:length(allgrpdat_full)
     xstart = dsearchn(allgrpdat_full{1,pp}.time', tstart)
     xstop  = dsearchn(allgrpdat_full{1,pp}.time', tstop)

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

    chan_n1 = mean(allgrpdat_full{1,pp}.avg(:,xstart:xstop),2); % average N1 a,plitude in all channels

    [indx_chan, pl_chan] = ismember(n1_amp.chan, allgrpdat_full{1,pp}.label)
    name = allgrpdat_full{2,pp}

    if sum(pl_chan == 0)
        indx_zero = find(pl_chan == 0)
        n1_amp.(name)(indx_zero) = nan()
        n1_amp.(name)(setdiff([1:64]',indx_zero)) = chan_n1(pl_chan(indx_chan));
        
    else
        n1_amp.(name) = chan_n1(pl_chan)
    end

    clear chan_n1 indx_chan pl_chan indx_zero
end

save('n1_amplitude_120','n1_amp')
writetable(n1_amp,'n1_amplitudes_120.csv', 'Delimiter',',')