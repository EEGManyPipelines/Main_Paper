clear all, close all
load('allgravgdat_2a.mat')

a = whos;
object_name = {a.name};

difference_wave = nan(64,205,88);
count=0
fig = figure
for t = 1:88 % normal small scale 11, 16, 42,51,66

    one_team = eval(object_name{t});
    %detect outliers
    mean_ampl = nanmean(nanmean(one_team.allgravg_new.avg));
    if abs(mean_ampl) >10
        count = count+1
        continue
    elseif abs(mean_ampl) < 10^-5 && ~ismember(t, [11, 16, 42,51,66])% if the scale is in volts
      
        one_team.allgravg_new.avg = one_team.allgravg_new.avg * 10^6;
        one_team.allgravg_old.avg = one_team.allgravg_old.avg * 10^6;

    end
    difference_wave(:,:,t) = one_team.allgravg_new.avg - one_team.allgravg_old.avg;
    
    plot(one_team.allgravg_new.time, difference_wave(38,:,t), 'LineWidth',1.2,'Color',[0 0.4470 0.7410 0.2]),hold on
    clearvars one_team 
end

yline(0,'LineWidth',1.5), xline(0,'--'), xlabel('Time'), ylabel('uV'), title('H2a difference wave')

saveas(fig,'diff wave h2a.png')
close

% save for plotting in R
 one_team = eval(object_name{t})
amp_h2a = squeeze(difference_wave(38,:,:))
time_h2a = one_team.allgravg_new.time

save('H2a_amplitude','amp_h2a')
save('H2a_time_xaxis','time_h2a')

%% take a mean over fronto-central channels from 300-500ms
frontocentral_ch = {'Cz', 'Fz', 'FC1', 'FC2', 'FC3', 'FC4', 'FC5', 'FC6'}
all_chan = sngavg_ff8cd5ce3123a097.allgravg_new.label
indx_chan = find(ismember(all_chan, frontocentral_ch'))
indx_time = sngavg_ff8cd5ce3123a097.allgravg_new.time <= 500 & sngavg_ff8cd5ce3123a097.allgravg_new.time >= 300
front_diff = nanmean(squeeze(nanmean(difference_wave(indx_chan,indx_time,:))));

IDs = extractAfter(object_name,'_')

T = table()
T.ID = IDs'
T.DW = front_diff'

writetable(T, 'h2a_difference_wave_front_tw.csv', 'Delimiter',',')
