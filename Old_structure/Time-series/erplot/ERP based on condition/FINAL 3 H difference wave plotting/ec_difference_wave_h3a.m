clear all, close all
load('allgravgdat_3a.mat')

a = whos;
object_name = {a.name};

difference_wave = nan(64,205,88);
count=0
fig = figure
for t = 1:88 % normal small scale 11, 16, 42,51,66

    one_team = eval(object_name{t});
   
    %detect outliers
    mean_ampl = nanmean(nanmean(one_team.allgravg_hit.avg));
    if abs(mean_ampl) >10
        count = count+1
        continue
    elseif abs(mean_ampl) < 10^-5 & ~ismember(t,[11, 16, 42,51,66])% if the scale is in volts
        
        %error('elena inspect scale') % inspect first, bring back to uV next
        one_team.allgravg_hit.avg = one_team.allgravg_hit.avg * 10^6;
        one_team.allgravg_mis.avg = one_team.allgravg_mis.avg * 10^6;

    end
    difference_wave(:,:,t) = one_team.allgravg_hit.avg - one_team.allgravg_mis.avg;
    indx_fz = find(ismember(one_team.allgravg_hit.label, 'Pz'));% 40

    plot(one_team.allgravg_hit.time, difference_wave(indx_fz,:,t), 'LineWidth',1.2,'Color',[0 0.4470 0.7410 0.2]),hold on

    clearvars one_team 
end

yline(0,'LineWidth',1.5), xline(0,'--'), xlabel('Time'), ylabel('uV'), title('H3a difference wave')
fontsize(fig, scale=1.5)
saveas(fig,'diff wave h3a Pz.png')
close

save('difference_wave_h3a','difference_wave')

% save for plotting in R
 one_team = eval(object_name{t})
amp_h3a = squeeze(difference_wave(40,:,:))
time_h3a = one_team.allgravg_hit.time

save('H3a_amplitude','amp_h3a')
save('H3a_time_xaxis','time_h3a')

%% take a mean over fronto-central channels from 300-500ms for the FN400
frontocentral_ch = {'CPz', 'Pz'}
all_chan = sngavg_ff8cd5ce3123a097.allgravg_mis.label
indx_chan = find(ismember(all_chan, frontocentral_ch'))
indx_time = sngavg_ff8cd5ce3123a097.allgravg_hit.time <= 600 & sngavg_ff8cd5ce3123a097.allgravg_hit.time >= 400
front_diff = nanmean(squeeze(nanmean(difference_wave(indx_chan,indx_time,:))));

IDs = extractAfter(object_name,'_')

T = table()
T.ID = IDs'
T.DW = front_diff'

writetable(T, 'h3a_difference_wave_LPC.csv', 'Delimiter',',')
