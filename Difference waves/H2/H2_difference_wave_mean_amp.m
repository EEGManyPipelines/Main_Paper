% This code calculates the difference wave for hypothesis 2 by subtracting
% grand average ERPs of already seen images from new images. Teams that
% submitted their data in volts are converted to microvolts.
% Then the mean difference wave is averaged between 300 to 500ms
% post-stimulus across frontal channels. 

clear all, close all
load('allgravgdat_2a.mat')

a = whos;
object_name = {a.name};
IDs_h2a = extractAfter(object_name,'_');

%Identifiers of teams whose data needs to be transformed from Volts to microVolts. 
Volt_teams = {'08de3c5e092173e4', '0ba1c7f1dafc1134','0bc9ee704db74104', '19e8ad8bf94af489',...
           '344dd59ded90cb34', '48e64dc185199502', '628a18bc8a3d36dd', ...
           '77fddd91c557626d', '7f3fe2bc79a9d3f9','8559e4d7314e45ec', '8c587a4cbf53865d',...
           '90420442f22fa870', '9985e8ae6679b0e2','CIMHPipe', 'TheCodeMechanics', 'Varuwa',...
           'a0cf32754296214f','a25b8419335d2131','aa6aa366e9788967', ...
           'b0edd369b6d8f4f1', 'bd3077a83b5b16bd', 'c0c75576f9cd0b2a','c4ed0094fc18efbc', 'c577d3cdf78548ce',...
           'c91e489c4acd0bf4','da33cf2264c9baa2', 'd5c8ed05b7af02a3', 'e13e7e07b99d853b', 'e146a94b29a41713', ...
           'e2d0d90e5cf594ed','e69a83408d1f3811','e72d90a6ff4b5108', 'ea6b1d1870708b82',...
           'ee8c062e3dc35b1d', 'eef1406b3fca3e9c', 'f92a1d6a49d0d40a', 'ff8bf48d04d11c84'}

indx_id = ismember(IDs_h2a,Volt_teams);
volt_indx = find(indx_id);

difference_wave = nan(64,205,88); % prepare an empty matrix
count=0
fig = figure
for t = 1:88 % normal small scale 11, 16, 42,51,66

    one_team = eval(object_name{t});

    %detect outliers
    mean_ampl = nanmean(nanmean(one_team.allgravg_new.avg));
    if abs(mean_ampl) >10 % if a mean amplitude is larger than 10 uV
        count = count+1
        continue
    elseif ismember(t, volt_indx)% if the scale is in volts
        one_team.allgravg_new.avg = one_team.allgravg_new.avg * 10^6;
        one_team.allgravg_old.avg = one_team.allgravg_old.avg * 10^6;

    end
    difference_wave(:,:,t) = one_team.allgravg_new.avg - one_team.allgravg_old.avg;
    
    % We plot Fz channel to inspect data
    indx_fz = find(ismember(one_team.allgravg_new.label, 'Fz'));

    plot(one_team.allgravg_new.time, difference_wave(indx_fz,:,t), 'LineWidth',1.2,'Color',[0 0.4470 0.7410 0.2]),hold on
    clearvars one_team 
end

 xline(0,'--'), xlabel('Time'), ylabel('uV'), title('H2a difference wave')

saveas(fig,'diff wave h2a.png')
close


%% take a mean over fronto-central channels from 300-500ms
frontocentral_ch = {'Cz', 'Fz','FCz', 'FC1', 'FC2', 'FC3', 'FC4', 'FC5', 'FC6'};
all_chan = sngavg_ff8cd5ce3123a097.allgravg_new.label; % the order of channel labels is matched across all teams in the files we use. We take an exemplary one here.
indx_chan = find(ismember(all_chan, frontocentral_ch'));
indx_time = sngavg_ff8cd5ce3123a097.allgravg_new.time <= 500 & sngavg_ff8cd5ce3123a097.allgravg_new.time >= 300;
front_diff = nanmean(squeeze(nanmean(difference_wave(indx_chan,indx_time,:))));

IDs = extractAfter(object_name,'_')

T = table()
T.ID = IDs'
T.DW = front_diff'

writetable(T, 'h2a_difference_wave_front_tw_v2.csv', 'Delimiter',',')

%% save for plotting difference wave across the whole epoch
one_team = eval(object_name{t}); % only used for the time reference. Note that epoch lenghts are matched across teams in the files we use.
amp_h2a = squeeze(nanmean(difference_wave(indx_chan,:,:)))% mean over the frontal channels for plotting
time_h2a = one_team.allgravg_new.time;

save('IDs_h2a_diff_wave','IDs_h2a')
save('H2a_amplitude_v2','amp_h2a')
save('H2a_time_xaxis','time_h2a')
