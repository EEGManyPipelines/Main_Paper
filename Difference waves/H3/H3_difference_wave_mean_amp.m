% This code calculates the difference wave for hypothesis 3 by subtracting
% grand average ERPs of hits (correctly identified images) from misses (incorrectly recognised images). Teams that
% submitted their data in volts are converted to microvolts.
% The code can be run for Fz and Pz channels. Curently it's set to Fz.
% Then the mean difference wave is averaged between 300 to 500ms
% post-stimulus across frontal channels. 

clear all, close all
load('allgravgdat_3a.mat')

a = whos;
object_name = {a.name};

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
    elseif ismember(t, volt_indx)% if the scale is in volts
        
       % error('Inspect scale') % inspect first, bring back to uV next
        one_team.allgravg_hit.avg = one_team.allgravg_hit.avg * 10^6;
        one_team.allgravg_mis.avg = one_team.allgravg_mis.avg * 10^6;

    end
    difference_wave(:,:,t) =  one_team.allgravg_mis.avg - one_team.allgravg_hit.avg;
    indx_fz = find(ismember(one_team.allgravg_hit.label, 'Fz'));

    if indx_fz ~= 40 % in case the channel order is wrong
        error("wrong indexing")
    end

    plot(one_team.allgravg_hit.time, difference_wave(indx_fz,:,t), 'LineWidth',1.2,'Color',[0 0.4470 0.7410 0.2]),hold on

    clearvars one_team indx_fz
end

yline(0,'LineWidth',1.5), xline(0,'--'), xlabel('Time'), ylabel('uV'), title('H3a difference wave')
fontsize(fig, scale=1.5)
saveas(fig,'diff wave h3a Fz.png')
close

save('difference_wave_h3a_reversed','difference_wave') % difference waves of all channels and all teams are saverd here

%% Save data from Fz and Pz channels for plotting only
one_team = eval(object_name{t}); % only used for the time reference. Note that epoch lenghts are matched across teams in the files we use.
amp_h3a_fz = squeeze(difference_wave(40,:,:));% Fz in every team is 40
time_h3a = one_team.allgravg_hit.time;

save('H3a_amplitude_reversed_Fz','amp_h3a_fz')
save('H3a_time_xaxis','time_h3a')

one_team = eval(object_name{t});
amp_h3a_pz = squeeze(difference_wave(60,:,:)) % Pz in every team is 60

save('H3a_amplitude_reversed_Pz','amp_h3a_pz')


%% Take a mean over Fz from 300-500ms 
frontocentral_ch = {'Fz'};
all_chan = sngavg_ff8cd5ce3123a097.allgravg_mis.label;
indx_chan = find(ismember(all_chan, frontocentral_ch'));
indx_time = sngavg_ff8cd5ce3123a097.allgravg_hit.time <= 500 & sngavg_ff8cd5ce3123a097.allgravg_hit.time >= 300;
front_diff = squeeze(nanmean(difference_wave(indx_chan,indx_time,:)));

IDs = extractAfter(object_name,'_');

T_Fz = table();
T_Fz.ID = IDs';
T_Fz.DW = front_diff;

writetable(T_Fz, 'h3a_difference_wave_FN400_reversed.csv', 'Delimiter',',')

%% take a mean over Pz from 300-500ms
parietal_ch = {['Pz']};
all_chan = sngavg_ff8cd5ce3123a097.allgravg_mis.label;
indx_chan = find(ismember(all_chan, parietal_ch));
indx_time = sngavg_ff8cd5ce3123a097.allgravg_hit.time <= 500 & sngavg_ff8cd5ce3123a097.allgravg_hit.time >= 300;
par_diff = squeeze(nanmean(difference_wave(indx_chan,indx_time,:)));

IDs = extractAfter(object_name,'_');

T_Pz = table();
T_Pz.ID = IDs';
T_Pz.DW = par_diff;

writetable(T_Pz, 'h3a_difference_wave_LPC_reversed.csv', 'Delimiter',',')
