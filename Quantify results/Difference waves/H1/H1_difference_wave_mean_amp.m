close all, clear all
addpath 'Y:\aebusch\ecesnait\Desktop\EEGManyPipelines\git\EEGManyPipes org\Main_Paper\Time-series\erplot\ERP based on condition\'
load("allgravgdat_h1.mat")

a = whos;
object_name = {a.name};
IDs_h1 = extractAfter(object_name,'_');

% find teams that uploaded data in Volts
nTeams = numel(a);
amp = zeros(nTeams,1);

for i = 1:nTeams
    x = eval(object_name{i});
    one_chan = x.allgravg_manmade.avg(18,:);
    one_chan = one_chan(~isnan(one_chan));
    one_chan = one_chan - median(one_chan);          % remove DC offset
    amp(i) = median(abs(one_chan));    % robust amplitude (or use std(x))
end

% Look at the distribution on a log scale
figure; histogram(log10(amp), 30);
xlabel('log_{10}(amplitude)'); ylabel('# teams');

% Flag teams whose amplitude is ~1e6 smaller than the typical team
ref = median(amp);
isVolts = amp < ref / 1e3;      % threshold halfway (in log terms) between 1 and 1e6

voltTeams = find(isVolts)

%%correct teams from Volts to microVolts. 
%Volt_teams = 'd5c8ed05b7af02a3';
 Volt_teams = {'08de3c5e092173e4', '0ba1c7f1dafc1134','0bc9ee704db74104', '19e8ad8bf94af489',...
            '344dd59ded90cb34', '48e64dc185199502', '628a18bc8a3d36dd', ...
            '77fddd91c557626d', '7f3fe2bc79a9d3f9','8559e4d7314e45ec', '8c587a4cbf53865d',...
            '90420442f22fa870', '9985e8ae6679b0e2','CIMHPipe', 'TheCodeMechanics', 'Varuwa',...
            'a0cf32754296214f','a25b8419335d2131','aa6aa366e9788967', ...
            'b0edd369b6d8f4f1', 'bd3077a83b5b16bd', 'c0c75576f9cd0b2a','c4ed0094fc18efbc', 'c577d3cdf78548ce',...
            'c91e489c4acd0bf4','da33cf2264c9baa2', 'd5c8ed05b7af02a3', 'e13e7e07b99d853b', 'e146a94b29a41713', ...
            'e2d0d90e5cf594ed','e69a83408d1f3811','e72d90a6ff4b5108', 'ea6b1d1870708b82',...
            'ee8c062e3dc35b1d', 'eef1406b3fca3e9c', 'f92a1d6a49d0d40a', 'ff8bf48d04d11c84','d5c8ed05b7af02a3'}
%not in volts: 

indx_id = ismember(IDs_h1,Volt_teams);
volt_indx = find(indx_id);

difference_wave = nan(64,205,length(a));
count=0
large_amp_teams = {}

fig = figure
for t = 1:length(a) 

    one_team = eval(object_name{t});
    %detect outliers
    mean_ampl = nanmean(nanmean(one_team.allgravg_manmade.avg));
    if abs(mean_ampl) >10
        count = count+1
        large_amp_teams{length(large_amp_teams)+1} = one_team.Team_Id
        continue
    elseif ismember(t, volt_indx)% if the scale is in volts
        one_team.allgravg_manmade.avg = one_team.allgravg_manmade.avg * 10^6;
        one_team.allgravg_natural.avg = one_team.allgravg_natural.avg * 10^6;

    end
    difference_wave(:,:,t) = one_team.allgravg_manmade.avg - one_team.allgravg_natural.avg;
    indx_cpz = find(ismember(one_team.allgravg_manmade.label, 'CPz'));% 

    if sum(difference_wave(indx_cpz,:,t))==0
        break
    end


    plot(one_team.allgravg_manmade.time, difference_wave(indx_cpz,:,t), 'LineWidth',1.2,'Color',[0 0.4470 0.7410 0.2]),hold on
    clearvars one_team 
end

 xline(0,'--'), xlabel('Time'), ylabel('uV'), title('H2a difference wave')


%% take a 
all_chan = sngavg_d5c8ed05b7af02a3.allgravg_manmade.label
indx_chan = find(ismember(all_chan, 'CPz'))
indx_time = sngavg_d5c8ed05b7af02a3.allgravg_manmade.time <= 120 & sngavg_d5c8ed05b7af02a3.allgravg_manmade.time >= 80
cpz_diff = squeeze(nanmean(difference_wave(indx_chan,indx_time,:)));

IDs = extractAfter(object_name,'_')

T = table()
T.ID = IDs'
T.DW = cpz_diff

writetable(T, 'h1_difference_wave_cpz_n100.csv', 'Delimiter',',')

% save for plotting in R
one_team = eval(object_name{t})
amp_h1 = squeeze(difference_wave(indx_chan,:,:))%
time_h1 = one_team.allgravg_manmade.time

save('IDs_h1_diff_wave_2','IDs')
save('H1_amplitude_v2_2','amp_h1')
save('H1_time_xaxis_2','time_h1')