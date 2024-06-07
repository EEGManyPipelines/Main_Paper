close all, clear all

% Load data
load('GA_ERPs_all_teams_manmade.mat')
load('GA_ERPs_all_teams_natural.mat')
one_team_manmade = load('allgrpdat_manmade_93af9133e6fa6cf5.mat')
one_team_natural = load('allgrpdat_natural_93af9133e6fa6cf5.mat')
all_manmade{1,119} = one_team_manmade.allgrpdat_manmade{1,1}
all_natural{1,119} = one_team_natural.allgrpdat_natural{1,1}
all_manmade{2,119} = '93af9133e6fa6cf5'
all_natural{2,119} = '93af9133e6fa6cf5'


% delete an empty indx
indx_empty = cellfun(@isempty, all_manmade(1,:))
all_manmade(:,indx_empty) = [];
all_natural(:,indx_empty) = [];
%115 is empty
all_manmade(:,115) = [];
all_natural(:,115) = [];
% probelmatic teams
%'93af9133e6fa6cf5.mat' - corrected events
%'9985e8ae6679b0e2.mat'- several event markers for the same event
% 'cba9657d4b55f7ca.mat'- several event markers for the same event
% 'fc67513ed5c4b400.mat' - several event markers for the same event
% '344dd59ded90cb34' - problem with channels - not solved yet
% '8559e4d7314e45ec' - different events - not solved
% 'c577d3cdf78548ce' -
% 'e72d90a6ff4b5108'
%  'e2d0d90e5cf594ed'

%% Calculate the difference wave (manmade - natural)
difference_wave = {}
for d = 1:length(all_manmade)
    if d==36 % all NaN values - inspect GA code fort this team ('93af9133e6fa6cf5.mat','9985e8ae6679b0e2.mat','cba9657d4b55f7ca.mat',...
        % 'fc67513ed5c4b400.mat', '344dd59ded90cb34','8559e4d7314e45ec','c577d3cdf78548ce','e72d90a6ff4b5108')
        continue
    end
    difference_wave{d}.id = all_manmade{2,d};
    difference_wave{d}.data = all_manmade{1,d}.avg - all_natural{1,d}.avg;
    difference_wave{d}.label = all_manmade{1,d}.label;

    if max(all_manmade{1,d}.time)<10 % convert from sec to msec
        difference_wave{d}.time = all_manmade{1,d}.time*1000;
    else
        difference_wave{d}.time = all_manmade{1,d}.time;
    end

    % Inspect zeroes and NaN values
    if sum(isnan(mean(difference_wave{d}.data,2)))>30%20,23,44,57,62,81,108 had a reference CPZ or conditions are equal there but not in other electrodes
        %         mean(difference_wave{d}.data,2)
        %         all_manmade{2,d}
        %         error('insoect nan')
        difference_wave{d} = []
    elseif sum(mean(difference_wave{d}.data,2)) == 0
        difference_wave{d} = []
    end
    %
end

% correct time for some teams
figure, plot(difference_wave{12}.time,difference_wave{12}.data)

difference_wave{12}.time = difference_wave{12}.time - 1000 %
all_natural{1,12}.time = all_natural{1,12}.time - 1000 %
all_manmade{1,12}.time = all_manmade{1,12}.time - 1000 %

% Remove zeros and NaN
indx_empty2 = cellfun(@isempty, difference_wave)
difference_wave(indx_empty2) = []
save('difference_wave_orig_109_clean.mat','difference_wave')

%% Plot
fig=figure
%col = parula(length(manmade_ref)),
n_plot = 0

data = all_manmade(1,:) %nature_ref,  manmade_ref
%uisetcolor
col = [0 0.5 0.5 0.05] %[0.9020 0.1255 0.2667 0.05]%[0 0.5 0.5 0.05] % the last digit is alpha
what_to_plot = 'avg' % 'data' - avg reference, no baseline correction, alternative 'data_bsl'
plot_title = 'GA ERPs for Man-made and Natural stimuli'
all_y=[]
for p = 1:length(data)

    cpz_indx = strcmp(data{p}.label, 'CPz')
    if isempty(find(cpz_indx))
        continue
    end

    % if the format is not channels x times, but times x channels
    if size(data{p}.(what_to_plot),1) > size(data{p}.(what_to_plot),2)
        y = data{p}.(what_to_plot)(:,cpz_indx); % CPz
    else
        y = data{p}.(what_to_plot)(cpz_indx,:); % CPz
    end

    x = data{p}.time;
    if max(y)>10
        continue
    elseif min(y) <-10
        continue
    end
    all_y(p,:) = resample(y(x>=-100 & x <=500),151,length(y(x>=-100 & x <=500)));% data, fs I want to fit, fs I have
    plot(x,y, 'Color', col,'LineWidth', 1.2), hold on
    n_plot = n_plot+1
end


fontsize(fig, 15, "points"),  title([plot_title,', N=', num2str(n_plot)]),...
    xlabel('Time(ms)'), ylabel('uV'), xlim([-100, 500]),yline(0, '-', 'LineWidth', 1),
hold on
% xl.LabelVerticalAlignment = 'middle';

plot([-100:3.99:500], nanmean(all_y), 'Color', [0 0.5 0.5],'LineWidth', 1.5),%[0.9020 0.1255 0.2667] , [0 0.5 0.5]

%add a vertical line
plot([-10 10], '--', 'Color','black'), ylim([-10,7.5])

% legent
txt={'\color[rgb]{0 0.5 0.5} - man-made','\color[rgb]{0.9020 0.1255 0.2667} - natural'};
text(330,6,txt, 'FontSize',15)

saveas(fig,['C:\Users\ecesnait\Desktop\EEGManyPipelines\git\EEGManyPipes org\Main_Paper\Time-series\erplot\ERP based on condition\figures\...' ...
    'GA_ERP_manmade_natural_orig_ref_bsl.png'])

load('difference_wave_orig_ERPs_117.mat')

% mean difference wave

fig = figure
data = difference_wave
what_to_plot = 'data'
plot_title = 'Difference wave for man made - natural conditions'
col = [0.9294    0.6941    0.1255 0.2]
for i = 1:length(data)
    cpz_indx = strcmp(data{i}.label, 'CPz')
    if isempty(find(cpz_indx))
        continue
    end

    % if the format is not channels x times, but times x channels
    if size(data{i}.(what_to_plot),1) > size(data{i}.(what_to_plot),2)
        y = data{i}.(what_to_plot)(:,cpz_indx); % CPz
    else
        y = data{i}.(what_to_plot)(cpz_indx,:); % CPz
    end

    x = data{i}.time;
 
    if max(y)>3
        continue
    elseif min(y) <-10
        continue
    elseif max(y)<1e-04 % different scale
        continue
    end
    all_y(i,:) = resample(y(x>=-100 & x <=500),151,length(y(x>=-100 & x <=500)));% data, fs
    plot(x,y, 'Color', col,'LineWidth', 1.2), hold on
 
end
hold on
%plot([-100:3.99:500], nanmedian(all_y), 'Color', 'r','LineWidth', 1.5), % [0.9294    0.6941    0.1255]
plot([-2 3], '--', 'Color','black'),yline(0, '-', 'LineWidth', 1),
title(plot_title),...
    xlabel('Time(ms)'), ylabel('uV'), xlim([-100, 500]),fontsize(fig, 15, "points")
%yline(0, '-', 'LineWidth', 1),

txt={'\color[rgb]{0.9294 0.6941 0.1255} - difference wave'};
text(300,2.8,txt, 'FontSize',12, 'FontWeight','bold')
save('ERP_difference_wave_orig','all_y')
saveas(fig,'GA_ERP_difference_wave_orig.png')

