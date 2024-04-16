clear all, close all

% Load data %
 addpath('C:\Users\ecesnait\Desktop\EEGManyPipelines\git\EEGManyPipes org\Main_Paper\Time-series\erplot\')
 addpath('C:\Users\ecesnait\Desktop\EEGManyPipelines\Matlab Scripts\toolboxes\fieldtrip-master\')


load('allgrpdat_condition_manmade_EEGLAB.mat')
load('allgrpdat_condition_natural_EEGLAB.mat')

% remove empty entries due to continuous or corrupt data files
empt_nat = cellfun(@isempty, allgrpdat_natural(1,:))
natural = allgrpdat_natural(:,~empt_nat)

empt_man = cellfun(@isempty, allgrpdat_manmade(1,:))
manmade = allgrpdat_manmade(:,~empt_man)

% Calculate the difference wave

difference_wave = struct()
for d = 1:length(manmade)

    difference_wave(d).id = manmade{2,d};
    difference_wave(d).avg = manmade{1,d}.avg - natural{1,d}.avg;
    difference_wave(d).label = manmade{1,d}.label;
    if max(manmade{1,d}.time)<10 % convert from sec to msec
        difference_wave(d).time = manmade{1,d}.time*1000;
    else
        difference_wave(d).time = manmade{1,d}.time;
    end
end

% correct time for some teams
wrong_time = 12

difference_wave(12).time = difference_wave(12).time - 1000 % 


%% Plot

fig=figure
col = parula(length(difference_wave)), n_plot = 0
for p = 1:length(difference_wave)
    cpz_indx = strcmp(difference_wave(p).label, 'CPz');
    
    % if the format is not channels x times, but times x channels
    if size(difference_wave(p).avg,1) > size(difference_wave(p).avg,2) 
        y = difference_wave(p).avg(:,cpz_indx); % CPz
    else
      y = difference_wave(p).avg(cpz_indx,:); % CPz
    end
    % exclude bad cases
 
%     if ismember(p, []) %
%         continue
%          x = difference_wave{1,p}.time;
%          figure, plot(x,y, 'LineWidth',1.2) ,fontsize(fig, 15, "points"), title(['Great Grand Average (FCz), p=',num2str(p)]),...
%             xlabel('Time(ms)'), ylabel('uV')
%     end
   x = difference_wave(p).time;
   if max(y)>3
       continue
   elseif max(y) <0.1*10^-5
     continue
   end
   plot(x,y, 'Color', col(p,:),'LineWidth', 1.2), hold on
   n_plot = n_plot+1
end
fontsize(fig, 15, "points"), xlim([-100, 500]), title(['ManMade - Natural (CPz), N=', num2str(n_plot)]),...
    xlabel('Time(ms)'), ylabel('uV'), yline(0, '-', 'no difference', 'LineWidth', 1.5, 'Labe')

% add a dashed line for the stimulus presentation time point
xl = xline(0, '--', 'stimulus')
xl.LabelVerticalAlignment = 'middle';

% Find the most commonly used window for N100 component and mark it (this is independently if it was found significant or not. It's where teams looked)
AQ = readtable('C:\Users\ecesnait\Desktop\EEGManyPipelines\Data\Analysis questionnaire\Analysis questionnaire final sample 168 corrected.xlsx');
w_start = AQ.ans_temp_roi_tw_start_h1
w_end = AQ.ans_temp_roi_tw_end_h1

%hist(str2double(w_start),100) % 100
%hist(str2double(w_end),100) % 200

% add grey area where did we expect N100 component to be
xbar = [100 150]
patch([xbar(1) xbar(1), xbar(2) xbar(2)],[min(ylim) max(ylim) max(ylim) min(ylim)], [0.8 0.8 0.8], 'FaceAlpha',.2,'EdgeColor',[0.8 0.8 0.8])

saveas(fig,'cond_diff_57.png')