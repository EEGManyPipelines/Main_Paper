clear all, close all

% Load data %
 load('allgrpdat_1_90.mat')

datapath = 'N:\EMP\EEGManyPipelines\EMP data extracted\';
grps = dir([datapath, '*.mat'])

allgrpdat_joined(2,:) = {grps.name}
% 
% % Join all
% allgrpdat_joined = [allgrpdat1.allgrpdat(1:20), allgrpdat2.allgrpdat(21:23), allgrpdat3.allgrpdat(24:34), allgrpdat4.allgrpdat(35:53), allgrpdat5.allgrpdat(54:60), ...
%     allgrpdat6.allgrpdat(61:90)]

%save('allgrpdat_1_90.mat', 'allgrpdat_joined')

% remove empty entries due to continuous or corrupt data files
allgrpdat_full = allgrpdat_joined(:,~cellfun(@isempty, allgrpdat_joined(1,:)))

% correct time for some teams
bad = [19,31, 35, 45, 47, 48, 53,58,62] % indx of bad ERPs
wrong_time = bad([5,6,9])

for i= 1:length(wrong_time)
    allgrpdat_full{wrong_time(i)}.time=allgrpdat_full{wrong_time(i)}.time * 1000 % from seconds to ms
end

%% Plot

fig=figure
col = parula(length(allgrpdat_full)), n_plot = 0
for p = 1:length(allgrpdat_full)
    cpz_indx = strcmp(allgrpdat_full{p}.label, 'FCz')
    
    % if the format is not channels x times, but times x channels
    if size(allgrpdat_full{p}.avg,1) > size(allgrpdat_full{p}.avg,2) 
        y = allgrpdat_full{p}.avg(:,cpz_indx); % CPz
    else
      y = allgrpdat_full{p}.avg(cpz_indx,:); % CPz
    end
    if ismember(p, [19, 31, 35, 45, 53,58,95]) %max(y) > 10 % p=19, 31, 35, 45, 48, 95 - FCz not found
        continue
         x = allgrpdat_full{p}.time;
         figure, plot(x,y, 'LineWidth',1.2) ,fontsize(fig, 15, "points"), title(['Great Grand Average (FCz), p=',num2str(p)]),...
            xlabel('Time(ms)'), ylabel('uV')
    end
   x = allgrpdat_full{p}.time;
   plot(x,y, 'Color', col(p,:),'LineWidth', 1.2), hold on
   n_plot = n_plot+1
end
fontsize(fig, 15, "points"), xlim([-100, 500]), title(['Great Grand Average (FCz), N=', num2str(n_plot)]),...
    xlabel('Time(ms)'), ylabel('uV')

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
xbar = [100 200]
patch([xbar(1) xbar(1), xbar(2) xbar(2)],[min(ylim) max(ylim) max(ylim) min(ylim)], [0.8 0.8 0.8], 'FaceAlpha',.2,'EdgeColor',[0.8 0.8 0.8])

saveas(fig,'GGA_ERP_97.png')