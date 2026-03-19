clear all, close all

% Load data %
load('allgrpdat_1_90.mat')
all_erp = ones(1,90)
indx_empty = find(cellfun(@isempty, allgrpdat_joined))
all_erp(indx_empty) = 0
bad_erp = [19, 31, 35, 45, 47, 48, 53, 58, 62];

%find empty
indx_empty = find(cellfun(@isempty, allgrpdat_joined))
true_bad = []
% for i = 1:length(bad_erp)
%     % find how many empty entreis before the bad erp indx
%     sum_empty = sum(indx_empty < bad_erp(i))
%     if sum(ismember(indx_empty,bad_erp(i): bad_erp(i) + sum_empty))
%         new_loc= bad_erp(i) + sum_empty + sum(ismember(indx_empty,bad_erp(i): bad_erp(i) + sum_empty))
%         true_bad = [true_bad, new_loc];
%     else
%         true_bad = [true_bad, bad_erp(i) + sum_empty];  
%     end
%     
% end

true_bad = [21, 38, 42, 55, 57, 60, 74, 77,78]

% Plot them to inspect
plot_nr = 74
cpz_indx = strcmp(allgrpdat_joined{plot_nr}.label, 'FCz')
y = allgrpdat_joined{plot_nr}.avg(cpz_indx,:); % CPz
x = allgrpdat_joined{plot_nr}.time;
fig = figure, plot(x,y, 'LineWidth',1.2) ,fontsize(fig, 15, "points"), title(['Great Grand Average (FCz), p=',num2str(plot_nr)]),...
    xlabel('Time(ms)'), ylabel('uV')


% 1,2,3,4 - scale; 
% 5,6,9 - wrong time scale
% 7,8 wrong averaging


