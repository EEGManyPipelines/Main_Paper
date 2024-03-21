clear all, close all

% Load analysis questionnaire
AQ = readtable('C:\Users\ecesnait\Desktop\EEGManyPipelines\Data\Analysis questionnaire\Analysis questionnaire final sample 168 corrected.xlsx');
ref = AQ.ans_reref_method
ID = AQ.teamID

%fill empty fields with the 'original' reference answer
ref(cellfun(@isempty,ref)) = {'orig'}

num_cats = countcats(categorical(ref))
%[categories(categorical(ref)) num_cats] % most common categories avg, mastoid and orig sums up to 148 of cases

ID_avg = ID(ismember(ref, 'avg'))
ID_mastoid = ID(ismember(ref, 'mastoid'))
ID_orig = ID(ismember(ref, 'orig'))

% match it to our GGA ERP plots
load('C:\Users\ecesnait\Desktop\EEGManyPipelines\git\EEGManyPipes org\Main_Paper\Time-series\erplot\allgrpdat files\allgrpdat_1_147_Andrea_memory.mat')

% remove empty entries due to continuous or corrupt data files
allgrpdat_full = allgrpdat_joined(:,~cellfun(@isempty, allgrpdat_joined(1,:)))
empty_row = cellfun(@isempty,allgrpdat_joined(1,:))

IDs_stand = [extractBefore(allgrpdat_joined(2,1:90), '.'), allgrpdat_joined(2,91:124), extractBefore(allgrpdat_joined(2,125:end), '_')]
IDs_stand = IDs_stand(~empty_row) % non-empty IDs
IDs_stand([19, 31, 35, 45, 53,59,110,119]) = []% remove for unexplained differences in scale
IDs_erp = IDs_stand

allgrpdat_full(:,[19, 31, 35, 45, 53,59,110,119]) = []

GGA_avg_ref = allgrpdat_full(:,ismember(IDs_erp,ID_avg))
GGA_mastoid_ref = allgrpdat_full(:,ismember(IDs_erp,ID_mastoid))
GGA_orig_ref = allgrpdat_full(:,ismember(IDs_erp,ID_orig))

indx_avg = ismember(IDs_erp,ID_avg)
id_avg = IDs_erp(indx_avg)'
AQ.ans_reref_method(ismember(AQ.teamID,id_avg))

% Plot
% GGA_orig_ref(:,8) = []

fig=figure
data = GGA_orig_ref(1,:), n_plot = 0
col = parula(length(data))
plot_title = 'Original reference (CPz)'
%data{12}.time=data{12}.time*1000
for p = 1:length(data)

    cpz_indx = strcmp(data{p}.label, 'CPz')
    if isempty(find(cpz_indx))
        continue
    end
    % if the format is not channels x times, but times x channels
    if size(data{p}.avg,1) > size(data{p}.avg,2)
        y = data{p}.avg(:,cpz_indx); % CPz
    %figure, plot(data{p}.time,data{p}.avg)
    elseif numel(size(data{p}.avg)) > 2
        y = squeeze(data{p}.avg); % CPz
        y=y(cpz_indx,:);
    else
        y = data{p}.avg(cpz_indx,:); % CPz
    end

    if max(y)>20 || min(y)<-20
        continue % control for large scales
%     elseif max(y)<0.1
%         continue
        % get ID
%         ID=GGA_avg_ref(2,p)
%         y=y*10^6 
    end

    x = data{p}.time;
%     if p==4%only for the average reference
%         x = data{p}.time - 1000
%     end
    if max(x) < 100
        x = data{p}.time*1000; % from seconds to ms
       % figure, plot(x,y, 'Color', col(p,:),'LineWidth', 1.2)
    end

    %plot(x,y, 'Color', col(p,:),'LineWidth', 1.2), hold on
    figure, plot(x,y, 'Color', col(p,:),'LineWidth', 1.2), xlim([-100, 500]), title([plot_title, num2str(p)])
     fig = gcf
        exportgraphics(fig, 'ERPs_based_on_reference.pdf','Append',true)
        close(fig)
end
fontsize(fig, 15, "points"), xlim([-100, 500]), title([plot_title,', N=', num2str(n_plot)]),...
    xlabel('Time(ms)'), ylabel('uV')

% add a dashed line for the stimulus presentation time point
xl = xline(0, '--', 'stimulus')
xl.LabelVerticalAlignment = 'middle';

% add grey area where did we expect N100 component to be
xbar = [100 200]
patch([xbar(1) xbar(1), xbar(2) xbar(2)],[min(ylim) max(ylim) max(ylim) min(ylim)], [0.8 0.8 0.8], 'FaceAlpha',.2,'EdgeColor',[0.8 0.8 0.8])

saveas(fig,[plot_title,num2str(n_plot),'.png'])