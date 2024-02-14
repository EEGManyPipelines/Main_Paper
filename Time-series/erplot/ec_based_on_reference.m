% Load analysis questionnaire
AQ = readtable('C:\Users\ecesnait\Desktop\EEGManyPipelines\Data\Analysis questionnaire\Analysis questionnaire final sample 168 corrected.xlsx');
ref = AQ.ans_reref_method
ID = AQ.teamID

%fill empty fields with the 'original' reference answer
ref(cellfun(@isempty,ref)) = {'orig'}

num_cats = countcats(categorical(ref))
[categories(categorical(ref)) num_cats] % most common categories avg, mastoid and orig sums up to 148 of cases

ID_avg = ID(ismember(ref, 'avg'))
ID_mastoid = ID(ismember(ref, 'mastoid'))
ID_orig = ID(ismember(ref, 'orig'))

% match it to our GGA ERP plots
load('C:\Users\ecesnait\Desktop\EEGManyPipelines\git\EEGManyPipes org\Main_Paper\Time-series\erplot\allgrpdat files\allgrpdat_1_90.mat')

IDs_erp = extractBefore(allgrpdat_full(2,:), '.mat')
GGA_avg_ref = allgrpdat_full(:,ismember(IDs_erp,ID_avg))
GGA_mastoid_ref = allgrpdat_full(:,ismember(IDs_erp,ID_mastoid))
GGA_orig_ref = allgrpdat_full(:,ismember(IDs_erp,ID_orig))

% Plot

fig=figure
col = parula(30)
data = GGA_orig_ref(1,:), n_plot = 0
for p = 1:length(data)
    if p == 8 % for original ref
        continue
    end
    cpz_indx = strcmp(data{p}.label, 'FCz')
    % if the format is not channels x times, but times x channels
    if size(data{p}.avg,1) > size(data{p}.avg,2)
        y = data{p}.avg(:,cpz_indx); % CPz
    else
        y = data{p}.avg(cpz_indx,:); % CPz
    end
    if max(y) > 10%ismember(p, [19, 31, 35, 45, 53,58]) %max(y) > 10 % p=19, 31, 35, 45, 48
        continue
    end

    x = data{p}.time;
    if max(x) <2
        x = data{p}.time*1000; % from seconds to ms
    end

    plot(x,y, 'Color', col(p,:),'LineWidth', 1.2), hold on
    n_plot = n_plot+1;
end
fontsize(fig, 15, "points"), xlim([-100, 500]), title(['GGA original ref (FCz), N=', num2str(n_plot)]),...
    xlabel('Time(ms)'), ylabel('uV')

% add a dashed line for the stimulus presentation time point
xl = xline(0, '--', 'stimulus')
xl.LabelVerticalAlignment = 'middle';

% add grey area where did we expect N100 component to be
xbar = [100 200]
patch([xbar(1) xbar(1), xbar(2) xbar(2)],[min(ylim) max(ylim) max(ylim) min(ylim)], [0.8 0.8 0.8], 'FaceAlpha',.2,'EdgeColor',[0.8 0.8 0.8])

saveas(fig,'GGA_ERP_orig_ref.png')