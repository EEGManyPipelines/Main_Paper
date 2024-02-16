% Load data
load('C:\Users\ecesnait\Desktop\EEGManyPipelines\git\EEGManyPipes org\Main_Paper\Time-series\erplot\allgrpdat files\allgrpdat_1_90.mat')
allgrpdat_full = allgrpdat_joined(:,~cellfun(@isempty, allgrpdat_joined(1,:)))

% correct time for some teams
bad = [13,19,31, 35, 45, 47, 48, 53,58,62] % indx of bad ERPs
wrong_time = bad([1,6,7,10])

for i= 1:length(wrong_time)
    if i == 1 % 0 is start of the epoch and 1000 is stimulus presentation
        allgrpdat_full{wrong_time(i)}.time = allgrpdat_full{wrong_time(i)}.time - 1000 % 
    else
       allgrpdat_full{wrong_time(i)}.time = allgrpdat_full{wrong_time(i)}.time * 1000 % from seconds to ms
    end
end

%%
bstart = -100;
bstop  = 0;

avg_ref_data = [];

for i=1:length(allgrpdat_full)i

    if ismember(i, [19, 31, 35, 45, 53,58]) %max(y) > 10 % p=19, 31, 35, 45, 48
        continue
    end

    if size(allgrpdat_full{i}.avg,1) > size(allgrpdat_full{i}.avg,2)
        allgrpdat_full{i}.avg = allgrpdat_full{i}.avg';
    end

    new_ref = allgrpdat_full{i}.avg - mean(allgrpdat_full{i}.avg,1);

    xstart = dsearchn(allgrpdat_full{i}.time', bstart);
    xstop  = dsearchn(allgrpdat_full{i}.time', bstop);

    bsl = mean(new_ref(:,xstart:xstop),2); % the shift we saw before was because we averaged data with the old reference but applied it to avg ref
    bsl = repmat(bsl, [1, length(allgrpdat_full{i}.time)]);
    new_ref_bsl = new_ref - bsl;

    avg_ref_data{i}.data = new_ref;
    avg_ref_data{i}.data_bsl = new_ref_bsl;
    avg_ref_data{i}.label = allgrpdat_full{i}.label;
    avg_ref_data{i}.time = allgrpdat_full{i}.time;

    for ch = 1:length(avg_ref_data{i}.label) % baseline correction shifts mean of some suibjects. Inspect why!
        if abs(mean(avg_ref_data{i}.data_bsl(ch,xstart:xstop))) > 0.1
            fprintf('Break team %d, chan %d.\n', i, ch)
           
        end
    end
         clear new_ref xstart xstop bsl new_ref_bsl
end

%% Plot
fig=figure
col = parula(70), n_plot = 0
data = avg_ref_data
what_to_plot = 'data_bsl'
plot_title = 'avg ref bsl'

for p = 1:length(data)
    if ismember(p, [19, 31, 35, 45, 53,58]) %max(y) > 10 % p=19, 31, 35, 45, 48
        continue
    end
    cpz_indx = strcmp(data{p}.label, 'FCz')

    % if the format is not channels x times, but times x channels
    if size(data{p}.(what_to_plot),1) > size(data{p}.(what_to_plot),2)
        y = data{p}.(what_to_plot)(:,cpz_indx); % CPz
    else
        y = data{p}.(what_to_plot)(cpz_indx,:); % CPz
    end


    x = data{p}.time;
    plot(x,y, 'Color', col(p,:),'LineWidth', 1.2), hold on
    n_plot = n_plot+1
end
fontsize(fig, 15, "points"),  title([plot_title,', N=', num2str(n_plot)]),...
    xlabel('Time(ms)'), ylabel('uV'), xlim([-100, 500])
saveas(fig,'GGA_ERP_avg_ref_same_bsl.png')