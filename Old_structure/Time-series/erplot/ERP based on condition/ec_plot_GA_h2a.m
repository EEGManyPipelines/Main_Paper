all_cond_erp = load('C:\Users\ecesnait\Desktop\EEGManyPipelines\Data\Time-series\allgravgdat.mat')
teams = fieldnames(all_cond_erp)

fig = figure

%uisetcolor()
new_color = [0.5373    0.7059    0.7804 0.2]
old_color = [0.8588    0.2667    0.4235 0.2]
median_erp_new = nan(60,205)
median_erp_old = nan(60,205)

for i=1:length(teams)
    ga_erp_new = all_cond_erp.(teams{i}).allgravg_new;
    ga_erp_old = all_cond_erp.(teams{i}).allgravg_old;

    
    if nanmean(ga_erp_new.avg(18,:)) < 10
        if nanmax(abs(ga_erp_new.avg(18,:))) <1*10^-5
            ga_erp_new.avg = ga_erp_new.avg .* 10^6
            ga_erp_old.avg = ga_erp_old.avg .* 10^6
        
        end
        plot(ga_erp_new.time,ga_erp_new.avg(18,:), 'Color',new_color,'LineWidth', 1.2), hold on
        plot(ga_erp_old.time,ga_erp_old.avg(18,:), 'Color',old_color,'LineWidth', 1.2)
        median_erp_new(i,:) = ga_erp_new.avg(18,:);
        median_erp_old(i,:) = ga_erp_old.avg(18,:);
    end

end
fontsize(fig, 15, "points"), xlim([-100, 500]),
    xlabel('Time(ms)'), ylabel('uV')

    xbar = [300 500]
patch([xbar(1) xbar(1), xbar(2) xbar(2)],[min(ylim) max(ylim) max(ylim) min(ylim)], [0.8 0.8 0.8], 'FaceAlpha',.2,'EdgeColor',[0.8 0.8 0.8])

% add a dashed line for the stimulus presentation time point
xl = xline(0, '--', 'stimulus')
xl.LabelVerticalAlignment = 'middle';
plot(ga_erp_new.time,nanmedian(median_erp_new),'Color',[0.6588    0.1176    0.3725],'LineWidth', 1.5), hold on
plot(ga_erp_new.time,nanmedian(median_erp_old),'Color',[0.1961    0.5216    0.6588],'LineWidth', 1.5)

saveas(fig,'GA_ERPs_H2a_60teams.png')

diff_wave_col = [0.9098    0.6941    0.2627 0.2]
fig = figure
median_diff = nan(60,205)
for i=1:length(teams)
    ga_erp_new = all_cond_erp.(teams{i}).allgravg_new;
    ga_erp_old = all_cond_erp.(teams{i}).allgravg_old;

    
    if nanmean(ga_erp_new.avg(18,:)) < 10
        if nanmax(abs(ga_erp_new.avg(18,:))) <1*10^-5
            ga_erp_new.avg = ga_erp_new.avg .* 10^6
            ga_erp_old.avg = ga_erp_old.avg .* 10^6
        
        end
        diff_wave = ga_erp_new.avg(18,:) - ga_erp_old.avg(18,:);
        plot(ga_erp_new.time,diff_wave, 'Color',diff_wave_col,'LineWidth', 1.2),hold on
        median_diff(i,:) = diff_wave;
    end

end
fontsize(fig, 15, "points"), xlim([-100, 500]),
    xlabel('Time(ms)'), ylabel('uV')

    xbar = [300 500]
patch([xbar(1) xbar(1), xbar(2) xbar(2)],[min(ylim) max(ylim) max(ylim) min(ylim)], [0.8 0.8 0.8], 'FaceAlpha',.2,'EdgeColor',[0.8 0.8 0.8])

% add a dashed line for the stimulus presentation time point
xl = xline(0, '--', 'stimulus')
xl.LabelVerticalAlignment = 'middle';

plot(ga_erp_new.time, nanmedian(median_diff), "Color",[0.9294    0.6941    0.1255],'LineWidth', 1.5)
saveas(fig,'diff_wave_H2a_60teams.png')
