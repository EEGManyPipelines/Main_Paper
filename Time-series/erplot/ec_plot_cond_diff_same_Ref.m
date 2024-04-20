close all, clear all

% Load data
load('allgrpdat_condition_manmade_EEGLAB.mat')
load('allgrpdat_condition_natural_EEGLAB.mat')

empt_nat = cellfun(@isempty, allgrpdat_natural(1,:))
natural = allgrpdat_natural(:,~empt_nat)

empt_man = cellfun(@isempty, allgrpdat_manmade(1,:))
manmade = allgrpdat_manmade(:,~empt_man)

%%
bstart = -100;
bstop  = 0;

manmade_ref = [];
nature_ref = [];

data = {'manmade', 'natural'}
for con = 1:2
    con_data = eval(data{con});
    con_data = con_data(1,:);
    for i = 1:length(con_data);
        if size(con_data{i}.avg,1) > size(con_data{i}.avg,2)
            con_data{i}.avg = con_data{i}.avg';
        end
        new_ref = con_data{i}.avg - mean(con_data{i}.avg,1);

        xstart = dsearchn(con_data{i}.time', bstart);
        xstop  = dsearchn(con_data{i}.time', bstop);

        bsl = mean(new_ref(:,xstart:xstop),2); % the shift we saw before was because we averaged data with the old reference but applied it to avg ref
        bsl = repmat(bsl, [1, length(con_data{i}.time)]);
        new_ref_bsl = new_ref - bsl;

%         figure,plot(con_data{i}.time,new_ref_bsl)

        avg_ref_data.data = new_ref;
        avg_ref_data.data_bsl = new_ref_bsl;
        avg_ref_data.label = con_data{i}.label;
        avg_ref_data.time = con_data{i}.time;

        if con ==1
            manmade_ref{i} = avg_ref_data;
        else
            nature_ref{i} = avg_ref_data;
        end

        clear new_ref xstart xstop bsl new_ref_bsl avg_ref_data
    end
end
% figure, plot(manmade{1,2}.time,manmade{1,2}.avg)
%% Calculate the difference wave

difference_wave = {}
for d = 1:length(manmade)

    difference_wave{d}.id = manmade{2,d};
    difference_wave{d}.data = manmade_ref{1,d}.data - nature_ref{1,d}.data;
    difference_wave{d}.data_bsl = manmade_ref{1,d}.data_bsl - nature_ref{1,d}.data_bsl;
    difference_wave{d}.label = manmade{1,d}.label;

%     test =manmade_ref{1,d}.data - nature_ref{1,d}.data;
%     figure, plot(manmade_ref{1,d}.time,manmade_ref{1,d}.data(32,:)), hold on,
%     plot(nature_ref{1,d}.time,nature_ref{1,d}.data(32,:)), hold on,
%     plot(nature_ref{1,d}.time,test(32,:)),legend({'manmade', 'natural','difference'}),yline(0, '-', 'no difference', 'LineWidth', 1.5),
%     xlim([-100, 500]), title('without baseline cor')
% 
%  test =manmade_ref{1,d}.data_bsl - nature_ref{1,d}.data_bsl;
%     figure, plot(manmade_ref{1,d}.time,manmade_ref{1,d}.data_bsl(32,:)), hold on,
%     plot(nature_ref{1,d}.time,nature_ref{1,d}.data_bsl(32,:)), hold on,
%     plot(nature_ref{1,d}.time,test(32,:)),legend({'manmade', 'natural','difference'}),yline(0, '-', 'no difference', 'LineWidth', 1.5),
%     xlim([-100, 500]), title('with baseline cor')    


    if max(manmade{1,d}.time)<10 % convert from sec to msec
        difference_wave{d}.time = manmade{1,d}.time*1000;
        manmade_ref{d}.time = manmade{1,d}.time*1000;
        nature_ref{d}.time = natural{1,d}.time*1000;
    else
        difference_wave{d}.time = manmade{1,d}.time;
    end
end

% correct time for some teams
wrong_time = 12

difference_wave{12}.time = difference_wave{12}.time - 1000 %

%% Plot
fig=figure
%col = parula(length(manmade_ref)), 
n_plot = 0

data = manmade_ref
%uisetcolor
col = [0 0.5 0.5 0.05]%[0.9020 0.1255 0.2667 0.05]%[0 0.5 0.5 0.05] % the last digit is alpha
what_to_plot = 'data_bsl'
plot_title = 'GGA ERPs for Man-made and Natural stimuli'
all_y=[]
for p = 1:length(data)

    cpz_indx = strcmp(data{p}.label, 'CPz')

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
plot([-4 4], '--', 'Color','black')
% xl.LabelVerticalAlignment = 'middle';

plot([-100:3.99:500], nanmean(all_y), 'Color', [0 0.5 0.5],'LineWidth', 1.5) %[0.9020 0.1255 0.2667]
title(plot_title)

% mean difference wave
data = difference_wave
for i = 1:length(difference_wave)
     cpz_indx = strcmp(data{i}.label, 'CPz')

    % if the format is not channels x times, but times x channels
    if size(data{i}.(what_to_plot),1) > size(data{i}.(what_to_plot),2)
        y = data{i}.(what_to_plot)(:,cpz_indx); % CPz
    else
        y = data{i}.(what_to_plot)(cpz_indx,:); % CPz
    end

    x = data{i}.time;
    if max(y)>10
        continue
    elseif min(y) <-10
        continue
    end
    all_y(i,:) = resample(y(x>=-100 & x <=500),151,length(y(x>=-100 & x <=500)));% data, fs
end
plot([-100:3.99:500], nanmean(all_y),'--', 'Color', [0.9294    0.6941    0.1255],'LineWidth', 1.5)

txt={'\color[rgb]{0 0.5 0.5} - man-made','\color[rgb]{0.9020 0.1255 0.2667} - natural', '\color[rgb]{0.9294 0.6941 0.1255} -- difference'};
text(-90,5,txt, 'FontSize',12, 'FontWeight','bold')

saveas(fig,'GA_ERP_cond_diff_EEGLAB.png')