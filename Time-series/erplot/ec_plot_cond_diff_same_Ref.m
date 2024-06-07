close all, clear all

% Load data
load('GA_ERPs_all_teams_manmade.mat')
load('GA_ERPs_all_teams_natural.mat')

% delete an empty indx
indx_empty = cellfun(@isempty, all_manmade(1,:))
all_manmade(:,indx_empty) = [];
all_natural(:,indx_empty) = [];

%% apply the same baseline and average reference for all for plotting purposes
bstart = -100;
bstop  = 0;

manmade_ref = [];
nature_ref = [];

data = {'all_manmade', 'all_natural'}
for con = 1:2
    con_data = eval(data{con});
    con_data = con_data(1,:);
    for i = 1:length(con_data);
        if size(con_data{i}.avg,1) > size(con_data{i}.avg,2) % if data is not ch x time but time x ch -> transpose
            con_data{i}.avg = con_data{i}.avg';
        end
        new_ref = con_data{i}.avg - mean(con_data{i}.avg,1); % re-reference to average reference

        xstart = dsearchn(con_data{i}.time', bstart);
        xstop  = dsearchn(con_data{i}.time', bstop);

        bsl = mean(new_ref(:,xstart:xstop),2); % the shift we saw before was because we averaged data with the old reference but applied it to avg ref
        bsl = repmat(bsl, [1, length(con_data{i}.time)]);
        new_ref_bsl = new_ref - bsl;

%          figure,plot(con_data{i}.time,new_ref_bsl)

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
%% Calculate the difference wave (manmade - natural)

difference_wave = {}
for d = 1:length(all_manmade)

    difference_wave{d}.id = all_manmade{2,d};
    difference_wave{d}.data = all_manmade{1,d}.avg - all_natural{1,d}.avg;
   % difference_wave{d}.data_bsl = manmade_ref{1,d}.data_bsl - nature_ref{1,d}.data_bsl;
    difference_wave{d}.label = all_manmade{1,d}.label;

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


    if max(all_manmade{1,d}.time)<10 % convert from sec to msec
        difference_wave{d}.time = all_manmade{1,d}.time*1000;
%         manmade_ref{d}.time = all_manmade{1,d}.time*1000;
%         nature_ref{d}.time = all_natural{1,d}.time*1000;
    else
        difference_wave{d}.time = all_manmade{1,d}.time;
    end
end

% correct time for some teams
figure, plot(difference_wave{12}.time,difference_wave{12}.data)
wrong_time = 12
difference_wave{12}.time = difference_wave{12}.time - 1000 %
manmade_ref{12}.time = manmade_ref{12}.time - 1000 %
nature_ref{12}.time = nature_ref{12}.time - 1000 %

all_natural{1,12}.time = all_natural{1,12}.time - 1000 %
all_manmade{1,12}.time = all_manmade{1,12}.time - 1000 %

% convert from sec to msec
% for i = 1:length(all_manmade)
%   if max(all_manmade{1,i}.time)<10 % convert from sec to msec
%         all_manmade{1,i}.time = all_manmade{1,i}.time*1000;
%         all_natural{1,i}.time = all_natural{1,i}.time*1000;
%   end
% end

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
for i = 1:length(difference_wave)
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
    end
    all_y(i,:) = resample(y(x>=-100 & x <=500),151,length(y(x>=-100 & x <=500)));% data, fs
     plot(x,y, 'Color', col,'LineWidth', 1.2), hold on
end
hold on
%plot([-100:3.99:500], nanmedian(all_y), 'Color', 'r','LineWidth', 1.5), % [0.9294    0.6941    0.1255]
plot([-1 2], '--', 'Color','black'),
  title(plot_title),...
    xlabel('Time(ms)'), ylabel('uV'), xlim([-100, 500]),fontsize(fig, 15, "points")
  %yline(0, '-', 'LineWidth', 1),

txt={'\color[rgb]{0.9294 0.6941 0.1255} - difference wave'};
text(300,1.8,txt, 'FontSize',12, 'FontWeight','bold')
save('ERP_difference_wave_avg_ref_bsl','all_y')
saveas(fig,'GA_ERP_difference_wave.png')

find(mean(all_y,2)==0)