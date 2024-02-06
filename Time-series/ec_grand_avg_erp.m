clear all, close all
directory = 'N:\EMP\EEGManyPipelines\EMP data extracted\'
all_files = dir([directory, '*.mat'])

data_out = struct()
for t = 11:length(all_files)
    disp(['Processing participant... ',num2str(t)])

    all_data = load([directory, all_files(t).name]);
    all_data = all_data.(string(fieldnames(all_data)));

    all_fl_names = fieldnames(all_data);
    [~,pl] = ismember({'EEGts', 'time', 'chan', 'epoch'},all_fl_names);

    % fields can have very different names. Find the relevant structures
    % based on their size
    if numel(size(all_data(1).(all_fl_names{pl(1)}))) < 3 %check how many dimensions EEGts has. if it's conitnuous data:
        continue
        % epoch data. Not sure how to epoch when we have no EEG structure
        %epoched = pop_epoch(EEG_BS2, {}, [-0.200, 0.800], 'epochinfo', 'yes');

    else % if EEG data is epoched
        cpz_erp = []
        for p = 1:size(all_data,2) %average EEG
            ts = all_data(p).(all_fl_names{pl(1)}); % timeseries
            if ~isempty(ts)
                avg_ch = mean(ts,3);% across channels, across epochs
                find_cpz = find(ismember(all_data(p).(all_fl_names{pl(3)}), 'CPz'));
                cpz_erp(size(cpz_erp,1)+1,:) = avg_ch(find_cpz,:);
            end
        end

        %special cases due to memory limits
        if strcmp(all_files(t).name, '19068f1fe266c5e1_1.mat')
            clear all_data ts avg_ch
            all_data2 = load([directory, '19068f1fe266c5e1_2.mat']);
            all_data2 = all_data2.(string(fieldnames(all_data2)))

            %continue from subject 16
            for p = 16:size(all_data2,2) %average EEG
                ts = all_data2(p).(all_fl_names{pl(1)}); % timeseries
                if ~isempty(ts)
                    avg_ch = mean(ts,3);% across channels, across epochs
                    find_cpz = find(ismember(all_data2(p).(all_fl_names{pl(3)}), 'CPz'));
                    cpz_erp(size(cpz_erp,1)+1,:) = avg_ch(find_cpz,:);
                end
            end
            all_data = all_data2;
            clear all_data2
            
        end

    end
    %average across participants
    data_out(length(data_out)+1).avg_cpz_erp = mean(cpz_erp,1);
    data_out(length(data_out)+1).time = all_data(p).(all_fl_names{pl(2)}); %should be the same for all participants per team
    clear all_data ts avg_ch cpz_erp find_cpz pl all_fl_names
end

% cut all time windows to the same
 for tw = 1:length(data_out)
     first(tw) = data_out(tw).time(1)
     last(tw)= data_out(tw).time(end)
 end
tw_start = max(first) % 0
tw_end = min(last) % 746

% deal with different fs

 fig = figure
 for f = 1:length(data_out)
     indx_x = data_out(f).time >= 0 & data_out(f).time <=746 
     x = data_out(f).time(indx_x)
     y =  data_out(f).avg_cpz_erp(indx_x)
     figure, plot(x, y)
 end