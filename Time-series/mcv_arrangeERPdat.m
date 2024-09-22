
addpath('C:\Users\ncb623\EMP\Main_Paper\Time-series\erplot')
addpath('C:\Users\ncb623\fieldtrip')
cd('C:\Users\ncb623\EMP\data\Standardized')

%% arrangedata
files = find_files('C:\Users\ncb623\EMP\data\Standardized', '.mat');
alldatmat = nan(length(files), 33, 64, 205); % grp x subj x chan x time

for gg = 1:length(files)
    fprintf('Loading %s\n', files{gg})
    load(files{gg}, "alldatstand")

    for ss = 1:length(alldatstand)
        fprintf('Subj %i...\n', ss)
        if ~isempty(alldatstand{1,ss})
            subjtmp = ft_timelockanalysis([], alldatstand{1,ss});
            alldatmat(gg, ss, :, :) = subjtmp.avg;
        end
    end
    fprintf('Done %s\n', files{gg})
end
disp('ALL DONE')

%% Save
save('alldatmat.mat', 'alldatmat'); disp('done')