% TO DO
% -  [done] way to check for excluded subjects -> keep as NaN

addpath('C:\Users\ncb623\EMP\Main_Paper\Time-series\erplot')
addpath('C:\Users\ncb623\fieldtrip')
cd('C:\Users\ncb623\EMP\data\')
datadir = 'C:\Users\ncb623\EMP\data\TimelockAVG';

%% Arrange data
files = find_files(datadir, '.mat', 'H3');
alldatmat = nan(length(files), 33, 64, 205);            % grp x subj x chan x time

grpcll = cell(0,0);

for gg = 1:length(files)
    fprintf('Loading %s (%i of %i)\n', files{gg}, gg, length(files))
    load(fullfile(datadir, files{gg}), "alldatavg")

    tmp = strsplit(files{gg}, {'_', '.'});
    grpcll(gg) = tmp(2);

    for ss = 1:length(alldatavg)
        fprintf('Subj %i...\n', ss)
        if ~isempty(alldatavg{1,ss})
            alldatmat(gg, ss, :, :) = alldatavg{1,ss}.avg;
        end
    end
    fprintf('Done %s\n', files{gg})
end
disp('ALL DONE')

%% Save
grptab = cell2table(grpcll');

save('alldatmat.mat', 'alldatmat', 'grptab'); disp('done')
writetable(grptab, 'grptab.csv')

%END