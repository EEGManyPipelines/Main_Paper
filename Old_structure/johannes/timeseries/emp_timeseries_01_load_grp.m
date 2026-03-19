% emp_timeseries_01_load_grp.m

% ----------------------------------------------------------------------- %
%% Set up directories:

dirs            = [];
dirs.root       = 'C:/Users/johan/OneDrive/Documents/AI_EEGManyPipelines/';
dirs.scripts    = fullfile(dirs.root, 'code/Main_Paper/johannes/timeseries');
dirs.data       = fullfile(dirs.root, 'data/processedData/timeseries');
dirs.n100       = fullfile(dirs.data, 'N100 amplitudes');
dirs.EEG3D      = fullfile(dirs.data, 'EEG3D');

% ----------------------------------------------------------------------- %
%% Add fieldtrip toolbox:

dirs.fieldtrip = 'C:/Users/johan/OneDrive/Documents/github-repositories/fieldtrip';
if ~contains(lower(path), lower(fullfile(dirs.fieldtrip)))
    fprintf('Add Fieldtrip\n');
    addpath(dirs.fieldtrip);
end

cd(dirs.fieldtrip)

ft_defaults;
global ft_default;
ft_default.showlogo = 'no';

% ----------------------------------------------------------------------- %
%% Load in data:

cd(dirs.data)

% a) All group data:
% Specify file name:
% fileName        = 'allgrpdat_1_90.mat';
fileName        = 'allgrpdat_1_147.mat'; % for 147 teams:
% fileName        = 'allgrpdat_1_147.mat'; % for 147 teams:

% Combine file name and path:
fullFileName    = fullfile(dirs.data, fileName);

% b) N100 amplitudes:
% Specify file name:
% fileName        = 'chan_locs_CORENATS.mat';
% fileName        = 'n1_amplitude_120.mat';

% fullFileName    = fullfile(dirs.n100, fileName);

% c) Load data:
fprintf('Load %s ...\n', fullFileName);
data = load(fullFileName);
fprintf('... finished :-)\n');

% d) Plot:
% 2 x 90 data set
imagesc(data.allgrpdat_joined{1, 2}.avg) % 2x90
% imagesc(data.allgrpdat_joined{1, 2}.var) % 2x90
data.allgrpdat_joined{1, 2}.time

% From EEGLab to Fieldtrip:
% https://eeglab.org/others/EEGLAB_and_Fieldtrip.html
hdr     = ft_read_header(fullFileName);
data    = ft_read_data(fullFileName, 'header', hdr);
events  = ft_read_event(fullFileName, 'header', hdr);


% END