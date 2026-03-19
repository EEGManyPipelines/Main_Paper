% Utility function for making topoplots of data. The function takes an
% FieldTrip structure and makes a number topoplots based on the provided
% time intervals
%
% Use: 
%   util_topoplot(avgdat, tbins, marker, param, writeto)
% Where:
%   avgdat  : a FT data structure
%   tbins   : a vector with the time bins (default: -0.2:0.1:0.4)
%   marker  : a channel name to highlight (e.g., if we want to mark a
%             certain channel where we plot the time-series). Leave empty
%             for no highlight [is default]
%   param   : what prameter to plot [default is avg]
%   writeto : a directory where output files will be written (leave empty to
%             not write to disk [is default])

function util_topoplot(avgdat, tbins, marker, param, writeto)

% avgdat = ft_timelockgrandaverage([], allgrpdat_few{:});

% Check FieldTrip in path
if ~exist('ft_defaults', 'file')
    error('FieldTrip not found. Add to path (needed for plotting functions)')
end

% Check input
if nargin < 2
    tbins = (-199:100:400) / 1000;
end

if nargin < 3 || isempty(marker)
    marker = [];
    highlight = 'off';
else
    highlight = 'on';
end

if nargin < 4 || isempty(param)
    param = 'avg';
end

if nargin < 5 || isempty(writeto)
    writeto = [];
else
    if ~exist(writeto, 'dir')
        error('Output directory %s not found', writeto)
    end
end

% Make topoplots
cfg = [];
cfg.parameter           = param;
cfg.layout              = 'elec1010.lay';
cfg.zlim                = quantile(avgdat.(param)(:), [.05, .95]);
cfg.comment             = 'no'; 
cfg.highlight           = highlight;
cfg.highlightchannel    = marker;
cfg.highlightcolor      = [0 0 0];
cfg.highlightsymbol     = '*';
cfg.colorbar            = 'yes';
cfg.colormap            = '-RdBu';

if isempty(get(0,'children'))
    nfig = 0; 
else
    nfig = get(gcf, 'Number');
end

for tt = 1:(length(tbins))
%     subplot(1, length(tbins)-1, tt)

    fig = figure(tt+nfig);
    set(fig, 'Position',[200 200 500 500]);

    cfg.figure = fig;
    cfg.xlim = [tbins(tt), tbins(tt)];
    ft_topoplotER(cfg, avgdat)
    % title([num2str(tbins(tt)*1000), ' ms to ', num2str(tbins(tt+1)), ' ms'] )
    title([num2str(tbins(tt)*1000), ' ms'] )

    if writeto
        tmpfname = fullfile(writeto, ['topo_', num2str(tt)]);
        fprintf('Writing file %s...', tmpfname)
        print(fig, tmpfname, '-r800', '-dpng');
        disp('done')
    end
end

%END