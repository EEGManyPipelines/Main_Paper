% Utility function for making single channel plots of data. The function 
% takes a FieldTrip structure and makes a plot based on the provided
% channel name. If there are more than one time series, it will plot each
% as a sperate line.
%
% Use: 
%   util_singleplot(avgdat, chan, param, group, groupColors, writeto)
% Where:
%   avgdat      : a FT data structure
%   chan        : the channel name to plot
%   param       : what prameter to plot [default is avg]
%   group       : grouping variable vector with same length as number of lines
%                   to plot [optional]
%   groupColors : custom color map for grouping variable [optional]
%   writeto     : a directory where output files will be written (leave empty to
%                   not write to disk [is default])

function util_singleplot(avgdat, chan, param, group, groupColors, writeto)

% Check input
% if any(strfind(avgdat.dimord, 'rpt'))
if nargin < 3 || isempty(param)
    param = 'avg';
end

if length(size(avgdat.(param))) > 1
    n_lines = size(avgdat.avg, 1);
else
    n_lines = 1;
end

if nargin < 4 || isempty(group)
    group = ones(n_lines, 1);
end

if nargin < 5 || isempty(groupColors)
    groupColors = hsv(length(unique(group)));
end

if nargin < 6 || isempty(writeto)
    writeto = [];
else
    if ~exist(writeto, 'dir')
        error('Output directory %s not found', writeto)
    end
end

% Select channel
cfg = [];
cfg.channel = chan;
slctdat = ft_selectdata(cfg, avgdat);

% Plot
figure(); hold on
for ii = 1:n_lines
    grpidx = group(ii);
    plot(avgdat.time, squeeze(slctdat.(param)(ii,:)), 'Color', groupColors(grpidx,:));
end
hold off

% Export
if writeto
    tmpfname = fullfile(writeto, ['topo_', num2str(tt)]);
    fprintf('Writing file %s...', tmpfname)
    print(fig, tmpfname, '-r800', '-dpng');
    disp('done')
end