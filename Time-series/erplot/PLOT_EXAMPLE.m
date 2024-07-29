

%% General use example
load('GA_ERP_example.mat')

avgdat = ft_timelockgrandaverage([], allgrpdat_few{:});

tbins = -200:100:400;
outpath = '/home/mikkel/EMP/Main_Paper/data/plots';

util_topoplot(avgdat, tbins, 'Pz', []);
