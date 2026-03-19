 addpath('C:\Users\ecesnait\Desktop\EEGManyPipelines\git\EEGManyPipes org\Main_Paper\Time-series\erplot\ERP based on condition\allgrpdat files\')
    addpath('C:\Users\ecesnait\Desktop\EEGManyPipelines\Matlab Scripts\toolboxes\fieldtrip-master\')


load('allgrpdat_condition_manmade_EEGLAB.mat')
load('allgrpdat_condition_natural_EEGLAB.mat')

empt_nat = cellfun(@isempty, allgrpdat_natural(1,:))
natural = allgrpdat_natural(:,~empt_nat)

empt_man = cellfun(@isempty, allgrpdat_manmade(1,:))
manmade = allgrpdat_manmade(:,~empt_man)

for ii = 1:length(manmade)

    ec_plot_gaERP(manmade{1,ii},natural{1,ii}, manmade{2,ii},'CPz')%GA for manmade and natural, team ID, and electrode to plot
end


