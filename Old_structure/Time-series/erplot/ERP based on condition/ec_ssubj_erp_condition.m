function [fttmp_manmade, fttmp_natural] = ec_ssubj_erp_condition(groupdat, manmade_avg_epoch, natural_avg_epoch)

% This code creates the fttmp structure for two conditions (man made and natural stimuli) sepatarely that
% will be later used to caltutale the grand average ERP for the whole team
% whose data is stored in groupdat
% It requires:
% groupdat structure with time and channel information
% manmade_avg_epoch is the average of epochs for the man made stimuli (channels x time)
% same for natural stimuli

fttmp_manmade = [];
fttmp_manmade.avg       = manmade_avg_epoch;%sbj_avg_epoch(find_cpz,:)
fttmp_manmade.time      = groupdat(1).time;
fttmp_manmade.label     = groupdat(1).chan;
fttmp_manmade.dimord    = 'chan_time';

fttmp_natural = [];
fttmp_natural.avg       = natural_avg_epoch;%sbj_avg_epoch(find_cpz,:)
fttmp_natural.time      = groupdat(1).time;
fttmp_natural.label     = groupdat(1).chan;
fttmp_natural.dimord    = 'chan_time';

end