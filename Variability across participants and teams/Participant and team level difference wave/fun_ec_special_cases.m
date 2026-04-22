function [indx_manmade,indx_natural] = fun_ec_special_cases(grp,groupdat,pp)

if sum(strcmp(grp,{'standart_129c99e90c45f38c.mat','standart_a28bd183f8dca47f.mat'}))
    indx_tle = strcmp({groupdat{1,pp}.trialinfo.type}, 'TLE');
    groupdat{1,pp}.trialinfo(indx_tle) = [];
    char_type = char({groupdat{1,pp}.trialinfo.type}');
    indx_manmade = find(char_type(:,1)=='1');
    indx_natural = find(char_type(:,1)=='2');
    clear char_type
elseif sum(strcmp(grp,{'standart_1559fd3bafe5582c.mat', 'standart_2e6f06d6e89db2ca.mat','standart_d5c8ed05b7af02a3.mat'}))
    char_type = num2str([groupdat{1,pp}.trialinfo]')
    indx_manmade = find(char_type(:,1)=='1');
    indx_natural = find(char_type(:,1)=='2');
    clear char_type
elseif sum(strcmp(grp,{'standart_356c77bfd2662b9a_H1.mat','standart_The Hanncanny.mat','standart_gNeC.mat'}))
    indx_manmade = find(strcmp(groupdat{1,pp}.trialinfo, 'man_made'));
    indx_natural = find(strcmp(groupdat{1,pp}.trialinfo, 'natural'));
elseif strcmp(grp,'standart_8107271d26b4e6ce.mat')
    char_type = char([groupdat{1,pp}.trialinfo]);
    indx_manmade = find(char_type(:,1)=='1');
    indx_natural = find(char_type(:,1)=='2');
    clear char_type
elseif strcmp(grp,'standart_9c490673610ca32b.mat') % inspect
    for i = 1:length(groupdat{1,pp}.trialinfo)
        if length({groupdat{1,pp}.trialinfo{1,i}.value})>1
            both_val = {groupdat{1,pp}.trialinfo{1,i}.value}
            indx_num = cellfun(@isnumeric,both_val);
            char_type(i) = both_val{indx_num};            
        else
            char_type(i) = groupdat{1,pp}.trialinfo{1,i}.value;
        end
    end
    
    char_type = num2str(char_type')
    indx_manmade = find(char_type(:,1)=='1');
    indx_natural = find(char_type(:,1)=='2');
     clear char_type

elseif sum(strcmp(grp,{'standart_CognitiveSystems-KU.mat','standart_d9a070789fe1b133.mat'})) 
      char_type = num2str(groupdat{1,pp}.trialinfo.type);
    indx_manmade = find(char_type(:,1)=='1');
    indx_natural = find(char_type(:,1)=='2');
    clear char_type
% elseif strcmp(grp,'standart_fc67513ed5c4b400.mat')
%     char_type = cellfun(@char, [groupdat{1,pp}.trialinfo.eventscene_category],'UniformOutput', false)
%     indx_empty = find(cellfun(@isempty,char_type));
%     char_type(indx_empty) = [];
%     groupdat{1,pp}.trialinfo(indx_empty) = []
%     unique(char_type)
%      indx_manmade = find(strcmp(char_type, 'manmade'));
%      indx_natural = find(strcmp(char_type, 'natural'));
%      clear char_type
elseif strcmp(grp,'standart_93af9133e6fa6cf5.mat')
    indx_manmade = find(strcmp({groupdat{1,pp}.trialinfo.eventscene_category}, 'manmade'));
    indx_natural = find(strcmp({groupdat{1,pp}.trialinfo.eventscene_category}, 'natural'));
% elseif strcmp(grp,'standart_ENSLab.mat')
%     for i = 1:length(groupdat{1,pp}.trialinfo)
%         if length(groupdat{1,pp}.trialinfo(i).eventscene_category)>1
%             both_val = isequal(groupdat{1,pp}.trialinfo(i).eventscene_category{1},groupdat{1,pp}.trialinfo(i).eventscene_category{2}) 
%             if both_val
%             char_type(i) = groupdat{1,pp}.trialinfo(i).eventscene_category{1};     
%             else
%                 error('check values')
%             end
%         else
%             char_type(i) = groupdat{1,pp}.trialinfo(i).eventscene_category{1};
%         end
% end
%  indx_manmade = find(strcmp(char_type, 'manmade'));
%  indx_natural = find(strcmp(char_type, 'natural'));
% clear char_type
end

