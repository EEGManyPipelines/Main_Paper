function [indx_manmade,indx_natural] = ec_special_cases(grp,groupdat)

if sum(strcmp(grp,{'129c99e90c45f38c.mat','a28bd183f8dca47f.mat'}))
    indx_tle = strcmp({groupdat(1).epoch.type}, 'TLE');
    groupdat(1).epoch(indx_tle) = [];
    char_type = char({groupdat(1).epoch.type}');
    indx_manmade = find(char_type(:,1)=='1');
    indx_natural = find(char_type(:,1)=='2');
    clear char_type
elseif sum(strcmp(grp,{'1559fd3bafe5582c.mat', '2e6f06d6e89db2ca.mat','d5c8ed05b7af02a3.mat'}))
    char_type = num2str([groupdat(1).epoch]')
    indx_manmade = find(char_type(:,1)=='1');
    indx_natural = find(char_type(:,1)=='2');
    clear char_type
elseif sum(strcmp(grp,{'356c77bfd2662b9a_H1.mat','The Hanncanny.mat','gNeC.mat'}))
    indx_manmade = find(strcmp(groupdat(1).epoch, 'man_made'));
    indx_natural = find(strcmp(groupdat(1).epoch, 'natural'));
elseif strcmp(grp,'8107271d26b4e6ce.mat')
    char_type = char([groupdat(1).epoch]);
 indx_manmade = find(char_type(:,1)=='1');
    indx_natural = find(char_type(:,1)=='2');
    clear char_type
elseif strcmp(grp,'9c490673610ca32b.mat')
    for i = 1:length(groupdat(1).epoch)
        if length({groupdat(1).epoch{1,i}.value})>1
            both_val = {groupdat(1).epoch{1,i}.value}
            indx_num = cellfun(@isnumeric,both_val);
            char_type(i) = both_val{indx_num};            
        else
            char_type(i) = groupdat(1).epoch{1,i}.value;
        end
    end
    char_type = num2str(char_type')
    indx_manmade = find(char_type(:,1)=='1');
    indx_natural = find(char_type(:,1)=='2');
     clear char_type
elseif sum(strcmp(grp,{'CognitiveSystems-KU.mat','d9a070789fe1b133.mat'}))
      char_type = num2str(groupdat(1).epoch.type);
    indx_manmade = find(char_type(:,1)=='1');
    indx_natural = find(char_type(:,1)=='2');
    clear char_type
elseif strcmp(grp,'fc67513ed5c4b400.mat')
    char_type = cellfun(@char, [groupdat(1).epoch.eventscene_category],'UniformOutput', false)
    indx_empty = find(cellfun(@isempty,char_type));
    char_type(indx_empty) = [];
    groupdat(1).epoch(indx_empty) = []
    unique(char_type)
     indx_manmade = find(strcmp(char_type, 'manmade'));
     indx_natural = find(strcmp(char_type, 'natural'));
     clear char_type
end

end