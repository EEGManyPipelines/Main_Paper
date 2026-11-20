%% Getting Indices for Hypothesis 2a
% following are attempts at retrieving the trial-information from
% the trialinfo field given by the data. At first we check wether
% the subject has any data and if not, we skip them.
function [ind_manmade,ind_natural,error_catch] = getIndicesGeneral_h2a(trialinfo,subject,team_id)

%% MNE Cases
% the following teams all have a special trial-structure (that came from MNE-Python processing). 
% to extract the indices we need some special procedure depending on the format of the
% trigger-codes. This will be handled in another function: getIndicesMNE
if ismember(team_id,{'04986d243980b3d8' '08de3c5e092173e4' '0ba1c7f1dafc1134' '0bc9ee704db74104' '17a098cf2c77f087' '19e8ad8bf94af489' ...
        '344dd59ded90cb34' '628a18bc8a3d36dd' '69133905229920b3' '77fddd91c557626d' '7f3fe2bc79a9d3f9' '8559e4d7314e45ec' ...
        '8966ebbcd4c11329' '8c587a4cbf53865d' '90420442f22fa870' 'CIMHPipe' 'CSAI' 'HSU-HH' 'TheCodeMechanics' 'Varuwa' ...
        'a0cf32754296214f' 'a25b8419335d2131' 'aa6aa366e9788967' 'b0edd369b6d8f4f1' 'bd3077a83b5b16bd' 'c0c75576f9cd0b2a' ...
        'c1a5013b850d9b0a' 'c4ed0094fc18efbc' 'c577d3cdf78548ce' 'c91e489c4acd0bf4' 'da33cf2264c9baa2' 'e13e7e07b99d853b' ...
        'e146a94b29a41713' 'e2d0d90e5cf594ed' 'e69a83408d1f3811' 'e72d90a6ff4b5108' 'ea6b1d1870708b82' 'ee8c062e3dc35b1d' ...
        'eef1406b3fca3e9c' 'f92a1d6a49d0d40a' 'f957e6a116a37043' 'ff8bf48d04d11c84' 'neuro.sfedu' '48e64dc185199502' '6f9edbeaf8fa533f'})

    [ind_manmade,ind_natural,error_catch] = getIndicesMNE_h1(trialinfo,subject,team_id);
    return
end

%% Special Cases

% These have strange triggercodes. Here we first find the proper lines that
% correspond to a stimulus. then we take the proper indices out of those
if ismember(team_id, {'129c99e90c45f38c','a28bd183f8dca47f'}) % gg 10,21,75
    indx_tle = strcmp({trialinfo.type}, 'TLE');
    trialinfo(indx_tle) = [];
    char_type = char({trialinfo.type}');
    ind_manmade = find(char_type(:,1)=='1');
    ind_natural = find(char_type(:,1)=='2');
    clear char_type
    error_catch = {};
    return
end

if ismember(team_id,{'7e05fa9e0f3d490f','80d914dc5270b2e7'} )
    char_type = [trialinfo.eventscene_category];
    ind_manmade = find(strcmp(char_type, 'manmade'));
    ind_natural = find(strcmp(char_type, 'natural'));
    clear char_type
    error_catch = {};
    return
end

if ismember(team_id,{'1559fd3bafe5582c', '2e6f06d6e89db2ca','d5c8ed05b7af02a3'})
    char_type = num2str([trialinfo]')
    ind_manmade = find(char_type(:,1)=='1');
    ind_natural = find(char_type(:,1)=='2');
    clear char_type
     error_catch = {};
    return
end

if ismember(team_id,{'The Hanncanny','gNeC'})
    ind_manmade = find(strcmp(trialinfo, 'man_made'));
    ind_natural = find(strcmp(trialinfo, 'natural'));
    error_catch = {};
    return
end

if strcmp(team_id,'8107271d26b4e6ce')
    char_type = char([trialinfo]);
    ind_manmade = find(char_type(:,1)=='1');
    ind_natural = find(char_type(:,1)=='2');
    clear char_type
     error_catch = {};
    return
end

if strcmp(team_id,'3c8c07696fbd0314')
    char_type = char(trialinfo.value);
    ind_manmade = find(char_type(:,1)=='1');
    ind_natural = find(char_type(:,1)=='2');
    clear char_type
     error_catch = {};
    return
end
% similar to above, the trialinfo in this team itself contains structures
% that themselves have the trialinfo that we need. Sometines it contains
% codes vor artifacts, that we skip if included
if ismember(team_id, {'9c490673610ca32b','ac0f676d6813a969'}) % gg 63
    ind_manmade2 = [];
    ind_natural2 = [];
    for j = 1:size(trialinfo,2)
        if size(trialinfo{1,j},2) == 1
            char_type = num2str(trialinfo{1,j}.value);
        else
            cell = {trialinfo{1,j}.value};
            char_type = num2str(cell{1});
        end    
        if char_type(1) == '1'
            ind_manmade2(size(ind_manmade2,2)+1) = j;
        elseif char_type(1) == '2'
            ind_natural2(size(ind_natural2,2)+1) = j;
        end
    end
    ind_manmade=ind_manmade2;
    ind_natural=ind_natural2;
    error_catch={};
    return
end

% similar to above, this team has the trialinfo in form of a table. 
if ismember(team_id, {'CognitiveSystems-KU','d9a070789fe1b133'})
    char_type = num2str(trialinfo.type);
    ind_manmade = find(char_type(:,1)=='1');
    ind_natural = find(char_type(:,1)=='2');
    clear char_type
    error_catch = {};
    return
end

if strcmp(team_id,'93af9133e6fa6cf5')
    ind_manmade = find(strcmp({trialinfo.eventscene_category}, 'manmade'));
    ind_natural = find(strcmp({trialinfo.eventscene_category}, 'natural'));
    error_catch = {};
    return
end

if strcmp(team_id,'018ab79008f2e342')
         
                    char_type = num2str([trialinfo.eventtype]')
                    ind_manmade = find(char_type(:,1)=='1');
                    ind_natural = find(char_type(:,1)=='2');
          
        error_catch = {};
        return
end
if strcmp(team_id,'13bef9b0362773b3')
         
       char_type = [trialinfo.eventscene_category];
        ind_manmade = ismember(char_type,'manmade');
        ind_natural = ismember(char_type,'natural');
          
        error_catch = {};
        return
end

if strcmp(team_id,'26ab3771b0c195f5')
         
       char_type = [trialinfo{:}] ;
  char_type = num2str(char_type');
    
    ind_manmade = find(char_type(:,1)=='1');
    ind_natural = find(char_type(:,1)=='2');
          
        error_catch = {};
        return
end

 if ismember(team_id,{'04b28b5356eb17bd','0a0e092a0c1e6c8f','1291ad7d784e90e5'})
          if isa([trialinfo.eventtype],'char')
                    char_type = char({trialinfo.eventtype});
                    ind_manmade = find(char_type(:,1)=='1');
                    ind_natural = find(char_type(:,1)=='2');

         elseif isa([trialinfo.eventtype],'double')
                    char_type = num2str([trialinfo.eventtype]')
                    ind_manmade = find(char_type(:,1)=='1');
                    ind_natural = find(char_type(:,1)=='2');
          end
        error_catch = {};
        return
end
% this case just has a strange formatting, where the trialinfo contains
% % structs that themselves contain the proper information
if ismember(team_id, {'1e41774b7169df7d'}) % gg 15
    ind_manmade = [];
    ind_natural = [];
    for j = 1:size(trialinfo,1)
        char_type = trialinfo{j,1}.image;
        if strcmp(char_type,'man_made')
            ind_manmade(size(ind_manmade,2)+1) = j;
        elseif strcmp(char_type,'natural')
            ind_natural(size(ind_natural,2)+1) = j;
        end
    end

    error_catch={};
    return
end



% this one has the conditions spread out strangely. we take what we need
% from the 3rd column and give out the indices.
if ismember(team_id, {'Chile Maule'}) % gg 65
    char_type = trialinfo(:,4);
    ind_manmade = find(strcmp(char_type, 'manmade'));
    ind_natural = find(strcmp(char_type, 'natural'));
    error_catch = {};
    return
end
% 
% % similar to above, but contains triggercodes we take from 3rd column
% if ismember(team_id, {'TeamKeele','Kaiser Soze'}) % gg 69
%     char_type = [trialinfo];
%     ind_manmade = char_type(:,1) == 1;
%     ind_natural = char_type(:,1) == 2;
%     error_catch = {};
%     return
% end
% 

% % this case has the trialinfo in form of a table. We take the triggercodes
% % from the field 'values'
% if ismember(team_id, {'3c8c07696fbd0314'})
%     char_type = char(trialinfo.value);
%     ind_manmade = find(char_type(:,1)=='1');
%     ind_natural = find(char_type(:,1)=='2');
%     error_catch = {};
%     return
% end
% 

% 
% % has strange formatting and also multiple trials per cell
% if ismember(team_id,{'7c793519c38db522'}) 
%     ind_manmade = [];
%     ind_natural = [];
%     error_catch.errorMSG = 'Found Multiple Trials per Cell';
%     return
% end


%% General Cases
 if isfield(trialinfo, 'eventscene_category') % the easiest case which tells us directly the condition
        ind_manmade = find(strcmp({trialinfo.eventscene_category}, 'manmade'));
        ind_natural = find(strcmp({trialinfo.eventscene_category}, 'natural'));
        if isempty(ind_manmade)
            ind_manmade = find(strcmp([trialinfo.eventscene_category], 'manmade'));
            ind_natural = find(strcmp([trialinfo.eventscene_category], 'natural'));

        end
        error_catch = {};
        return
 elseif isfield(trialinfo, 'eventtype')
         if isa([trialinfo.eventtype],'char')
                    char_type = char({trialinfo.eventtype});
                    ind_manmade = find(char_type(:,1)=='1');
                    ind_natural = find(char_type(:,1)=='2');

         elseif isa([trialinfo.eventtype],'double')
                    char_type = num2str([trialinfo.eventtype]')
                    ind_manmade = find(char_type(:,1)=='1');
                    ind_natural = find(char_type(:,1)=='2');
          end
        error_catch = {};
        return
  elseif isfield(trialinfo,'type')
                  char_type = char({trialinfo.type});
                    ind_manmade = find(char_type(:,1)=='1');
                    ind_natural = find(char_type(:,1)=='2');
                     error_catch = {};
        return
  end
if isa(trialinfo, "numeric") % this case tells us directly the triggercode
    char_type = num2str(trialinfo);
    if size(char_type,1)==1
        char_type = num2str(trialinfo');
    end
    ind_manmade = find(char_type(:,1)=='1');
    ind_natural = find(char_type(:,1)=='2');
    error_catch = {};
    return
end
if isa(trialinfo, "cell")
    char_type = char(trialinfo);
    if size(char_type,2)==4
        ind_manmade = find(char_type(:,1)=='1');
        ind_natural = find(char_type(:,1)=='2');
        error_catch = {};
        return
    end
end


% if no indices have been found previous, just return empty indices and an
% error message

ind_manmade = [];
ind_natural = [];
error_catch.errorMSG = 'No Usable Trialdata Found';
end



