%% Getting Indices for Hypothesis 2a
% following are attempts at retrieving the trial-information from
% the trialinfo field given by the data. At first we check wether
% the subject has any data and if not, we skip them.
function [ind_new,ind_old,error_catch] = getIndicesGeneral_h2a(trialinfo,subject,team_id)


% get the trialinfotable to compare
tablefolder = 'Z:\nbuschgold\ecesnait\EMP\Patrick\events\';
fishytextfilepath = 'Z:\nbuschgold\ecesnait\EMP\EMP time series exp\TimelockAVG_Hyp2a\';


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
    
    [ind_new,ind_old,error_catch] = getIndicesMNE_h2a(trialinfo,subject,team_id);
    return
end

%% Special Cases

% These have strange triggercodes. Here we first find the proper lines that
% correspond to a stimulus. then we take the proper indices out of those
if ismember(team_id, {'129c99e90c45f38c','2f7c7b90372f9c9e','a28bd183f8dca47f'}) % gg 10,21,75
    char_type = char({trialinfo.type}');
    ind_stim = find(strcmp({trialinfo.code}', 'Stimulus'));
    char_type = char_type(ind_stim,:);
    ind_new = find(char_type(:,2) =='0');
    ind_old = find(char_type(:,2) =='1');
    error_catch = {};
    return
end

if ismember(team_id,{'1559fd3bafe5582c', '2e6f06d6e89db2ca','d5c8ed05b7af02a3'})
    char_type = num2str([trialinfo]')
    ind_new = find(char_type(:,2)=='0');
    ind_old = find(char_type(:,2)=='1');
    clear char_type
     error_catch = {};
    return
end

if strcmp(team_id,'26ab3771b0c195f5')
         
    char_type = [trialinfo{:}] ;
  char_type = num2str(char_type');
    
    ind_new = find(char_type(:,2)=='0');
    ind_old = find(char_type(:,2)=='1');
          
        error_catch = {};
        return
end

if ismember(team_id,{'The Hanncanny','gNeC'})
    ind_new = find(strcmp(trialinfo, 'new'));
    ind_old = find(strcmp(trialinfo, 'old'));
    error_catch = {};
    return
end

% this case just has a strange formatting, where the trialinfo contains
% structs that themselves contain the proper information
if ismember(team_id, {'1e41774b7169df7d'}) % gg 15
    ind_news = [];
    ind_olds = [];
    for j = 1:size(trialinfo,1)
        char_type = trialinfo{j,1}.oldNew;
        if strcmp(char_type,'new')
            ind_news(size(ind_news,2)+1) = j;
        elseif strcmp(char_type,'old')
            ind_olds(size(ind_olds,2)+1) = j;
        end
    end
    ind_new=ind_news;
    ind_old=ind_olds;
    error_catch={};
    return
end

% similar to above, the trialinfo in this team itself contains structures
% that themselves have the trialinfo that we need. Sometines it contains
% codes vor artifacts, that we skip if included
if ismember(team_id, {'9c490673610ca32b','ac0f676d6813a969'}) % gg 63
    ind_news = [];
    ind_olds = [];
    for j = 1:size(trialinfo,2)
        if size(trialinfo{1,j},2) == 1
            char_type = num2str(trialinfo{1,j}.value);
        else
            cell = {trialinfo{1,j}.value};
            char_type = num2str(cell{1});
        end    
        if char_type(2) == '0'
            ind_news(size(ind_news,2)+1) = j;
        elseif char_type(2) == '1'
            ind_olds(size(ind_olds,2)+1) = j;
        end
    end
    ind_new=ind_news;
    ind_old=ind_olds;
    error_catch={};
    return
end

% this one has the conditions spread out strangely. we take what we need
% from the 3rd column and give out the indices.
if strcmp(team_id, {'Chile Maule'}) % gg 65
    char_type = trialinfo(:,3);
    ind_new = find(strcmp(char_type, 'new'));
    ind_old = find(strcmp(char_type, 'old'));
    error_catch = {};
    return
end

% similar to above, but contains triggercodes we take from 3rd column
if strcmp(team_id, {'TeamKeele','Kaiser Soze'}) % gg 69
    char_type = [trialinfo];
    ind_new = char_type(:,2) == 0;
    ind_old = char_type(:,2) == 1;
    error_catch = {};
    return
end


% this case has the trialinfo in form of a table. We take the triggercodes
% from the field 'values'
if strcmp(team_id, {'3c8c07696fbd0314'})
    char_type = char(trialinfo.value);
    ind_new = find(char_type(:,2)=='0');
    ind_old = find(char_type(:,2)=='1');
    error_catch = {};
    return
end

% similar to above, this team has the trialinfo in form of a table. We take
% the triggercodes from the field 'old'
if ismember(team_id, {'CognitiveSystems-KU','d9a070789fe1b133'})
    char_type = trialinfo.old;
    ind_new = find(strcmp(char_type, 'new'));
    ind_old = find(strcmp(char_type, 'old'));
    error_catch = {};
    return
end

% has strange formatting and also multiple trials per cell
if strcmp(team_id,{'7c793519c38db522'}) 
    ind_new = [];
    ind_old = [];
    error_catch.errorMSG = 'Found Multiple Trials per Cell';
    return
end
if strcmp(team_id,'018ab79008f2e342')
         
        char_type = num2str([trialinfo.eventtype]')
        ind_new = find(char_type(:,2)=='0');
        ind_old = find(char_type(:,2)=='1');
          
        error_catch = {};
        return
end

if strcmp(team_id,'841f7d890725a452')
         
        char_type = char({trialinfo.type})
        ind_new = find(char_type(:,2)=='0');
        ind_old = find(char_type(:,2)=='1');
         
        error_catch = {};
        return
end

%% General Cases
if isa(trialinfo, "struct") 
    if isfield(trialinfo, "eventold") % the easiest case which tells us directly the condition
        char_type = [trialinfo.eventold]';
        if isa(char_type, 'char') % sometimes gets that way
            char_type = {trialinfo.eventold}'; % alternative formatting
        end
        if (length(trialinfo)-length(char_type) ~= 0) % unusable. multiple trials in one cell
            ind_new = [];
            ind_old = [];
            error_catch.errorMSG = 'Found Multiple Trials per Cell';
            return
        end
        if isa(char_type{1}, 'char') % true if usable data!
            ind_new = find(strcmp(char_type, 'new'));
            ind_old = find(strcmp(char_type, 'old'));
            error_catch = {};
            return
        end
    elseif isfield(trialinfo, "eventtype")
        char_type = [trialinfo.eventtype]';        
        if isa(char_type, "numeric")
            char_type = num2str(char_type);
        end
        if size(char_type,2)==1
            char_type = char({trialinfo.eventtype}');
        end
        if (length(trialinfo)-length(char_type) ~= 0) % unusable. multiple trials in one cell
            ind_new = [];
            ind_old = [];
            error_catch.errorMSG = 'Found Multiple Trials per Cell';
            return
        end
        ind_new = find(char_type(:,2)=='0');
        ind_old = find(char_type(:,2)=='1');
        error_catch = {};
        return
    end
elseif isa(trialinfo, "numeric") % this case tells us directly the triggercode
    char_type = num2str(trialinfo);
    if size(char_type,1)==1
        char_type = num2str(trialinfo');
    end
    ind_new = find(char_type(:,2)=='0');
    ind_old = find(char_type(:,2)=='1');
    error_catch = {};
    return
elseif isa(trialinfo, "cell")
    char_type = char(trialinfo);
    if size(char_type,2)==4
        ind_new = find(char_type(:,2)=='0');
        ind_old = find(char_type(:,2)=='1');
        error_catch = {};
        return
    end
end

% if no indices have been found previous, just return empty indices and an
% error message

ind_new = [];
ind_old = [];
error_catch.errorMSG = 'No Usable Trialdata Found';
end



