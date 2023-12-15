clear all

%Load Analysis Qeustionnaire
AQ = readtable('C:\Users\ecesnait\Desktop\EEGManyPipelines\git\EEGManyPipelines-personal\Matlab\Analysis Questionnaire\corrected_AQ.xlsx');

%Load the original H1 table with some of the variables already addded
orig_table_h1 = readtable("C:\Users\ecesnait\Desktop\EEGManyPipelines\git\EEGManyPipelines-personal\Matlab\Big Analysis\initial_variables_h1_pval.xlsx");

% Inspect the AQ for more of the relevant variables to add
varNames = AQ.Properties.VariableNames
hp1_indx_all = regexp(varNames, '_h1', 'match') % questions for h1
hp1_indx = find(~cellfun(@isempty, hp1_indx_all))
hp1_names = varNames(hp1_indx)'

AQ_h1 = AQ(:,hp1_indx)
quest_interest_1 = [4, 6, 42:44] % if averaged over channels, corrected for multiple comparissons, 
new_table = [orig_table_h1,AQ_h1(:,quest_interest_1)]

AQ_preprocess = AQ(:,1:190)
quest_interest_2 = [10, 12, 14, 46,75,80,111,151,152] % if averaged over channels, corrected for multiple comparissons, 
new_table = [new_table,AQ(:,quest_interest_2)]

writetable(new_table, "all_var_AQ_h1.xlsx")
