function is_same = check_field(epo_struct, field)
    % Extract the first label to use as a reference
    refLabel = epo_struct{1}.(field);

    % Initialize flag as true
    is_same = true;

    % Loop through the array starting from the second element
    for i = 2:length(epo_struct)
        if ~isequal(refLabel, epo_struct{i}.(field))
            is_same = false;
            
            break; % Exit loop early if any label does not match the reference
        end
    end
end
[refLabel, epo_struct{i}.(field)]