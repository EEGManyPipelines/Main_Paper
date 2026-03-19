function [ev_id_manmade, ev_id_natural] = get_event_id(events_struct, field_names)

  ev_id_manmade = [];
  ev_id_natural = [];

  stim_names = fieldnames(events_struct);

  for ns=1:numel(stim_names)
    str_numbers = regexp(stim_names{ns}, '\d+(\.\d+)?', 'match');
    % the field of the events struct contains the event number
    if isempty(str_numbers) == 0
      if startsWith(str_numbers{1}, "1")  % all markers for man-made stimuli start with 1
        ev_id_manmade = [ev_id_manmade events_struct.(stim_names{ns})];
      elseif startsWith(str_numbers{1}, "2")  % all markers for natural stimuli start with 2            
        ev_id_natural = [ev_id_natural events_struct.(stim_names{ns})];
      end
    else
      % the field of the events struct contains the string natural or
      % man-made
      if strfind(stim_names{ns}, field_names{1})  % all markers for man-made stimuli start with 1
        ev_id_manmade = [ev_id_manmade events_struct.(stim_names{ns})];
      elseif strfind(stim_names{ns}, field_names{2})  % all markers for natural stimuli start with 2            
        ev_id_natural = [ev_id_natural events_struct.(stim_names{ns})];
      end
    end
  end