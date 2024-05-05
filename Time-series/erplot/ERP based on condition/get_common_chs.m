function allsubjdat = get_common_chs(allsubjdat)

all_labels = {};
for i=1:numel(allsubjdat)
  tmp = allsubjdat{i}.label;
  all_labels{i} = cellfun(@strtrim, tmp', 'UniformOutput', false);
end

common_chs = all_labels{1}
for i = 2:numel(all_labels)
  common_chs = intersect(common_chs, all_labels{i}, 'stable')
end

for i = 1:numel(all_labels)
  i
  [common_chs_new, i_common_chs(:,i), i_other(:, i)] = intersect(all_labels{i}, common_chs, 'stable');
  if ~isequal(common_chs_new, common_chs)
    errot('something wrong')
    return
  end
  allsubjdat{i}.label = common_chs_new';
  allsubjdat{i}.avg = allsubjdat{i}.avg(i_common_chs(:,i), :);
end