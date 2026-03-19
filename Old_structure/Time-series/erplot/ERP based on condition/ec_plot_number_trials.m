trials = {num_trials.manmade_natural}

indx_empty = cellfun(@isempty, trials)
trials(indx_empty) = []
trials(27) = []
for i = 1:62

    mean_trials = mean(trials{i})
    manmade_mean(i) = mean_trials(1)
    natural_mean(i) = mean_trials(2)

end
find_zero = natural_mean == 0
natural_mean(find_zero) = []
manmade_mean(find_zero) = []
all_mean = [manmade_mean',natural_mean']

data = sum(all_mean,2);
x = histogram(data,'BinWidth',40), ylim([0 35]),ylabel('Number fo teams'), xlabel('number of trials'),set(gca,'fontsize',12);
E = x.BinEdges;
y = x.BinCounts;
xloc = E(1:end-1)+diff(E)/2;

xloc(setdiff(1:23,find(y))) = nan()%remove zero
text(xloc, y+1, string(y), 'FontSize',12)
