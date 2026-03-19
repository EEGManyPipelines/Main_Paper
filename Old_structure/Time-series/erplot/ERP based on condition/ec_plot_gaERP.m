function ec_plot_gaERP(allgrpdat_manmade,allgrpdat_natural, groupID, ch_label)

indx_cpz = find(ismember(allgrpdat_manmade.label,ch_label))
if ~isempty(indx_cpz)

    x = allgrpdat_manmade.time
    figure
    if size(allgrpdat_natural.avg,1) > 80

        plot(x,allgrpdat_manmade.avg(:,indx_cpz),'LineWidth',1.5), hold on
        plot(x,allgrpdat_natural.avg(:,indx_cpz),'LineWidth',1.5)
    else

        plot(x,allgrpdat_manmade.avg(indx_cpz,:),'LineWidth',1.5),  hold on
        plot(x,allgrpdat_natural.avg(indx_cpz,:),'LineWidth',1.5)
    end
    title([groupID(1:end-4),', ', ch_label]),legend({'man made', 'natural'})
    fig = gcf
    exportgraphics(fig, 'ERPs_condition_EEGLAB_CPz.pdf','Append',true)
    close(fig)
end

end
