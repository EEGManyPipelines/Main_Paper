% Plot output from regression analysis.
% 1) as images
% 2) as topographies (TBA)

cd('C:\Users\ncb623\EMP\data\Standardized')
load('thetadat.mat')

%% Image
c = [0, ceil(max([grp(:);subj(:);resid(:)]))];
figure;
subplot(1,3,1)
imagesc(grp); title('Theta grp')
ylabel('Channel #')
clim(c)
subplot(1,3,2)
imagesc(subj); title('Theta subj')
clim(c)
xlabel("time index")
subplot(1,3,3)
imagesc(resid); title('Residual')
clim(c)
