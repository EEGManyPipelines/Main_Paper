% Plot output from regression analysis.
% 1) as images
% 2) as topographies (TBA)
cd('C:\Users\ncb623\EMP\data')

%% Only rfx model
load('thetadat.mat')

%% Image
c = [0, ceil(max([grp(:);subj(:);resid(:)]))];
figure;
subplot(1,3,1); colorbar
imagesc(grp); title('Theta grp')
ylabel('Channel #')
% clim(c)
subplot(1,3,2);colorbar
imagesc(subj); title('Theta subj')
% clim(c)
xlabel("time index")
subplot(1,3,3); colorbar
imagesc(resid); title('Residual')
% clim(c)


%% Fx model
load('fxmod.mat')

% ICA
c = [floor(min(beta_ica(:))), ceil(max(beta_ica(:)))];

figure;
subplot(2,1,1); colorbar
imagesc(beta_ica(:,:,1)); title('No ICA (beta coef)')
ylabel('Channel #');
clim(c)
subplot(2,1,2);colorbar
imagesc(beta_ica(:,:,2)); title('ICA (Beta coef)')
clim(c)
ylabel('Channel #'); xlabel("time index")

% Reref
c = [floor(min(beta_ref(:))), ceil(max(beta_ref(:)))];

figure;
subplot(3,1,1); 
imagesc(beta_ref(:,:,1)); title('Avg ref (beta coef)')
ylabel('Channel #'); xlabel("time index")
clim(c); colorbar
subplot(3,1,2);colorbar
imagesc(beta_ref(:,:,2)); title('Mastoid ref (Beta coef)')
clim(c); colorbar
ylabel('Channel #');
subplot(3,1,3);
imagesc(beta_ref(:,:,3)); title('Original ref (Beta coef)')
ylabel('Channel #'); xlabel("time index")
clim(c); colorbar

% Rfxs
c = [0, ceil(max([thet_grp(:);thet_sub(:);thet_resid(:)]))];

figure;
subplot(3,1,1); 
imagesc(thet_grp); title('Theta grp')
ylabel('Channel #')
clim(c); colorbar
subplot(3,1,2);
imagesc(thet_sub); title('Theta subj')
ylabel('Channel #')
clim(c); colorbar
subplot(3,1,3);
imagesc(thet_resid); title('Residual')
clim(c); colorbar
ylabel('Channel #'); xlabel("time index")


