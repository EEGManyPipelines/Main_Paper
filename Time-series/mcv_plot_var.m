% Plot output from regression analysis.
% 1) as images
% 2) as topographies (TBA)
cd('C:\Users\ncb623\EMP\data')

%% Only rfx model
load('thetadat.mat')
grptab = readtable('grptab.csv');

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
subplot(3,1,1); colorbar
imagesc(beta_ica(:,:,1)); title('No ICA (beta coef)')
ylabel('Channel #');
clim(c); colorbar
subplot(3,1,2);colorbar
imagesc(beta_ica(:,:,1)+beta_ica(:,:,2)); title('ICA (Beta coef)')
clim(c); colorbar
ylabel('Channel #'); xlabel("time index")
subplot(3,1,3);colorbar
% imagesc(t_ica); title('Coef t-values')
imagesc(t_ica < -2 | t_ica > 2); title('Coef t-values')
% clim([-5 5]); colorbar
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

%% Fx model #2
load('fxmod_software.mat')

% Toolbox
c = [floor(min(beta_tool(:))), ceil(max(beta_tool(:)))];

figure;
subplot(5,1,1); colorbar
imagesc(beta_tool(:,:,1)); title('BrainVision')
ylabel('Channel #');
clim(c); colorbar
subplot(5,1,2);colorbar
imagesc(beta_tool(:,:,1)); title('EEGLab')
clim(c); colorbar
ylabel('Channel #');
subplot(5,1,3);colorbar
imagesc(beta_tool(:,:,3)); title('FieldTrip')
clim(c); colorbar
ylabel('Channel #'); 
subplot(5,1,4);colorbar
imagesc(beta_tool(:,:,4)); title('Other')
clim(c); colorbar
ylabel('Channel #'); 
subplot(5,1,5);colorbar
imagesc(beta_tool(:,:,5)); title('SPM')
clim(c); colorbar
ylabel('Channel #'); 

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

%% Fx model #3
load('fxmod_H1.mat')

% ICA
c = [floor(min(beta_H1(:))), ceil(max(beta_H1(:)))];

figure;
subplot(3,1,1); colorbar
imagesc(beta_H1(:,:,1)); title('H1=FALSE')
ylabel('Channel #');
clim(c); colorbar
subplot(3,1,2);colorbar
imagesc(beta_H1(:,:,1)+beta_H1(:,:,2)); title('H1=TRUE')
clim(c); colorbar
ylabel('Channel #');
subplot(3,1,3);colorbar
imagesc(t_H1); title('Coef t-values')
% imagesc(t_ica < -2 | t_ica > 2); title('Coef t-values')
colorbar
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
