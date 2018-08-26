%% Plot the spectral absorption coefficients
addpath('C:\LocalUserData\User-data\hadi1\hadi_phd_data\SAFE\PROSPECT\PROSPECT5_Matlab\PROSPECT5_Matlab\PROSPECT_5B_MATLAB');
addpath('C:\LocalUserData\User-data\hadi1\PHD_RESEARCH\STUDY_II_TROPICS_PARAS\Rproj_TROPICS_PARAS\src\GSAT\GSAT');
addpath('C:\LocalUserData\User-data\hadi1\hadi_phd_data\SAFE\PROSPECT\PROSPECT4_Matlab\PROSPECT_4_MATLAB');
spc_abs_P4 = dataSpec_P4; 
% [1] = wavelength (nm)
% [2] = refractive index of leaf material
% [3] = specific absorption coefficient of chlorophyll (a+b) (cm2.microg-1)
% [4] = specific absorption coefficient of carotenoids (cm2.microg-1)
% (null because PROSPECT-4 does not take carotenoids into account)
% [5] = specific absorption coefficient of water (cm-1)
% [6] = specific absorption coefficient of dry matter (cm2.g-1)

spc_abs_P5B = dataSpec_P5B; 
% [1] = wavelength (nm)
% [2] = refractive index of leaf material
% [3] = specific absorption coefficient of chlorophyll (a+b) (cm2.microg-1)
% [4] = specific absorption coefficient of carotenoids (cm2.microg-1)
% [5] = specific absorption coefficient of brown pigments (arbitrary units)
% [6] = specific absorption coefficient of water (cm-1)
% [7] = specific absorption coefficient of dry matter (cm2.g-1)

% Export to R and plot in R
csvwrite('C:\LocalUserData\User-data\hadi1\PHD_RESEARCH\STUDY_II_TROPICS_PARAS\Rproj_TROPICS_PARAS\data\leaf\spc_abs_P4.csv', spc_abs_P4);
csvwrite('C:\LocalUserData\User-data\hadi1\PHD_RESEARCH\STUDY_II_TROPICS_PARAS\Rproj_TROPICS_PARAS\data\leaf\spc_abs_P5B.csv', spc_abs_P5B);