
# Height ------------------------------------------------------------------
SAFE_biomassQ <- read.xlsx("C:/LocalUserData/User-data/hadi1/hadi_phd_data/SAFE/ORIGINAL_SPREADSHEET/LAI_Biomass_Quality_Status_February2015.xlsx", 
                           sheetName="Height_Volume", header=T)
SAFE_biomassQ$SampleID <- as.character(SAFE_biomassQ$Plot)
  

fieldData.SAFE.all <- read.xlsx("C:/LocalUserData/User-data/hadi1/hadi_phd_data/SAFE/LAI_Malaysia_NoDoublon_usableplots.xls",
                            sheetName ="193plots", header=T)


SAFE_biomassQ <- merge(SAFE_biomassQ, fieldData.SAFE.all, by="SampleID")


plot_laiheight <- myplot(SAFE_biomassQ, SAFE_biomassQ$LAI_true_v6, SAFE_biomassQ$Height_max_m, SAFE_biomassQ$Type, "LAIt_uk", "Max Height", 
                            c(0,6), c(0,50), seq(0,6,by=1), seq(0,50,by=5), 0.5, 50, c(2:4,6,8,9), c(0.15,0.4))

setwd("C:/LocalUserData/User-data/hadi1/hadi_phd_data/SAFE/RESULTS/highres_fig")
tiff("LAI_height_193plots.tiff", res=1000, height=8, width=8, units="cm", compression="lzw", pointsize=8)
plot_laiheight
dev.off()


SAFE_gaps_spc_DiptElae_addtraits <- merge(SAFE_gaps_spc_DiptElae_addtraits, SAFE_biomassQ, mby="SampleID")
plot_redheight <- myplot(SAFE_gaps_spc_DiptElae_addtraits, SAFE_gaps_spc_DiptElae_addtraits$Height_max_m, SAFE_gaps_spc_DiptElae_addtraits$SR_B3_red, 
                         SAFE_gaps_spc_DiptElae_addtraits$Type, "Max Height", "Landsat RED", 
                         c(0,50), c(0,0.1), seq(0,50,by=10), seq(0,0.1,by=0.025), 1, 0.1, c(2:4,6,8,9), c(0.8,0.8))

plot_nirheight <- myplot(SAFE_gaps_spc_DiptElae_addtraits, SAFE_gaps_spc_DiptElae_addtraits$Height_max_m, SAFE_gaps_spc_DiptElae_addtraits$SR_B4_nir, 
                         SAFE_gaps_spc_DiptElae_addtraits$Type, "Max Height", "Landsat NIR", 
                         c(0,50), c(0,0.6), seq(0,50,by=10), seq(0,0.6,by=0.1), 1, 0.6, c(2:4,6,8,9), c(0.8,0.2))

plot_swir1height <- myplot(SAFE_gaps_spc_DiptElae_addtraits, SAFE_gaps_spc_DiptElae_addtraits$Height_max_m, SAFE_gaps_spc_DiptElae_addtraits$SR_B5_swir1, 
                         SAFE_gaps_spc_DiptElae_addtraits$Type, "Max Height", "Landsat SWIR1", 
                         c(0,50), c(0,0.25), seq(0,50,by=10), seq(0,0.25,by=0.05), 1, 0.25, c(2:4,6,8,9), c(0.8,0.2))

setwd("C:/LocalUserData/User-data/hadi1/hadi_phd_data/SAFE/RESULTS/highres_fig")
tiff("L7_height_56plots.tiff", res=1000, height=12, width=12, units="cm", compression="lzw", pointsize=8)
grid.arrange(plot_redheight, plot_nirheight, plot_swir1height, ncol=2)
dev.off()

