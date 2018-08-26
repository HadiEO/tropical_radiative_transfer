#  Leaf single scattering albedo simulated with PROSPECT -------------------------------------------
wL.Birch <- read_csv2("data/leaf/boreal_leaf_spc.csv") 
wL.DiptMean <- read_csv2("results/leaf/wL_DiptMean.csv")
wL.Elaeis <- read_csv2("results/leaf/wL_Elaeis.csv")
wL.HawaiiMean <- read_csv2("results/leaf/wL_HawaiiMean.csv")
wL.GlobalMean <- read_csv2("results/leaf/wL_GlobalMean.csv")
wL.ShoreaMean <- read_csv2("results/leaf/wL_ShoreaMean.csv")
wL.Jacq96Mean <- read_csv2("results/leaf/wL_Jacq96Mean.csv")
wL.liana <- read_csv2("results/leaf/wL_LianaMean.csv")

# Add bias (mod - obs) to wL.DiptMean
biasRefl.vis <- read_csv2("results/leaf/prospect_bias_HAWAII_refl_vis_digitize_full.csv")
biasRefl.irswir <- read_csv2("results/leaf/prospect_bias_HAWAII_refl_irswir_digitize_full.csv")
biasRefl <- rbind(biasRefl.vis, biasRefl.irswir[-1,])                                              # rbind

biasTrans.vis <- read_csv2("results/leaf/prospect_bias_HAWAII_trans_vis_digitize_full.csv")
biasTrans.irswir <- read_csv2("results/leaf/prospect_bias_HAWAII_trans_irswir_digitize_full.csv")
biasTrans <- rbind(biasTrans.vis, biasTrans.irswir[-1,])


# Add bias 
wL.DiptMean.addBias <- wL.DiptMean %>% dplyr::filter(Wavelength >= 400) %>% dplyr::filter(Wavelength <= 2450)                               # Wav in 400-2450nm
wL.DiptMean.addBias <- wL.DiptMean.addBias %>% mutate(Reflectance = Reflectance - biasRefl$y,
                                                      Transmittance = Transmittance - biasTrans$y,
                                                      Albedo = Reflectance + Transmittance)
write.csv2(wL.DiptMean.addBias, "results/leaf/wL_DiptMean_addBias.csv")

# Plot to check
plot(wL.DiptMean$Wavelength, wL.DiptMean$Reflectance, ylim = c(0,0.6))
points(wL.DiptMean.addBias$Wavelength, wL.DiptMean.addBias$Reflectance, col = "red")
plot(wL.DiptMean$Wavelength, wL.DiptMean$Transmittance, ylim = c(0,0.6))
points(wL.DiptMean.addBias$Wavelength, wL.DiptMean.addBias$Transmittance, col = "red")
plot(wL.DiptMean$Wavelength, wL.DiptMean$Albedo, ylim = c(0,1))
points(wL.DiptMean.addBias$Wavelength, wL.DiptMean.addBias$Albedo, col = "red")


# Global humid tropics from Asner et al. (2011) Spectroscopy of canopy chemicals in humid tropical forests

globaltropics.lRefl.min <- read_csv2("results/leaf/globalhumidtropics_leafRefl_min_digitize_full.csv")
globaltropics.lRefl.max <- read_csv2("results/leaf/globalhumidtropics_leafRefl_max_digitize_full.csv")
globaltropics.lTrans.min <- read_csv2("results/leaf/globalhumidtropics_leafTrans_min_digitize_full.csv")
globaltropics.lTrans.max <- read_csv2("results/leaf/globalhumidtropics_leafTrans_max_digitize_full.csv")

globaltropics.lTrans.max.L7 <- as_data_frame(my_spectralResampling(wL.hypers = globaltropics.lTrans.max$y * 0.01, wav.hypers = globaltropics.lTrans.max$x))

write.csv2(globaltropics.lTrans.max.L7, "results/leaf/globaltropicsleaf_trans_max_L7.csv")


# Multivariate sampling
# wL.Dipt2500 <- readRDS("results/leaf/leaf_sim_Dipt2500.rds")

# Stem from Asner (1998)
stem <- read_csv2("data/leaf/Asner1998_stemRefl_mean_digitize_full.csv")

stem.L7 <- as_data_frame(my_spectralResampling(wL.hypers = stem$y, wav.hypers = stem$x))
write.csv2(stem.L7, "results/leaf/stem_mean_L7.csv")



# Understory reflectance -----------------------------------------
soil.spc <- read_csv2("data/soil/soil_spc_sel.csv")
soil.wav <- seq(350, 2500, by=10)
soil.spc.prim <- t(soil.spc[soil.spc$Vegetation=="Primary, Central Kalimantan, Bukit Raya Nature Reserve", 4:ncol(soil.spc)])
soil.spc.OP <- t(soil.spc[soil.spc$Vegetation=="Ecuador, forest cleared, now oil palm and pasture", 4:ncol(soil.spc)])
# Mean
soil.spc.prim.mean <- tibble(Wavelength = soil.wav, Reflectance = rowMeans(soil.spc.prim))
write.csv2(soil.spc.prim.mean, "data/soil/soil_spc_prim_mean.csv")
soil.spc.OP.mean <- tibble(Wavelength = soil.wav, Reflectance = rowMeans(soil.spc.oilpalm))
# Min & max
soil.spc.prim.min <- tibble(Wavelength = soil.wav, Reflectance =  soil.spc.prim[,1])
soil.spc.prim.max <- tibble(Wavelength = soil.wav, Reflectance = soil.spc.prim[,4])
soil.spc.OP.min <- tibble(Wavelength = soil.wav, Reflectance = soil.spc.oilpalm[,4])
soil.spc.OP.maxvis <- tibble(Wavelength = soil.wav, Reflectance = soil.spc.oilpalm[,7])
soil.spc.OP.max <- tibble(Wavelength = soil.wav, Reflectance = soil.spc.oilpalm[,3])
# Boreal herb rich
ustory.herbrich <- read_csv2("data/soil/boreal_herbrich_spectra.csv")

# Estimated as intercept of lm(BRF~ECC)
soil.interc.forest <- read_csv2("data/soil/soil_interc_forest.csv")
soil.interc.OP <- read_csv2("data/soil/soil_interc_OP.csv")

# Litter from Asner (1998)
litter <- read_csv2("data/soil/Asner1998_litterRefl_mean_digitize_full.csv")
litter.L7 <- as_data_frame(my_spectralResampling(wL.hypers = litter$y, wav.hypers = litter$x))
write.csv2(litter.L7, "results/soil/litter_mean_L7.csv")

# Canopy spectral CV (Asner)
canReflCV <- read_csv2("results/leaf/Diptero_canopyReflCV_Asner_digitize_full.csv")
canReflCV.L7 <- as_data_frame(my_spectralResampling(wL.hypers = canReflCV$y, wav.hypers = canReflCV$x))
write.csv2(canReflCV.L7, "results/leaf/canReflCV.L7.csv")

# Resampling to L7 sensor -------------------------------------------------
source("munge/spc_resample.R")    

# Leaf albedo
spc_resample(wL.DiptMean, "Albedo", "Wavelength", sensor = "Landsat7") %>% write.csv2("results/leaf/wL_DiptMean_L7.csv")
spc_resample(wL.Elaeis, "Albedo", "Wavelength", sensor = "Landsat7") %>% write.csv2("results/leaf/wL_Elaeis_L7.csv")
spc_resample(wL.HawaiiMean, "Albedo", "Wavelength", sensor = "Landsat7") %>% write.csv2("results/leaf/wL_HawaiiMean_L7.csv")
spc_resample(wL.GlobalMean, "Albedo", "Wavelength", sensor = "Landsat7") %>% write.csv2("results/leaf/wL_GlobalMean_L7.csv")
spc_resample(wL.ShoreaMean, "Albedo", "Wavelength", sensor = "Landsat7") %>% write.csv2("results/leaf/wL_ShoreaMean_L7.csv")
spc_resample(wL.Jacq96Mean, "Albedo", "Wavelength", sensor = "Landsat7") %>% write.csv2("results/leaf/wL_Jacq96Mean_L7.csv")
spc_resample(wL.Birch, "Birch", "Wavelength", sensor = "Landsat7") %>% write.csv2("results/leaf/wL_Birch_L7.csv")
spc_resample(wL.liana, "Albedo", "Wavelength", sensor = "Landsat7") %>% write.csv2("results/leaf/wL_Liana_L7.csv")
spc_resample(wL.DiptMean.addBias, "Albedo", "Wavelength", sensor = "Landsat7") %>% write.csv2("results/leaf/wL_DiptMean_addBias_L7.csv")


# understory reflectance
spc_resample(soil.spc.prim.mean, "Reflectance", "Wavelength", sensor = "Landsat7") %>%  write.csv2("results/soil/R_gr_prim_mean_L7.csv")
spc_resample(soil.spc.prim.min, "Reflectance", "Wavelength", sensor = "Landsat7") %>%  write.csv2("results/soil/R_gr_prim_min_L7.csv")
spc_resample(soil.spc.prim.max, "Reflectance", "Wavelength", sensor = "Landsat7") %>%  write.csv2("results/soil/R_gr_prim_max_L7.csv")

spc_resample(soil.spc.OP.mean, "Reflectance", "Wavelength", sensor = "Landsat7") %>%  write.csv2("results/soil/R_gr_oilpalm_mean_L7.csv")
spc_resample(soil.spc.OP.min, "Reflectance", "Wavelength", sensor = "Landsat7") %>%  write.csv2("results/soil/R_gr_oilpalm_min_L7.csv")
spc_resample(soil.spc.OP.max, "Reflectance", "Wavelength", sensor = "Landsat7") %>%  write.csv2("results/soil/R_gr_oilpalm_max_L7.csv")
spc_resample(soil.spc.OP.maxvis, "Reflectance", "Wavelength", sensor = "Landsat7") %>%  write.csv2("results/soil/R_gr_oilpalm_maxvis_L7.csv")
spc_resample(ustory.herbrich, "Reflectance", "Wavelength", sensor = "Landsat7") %>%  write.csv2("results/soil/R_gr_herbrich_L7.csv")

# Landsat BRF of plots with smallest ECC
gaps.spc.field.anc <- read_csv2("results/gaps_spc_field_anc.csv")
source("munge/convert_columns.R")
gaps.spc.field.anc <-  gaps.spc.field.anc %>%  mutate(ECC_my = (1 - Gaps1_my) * 100) # add ECC

gaps.spc.field.anc %>% arrange(LAIt_CIcc_my) %>% .[1,] %>%  View 



# Canopy spectral CV (Asner)
spc_resample(wL.DiptMean, "Albedo", "Wavelength", sensor = "Landsat7") %>% write.csv2("results/leaf/wL_DiptMean_L7.csv")





# Plot --------------------------------------------------------------------
wL_DiptMean_L7 <- read_csv2("results/leaf/wL_DiptMean_L7.csv")
wL_Elaeis_L7 <- read_csv2("results/leaf/wL_Elaeis_L7.csv")
wL_HawaiiMean_L7 <- read_csv2("results/leaf/wL_HawaiiMean_L7.csv")
wL_GlobalMean_L7 <- read_csv2("results/leaf/wL_GlobalMean_L7.csv")
wL_ShoreaMean_L7 <- read_csv2("results/leaf/wL_ShoreaMean_L7.csv")
wL_Jacq96Mean_L7 <- read_csv2("results/leaf/wL_Jacq96Mean_L7.csv")
wL_Birch_L7 <- read_csv2("results/leaf/wL_Birch_L7.csv")
wL_Liana_L7 <- read_csv2("results/leaf/wL_Liana_L7.csv")
wL_DiptMean_addBias_L7 <- read_csv2("results/leaf/wL_DiptMean_addBias_L7.csv")
stem.L7 <- read_csv2("results/leaf/stem_mean_L7.csv")



R_gr_prim_mean_L7 <- read_csv2("results/soil/R_gr_prim_mean_L7.csv")
R_gr_prim_min_L7 <- read_csv2("results/soil/R_gr_prim_min_L7.csv")
R_gr_prim_max_L7 <- read_csv2("results/soil/R_gr_prim_max_L7.csv")
R_gr_oilpalm_mean_L7 <- read_csv2("results/soil/R_gr_oilpalm_mean_L7.csv")
R_gr_oilpalm_min_L7 <- read_csv2("results/soil/R_gr_oilpalm_min_L7.csv")
R_gr_oilpalm_max_L7 <- read_csv2("results/soil/R_gr_oilpalm_max_L7.csv")
R_gr_oilpalm_maxvis_L7 <- read_csv2("results/soil/R_gr_oilpalm_maxvis_L7.csv")
R_gr_herbrich_L7 <- read_csv2("results/soil/R_gr_herbrich_L7.csv")
litter.L7 <- read_csv2("results/soil/litter_mean_L7.csv")


L7.wav <- c(483, 560, 662, 835, 1648, 2206)                                     # ETM+ bands centers From Chander et al. (2009)



pdf("graphs/wL_Rg.pdf")

par(oma = c(0,0,0,0), mai = c(0.7,0.7,0.2,0.2),                  
    ps = 8, cex = 1, mgp = c(1.8, 0.5, 0), mfrow=c(2,1),                 # Column 1 wL, column 2 Rg
    xaxs="i", yaxs="i")                  

plot(L7.wav, wL_DiptMean_L7[1,-1], type='o', xlab="Waveband (nm)",         # Leaf albedo
     xaxp=c(400,2400,10), xlim=c(450,2300),
     ylab="Leaf albedo", ylim=c(0,1), main="", col="blue", lwd=2)
lines(L7.wav, wL_Elaeis_L7[1,-1], lwd=2, col="magenta", type='o')
lines(L7.wav, wL_HawaiiMean_L7[1,-1], lwd=1, col="dark orange", type='o')
lines(L7.wav, wL_GlobalMean_L7[1,-1], lwd=1, col="grey20", type='o')
lines(L7.wav, wL_ShoreaMean_L7[1,-1], lwd=1, col="green", type='o')
lines(L7.wav, wL_Jacq96Mean_L7[1,-1], lwd=1, col="brown", type='o')
lines(L7.wav, wL_Birch_L7[1,-1], lwd=1, col="cyan", type='o')

legend("topright", c("Diptero.", "Elae.", "Hawaii", "Global", "Shorea", "Jacq96", "Birch"),
       lty=rep(1,7), lwd=c(2,2,1,1,1,1,1), bty='n',
       col=c("blue", "magenta", "dark orange", "grey20", "green", "brown", "cyan"))


plot(L7.wav, R_gr_prim_mean_L7[1,-1], xlab="Waveband (nm)",                 # Soil reflectance
     xaxp=c(400,2400,10), xlim=c(450,2300),
     ylab="Soil reflectance", ylim=c(0,1), main="",
     lwd=2, col="blue", type='o')
lines(L7.wav, R_gr_oilpalm_mean_L7[1,-1], lwd=2, col="magenta", type='o')
lines(L7.wav, R_gr_prim_min_L7[1,-1], lwd=1, col="dark orange", type='o')
lines(L7.wav, R_gr_prim_max_L7[1,-1], lwd=1, col="grey20", type='o')
lines(L7.wav, R_gr_oilpalm_min_L7[1,-1], lwd=1, col="green", type='o')
lines(L7.wav, R_gr_oilpalm_max_L7[1,-1], lwd=1, col="brown", type='o')
lines(L7.wav, R_gr_oilpalm_maxvis_L7[1,-1], lwd=1, col="cyan", type='o')
lines(L7.wav, R_gr_herbrich_L7[1,-1], lwd=2, col="purple", type='o')

lines(L7.wav, soil.interc.forest[1,-1], lwd=2, col="black", type='o', lty=2)    # Add soil estimated from lm(BRF~ECC)
lines(L7.wav, soil.interc.OP[1,-1], lwd=2, col="grey50", type='o', lty=2)

lines(L7.wav, litter.L7, lwd=2, col="red", type='o', lty = 2)            # Add litter

legend("topright", c("Primary mean", "Oil palm mean", "Primary min", "Primary max", "Oil palm min", "Oil palm max", "Oil palm maxvis", "Boreal herb-rich", "Estimated forest", "Estimated oil palm", "Litter"),
       lty=c(rep(1,8),2,2,2), lwd=c(2,2,1,1,1,1,1,2,2,2,2), bty='n',
       col=c("blue", "magenta", "dark orange", "grey20", "green", "brown", "cyan", "purple",
             "black", "grey50", "red"))

dev.off()


# wL & Rg [FOR MANUSCRIPT] ------------------------------------------------
pdf("graphs/FINAL_for_manuscript/wL_update_noStem.pdf", width = 3.5, height = 3, pointsize = 10)
# pdf("graphs/FINAL_for_manuscript/wL_update.pdf", width = 3.5, height = 3, pointsize = 10)
# pdf("graphs/FINAL_for_manuscript/wL_update_addliana.pdf", width = 3.5, height = 3, pointsize = 10)

# par(oma = c(0,0,0,0), mai = c(0.7,0.7,0.2,0.2),                  
#     ps = 10, cex = 1, mgp = c(1.8, 0.5, 0), mfrow=c(2,1),                 # Column 1 wL, column 2 Rg
#     xaxs="i", yaxs="i")  

# par(oma = c(0,0,0,0), mai = c(0.45,0.35,0.1,0.1),                         # Legend inside plotting area
#     ps = 8, mgp = c(1.3, 0.5, 0), mfrow=c(2,1))

# par(oma = c(0,0,0,0), mai = c(0.45,0.35,0.1,0.1),                          # Legend outside plotting area
#     ps = 8, mgp = c(1.7, 0.5, 0), mfrow=c(1,1), mar = c(2.7,2.5,3,0.5))

# No stem
par(oma = c(0,0,0,0), mai = c(0.45,0.35,0.1,0.1),                          # Legend outside plotting area
    ps = 8, mgp = c(1.7, 0.5, 0), mfrow=c(1,1), mar = c(2.7,2.5,1.5,0.5))

plot(L7.wav, wL_DiptMean_L7[1,-1], type='o', xlab="Wavelength (nm)",         # Leaf albedo
     xaxp=c(400,2400,10), xlim=c(450,2300),
     ylab="Albedo, Reflectance", ylim=c(0,1), main="", col="black", lwd=2, lty=1, pch=0)
# lines(L7.wav, wL_ShoreaMean_L7[1,-1], lwd=2, col="grey30", type='o', lty=1, pch=2)
lines(L7.wav, wL_HawaiiMean_L7[1,-1], lwd=2, col="grey50", type='o', lty=1, pch=1)
# lines(L7.wav, stem.L7[1,-1], lwd=1, col="grey50", type='o', lty=2, pch=4)
# lines(L7.wav, wL_Liana_L7[1,-1], lwd=1, col="red", type='o', lty=2, pch=4)



# legend(1000, 1.1, c("Family Diptero. leaf", "Genus Shorea leaf", "Tropical Hawaii* leaf", "Woody stem**"),    # Legend inside plotting area
#        lty=c(rep(1,3), 2), lwd=c(rep(2,3),1), pch=c(0,2,1,4), bty='n', ncol=2,
#        col=c("black", "grey30", "grey50", "grey50"))

# par(oma = c(0,0,0,0), mar = c(0,0,0,0), new = TRUE)
# plot(0,0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
# legend("top", c("Dipterocarpaceae leaf     ", "Tropical Hawaii leaf", "Woody stem"),      # legend outside plotting area
#        lty=c(rep(1,2), 2), lwd=c(rep(2,2),1), pch=c(0,1,4), bty='n', ncol=2,
#        col=c("black", "grey50", "grey50"),
#        inset = c(0,-0.001), x.intersp = 0.5)

# No stem
par(oma = c(0,0,0,0), mar = c(0,0,0,0), new = TRUE)
plot(0,0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("top", c("Dipterocarpaceae leaf     ", "Tropical Hawaii leaf"),      # legend outside plotting area
       lty=rep(1,2), lwd=rep(2,2), pch=c(0,1), bty='n', ncol=2,
       col=c("black", "grey50"),
       inset = c(0,-0.01), x.intersp = 0.5)

dev.off()

#####################

pdf("graphs/FINAL_for_manuscript/Rg_update_noLitter.pdf", width = 3.5, height = 2.75, pointsize = 10)    
# pdf("graphs/FINAL_for_manuscript/Rg_update.pdf", width = 3.5, height = 2.75, pointsize = 10)                     # Separate wL & Rg

# par(oma = c(0,0,0,0), mai = c(0.45,0.35,0.1,0.1),                          # Legend outside plotting area
#     ps = 8, mgp = c(1.7, 0.5, 0), mfrow=c(1,1), mar = c(2.7,2.5,1.5,0.5))

# No litter
par(oma = c(0,0,0,0), mai = c(0.45,0.35,0.1,0.1),                          # Legend outside plotting area
    ps = 8, mgp = c(1.7, 0.5, 0), mfrow=c(1,1), mar = c(2.7,2.5,0.5,0.5))

plot(L7.wav, R_gr_prim_mean_L7[1,-1], xlab="Wavelength (nm)",                 # Soil reflectance
     xaxp=c(400,2400,10), xlim=c(450,2300),
     ylab="Reflectance", ylim=c(0,1), main="",
     lty=1, lwd=2, col="black", type='o', pch=1)
lines(L7.wav, R_gr_prim_min_L7[1,-1], lwd=1, col="black", type='o', lty=2, pch=1)
lines(L7.wav, R_gr_prim_max_L7[1,-1], lwd=1, col="black", type='o', lty=2, pch=1)
# lines(L7.wav, litter.L7[1,-1], lwd=2, col="grey50", type='o', lty = 1, pch=0)            # Add litter

# legend(1500, 1.1, c("Soil", "Litter***"),                # legend inside plotting area
#        lty=c(1,1), lwd=c(2,2), bty='n', pch=c(1,0),
#        col=c("black", "grey50"))

par(oma = c(0,0,0,0), mar = c(0,0,0,0), new = TRUE)
plot(0,0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
# No litter no need legend
# legend("top", c("Soil      ", "Litter"),                  # legend outside plotting area
#        lty=c(1,1), lwd=c(2,2), bty='n', pch=c(1,0), ncol = 2,
#        col=c("black", "grey50"),
#        inset = c(0,-0.01), x.intersp = 0.5)

dev.off()








# Plot Elaeis leaf reflectance --------------------------------------------
oilpalm.leafR <- spc_resample(wL.Elaeis, "Reflectance", "Wavelength", sensor = "Landsat7")
Dipt.leafR <- spc_resample(wL.DiptMean, "Reflectance", "Wavelength", sensor = "Landsat7") 

pdf("graphs/R_Elaeis_DiptMean.pdf")
par(oma = c(0,0,0,0), mai = c(0.7,0.7,0.2,0.2),                  
    ps = 8, cex = 1, mgp = c(1.8, 0.5, 0),            
    xaxs="i", yaxs="i")  
plot(L7.wav, oilpalm.leafR[1,], type='o', xlab="Waveband (nm)",         # Leaf reflectance
     xaxp=c(400,2400,10), xlim=c(450,2300),
     ylab="Leaf reflectance", ylim=c(0,1), main="PROSPECT", col="blue", lwd=2)
lines(L7.wav, Dipt.leafR[1,], lwd=1, col="magenta", type='o')
legend("topright", c("Elaeis", "Diptero."), lty = c(1,1), lwd = c(2,1), bty = "n", col = c("blue", "magenta"))
dev.off()




# Plot leaf albedo sampling -----------------------------------------------
source("tests/prospect_summary.R")                                        # Import functions to (1) summarize prospect simulations
                                                                          # (2) resample (1) to ETM+ wavelengths

wL.Dipt2500 <- readRDS("results/leaf/leaf_sim_Dipt2500.rds")

wL.Dipt2500.addN <- readRDS("results/leaf/leaf_sim_Dipt2500mean_HAWAIIminmax_globcor_addN.rds")

wL.Dipt2500.summary <- wL.samples.summary(wL.Dipt2500)                    # Apply the function
wL.Dipt2500.summary.resample <- wL.summary.resample(wL.Dipt2500.summary)

wL.Dipt2500.summary.addN <- wL.samples.summary(wL.Dipt2500.addN)                    # Apply the function
wL.Dipt2500.summary.resample.addN <- wL.summary.resample(wL.Dipt2500.summary.addN)

pdf("graphs/wL_Diptero_2500.pdf")

ggplot(wL.Dipt2500.summary.resample, aes(x = wav, y = wL.mean)) + 
  geom_errorbar(aes(ymin = wL.min , ymax = wL.max), colour = "magenta") + 
  geom_errorbar(aes(ymin = wL.meanminsd, ymax = wL.meanplussd), colour = "blue") +
  geom_line() + geom_point(cex = 2) + 
  scale_y_continuous(name="Simulated wL (n = 2500)", limits=c(0,1), breaks=seq(0,1,0.2), expand=c(0,0)) + 
  scale_x_continuous(name="Wavelength (nm)", limits=c(400,2300), breaks=seq(400,2300,200), expand=c(0,0)) +
  theme_bw(base_size = 12) + # base_family
  theme(axis.line = element_line(colour = "black", size=0.7), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), panel.background = element_blank(), 
        axis.text=element_text(size=14), axis.title.x=element_text(size=14,face="bold"), # size = 7; size = 8
        axis.title.y=element_text(size=14,face="bold"), # size = 8
        aspect.ratio=1, legend.text=element_text(size=14), legend.title=element_text(size=14), # size = 6
        legend.background=element_rect(fill="transparent"),
        legend.position="topright") + guides(colour = guide_legend(override.aes = list(size=1.5))) 

dev.off()

  


# Plot leaf albedo samplingn with PROSPECT noise -----------------------------------------------
# (2) resample (1) to ETM+ wavelengths

temp <- readRDS("results/leaf/leaf_sim_Dipt2500_addprospnoise.rds")
wL.Dipt2500.prospErr <- temp[c("Wavelength", "Refl_prosp.bias.sepc", "Tran_prosp.bias.sepc", "Albedo_prosp.bias.sepc")]                           
names(wL.Dipt2500.prospErr) <- c("Wavelength", "Reflectance", "Transmittance", "Albedo")     # Rename column for function

wL.Dipt2500.prospErr.summary <- wL.samples.summary(wL.Dipt2500.prospErr)                    # Apply the function
wL.Dipt2500.prospErr.summary.resample <- wL.summary.resample(wL.Dipt2500.prospErr.summary)

pdf("graphs/wL_Diptero_2500_prospErr.pdf")

ggplot(wL.Dipt2500.prospErr.summary.resample, aes(x = wav, y = wL.mean)) + 
  geom_errorbar(aes(ymin = wL.min , ymax = wL.max), colour = "magenta") + 
  geom_errorbar(aes(ymin = wL.meanminsd, ymax = wL.meanplussd), colour = "blue") +
  geom_line() + geom_point(cex = 2) + 
  scale_y_continuous(name="Simulated wL (n = 2500)", limits=c(0,1.1), breaks=seq(0,1.1,0.2), expand=c(0,0)) + 
  scale_x_continuous(name="Wavelength (nm)", limits=c(400,2300), breaks=seq(400,2300,200), expand=c(0,0)) +
  theme_bw(base_size = 12) + # base_family
  theme(axis.line = element_line(colour = "black", size=0.7), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), panel.background = element_blank(), 
        axis.text=element_text(size=14), axis.title.x=element_text(size=14,face="bold"), # size = 7; size = 8
        axis.title.y=element_text(size=14,face="bold"), # size = 8
        aspect.ratio=1, legend.text=element_text(size=14), legend.title=element_text(size=14), # size = 6
        legend.background=element_rect(fill="transparent"),
        legend.position="topright") + guides(colour = guide_legend(override.aes = list(size=1.5))) 

dev.off()

# Influence of N on PROSPECT sim ------------------------------------------
# First, run different N in PROSPECT_sim.R
pdf("graphs/N_effect_prospect_Dipt.pdf")

par(oma = c(0,0,0,0), mai = c(0.7,0.7,0.2,0.2),                  
    ps = 8, cex = 1, mgp = c(1.8, 0.5, 0), mfrow=c(2,2),                 # Column 1 wL, column 2 Rg
    xaxs="i", yaxs="i")                  

plot(L7.wav, wL_1[1,], type='o', xlab="Waveband (nm)",                        # Leaf albedo
     xaxp=c(400,2400,10), xlim=c(450,2300),
     ylab="Leaf albedo", ylim=c(0,1), main="", col="blue", lwd=1)
lines(L7.wav, wL_1.7[1,], lwd=2, col="magenta", type='o')
lines(L7.wav, wL_2[1,], lwd=1, col="dark orange", type='o')
lines(L7.wav, wL_2.5[1,], lwd=1, col="green", type='o')
lines(L7.wav, wL_3[1,], lwd=1, col="brown", type='o')

legend("topright", c("N = 1", "N = 1.7", "N = 2", "N = 2.5", "N = 3"),
       lty=rep(1,5), lwd=c(1,2,1,1,1), bty='n',
       col=c("blue", "magenta", "dark orange","green", "brown"))

plot(L7.wav, R_1[1,], type='o', xlab="Waveband (nm)",                        # Leaf reflectance
     xaxp=c(400,2400,10), xlim=c(450,2300),
     ylab="Leaf reflectance", ylim=c(0,1), main="", col="blue", lwd=1)
lines(L7.wav, R_1.7[1,], lwd=2, col="magenta", type='o')
lines(L7.wav, R_2[1,], lwd=1, col="dark orange", type='o')
lines(L7.wav, R_2.5[1,], lwd=1, col="green", type='o')
lines(L7.wav, R_3[1,], lwd=1, col="brown", type='o')

legend("topright", c("N = 1", "N = 1.7", "N = 2", "N = 2.5", "N = 3"),
       lty=rep(1,5), lwd=c(1,2,1,1,1), bty='n',
       col=c("blue", "magenta", "dark orange","green", "brown"))


plot(L7.wav, T_1[1,], type='o', xlab="Waveband (nm)",                        # Leaf transmittance
     xaxp=c(400,2400,10), xlim=c(450,2300),
     ylab="Leaf transmittance", ylim=c(0,1), main="", col="blue", lwd=1)
lines(L7.wav, T_1.7[1,], lwd=2, col="magenta", type='o')
lines(L7.wav, T_2[1,], lwd=1, col="dark orange", type='o')
lines(L7.wav, T_2.5[1,], lwd=1, col="green", type='o')
lines(L7.wav, T_3[1,], lwd=1, col="brown", type='o')

legend("topright", c("N = 1", "N = 1.7", "N = 2", "N = 2.5", "N = 3"),
       lty=rep(1,5), lwd=c(1,2,1,1,1), bty='n',
       col=c("blue", "magenta", "dark orange","green", "brown"))

dev.off()


# PROSPECT-4 vs PROSPECT-5 ------------------------------------------------
# First, run PROSPECT-4 and PROSPECT-5 sims in PROSPECT_sim.R
pdf("graphs/prosp5_RvsMatlab_Dipt.pdf")                                      #### Check R Prospect-5 code: OK, the same

par(oma = c(0,0,0,0), mai = c(0.7,0.7,0.2,0.2),                  
    ps = 8, cex = 1, mgp = c(1.8, 0.5, 0), mfrow=c(2,2),                 # Column 1 wL, column 2 Rg
    xaxs="i", yaxs="i")                  

plot(L7.wav, R_v5[1,], type='o', xlab="Waveband (nm)",                        # Leaf reflectance
     xaxp=c(400,2400,10), xlim=c(450,2300),
     ylab="Leaf reflectance", ylim=c(0,1), main="", col="blue", lwd=1)
lines(L7.wav, R_v5_matlab[1,], lwd=1, col="dark orange", type='o')
legend("topright", c("R code", "Matlab code"),
       lty=rep(1,2), lwd=c(1,1), bty='n',
       col=c("blue", "dark orange"))

plot(L7.wav, T_v5[1,], type='o', xlab="Waveband (nm)",                        # Leaf transmittance
     xaxp=c(400,2400,10), xlim=c(450,2300),
     ylab="Leaf transmittance", ylim=c(0,1), main="", col="blue", lwd=1)
lines(L7.wav, T_v5_matlab[1,], lwd=1, col="dark orange", type='o')
legend("topright", c("R code", "Matlab code"),
       lty=rep(1,2), lwd=c(1,1), bty='n',
       col=c("blue", "dark orange"))

plot(L7.wav, wL_v5[1,], type='o', xlab="Waveband (nm)",                        # Leaf albedo
     xaxp=c(400,2400,10), xlim=c(450,2300),
     ylab="Leaf albedo", ylim=c(0,1), main="", col="blue", lwd=1)
lines(L7.wav, wL_v5_matlab[1,], lwd=1, col="dark orange", type='o')
legend("topright", c("R code", "Matlab code"),
       lty=rep(1,2), lwd=c(1,1), bty='n',
       col=c("blue", "dark orange"))

dev.off()

pdf("graphs/prosp4_RvsMatlab_Dipt.pdf")                                      #### Check R Prospect-4 code : OK, the same

par(oma = c(0,0,0,0), mai = c(0.7,0.7,0.2,0.2),                  
    ps = 8, cex = 1, mgp = c(1.8, 0.4, 0), mfrow=c(2,2),                 # Column 1 wL, column 2 Rg
    xaxs="i", yaxs="i")                  

plot(L7.wav, R_v4[1,], type='o', xlab="Waveband (nm)",                        # Leaf reflectance
     xaxp=c(400,2400,10), xlim=c(440,2300),
     ylab="Leaf reflectance", ylim=c(0,1), main="", col="blue", lwd=1)
lines(L7.wav, R_v4_matlab[1,], lwd=1, col="dark orange", type='o')
legend("topright", c("R code", "Matlab code"),
       lty=rep(1,2), lwd=c(1,1), bty='n',
       col=c("blue", "dark orange"))

plot(L7.wav, T_v4[1,], type='o', xlab="Waveband (nm)",                        # Leaf transmittance
     xaxp=c(400,2400,10), xlim=c(440,2300),
     ylab="Leaf transmittance", ylim=c(0,1), main="", col="blue", lwd=1)
lines(L7.wav, T_v4_matlab[1,], lwd=1, col="dark orange", type='o')
legend("topright", c("R code", "Matlab code"),
       lty=rep(1,2), lwd=c(1,1), bty='n',
       col=c("blue", "dark orange"))

plot(L7.wav, wL_v4[1,], type='o', xlab="Waveband (nm)",                        # Leaf albedo
     xaxp=c(400,2400,10), xlim=c(440,2300),
     ylab="Leaf albedo", ylim=c(0,1), main="", col="blue", lwd=1)
lines(L7.wav, wL_v4_matlab[1,], lwd=1, col="dark orange", type='o')
legend("topright", c("R code", "Matlab code"),
       lty=rep(1,2), lwd=c(1,1), bty='n',
       col=c("blue", "dark orange"))

dev.off()


# Plot PROSPECT-4 vs PROSPECT-5

pdf("graphs/prosp4vsprosp5_Dipt.pdf")                                      #### PROSPECT-4 vs PROSPECT-5 : no difference

par(oma = c(0,0,0,0), mai = c(0.7,0.7,0.2,0.2),                  
    ps = 8, cex = 1, mgp = c(1.8, 0.5, 0), mfrow=c(2,2),                 # Column 1 wL, column 2 Rg
    xaxs="i", yaxs="i")                  

plot(L7.wav, R_v4[1,], type='o', xlab="Waveband (nm)",                        # Leaf reflectance
     xaxp=c(400,2400,10), xlim=c(450,2300),
     ylab="Leaf reflectance", ylim=c(0,1), main="", col="blue", lwd=1)
lines(L7.wav, R_v5[1,], lwd=1, col="dark orange", type='o')
legend("topright", c("PROSPECT-4", "PROSPECT-5"),
       lty=rep(1,2), lwd=c(1,1), bty='n',
       col=c("blue", "dark orange"))

plot(L7.wav, T_v4[1,], type='o', xlab="Waveband (nm)",                        # Leaf transmittance
     xaxp=c(400,2400,10), xlim=c(450,2300),
     ylab="Leaf transmittance", ylim=c(0,1), main="", col="blue", lwd=1)
lines(L7.wav, T_v5[1,], lwd=1, col="dark orange", type='o')
legend("topright", c("PROSPECT-4", "PROSPECT-5"),
       lty=rep(1,2), lwd=c(1,1), bty='n',
       col=c("blue", "dark orange"))

plot(L7.wav, wL_v4[1,], type='o', xlab="Waveband (nm)",                        # Leaf albedo
     xaxp=c(400,2400,10), xlim=c(450,2300),
     ylab="Leaf albedo", ylim=c(0,1), main="", col="blue", lwd=1)
lines(L7.wav, wL_v5[1,], lwd=1, col="dark orange", type='o')
legend("topright", c("PROSPECT-4", "PROSPECT-5"),
       lty=rep(1,2), lwd=c(1,1), bty='n',
       col=c("blue", "dark orange"))

dev.off()




# Spectral absorption coefficients in PROSPECT ----------------------------
spc_abs_P4 <- read_csv("data/leaf/spc_abs_P4.csv", col_names = FALSE)
colnames(spc_abs_P4) <- c("Wavelength", "Refr.idx", "Cab.abs", "Car.abs", "Cw.abs", "Cm.abs")

spc_abs_P5B <- read_csv("data/leaf/spc_abs_P5B.csv", col_names = FALSE)
colnames(spc_abs_P5B) <- c("Wavelength", "Refr.idx", "Cab.abs", "Car.abs", "Bp.abs", "Cw.abs", "Cm.abs")

# Plot
pdf("graphs/PROSPECT_absorption_coefficients.pdf")

par(mar = c(5,5,2,5), ps = 16)
plot(spc_abs_P4$Wavelength, spc_abs_P4$Cab.abs, ylim = c(0, 0.35), type = 'l', lwd = 3, col = "magenta", 
     xlab = "Wavelength (nm)", ylab = "Absorption coefficient (pigments)", xaxp = c(400,2400,10), yaxp = c(0,0.4,8))
lines(spc_abs_P5B$Wavelength, spc_abs_P5B$Cab.abs, col = "blue", lwd = 2)
lines(spc_abs_P5B$Wavelength, spc_abs_P5B$Car.abs, col = "chartreuse4", lwd = 2)
 
par(new = T)                                                                                      # Add secondary y-axis
plot(spc_abs_P4$Wavelength, spc_abs_P4$Cw.abs, type = 'l', lwd = 2, lty = 1, col = "dark orange", 
     xlab = NA, ylab = NA, axes = FALSE)
lines(spc_abs_P4$Wavelength, spc_abs_P4$Cm.abs, lwd = 2, lty = 1, col = "cyan3")

# Add Landsat-7 bands
rect.fill <- adjustcolor("grey", alpha.f = 0.4)
rect.fill.b1 <- adjustcolor("grey", alpha.f = 0.2)
rect(450, 0, 520, 140, border = "transparent", col = rect.fill.b1)
rect(520, 0, 600, 140, border = "transparent", col = rect.fill)
rect(630, 0, 690, 140, border = "transparent", col = rect.fill)
rect(770, 0, 900, 140, border = "transparent", col = rect.fill)
rect(1550, 0, 1750, 140, border = "transparent", col = rect.fill)
rect(2090, 0, 2350, 140, border = "transparent", col = rect.fill)
# Add 710-790nm DASF retrieval wavelength
rect(710, 0, 790, 140, border = "grey", density = 30, col = "grey")

axis(side = 4)                                                                                   # Label to 2nd y-axis
mtext(side = 4, line = 3, "Absorption coefficient (water, dry matter)")
legend(860, 125, legend = c("Cab (P4)", "Cab (P5)", "Car (P5)", "Cw (P4, P5)", "Cm (P4, P5)", "ETM+ channels"), 
       lty = c(1,1,1,1,1,1), lwd = c(2,2,2,2,2,7), col = c("magenta", "blue", "chartreuse4", "dark orange", "cyan3", "grey"))

dev.off()




# FOR MANUSCRIPT: plot leaf albedo sampling --------------------------------

# Plot leaf albedo sampling -----------------------------------------------
source("tests/prospect_summary.R")                                        # Import functions to (1) summarize prospect simulations
# (2) resample (1) to ETM+ wavelengths

wL.Dipt2500 <- readRDS("results/leaf/leaf_sim_Dipt2500mean_HAWAIIminmax_globcor.rds")
wL.Dipt2500.summary <- wL.samples.summary(wL.Dipt2500)                    # Apply the function
wL.Dipt2500.summary.resample <- wL.summary.resample(wL.Dipt2500.summary)

pdf("graphs/FINAL_for_manuscript/wL_Diptero_2500_final_mean.pdf", width = 3, height = 2.5, pointsize = 8)

ggplot(wL.Dipt2500.summary.resample, aes(x = wav, y = wL.mean)) +
  geom_errorbar(aes(ymin = wL.min , ymax = wL.max), colour = "grey60") +
  geom_errorbar(aes(ymin = wL.meanminsd, ymax = wL.meanplussd), colour = "grey30") +
  geom_line(colour = "grey70", linetype = 2) + geom_point(shape = 16, cex = 2) +
  scale_y_continuous(name="Modelled leaf albedo", limits=c(0,1), breaks=seq(0,1,0.2), expand=c(0,0)) +
  scale_x_continuous(name="Wavelength (nm)", limits=c(400,2300), breaks=seq(400,2300,200), expand=c(0,0)) +
  theme_bw(base_size = 8, base_family = "Helvetica") +
  theme(axis.line = element_line(colour = "black"), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), panel.background = element_blank(),
        axis.text=element_text(size=7), axis.title.x=element_text(size=8,face="bold"), # size = 7; size = 8
        axis.title.y=element_text(size=8,face="bold"), # size = 8
        legend.text=element_text(size=7), legend.title=element_text(size=7), # size = 6
        legend.background=element_rect(fill="transparent"),
        legend.position="none")
+ guides(colour = guide_legend(override.aes = list(size=1.5)))

# ggplot(wL.Dipt2500.summary.resample, aes(x = wav, y = wL.median)) +                                                         # median
#   geom_errorbar(aes(ymin = wL.min , ymax = wL.max), colour = "grey60") + 
#   geom_errorbar(aes(ymin = wL.medianminsd, ymax = wL.medianplussd), colour = "grey30") +
#   geom_line(colour = "grey70", linetype = 2) + geom_point(shape = 16, cex = 2) + 
#   scale_y_continuous(name="Modelled leaf albedo", limits=c(0,1), breaks=seq(0,1,0.2), expand=c(0,0)) + 
#   scale_x_continuous(name="Wavelength (nm)", limits=c(400,2300), breaks=seq(400,2300,200), expand=c(0,0)) +
#   theme_bw(base_size = 8, base_family = "Helvetica") +
#   theme(axis.line = element_line(colour = "black"), panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(), panel.background = element_blank(), 
#         axis.text=element_text(size=7), axis.title.x=element_text(size=8,face="bold"), # size = 7; size = 8
#         axis.title.y=element_text(size=8,face="bold"), # size = 8
#         legend.text=element_text(size=7), legend.title=element_text(size=7), # size = 6
#         legend.background=element_rect(fill="transparent"),
#         legend.position="none")

  
dev.off()
