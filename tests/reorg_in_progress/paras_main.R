source("cache/libPaths.R")
require(xlsx)
require(tidyverse)

# Input A: gaps, spectra, and field data ----------------------------------------------------------
gaps.spc.field.anc <- read_csv2("results/gaps_spc_field_anc.csv")
source("munge/convert_columns.R")


# Add image SZA -----------------------------------------------------------
source("munge/add_img_sza.R")
gaps.spc.field.anc <- add_img_sza(gaps.spc.field.anc)

# Recode forest quality column --------------------------------------------
gaps.spc.field.anc <- mutate(gaps.spc.field.anc, scene.Date = as.character(scene.Date))
gaps.spc.field.anc <- gaps.spc.field.anc %>% 
  mutate(ForestQuality = 
           dplyr::if_else(ForestQuality == "1", "Very poor", 
                          dplyr::if_else(ForestQuality == "2", "Poor", 
                                         dplyr::if_else(ForestQuality == "3", "OK",
                                                        dplyr::if_else(ForestQuality == "4", "Good", "Very good")))))


# Recode forest type
gaps.spc.field.anc <- gaps.spc.field.anc %>% 
  mutate(Type.new = dplyr::if_else(Type == "Salvage Logged" & ForestQuality %in% c("Very poor", "Poor"),
                                   "Salvage Logged stage 2", Type))

gaps.spc.field.anc <- gaps.spc.field.anc %>% 
  mutate(Type.new = dplyr::if_else(Type == "Salvage Logged" & !ForestQuality %in% c("Very poor", "Poor"),
                                   "Salvage Logged stage 1", Type.new))
# Level the forest type
gaps.spc.field.anc <- gaps.spc.field.anc %>% mutate(Type.new = factor(Type.new, 
                                    levels = c("Primary", "Lightly Logged", "Twice Logged", "Salvage Logged stage 1", "Salvage Logged stage 2", "Oil Palm")))


write.csv2(gaps.spc.field.anc, "results/gaps_spc_field_anc_processed_88.csv")


# Exclude 'poor' and 'very poor' forest quality plots ---------------------
rm.qual <- c("Poor", "Very poor")
gaps.spc.field.anc <- gaps.spc.field.anc %>%  dplyr::filter(!ForestQuality %in% rm.qual) # 65 plots








# Input B: Leaf single scattering albedo simulated with PROSPECT -------------------------------------------
wL.birch <- read_csv2("data/leaf/boreal_leaf_spc.csv") %>% select(Birch)
wL.DiptMean <- read_csv2("results/leaf/wL_DiptMean.csv")
wL.Elaeis <- read_csv2("results/leaf/wL_Elaeis.csv")
wL.HawaiiMean <- read_csv2("results/leaf/wL_HawaiiMean.csv")
wL.GlobalMean <- read_csv2("results/leaf/wL_GlobalMean.csv")
wL.ShoreaMean <- read_csv2("results/leaf/wL_ShoreaMean.csv")
wL.Jacq96Mean <- read_csv2("results/leaf/wL_Jacq96Mean.csv")

# Multivariate sampling
wL.Dipt2500 <- readRDS("results/leaf/leaf_sim_Dipt2500mean_HAWAIIminmax_globcor.rds")     # Final realistic range

# wL.Dipt2500.prospErr <- readRDS("results/leaf/leaf_sim_Dipt2500_addprospnoise.rds")        # With PROSPECT bias and SEPC

wL.Dipt2500.addN <- readRDS("results/leaf/leaf_sim_Dipt2500mean_HAWAIIminmax_globcor_addN.rds")     # Final realistic range


# Input C: Understory reflectance -----------------------------------------
soil.spc <- read_csv2("data/soil/soil_spc_sel.csv")
soil.wav <- seq(350, 2500, by=10)
soil.spc.prim <- t(soil.spc[soil.spc$Vegetation=="Primary, Central Kalimantan, Bukit Raya Nature Reserve", 4:ncol(soil.spc)])
soil.spc.OP <- t(soil.spc[soil.spc$Vegetation=="Ecuador, forest cleared, now oil palm and pasture", 4:ncol(soil.spc)])
# Mean
soil.spc.prim.mean <- tibble(Wavelength = soil.wav, Reflectance = rowMeans(soil.spc.prim))
soil.spc.OP.mean <- tibble(Wavelength = soil.wav, Reflectance = rowMeans(soil.spc.oilpalm))
# Min & max
soil.spc.prim.min <- tibble(Wavelength = soil.wav, Reflectance =  soil.spc.prim[,1])
soil.spc.prim.max <- tibble(Wavelength = soil.wav, Reflectance = soil.spc.prim[,4])
soil.spc.OP.min <- tibble(Wavelength = soil.wav, Reflectance = soil.spc.oilpalm[,4])
soil.spc.OP.maxvis <- tibble(Wavelength = soil.wav, Reflectance = soil.spc.oilpalm[,7])
soil.spc.OP.max <- tibble(Wavelength = soil.wav, Reflectance = soil.spc.oilpalm[,3])
  
# Boreal herb rich
ustory.herbrich <- read_csv2("data/soil/boreal_herbrich_spectra.csv")

# Soil estimated as intercept in BRF-ECC relation
soil.interc.forest <- read_csv2("data/soil/soil_interc_forest.csv")
soil.interc.OP <- read_csv2("data/soil/soil_interc_OP.csv")

# Litter from Asner (1998)
soil.litter <- read_csv2("results/soil/litter_L7.csv")






# Run PARAS BRF simulation ------------------------------------------------
source("tests/reorg_in_progress/paras_BRF.R")                            ###################### source the PARAS functions
# Run separately for different tree species


# Final Case --------------------------------------------------------------                                     # Final case
# Just simulate good quality forest plots
paras.lDipt.sPrim <- gaps.spc.field.anc %>% dplyr::filter(Biome == "Forest") %>% 
  paras_BRF(., wL_hypers = wL.DiptMean, R_gr_hypers = soil.spc.prim.mean, sensor = "Landsat7", new.q = FALSE)

paras.lDipt.sLitt <- gaps.spc.field.anc %>% dplyr::filter(Biome == "Forest") %>% 
  paras_BRF(., wL_hypers = wL.DiptMean, soil.L7 = soil.litter, sensor = "Landsat7", new.q = FALSE)


write.csv2(paras.lDipt.sPrim, "results/PARAS/remove_poor_verypoor/FinalCase_lDipt_sPrim_mean_goodforest.csv")
write.csv2(paras.lDipt.sLitt, "results/PARAS/remove_poor_verypoor/FinalCase_lDipt_sLitt_mean_goodforest.csv")


# Final wL sampling case --------------------------------------------------                                      # Final case

paras.lDipt.sPrim <- paras_run_sampling(wL.samples = wL.Dipt2500,
                                        forest.data = gaps.spc.field.anc %>% dplyr::filter(Biome == "Forest"),
                                        R_gr_hypers = soil.spc.prim.mean)

saveRDS(paras.lDipt.sPrim, "results/PARAS/remove_poor_verypoor/FinalCase_lDiptSampling_sPrim_mean_goodforest.rds")




# Case 1 -------------------------------------------------------------------
# Run 1: Forest plots with Dipterocarp leaf albedo & primary forest soil reflectance
paras.lDipt.sPrim <- gaps.spc.field.anc %>% dplyr::filter(Biome == "Forest") %>% 
  paras_BRF(., wL_hypers = wL.DiptMean, R_gr_hypers = soil.spc.prim.mean, sensor = "Landsat7", new.q = FALSE)

# Run 2: Oil palm plots with Elaeis leaf albedo & OP plantation soil
paras.lElae.sPalm <- gaps.spc.field.anc %>% dplyr::filter(Biome == "Plantation_Palm") %>% 
  paras_BRF(., wL_hypers = wL.Elaeis, R_gr_hypers = soil.spc.OP.mean, sensor = "Landsat7", new.q = FALSE)

# Combine forest and plantation plots
paras.case1 <- rbind(paras.lDipt.sPrim, paras.lElae.sPalm)
# Save
write.csv2(paras.case1, "results/PARAS/remove_poor_verypoor/Case1_DiptPrim_ElaePalm_mean.csv")




# Case 2 ------------------------------------------------------------------
# 2500 samples wL.Dipt2500 (column = sample)

paras.lDipt.sPrim <- paras_run_sampling(wL.samples = wL.Dipt2500,
          forest.data = gaps.spc.field.anc %>% dplyr::filter(Biome == "Forest"),
          R_gr_hypers = soil.spc.prim.mean)

saveRDS(paras.lDipt.sPrim, "results/PARAS/remove_poor_verypoor/Dipt2500.rds")

 

# Case 3     --------------------------------------------------------------
# Use Diptero leaf albedo for both forest and oil palm plots, and
# Diptero leaf reflectance for understory for both forest and oil palm plots
paras.case3 <- paras_BRF(gaps.spc.field.anc, wL_hypers = wL.DiptMean, R_gr_hypers = wL.DiptMean, sensor = "Landsat7")

write.csv2(paras.case3, "results/PARAS/Case3_DiptDiptr_DiptDiptr.csv")


# Case 4 ------------------------------------------------------------------
# Use Diptero leaf albedo for both forest and oil palm plots, and
# Boreal herb rich understory HDRF for understory for both forest and oil palm plots
paras.case4 <- paras_BRF(gaps.spc.field.anc, wL_hypers = wL.DiptMean, R_gr_hypers = ustory.herbrich, sensor = "Landsat7", new.q = FALSE)

write.csv2(paras.case4, "results/PARAS/remove_poor_verypoor/Case4_DiptHerb_DiptHerb.csv")

# Case 5 ------------------------------------------------------------------
# Use Hawaii leaf albedo for both forest and oil palm plots, and
# Just run black soil to check if the levels for high LAI plots are ok
paras.case5 <- paras_BRF(gaps.spc.field.anc, wL_hypers = wL.HawaiiMean, R_gr_hypers = ustory.herbrich, sensor = "Landsat7")

write.csv2(paras.case5, "results/PARAS/Case5_HawaiiHerb_HawaiiHerb.csv")


# Case 6 ------------------------------------------------------------------
# Similar to Case 1, but with minimum soil reflectance
# Run 1: Forest plots with Dipterocarp leaf albedo & primary forest soil reflectance
paras.lDipt.sPrim <- gaps.spc.field.anc %>% dplyr::filter(Biome == "Forest") %>% 
  paras_BRF(., wL_hypers = wL.DiptMean, R_gr_hypers = soil.spc.prim.min, sensor = "Landsat7")

# Run 2: Oil palm plots with Elaeis leaf albedo & OP plantation soil
paras.lElae.sPalm <- gaps.spc.field.anc %>% dplyr::filter(Biome == "Plantation_Palm") %>% 
  paras_BRF(., wL_hypers = wL.Elaeis, R_gr_hypers = soil.spc.OP.min, sensor = "Landsat7")

# Combine forest and plantation plots
paras.case6 <- rbind(paras.lDipt.sPrim, paras.lElae.sPalm)
# Save
write.csv2(paras.case6, "results/PARAS/Case6_DiptPrim_ElaePalm_soilMin.csv")


# Case 7 ------------------------------------------------------------------
# Case 1 but run with p = 1 - (iD_my / LAIt_uk) and LAIt_uk; so my input is only iD = 1 - DIFN
paras.res <- read_csv2("results/PARAS/Case1_DiptPrim_ElaePalm_mean.csv")                                 # iD is not yet in gaps.spc.field.anc

# Run 1: Forest plots with Dipterocarp leaf albedo & primary forest soil reflectance
paras.lDipt.sPrim <- paras.res %>% mutate(p = 1 - (1-DIFN)/LAI_true_v6, LAIt_CIcc_my = LAI_true_v6) %>% # temporarily change the p and LAIt values 
  dplyr::filter(Biome == "Forest") %>% 
  paras_BRF(., wL_hypers = wL.DiptMean, R_gr_hypers = soil.spc.prim.mean, sensor = "Landsat7")

# Run 2: Oil palm plots with Elaeis leaf albedo & OP plantation soil
paras.lElae.sPalm <- paras.res %>% mutate(p = 1 - (1-DIFN)/LAI_true_v6, LAIt_CIcc_my = LAI_true_v6) %>% # temporarily change the p and LAIt values 
  dplyr::filter(Biome == "Plantation_Palm") %>% 
  paras_BRF(., wL_hypers = wL.Elaeis, R_gr_hypers = soil.spc.OP.mean, sensor = "Landsat7")

# Combine forest and plantation plots
paras.case7 <- rbind(paras.lDipt.sPrim, paras.lElae.sPalm)
# Save
write.csv2(paras.case7, "results/PARAS/Case7_case1_pUK_LAItUK.csv")


# Case 8 ------------------------------------------------------------------
# Case 4 but with 2500 wL samples
paras.case8 <- paras_run_sampling(wL.samples = wL.Dipt2500,
          forest.data = gaps.spc.field.anc, R_gr_hypers = ustory.herbrich)

saveRDS(paras.case8, "results/PARAS/Case8_case4with2500samples.rds")

# Case 9 -------------------------------------------------------------------
# Case 1, but with herb rich understory for both OP and forest
# Run 1: Forest plots with Dipterocarp leaf albedo & primary forest soil reflectance
paras.lDipt.sPrim <- gaps.spc.field.anc %>% dplyr::filter(Biome == "Forest") %>% 
  paras_BRF(., wL_hypers = wL.DiptMean, R_gr_hypers = ustory.herbrich, sensor = "Landsat7")

# Run 2: Oil palm plots with Elaeis leaf albedo & OP plantation soil
paras.lElae.sPalm <- gaps.spc.field.anc %>% dplyr::filter(Biome == "Plantation_Palm") %>% 
  paras_BRF(., wL_hypers = wL.Elaeis, R_gr_hypers = ustory.herbrich, sensor = "Landsat7")

# Combine forest and plantation plots
paras.case9 <- rbind(paras.lDipt.sPrim, paras.lElae.sPalm)
# Save
write.csv2(paras.case9, "results/PARAS/Case9_case1_lDiptElae_sHerbrich.csv")



# Case 10 -----------------------------------------------------------------
# case 8, but also with additional expected noise from 
# (1) PROSPECT model
# (2) Surface reflectance product

# Check (1) first, named Case 10a
temp <- wL.Dipt2500.prospErr[c("Wavelength", "Albedo_prosp.bias.sepc")]
names(temp)[2] <- "Albedo"                                                     # Rename perturbed Albedo to "Albedo" for paras_BRF function

# paras.case10a <- paras_run_sampling(wL.samples = temp,
#                                   forest.data = gaps.spc.field.anc, R_gr_hypers = ustory.herbrich)
# 
# saveRDS(paras.case10a, "results/PARAS/Case10a_case8_prospErr.rds")

paras.case10a <- paras_run_sampling(wL.samples = temp,
                                    forest.data = gaps.spc.field.anc %>% dplyr::filter(Biome == "Forest"), 
                                    R_gr_hypers = soil.spc.prim.mean)

saveRDS(paras.case10a, "results/PARAS/remove_poor_verypoor/Case10a_case2_prospErr.rds")

# For (2), run (1) then add atmospheric noise (5% * SR + 0.005) to simulated ETM+ BRF
# For now just add the noise to the error bar in paras_summary.R (function paras.summary.addSRnoise)




# Case 11 -----------------------------------------------------------------
# Case 4 (best) but with p = 1 - iD/LAIt_UK
paras.case11 <- 
  paras_BRF_LAItUK(gaps.spc.field.anc, wL_hypers = wL.DiptMean, R_gr_hypers = ustory.herbrich, sensor = "Landsat7")
write.csv2(paras.case11, "results/PARAS/Case11_case4_withLAItUK.csv")


# Case 12 -----------------------------------------------------------------
# Arbitrary change of parameter one at a time
# Case 4 with Q + 20%
# For now, just change the paras_BRF code to get quick results
paras.case12 <- paras_BRF(gaps.spc.field.anc, wL_hypers = wL.DiptMean, R_gr_hypers = ustory.herbrich, sensor = "Landsat7", chg.Q = 0.2)

write.csv2(paras.case12, "results/PARAS/Case12_case4_Qplus20%.csv")


# Case 13 -----------------------------------------------------------------             # WRONG! DO NOT USE RESULT
# # CCse 4, but orrect LAIt for woody element 11% 
# paras.case13 <- paras_BRF(gaps.spc.field.anc, wL_hypers = wL.DiptMean, R_gr_hypers = ustory.herbrich, sensor = "Landsat7", wood = 0.11)
# 
# write.csv2(paras.case13, "results/PARAS/Case13_case4_wood11%.csv")



# Case 14 -----------------------------------------------------------------
# Test oil palm RED albedo using values in 
# https://www.researchgate.net/publication/267630163_A_portable_Reflectance-Absorptance-Transmittance_RAT_meter_for_vascular_Plant_leaves
# Albedo = 1 - Abt_625 = 100 - 95.6 = 4.4% = 0.044; R_gr_oilpalm_mean_L7$b3 = 0.3249016

paras.case14 <- cbind(gaps.spc.field.anc, data$BRF_b3_bs, data$BRF_b3_s) %>%  dplyr::filter(Type == "Oil Palm")
write.csv2(paras.case14, "results/PARAS/Case14_case1_onlyOPred.csv")

temp <- paras.res.Case1 %>% dplyr::filter(Type == "Oil Palm")

pdf("graphs/PARAS/Case_14/L7vsPARAS_LAIt_red_OP.pdf")
plot(paras.case14$LAIt_CIcc_my, paras.case14$SR_B3_red, pch = 0, main = "Oil palm plots\nunderstory plantation soil",
     xlim = c(0,5), ylim = c(0,0.3), xlab = "LAI true", ylab = "BRF red")
points(paras.case14$LAIt_CIcc_my, paras.case14$`data$BRF_b3_s`, pch = 15)
points(temp$LAIt_CIcc_my, temp$BRF_b3_s, pch = 1)
legend("topright", pch = c(0,15,1), 
       c("Landsat", "PARAS (wL = 0.044)", "PARAS (wL = 0.193 PROSPECT)"))
dev.off()



# Case 15 -----------------------------------------------------------------
# Case 1, but with new q = 1/2 * (1/T0-1)
paras.case15$q.new <- 1/2 * (1/paras.case15$DIFN)

# Run 1: Forest plots with Dipterocarp leaf albedo & primary forest soil reflectance
paras.lDipt.sPrim <- gaps.spc.field.anc %>% dplyr::filter(Biome == "Forest") %>% 
  paras_BRF(., wL_hypers = wL.DiptMean, R_gr_hypers = soil.spc.prim.mean, sensor = "Landsat7", new.q = TRUE)

# Run 2: Oil palm plots with Elaeis leaf albedo & OP plantation soil
paras.lElae.sPalm <- gaps.spc.field.anc %>% dplyr::filter(Biome == "Plantation_Palm") %>% 
  paras_BRF(., wL_hypers = wL.Elaeis, R_gr_hypers = soil.spc.OP.mean, sensor = "Landsat7", new.q = TRUE)

# Combine forest and plantation plots
paras.case15 <- rbind(paras.lDipt.sPrim, paras.lElae.sPalm)
# Save
write.csv2(paras.case15, "results/PARAS/remove_poor_verypoor/Case15_case1_newq.csv")


# Case 16 -----------------------------------------------------------------
# Case 1, but with new q = 1/2*(1+T0)/(1-p)-1

# Run 1: Forest plots with Dipterocarp leaf albedo & primary forest soil reflectance
paras.lDipt.sPrim <- gaps.spc.field.anc %>% dplyr::filter(Biome == "Forest") %>% 
  paras_BRF(., wL_hypers = wL.DiptMean, R_gr_hypers = soil.spc.prim.mean, sensor = "Landsat7", new.q = TRUE)

# Run 2: Oil palm plots with Elaeis leaf albedo & OP plantation soil
paras.lElae.sPalm <- gaps.spc.field.anc %>% dplyr::filter(Biome == "Plantation_Palm") %>% 
  paras_BRF(., wL_hypers = wL.Elaeis, R_gr_hypers = soil.spc.OP.mean, sensor = "Landsat7", new.q = TRUE)

# Combine forest and plantation plots
paras.case16 <- rbind(paras.lDipt.sPrim, paras.lElae.sPalm)
# Save
write.csv2(paras.case16, "results/PARAS/remove_poor_verypoor/Case16_case1_newq.csv")

windows()
plot(paras.res.Case1$q, paras.case16$q, xlim = c(0,3), ylim = c(0,3), 
     xlab = "q = 1-exp(-0.1684*LAIt)", ylab = "q = 1/2*(1+DIFN)/(1-p)-1"); abline(0,1)

plot(paras.res.Case1$p, paras.case16$p); abline(0,1)


# Case 17 -----------------------------------------------------------------
# New q = 1/2*(1+T0)/(1-p)-1, but fixed wL = Diptero and R_gr = primary forest soil

paras.case17 <- paras_BRF(gaps.spc.field.anc, wL_hypers = wL.DiptMean, R_gr_hypers = soil.spc.prim.mean, sensor = "Landsat7", new.q = TRUE)

write.csv2(paras.case17, "results/PARAS/remove_poor_verypoor/Case17_newq_wLDipt_RgPrim.csv")

# Case 18 -----------------------------------------------------------------
# Old q, but fixed wL = Diptero and R_gr = BRF-ECC intercept separately for forest and plantation
# Run 1: Forest plots 
paras.lDipt.sPrim <- gaps.spc.field.anc %>% dplyr::filter(Biome == "Forest") %>% 
  paras_BRF(., wL_hypers = wL.DiptMean, soil.L7 = soil.interc.forest, sensor = "Landsat7", new.q = FALSE)

# Run 2: Oil palm plots w
paras.lElae.sPalm <- gaps.spc.field.anc %>% dplyr::filter(Biome == "Plantation_Palm") %>% 
  paras_BRF(., wL_hypers = wL.DiptMean, soil.L7 = soil.interc.OP, sensor = "Landsat7", new.q = FALSE)

# Combine forest and plantation plots
paras.case18 <- rbind(paras.lDipt.sPrim, paras.lElae.sPalm)
write.csv2(paras.case18, "results/PARAS/remove_poor_verypoor/Case18_oldq_wLDipt_soilinterc.csv")

# Case 19 -----------------------------------------------------------------
# Newest q = (R1-T1)/(R1+T1), but fixed wL = Diptero and R_gr = primary forest soil

paras.case19 <- paras_BRF(gaps.spc.field.anc, wL_hypers = wL.DiptMean, R_gr_hypers = soil.spc.prim.mean, sensor = "Landsat7", new.q = TRUE)
write.csv2(paras.case19, "results/PARAS/remove_poor_verypoor/Case19_newestq_wLDipt_RgPrim.csv")

# Case 20 -----------------------------------------------------------------
# Newest q = (R1-T1)/(R1+T1), but fixed wL = Diptero and R_gr = BRF-ECC intercept separately for forest and plantation
# Run 1: Forest plots 
paras.lDipt.sPrim <- gaps.spc.field.anc %>% dplyr::filter(Biome == "Forest") %>% 
  paras_BRF(., wL_hypers = wL.DiptMean, soil.L7 = soil.interc.forest, sensor = "Landsat7", new.q = TRUE)

# Run 2: Oil palm plots 
paras.lElae.sPalm <- gaps.spc.field.anc %>% dplyr::filter(Biome == "Plantation_Palm") %>% 
  paras_BRF(., wL_hypers = wL.DiptMean, soil.L7 = soil.interc.OP, sensor = "Landsat7", new.q = TRUE)

# Combine forest and plantation plots
paras.case20 <- rbind(paras.lDipt.sPrim, paras.lElae.sPalm)
write.csv2(paras.case20, "results/PARAS/remove_poor_verypoor/Case20_newestq_wLDipt_soilinterc.csv")

# Case 21 -----------------------------------------------------------------
# Newest q = (R1-T1)/(R1+T1), wL = Dipt,and R_gr = measured primary forest & plantation
# Run 1: Forest plots 
paras.lDipt.sPrim <- gaps.spc.field.anc %>% dplyr::filter(Biome == "Forest") %>% 
  paras_BRF(., wL_hypers = wL.DiptMean, R_gr_hypers = soil.spc.prim.mean, sensor = "Landsat7", new.q = TRUE)

# Run 2: Oil palm plots 
paras.lElae.sPalm <- gaps.spc.field.anc %>% dplyr::filter(Biome == "Plantation_Palm") %>% 
  paras_BRF(., wL_hypers = wL.DiptMean, R_gr_hypers = soil.spc.OP.mean, sensor = "Landsat7", new.q = TRUE)

# Combine forest and plantation plots
paras.case21 <- rbind(paras.lDipt.sPrim, paras.lElae.sPalm)
write.csv2(paras.case21, "results/PARAS/remove_poor_verypoor/Case21_newestq_wLDipt_soilmeasPrimPalm.csv")

# Case 22 -----------------------------------------------------------------
# Old q, wL = Dipt,and R_gr = measured primary forest & plantation
# Run 1: Forest plots 
paras.lDipt.sPrim <- gaps.spc.field.anc %>% dplyr::filter(Biome == "Forest") %>% 
  paras_BRF(., wL_hypers = wL.DiptMean, R_gr_hypers = soil.spc.prim.mean, sensor = "Landsat7", new.q = FALSE)

# Run 2: Oil palm plots 
paras.lElae.sPalm <- gaps.spc.field.anc %>% dplyr::filter(Biome == "Plantation_Palm") %>% 
  paras_BRF(., wL_hypers = wL.DiptMean, R_gr_hypers = soil.spc.OP.mean, sensor = "Landsat7", new.q = FALSE)

# Combine forest and plantation plots
paras.case22 <- rbind(paras.lDipt.sPrim, paras.lElae.sPalm)
write.csv2(paras.case22, "results/PARAS/remove_poor_verypoor/Case22_oldq_wLDipt_soilmeasPrimPalm.csv")


# Case 23 -----------------------------------------------------------------
# "old" Q in Heiskanen et al. (2011) : Q = 0.059L + 0.495

Q <- c(0.533, 0.575, 0.664, 0.739, 0.784, 0.758)
L <- c(0.70, 1.36, 2.76, 4.11, 4.97, 4.39)
lm(Q~L)

# Case 24 -----------------------------------------------------------------
# Old q, wL = Dipt,and R_gr = litter
# Run 1: Forest plots 
paras.lDipt.sPrim <- gaps.spc.field.anc %>% dplyr::filter(Biome == "Forest") %>% 
  paras_BRF(., wL_hypers = wL.DiptMean, soil.L7 = soil.litter, sensor = "Landsat7", new.q = FALSE)

write.csv2(paras.lDipt.sPrim, "results/PARAS/remove_poor_verypoor/Case24_oldq_wLDipt_soilLitter.csv")


