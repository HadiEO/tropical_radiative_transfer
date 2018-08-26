# Local sensitivity analysis (LSA) with arbitrary change
# 2*std sensitivity criterion as in Asner (1998) ? 20%
# Base case: good quality forest plots, Diptero leaf, primary forest soil
# Assess metrics: rRMSE vs base case (& vs Landsat BRF)

source("cache/libPaths.R")
require(xlsx)
require(tidyverse)


# Base-case simulation ----------------------------------------------------

gaps.spc.field.anc <- read_csv2("results/gaps_spc_field_anc.csv")
source("munge/add_img_sza.R"); gaps.spc.field.anc <- add_img_sza(gaps.spc.field.anc)                       # Add image SZA

gaps.spc.field.anc <- mutate(gaps.spc.field.anc, scene.Date = as.character(scene.Date))                    # Recode forest quality column
gaps.spc.field.anc <- gaps.spc.field.anc %>% 
  mutate(ForestQuality = 
           dplyr::if_else(ForestQuality == "1", "Very poor", 
                          dplyr::if_else(ForestQuality == "2", "Poor", 
                                         dplyr::if_else(ForestQuality == "3", "OK",
                                                        dplyr::if_else(ForestQuality == "4", "Good", "Very good")))))

rm.qual <- c("Poor", "Very poor")                                                                       # Exclude 'poor' and 'very poor' forest quality plots
gaps.spc.field.anc <- gaps.spc.field.anc %>%  dplyr::filter(!ForestQuality %in% rm.qual) # 65 plots


wL.DiptMean <- read_csv2("results/leaf/wL_DiptMean.csv")                                                 # Leaf albedo

soil.spc <- read_csv2("data/soil/soil_spc_sel.csv")                                                      # Primary soil reflectance
soil.wav <- seq(350, 2500, by=10)
soil.spc.prim <- t(soil.spc[soil.spc$Vegetation=="Primary, Central Kalimantan, Bukit Raya Nature Reserve", 4:ncol(soil.spc)])
soil.spc.prim.mean <- tibble(Wavelength = soil.wav, Reflectance = rowMeans(soil.spc.prim))            

source("tests/reorg_in_progress/paras_BRF.R")                                        ###################### source the PARAS functions

sim.base <- gaps.spc.field.anc %>% dplyr::filter(Biome == "Forest") %>% 
  paras_BRF(., wL_hypers = wL.DiptMean, R_gr_hypers = soil.spc.prim.mean, sensor = "Landsat7", new.q = FALSE)


# Mean + std value
sim.base %>% transmute(mean = mean(p), sd = sd(p))



# Change LAIt, p, Q, wL, Rg
# (1) Change LAIt
sim.LAIt.plus20 <- gaps.spc.field.anc %>% dplyr::filter(Biome == "Forest") %>% 
  paras_BRF(., wL_hypers = wL.DiptMean, R_gr_hypers = soil.spc.prim.mean, sensor = "Landsat7", new.q = FALSE,
            chg.LAIt = 0.2)

sim.LAIt.min20 <- gaps.spc.field.anc %>% dplyr::filter(Biome == "Forest") %>% 
  paras_BRF(., wL_hypers = wL.DiptMean, R_gr_hypers = soil.spc.prim.mean, sensor = "Landsat7", new.q = FALSE,
            chg.LAIt = -0.2)

# (2) Change p
sim.p.plus20 <- gaps.spc.field.anc %>% dplyr::filter(Biome == "Forest") %>% 
  paras_BRF(., wL_hypers = wL.DiptMean, R_gr_hypers = soil.spc.prim.mean, sensor = "Landsat7", new.q = FALSE,
            chg.p = 0.2)

sim.p.min20 <- gaps.spc.field.anc %>% dplyr::filter(Biome == "Forest") %>% 
  paras_BRF(., wL_hypers = wL.DiptMean, R_gr_hypers = soil.spc.prim.mean, sensor = "Landsat7", new.q = FALSE,
            chg.p = -0.2)

sim.p.plus10 <- gaps.spc.field.anc %>% dplyr::filter(Biome == "Forest") %>% 
  paras_BRF(., wL_hypers = wL.DiptMean, R_gr_hypers = soil.spc.prim.mean, sensor = "Landsat7", new.q = FALSE,
            chg.p = 0.1)

sim.p.min10 <- gaps.spc.field.anc %>% dplyr::filter(Biome == "Forest") %>% 
  paras_BRF(., wL_hypers = wL.DiptMean, R_gr_hypers = soil.spc.prim.mean, sensor = "Landsat7", new.q = FALSE,
            chg.p = -0.1)

# (3) Change Q
sim.Q.plus20 <- gaps.spc.field.anc %>% dplyr::filter(Biome == "Forest") %>% 
  paras_BRF(., wL_hypers = wL.DiptMean, R_gr_hypers = soil.spc.prim.mean, sensor = "Landsat7", new.q = FALSE,
            chg.Q = 0.2)

sim.Q.min20 <- gaps.spc.field.anc %>% dplyr::filter(Biome == "Forest") %>% 
  paras_BRF(., wL_hypers = wL.DiptMean, R_gr_hypers = soil.spc.prim.mean, sensor = "Landsat7", new.q = FALSE,
            chg.Q = -0.2)

# (4) Change wL
sim.wL.plus20 <- gaps.spc.field.anc %>% dplyr::filter(Biome == "Forest") %>% 
  paras_BRF(., wL_hypers = wL.DiptMean, R_gr_hypers = soil.spc.prim.mean, sensor = "Landsat7", new.q = FALSE,
            chg.wL = 0.2)

sim.wL.min20 <- gaps.spc.field.anc %>% dplyr::filter(Biome == "Forest") %>% 
  paras_BRF(., wL_hypers = wL.DiptMean, R_gr_hypers = soil.spc.prim.mean, sensor = "Landsat7", new.q = FALSE,
            chg.wL = -0.2)


# (5) Change Rg
sim.R_gr.plus20 <- gaps.spc.field.anc %>% dplyr::filter(Biome == "Forest") %>% 
  paras_BRF(., wL_hypers = wL.DiptMean, R_gr_hypers = soil.spc.prim.mean, sensor = "Landsat7", new.q = FALSE,
            chg.R_gr = 0.2)

sim.R_gr.min20 <- gaps.spc.field.anc %>% dplyr::filter(Biome == "Forest") %>% 
  paras_BRF(., wL_hypers = wL.DiptMean, R_gr_hypers = soil.spc.prim.mean, sensor = "Landsat7", new.q = FALSE,
            chg.R_gr = -0.2)

# (6) Incorporate wood element
stem.L7 <- read.csv2("results/leaf/stem_mean_L7.csv")
stem.L7 <- stem.L7 %>%  dplyr::select(-X)
sim.wood.11 <- gaps.spc.field.anc %>% dplyr::filter(Biome == "Forest") %>% 
  paras_BRF(., wL_hypers = wL.DiptMean, R_gr_hypers = soil.spc.prim.mean, sensor = "Landsat7", new.q = FALSE,
            wood_to_plant_ratio = 0.11, R_wood.L7 = stem.L7)       

sim.wood.20 <- gaps.spc.field.anc %>% dplyr::filter(Biome == "Forest") %>% 
  paras_BRF(., wL_hypers = wL.DiptMean, R_gr_hypers = soil.spc.prim.mean, sensor = "Landsat7", new.q = FALSE,
            wood_to_plant_ratio = 0.2, R_wood.L7 = stem.L7)    

# (7) Soil = litter (mean)
litter.L7 <- read.csv2("results/soil/litter_mean_L7.csv")
litter.L7 <- litter.L7 %>%  dplyr::select(-X)
sim.litter.mean <- gaps.spc.field.anc %>% dplyr::filter(Biome == "Forest") %>% 
  paras_BRF(., wL_hypers = wL.DiptMean, soil.L7 = litter.L7, sensor = "Landsat7", new.q = FALSE)     


# (8) New homogenous Q
sim.homo.Q <- gaps.spc.field.anc %>% dplyr::filter(Biome == "Forest") %>% 
  paras_BRF(., wL_hypers = wL.DiptMean, R_gr_hypers = soil.spc.prim.mean, sensor = "Landsat7", new.q = TRUE)     

# (9) 40% liana in wL
wL.add40liana <- wL.DiptMean %>% dplyr::select(Wavelength, Albedo)
wL.add40liana$Albedo <- 0.6 * wL.DiptMean$Albedo + 0.4 * wL.liana$Albedo
sim.add40liana <- gaps.spc.field.anc %>% dplyr::filter(Biome == "Forest") %>% 
  paras_BRF(., wL_hypers = wL.add40liana, R_gr_hypers = soil.spc.prim.mean, sensor = "Landsat7", new.q = FALSE)     


# (10) Add PROSPECT bias for HAWAII
wL.DiptMean.addBias <- read_csv2(wL.DiptMean.addBias, "results/leaf/wL_DiptMean_addBias.csv")
sim.addProspectBias <- gaps.spc.field.anc %>% dplyr::filter(Biome == "Forest") %>% 
  paras_BRF(., wL_hypers = wL.DiptMean.addBias, R_gr_hypers = soil.spc.prim.mean, sensor = "Landsat7", new.q = FALSE)     


# Comparison statistics ---------------------------------------------------
# Change LAIt, p, Q, wL, Rg
source("tests/accuracy_table.R")                                                       # source the function


# MD = perturbed case - base case-------------------------------------------------------

comp.LAIt.min20 <- comparison_table(basecase = sim.base, perturbedcase = sim.LAIt.min20) %>% dplyr::select(band, MD, rMD)
comp.LAIt.plus20 <- comparison_table(basecase = sim.base, perturbedcase = sim.LAIt.plus20) %>% dplyr::select(band, MD, rMD)

comp.p.min20 <- comparison_table(basecase = sim.base, perturbedcase = sim.p.min20) %>% dplyr::select(band, MD, rMD)
comp.p.plus20 <- comparison_table(basecase = sim.base, perturbedcase = sim.p.plus20) %>% dplyr::select(band, MD, rMD)
comp.p.min10 <- comparison_table(basecase = sim.base, perturbedcase = sim.p.min10) %>% dplyr::select(band, MD, rMD)
comp.p.plus10 <- comparison_table(basecase = sim.base, perturbedcase = sim.p.plus10) %>% dplyr::select(band, MD, rMD)


comp.Q.min20 <- comparison_table(basecase = sim.base, perturbedcase = sim.Q.min20) %>% dplyr::select(band, MD, rMD)
comp.Q.plus20 <- comparison_table(basecase = sim.base, perturbedcase = sim.Q.plus20) %>% dplyr::select(band, MD, rMD)

comp.wL.min20 <- comparison_table(basecase = sim.base, perturbedcase = sim.wL.min20) %>% dplyr::select(band, MD, rMD)
comp.wL.plus20 <- comparison_table(basecase = sim.base, perturbedcase = sim.wL.plus20) %>% dplyr::select(band, MD, rMD)

comp.R_gr.min20 <- comparison_table(basecase = sim.base, perturbedcase = sim.R_gr.min20) %>% dplyr::select(band, MD, rMD)
comp.R_gr.plus20 <- comparison_table(basecase = sim.base, perturbedcase = sim.R_gr.plus20) %>% dplyr::select(band, MD, rMD)

comp.wood.11 <- comparison_table(basecase = sim.base, perturbedcase = sim.wood.11) %>% dplyr::select(band, MD, rMD)
comp.wood.20 <- comparison_table(basecase = sim.base, perturbedcase = sim.wood.20) %>% dplyr::select(band, MD, rMD)

comp.litter.mean <- comparison_table(basecase = sim.base, perturbedcase = sim.litter.mean) %>% dplyr::select(band, MD, rMD)

comp.homo.Q <- comparison_table(basecase = sim.base, perturbedcase = sim.homo.Q) %>% dplyr::select(band, MD, rMD)
comp.add40liana <- comparison_table(basecase = sim.base, perturbedcase = sim.add40liana) %>% dplyr::select(band, MD, rMD)
comp.addProspectBias <- comparison_table(basecase = sim.base, perturbedcase = sim.addProspectBias) %>% dplyr::select(band, MD, rMD)



write.csv2(cbind(comp.LAIt.min20, comp.LAIt.plus20), "results/sensitivity/LSA_LAIt_20.csv")
write.csv2(cbind(comp.p.min20, comp.p.plus20), "results/sensitivity/LSA_p_20.csv")
write.csv2(cbind(comp.p.min10, comp.p.plus10), "results/sensitivity/LSA_p_10.csv")
write.csv2(cbind(comp.Q.min20, comp.Q.plus20), "results/sensitivity/LSA_Q_20.csv")
write.csv2(cbind(comp.wL.min20, comp.wL.plus20), "results/sensitivity/LSA_wL_20.csv")
write.csv2(cbind(comp.R_gr.min20, comp.R_gr.plus20), "results/sensitivity/LSA_R_gr_20.csv")
write.csv2(comp.wood.11, "results/sensitivity/LSA_wood_11.csv")
write.csv2(comp.wood.20, "results/sensitivity/LSA_wood_20.csv")

write.csv2(comp.litter.mean, "results/sensitivity/LSA_litter_mean.csv")
write.csv2(comp.homo.Q, "results/sensitivity/LSA_homo_Q.csv")
write.csv2(comp.add40liana, "results/sensitivity/LSA_add40liana.csv")
write.csv2(comp.addProspectBias, "results/sensitivity/LSA_addProspectBias.csv")


# RMSE vs Landsat BRF -----------------------------------------------------
acc.LAIt.min20 <- accuracy_table(sim.LAIt.min20)
acc.LAIt.plus20 <- accuracy_table(sim.LAIt.plus20)

acc.p.min20 <- accuracy_table(sim.p.min20)
acc.p.plus20 <- accuracy_table(sim.p.plus20)
acc.p.min10 <- accuracy_table(sim.p.min10)
acc.p.plus10 <- accuracy_table(sim.p.plus10)

acc.Q.min20 <- accuracy_table(sim.Q.min20)
acc.Q.plus20 <- accuracy_table(sim.Q.plus20)

acc.wL.min20 <- accuracy_table(sim.wL.min20)
acc.wL.plus20 <- accuracy_table(sim.wL.plus20)

acc.R_gr.min20 <- accuracy_table(sim.R_gr.min20)
acc.R_gr.plus20 <- accuracy_table(sim.R_gr.plus20)

acc.wood.11 <- accuracy_table(sim.wood.11)
acc.wood.20 <- accuracy_table(sim.wood.20)

acc.litter.mean <- accuracy_table(sim.litter.mean)
acc.homo.Q <- accuracy_table(sim.homo.Q)
acc.add40liana <- accuracy_table(sim.add40liana)
acc.addProspectBias <- accuracy_table(sim.addProspectBias)



write.csv2(cbind(acc.LAIt.min20, acc.LAIt.plus20), "results/sensitivity/LSA_LAIt_20_acc.csv")
write.csv2(cbind(acc.p.min20, acc.p.plus20), "results/sensitivity/LSA_p_20_acc.csv")
write.csv2(cbind(acc.p.min10, acc.p.plus10), "results/sensitivity/LSA_p_10_acc.csv")
write.csv2(cbind(acc.Q.min20, acc.Q.plus20), "results/sensitivity/LSA_Q_20_acc.csv")
write.csv2(cbind(acc.wL.min20, acc.wL.plus20), "results/sensitivity/LSA_wL_20_acc.csv")
write.csv2(cbind(acc.R_gr.min20, acc.R_gr.plus20), "results/sensitivity/LSA_R_gr_20_acc.csv")
write.csv2(acc.wood.11, "results/sensitivity/LSA_wood_11_acc.csv")
write.csv2(acc.wood.20, "results/sensitivity/LSA_wood_20_acc.csv")

write.csv2(acc.litter.mean, "results/sensitivity/LSA_litter_mean_acc.csv")
write.csv2(acc.homo.Q, "results/sensitivity/LSA_homo_Q_acc.csv")
write.csv2(acc.add40liana, "results/sensitivity/LSA_add40liana_acc.csv")
write.csv2(acc.addProspectBias, "results/sensitivity/LSA_addProspectBias_acc.csv")



acc.base <- accuracy_table(sim.base)                                                              # Base case
write.csv2(acc.base, "results/sensitivity/LSA_base_acc.csv")


# p
sim.base %>% group_by(Type.new) %>%  summarise(mean.p = round(mean(p), 3), sd.p = round(sd(p), 3))
