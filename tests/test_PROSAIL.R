# Read gaps, spectra, ancillary data
gaps <- read_csv2("results/gaps_spc_field_anc_processed_88.csv")

# Keep only closed-canopy forest
# Keep forest
gaps <- gaps %>% dplyr::filter(Biome == "Forest")            # 75 plots
# Exclude 'poor' and 'very poor' forest quality plots
rm.qual <- c("Poor", "Very poor")
gaps <- gaps %>%  dplyr::filter(!ForestQuality %in% rm.qual) # 52 plots

# Leaf albedo
# wL.DiptMean <- read_csv2("results/leaf/wL_DiptMean.csv")
prospect_in <- read_csv2("data/leaf/PROSPECT_input_basedonHAWAII.csv")
source("tests/multivariate_sampling.R")
Dipt.bio.sim <- leaf.bio.sim.cop(n = 2500, leaf.data = prospect_in, 
                                 mean = "Dipt_mean", min = "Dipt_min", max = "Dipt_max", 
                                 cor = c(0.37, -0.52, -0.67))

# Soil reflectance
soil.prim <- read_csv2("data/soil/soil_spc_prim_mean.csv")
soil.prim.speclib <- speclib(soil.prim$Reflectance, soil.prim$Wavelength)


# Prepare parameter list
parameter <- data.frame(# Leaf optical properties
                        N = 1.7, 
                        Cab = Dipt.bio.sim$Cab,             # 75.09
                        Car = 0, 
                        Cw = Dipt.bio.sim$Cw,                          # 0.0152
                        Cbrown = 0, 
                        Cm = Dipt.bio.sim$Cm,                          # 0.0139
                        
                        # Canopy structure
                        LAI = mean(gaps$LAIt_CIcc_my),                                     # I think it's true LAI
                        TypeLidf = 1,                                # 1 for stat. distribution, otherwise average leaf angle (degree)
                        lidfa = -0.35,                                   # Average leaf slope (-0.35 for spherical)
                        lidfb = -0.15,                                   # Bimodality of leaf distribution (-0.15 for spherical)
                        hspot = 0.01,                               # From Feret & Asner (2011) for Hawaiian tropical species
                        
                        # Illumination-sensor geometry
                        tts = mean(gaps$img_sza),                                     # Solar zenith angle
                        tto = 0,                                     # Observer zenith angle
                        psi = 0,                                     # Relative azimuth angle
                        
                        psoil = 0                                   # Dry/wet soil factor
                        # rsoil = soil.prim.speclib                 # Default soil spectra
                        )

mod.BRF.defRsoil <- PROSAIL(parameterList = parameter)

# Resample to Landsat-7
mod.BRF.defRsoil.L7 <- spectralResampling(mod.BRF.defRsoil, sensor = "Landsat7")
mod.BRF.defRsoil.L7.df <- as.data.frame(mod.BRF.defRsoil.L7)
apply(mod.BRF.defRsoil.L7.df, 2, function(z) mean(z) - sd(z))
apply(mod.BRF.defRsoil.L7.df, 2, function(z) mean(z) + sd(z))
apply(mod.BRF.defRsoil.L7.df, 2, sd)
gaps %>% dplyr::select(starts_with("SR")) %>% apply(2, function(z) max(z) - min(z))



# Append modelled BRF to gaps
gaps <- gaps %>% mutate(BRF_b1_s = mod.BRF.defRsoil.L7.df$Band_1, 
                        BRF_b2_s = mod.BRF.defRsoil.L7.df$Band_2,
                        BRF_b3_s = mod.BRF.defRsoil.L7.df$Band_3,
                        BRF_b4_s = mod.BRF.defRsoil.L7.df$Band_4,
                        BRF_b5_s = mod.BRF.defRsoil.L7.df$Band_5,
                        BRF_b7_s = mod.BRF.defRsoil.L7.df$Band_7)

# write.csv2(gaps, "results/PROSAIL/closCan_DiptMean_defRsoil.csv")
# write.csv2(gaps, "results/PROSAIL/closCan_DiptMean_defRsoil_LAIe.csv")
# write.csv2(gaps, "results/PROSAIL/closCan_DiptMean_defRsoil_hspot025_ALA45.csv")
write.csv2(gaps, "results/PROSAIL/closCan_DiptMean_defRsoil_ALA45.csv")



