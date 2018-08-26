source("cache/libPaths.R")
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


# Exclude 'poor' and 'very poor' forest quality plots ---------------------
rm.qual <- c("Poor", "Very poor")
gaps.spc.field.anc <- gaps.spc.field.anc %>%  dplyr::filter(!ForestQuality %in% rm.qual) # 65 plots


# Scope of sensitivity analysis -------------------------------------------
# Just the forest plots ?
forest.data <- gaps.spc.field.anc %>%  dplyr::filter(Biome == "Forest")        # Do sensitivity analysis for forest plots only

# paras.BRF.varymaininput <- function(LAIe, CI, DIFN, i0, cgf.view,   # Gap fractions data from DHP
#                                     N, Cab, Cw, Cm,                 # PROSPECT input to simulate wL
#                                     Rg.b1, Rg.b2, Rg.b3, Rg.b4, Rg.b5, Rg.b7) # Ground reflectance, scalar for each ETM+ band    
#   

# PARAS inputs that are measured (spectral invariant) ------------------------------------------
# (below in progress...)
forest.data <- forest.data %>% 
  mutate(
    cgf.view = Gaps1_my,                                                        # Sensor view (nadir for Landsat)
    cgf.sun = Gaps2_my + ((img_sza-23)/(38-23))*(Gaps3_my-Gaps2_my),            # Interpolate cgf_sun
    i0 = 1 - cgf.sun,
    DIFN = 0.066*Gaps1_my + 0.189*Gaps2_my + 0.247*Gaps3_my + 0.249*Gaps4_my +  # based on LAI-2000
      0.249*Gaps5_my,
    LAIe = LAI_my,
    LAIt = LAIt_CIcc_my,
    CI = LAIe / LAIt
  ) 

LAIe.range <- range(forest.data$LAIe)                                          # range for forest plots
CI.range <- range(forest.data$CI)
# DIFN.range <- range(forest.data$DIFN)
i0.range <- range(forest.data$i0)
cgf.view.range <- range(forest.data$cgf.view)



# PARAS inputs that are uncertain (depends on spectral inputs wL & R_gr) ------------------
N.range = c(1.5, 3.5)                                                           # Inputs to simulate wL (Mean +/- 2*std)
Cab.range = c(27.35, 122.83)                                                           
Cw.range = c(0.0128, 0.0176)
Cm.range = c(0.0047, 0.0231)

R_gr_prim_mean_L7 <- read_csv2("results/soil/R_gr_prim_mean_L7.csv")                  # Rg
R_gr_prim_min_L7 <- read_csv2("results/soil/R_gr_prim_min_L7.csv")          
R_gr_prim_max_L7 <- read_csv2("results/soil/R_gr_prim_max_L7.csv")
Rg.b1.range <- c(R_gr_prim_min_L7$b1, R_gr_prim_max_L7$b1)
Rg.b2.range <- c(R_gr_prim_min_L7$b2, R_gr_prim_max_L7$b2)
Rg.b3.range <- c(R_gr_prim_min_L7$b3, R_gr_prim_max_L7$b3)
Rg.b4.range <- c(R_gr_prim_min_L7$b4, R_gr_prim_max_L7$b4)
Rg.b5.range <- c(R_gr_prim_min_L7$b5, R_gr_prim_max_L7$b5)
Rg.b7.range <- c(R_gr_prim_min_L7$b7, R_gr_prim_max_L7$b7)
