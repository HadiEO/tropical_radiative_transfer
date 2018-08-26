source("cache/libPaths.R")
require(tidyverse)

paras_BRF <- function(data, wL_hypers, R_gr_hypers, sensor = "Landsat7", chg.Q = NA, wood_to_plant_ratio = NA, R_wood.L7 = NA, 
                      new.q = FALSE, soil.L7 = NULL,
                      chg.p = NA, chg.LAIt = NA, chg.R_gr = NA, chg.wL = NA) {
  input <- data                                                                  # keep original input data for sampling.mode
  

  if(!is.na(chg.LAIt)) {                                                            # Change LAIt arbitrarily
    data <- data %>% mutate(LAIt_CIcc_my = LAIt_CIcc_my + chg.LAIt * LAIt_CIcc_my)}
  
  
  # 1. Input A: derived from gap fractions + calculate p
  data <- data %>% 
    mutate(
      cgf_view = Gaps1_my,                                                        # Sensor view (nadir for Landsat)
      cgf_sun = Gaps2_my + ((img_sza-23)/(38-23))*(Gaps3_my-Gaps2_my),            # Interpolate cgf_sun
      i0_sun = 1 - cgf_sun,
      DIFN = 0.066*Gaps1_my + 0.189*Gaps2_my + 0.247*Gaps3_my + 0.249*Gaps4_my +  # based on LAI-2000
        0.249*Gaps5_my,
      iD = 1 - DIFN,
      p = 1 - (iD / LAIt_CIcc_my)                                                # analytical formula based on Stenberg (2007)
      ) 
  
  if(!is.na(chg.p)) {                                                            # Change p arbitrarily
    data <- data %>% mutate(p = p + chg.p * p)}
  
  
  
  
  # 2. Input B: leaf albedo wL
  source("munge/spc_resample.R")                                                   # Resample wL to intended sensor
  wL <- spc_resample(wL_hypers, "Albedo", "Wavelength", sensor = "Landsat7")
  
  # 3. Input C: understory reflectance
  if(is.null(soil.L7)) {                                                          # If supply soil refl readily resampled (e.g., BRF-vs-ECC intercept)
    R_gr <- spc_resample(R_gr_hypers, "Reflectance", "Wavelength", sensor = "Landsat7")
  } else {
    R_gr <- soil.L7
  }
  
  if(!is.na(wood_to_plant_ratio)) {
    wL <- (1 - wood_to_plant_ratio) * wL +                                   # if include woody element optical properties
          wood_to_plant_ratio * R_wood.L7 }                                  # R_wood.L7 resampled already yo ETM bands, ought to be a named data frame       
  
  if(!is.na(chg.wL)) {                                                            # Change wL arbitrarily
    wL <- wL + chg.wL * wL }
  
  if(!is.na(chg.R_gr)) {                                                            # Change R_gr arbitrarily
    R_gr <- R_gr + chg.R_gr * R_gr }
  
  # 3. Calculation A: canopy scattering coefficient wC = (wL - p*WL) / (1 - p*wL)
  data <- data %>% mutate(
    wC_b1 = (wL$b1 - p * wL$b1) / (1 - p * wL$b1),
    wC_b2 = (wL$b2 - p * wL$b2) / (1 - p * wL$b2),
    wC_b3 = (wL$b3 - p * wL$b3) / (1 - p * wL$b3),
    wC_b4 = (wL$b4 - p * wL$b4) / (1 - p * wL$b4),
    wC_b5 = (wL$b5 - p * wL$b5) / (1 - p * wL$b5),
    wC_b7 = (wL$b7 - p * wL$b7) / (1 - p * wL$b7)
  )
  

  # 4. Calculation B: fraction of backward scatter Q based on Mottus & Stenberg (2008)
  if(new.q == TRUE) {                                # Use new q 
    data <- data %>% mutate(
      q_b1 = ((0.25*wL$b1*(1-DIFN^2))-(0.5*wL$b1*DIFN*-log(DIFN)))/                   # log() computes natural logarithm
        ((0.25*wL$b1*(1-DIFN^2))+(0.5*wL$b1*DIFN*-log(DIFN))),
      q_b2 = ((0.25*wL$b2*(1-DIFN^2))-(0.5*wL$b2*DIFN*-log(DIFN)))/                   # log() computes natural logarithm
        ((0.25*wL$b2*(1-DIFN^2))+(0.5*wL$b2*DIFN*-log(DIFN))),
      q_b3 = ((0.25*wL$b3*(1-DIFN^2))-(0.5*wL$b3*DIFN*-log(DIFN)))/                   # log() computes natural logarithm
        ((0.25*wL$b3*(1-DIFN^2))+(0.5*wL$b3*DIFN*-log(DIFN))),
      q_b4 = ((0.25*wL$b4*(1-DIFN^2))-(0.5*wL$b4*DIFN*-log(DIFN)))/                   # log() computes natural logarithm
        ((0.25*wL$b4*(1-DIFN^2))+(0.5*wL$b4*DIFN*-log(DIFN))),
      q_b5 = ((0.25*wL$b5*(1-DIFN^2))-(0.5*wL$b5*DIFN*-log(DIFN)))/                   # log() computes natural logarithm
        ((0.25*wL$b5*(1-DIFN^2))+(0.5*wL$b5*DIFN*-log(DIFN))),
      q_b7 = ((0.25*wL$b7*(1-DIFN^2))-(0.5*wL$b7*DIFN*-log(DIFN)))/                   # log() computes natural logarithm
        ((0.25*wL$b7*(1-DIFN^2))+(0.5*wL$b7*DIFN*-log(DIFN))),

      Q_b1 = 0.5 + (0.5*q_b1) * ((1 - (p*wL$b1)) / (1 - (p*q_b1*wL$b1))),
      Q_b2 = 0.5 + (0.5*q_b2) * ((1 - (p*wL$b2)) / (1 - (p*q_b2*wL$b2))),
      Q_b3 = 0.5 + (0.5*q_b3) * ((1 - (p*wL$b3)) / (1 - (p*q_b3*wL$b3))),
      Q_b4 = 0.5 + (0.5*q_b4) * ((1 - (p*wL$b4)) / (1 - (p*q_b4*wL$b4))),
      Q_b5 = 0.5 + (0.5*q_b5) * ((1 - (p*wL$b5)) / (1 - (p*q_b5*wL$b5))),
      Q_b7 = 0.5 + (0.5*q_b7) * ((1 - (p*wL$b7)) / (1 - (p*q_b7*wL$b7)))
    )
  } else {
    data <- data %>% mutate(
      q = 1 - exp(-0.1684 * LAIt_CIcc_my),                                          # Asymmtery parameter q 
      Q_b1 = 0.5 + (0.5*q) * ((1 - (p*wL$b1)) / (1 - (p*q*wL$b1))),
      Q_b2 = 0.5 + (0.5*q) * ((1 - (p*wL$b2)) / (1 - (p*q*wL$b2))),
      Q_b3 = 0.5 + (0.5*q) * ((1 - (p*wL$b3)) / (1 - (p*q*wL$b3))),
      Q_b4 = 0.5 + (0.5*q) * ((1 - (p*wL$b4)) / (1 - (p*q*wL$b4))),
      Q_b5 = 0.5 + (0.5*q) * ((1 - (p*wL$b5)) / (1 - (p*q*wL$b5))),
      Q_b7 = 0.5 + (0.5*q) * ((1 - (p*wL$b7)) / (1 - (p*q*wL$b7)))
    )
    
  }
  
  
  if(!is.na(chg.Q)) {                                                            # Change Q arbitrarily
    data <- data %>% mutate(
      Q_b1 = Q_b1 + chg.Q * Q_b1,
      Q_b2 = Q_b2 + chg.Q * Q_b2,
      Q_b3 = Q_b3 + chg.Q * Q_b3,
      Q_b4 = Q_b4 + chg.Q * Q_b4,
      Q_b5 = Q_b5 + chg.Q * Q_b5,
      Q_b7 = Q_b7 + chg.Q * Q_b7
    )
    
  }
    
  # 5. Main calculation A: BRF black soil
  data <- data %>% mutate(
    BRF_b1_bs = Q_b1 * i0_sun * wC_b1,
    BRF_b2_bs = Q_b2 * i0_sun * wC_b2,
    BRF_b3_bs = Q_b3 * i0_sun * wC_b3,
    BRF_b4_bs = Q_b4 * i0_sun * wC_b4,
    BRF_b5_bs = Q_b5 * i0_sun * wC_b5,
    BRF_b7_bs = Q_b7 * i0_sun * wC_b7
  )

  # 6. Main calculation B: BRF with soil contribution
  data <- data %>% mutate(
    BRF_b1_s = (cgf_view * R_gr$b1 * cgf_sun) + (Q_b1 * i0_sun * wC_b1),          # Based on Rautiainen & Stenberg (2005)
    BRF_b2_s = (cgf_view * R_gr$b2 * cgf_sun) + (Q_b2 * i0_sun * wC_b2),
    BRF_b3_s = (cgf_view * R_gr$b3 * cgf_sun) + (Q_b3 * i0_sun * wC_b3),
    BRF_b4_s = (cgf_view * R_gr$b4 * cgf_sun) + (Q_b4 * i0_sun * wC_b4),
    BRF_b5_s = (cgf_view * R_gr$b5 * cgf_sun) + (Q_b5 * i0_sun * wC_b5),
    BRF_b7_s = (cgf_view * R_gr$b7 * cgf_sun) + (Q_b7 * i0_sun * wC_b7)
  )
  
  return(data)
  
}

# Function to run samples of leaf albedo ----------------------------------
# Todo: current implementation does not vary soil reflectance, only leaf albedo

paras_run_sampling <- function(forest.data, wL.samples, R_gr_hypers) {
  
  ptm <- proc.time()                                                            # Track run time
  
  store.runs <- array(999, c(nrow(forest.data), ncol(wL.samples$Albedo)))
  
  fun.out <- list(input = forest.data, 
                  b1 = list(bs = store.runs, s = store.runs),
                  b2 = list(bs = store.runs, s = store.runs),
                  b3 = list(bs = store.runs, s = store.runs),
                  b4 = list(bs = store.runs, s = store.runs),
                  b5 = list(bs = store.runs, s = store.runs),
                  b7 = list(bs = store.runs, s = store.runs))
  
  for (i in 1:ncol(wL.samples$Albedo)) {
    wL.sample <- tibble(Wavelength = wL.samples$Wavelength,
                        Albedo = wL.samples$Albedo[,i])
    
    paras.out <-  paras_BRF(forest.data, wL_hypers = wL.sample, R_gr_hypers = R_gr_hypers, sensor = "Landsat7")
    
    fun.out$b1$bs[,i] <- paras.out$BRF_b1_bs; fun.out$b1$s[,i] <- paras.out$BRF_b1_s
    fun.out$b2$bs[,i] <- paras.out$BRF_b2_bs; fun.out$b2$s[,i] <- paras.out$BRF_b2_s
    fun.out$b3$bs[,i] <- paras.out$BRF_b3_bs; fun.out$b3$s[,i] <- paras.out$BRF_b3_s
    fun.out$b4$bs[,i] <- paras.out$BRF_b4_bs; fun.out$b4$s[,i] <- paras.out$BRF_b4_s
    fun.out$b5$bs[,i] <- paras.out$BRF_b5_bs; fun.out$b5$s[,i] <- paras.out$BRF_b5_s
    fun.out$b7$bs[,i] <- paras.out$BRF_b7_bs; fun.out$b7$s[,i] <- paras.out$BRF_b7_s
    
  }
  
  return(fun.out)
  print(proc.time() - ptm)                                                     # Print run time
  
}



# p calculated with LAIt_UK -----------------------------------------------
paras_BRF_LAItUK <- function(data, wL_hypers, R_gr_hypers, sensor = "Landsat7") {
  input <- data                                                                  # keep original input data for sampling.mode
  
  # 1. Input A: derived from gap fractions + calculate p
  data <- data %>% 
    mutate(
      cgf_view = Gaps1_my,                                                        # Sensor view (nadir for Landsat)
      cgf_sun = Gaps2_my + ((img_sza-23)/(38-23))*(Gaps3_my-Gaps2_my),            # Interpolate cgf_sun
      i0_sun = 1 - cgf_sun,
      DIFN = 0.066*Gaps1_my + 0.189*Gaps2_my + 0.247*Gaps3_my + 0.249*Gaps4_my +  # based on LAI-2000
        0.249*Gaps5_my,
      iD = 1 - DIFN,
      p = 1 - (iD / LAI_true_v6)                                                # analytical formula based on Stenberg (2007)
    ) 
  
  # 2. Input B: leaf albedo wL
  source("munge/spc_resample.R")                                                   # Resample wL to intended sensor
  wL <- spc_resample(wL_hypers, "Albedo", "Wavelength", sensor = "Landsat7")
  
  # 3. Input C: understory reflectance
  R_gr <- spc_resample(R_gr_hypers, "Reflectance", "Wavelength", sensor = "Landsat7")
  
  
  # 3. Calculation A: canopy scattering coefficient wC = (wL - p*WL) / (1 - p*wL)
  data <- data %>% mutate(
    wC_b1 = (wL$b1 - p * wL$b1) / (1 - p * wL$b1),
    wC_b2 = (wL$b2 - p * wL$b2) / (1 - p * wL$b2),
    wC_b3 = (wL$b3 - p * wL$b3) / (1 - p * wL$b3),
    wC_b4 = (wL$b4 - p * wL$b4) / (1 - p * wL$b4),
    wC_b5 = (wL$b5 - p * wL$b5) / (1 - p * wL$b5),
    wC_b7 = (wL$b7 - p * wL$b7) / (1 - p * wL$b7)
  )
  
  # 4. Calculation B: fraction of backward scatter Q based on Mottus & Stenberg (2008)
  data <- data %>% mutate(
    q = 1 - exp(-0.1684 * LAI_true_v6),                                          # Asymmtery parameter q 
    Q_b1 = 0.5 + (0.5*q) * ((1 - (p*wL$b1)) / (1 - (p*q*wL$b1))),
    Q_b2 = 0.5 + (0.5*q) * ((1 - (p*wL$b2)) / (1 - (p*q*wL$b2))),
    Q_b3 = 0.5 + (0.5*q) * ((1 - (p*wL$b3)) / (1 - (p*q*wL$b3))),
    Q_b4 = 0.5 + (0.5*q) * ((1 - (p*wL$b4)) / (1 - (p*q*wL$b4))),
    Q_b5 = 0.5 + (0.5*q) * ((1 - (p*wL$b5)) / (1 - (p*q*wL$b5))),
    Q_b7 = 0.5 + (0.5*q) * ((1 - (p*wL$b7)) / (1 - (p*q*wL$b7)))
  )
  
  # 5. Main calculation A: BRF black soil
  data <- data %>% mutate(
    BRF_b1_bs = Q_b1 * i0_sun * wC_b1,
    BRF_b2_bs = Q_b2 * i0_sun * wC_b2,
    BRF_b3_bs = Q_b3 * i0_sun * wC_b3,
    BRF_b4_bs = Q_b4 * i0_sun * wC_b4,
    BRF_b5_bs = Q_b5 * i0_sun * wC_b5,
    BRF_b7_bs = Q_b7 * i0_sun * wC_b7
  )
  
  # 6. Main calculation B: BRF with soil contribution
  data <- data %>% mutate(
    BRF_b1_s = (cgf_view * R_gr$b1 * cgf_sun) + (Q_b1 * i0_sun * wC_b1),          # Based on Rautiainen & Stenberg (2005)
    BRF_b2_s = (cgf_view * R_gr$b2 * cgf_sun) + (Q_b2 * i0_sun * wC_b2),
    BRF_b3_s = (cgf_view * R_gr$b3 * cgf_sun) + (Q_b3 * i0_sun * wC_b3),
    BRF_b4_s = (cgf_view * R_gr$b4 * cgf_sun) + (Q_b4 * i0_sun * wC_b4),
    BRF_b5_s = (cgf_view * R_gr$b5 * cgf_sun) + (Q_b5 * i0_sun * wC_b5),
    BRF_b7_s = (cgf_view * R_gr$b7 * cgf_sun) + (Q_b7 * i0_sun * wC_b7)
  )
  
  return(data)
  
}






# For sensitivity analysis ------------------------------------------------

# paras_BRF <- function(data, wL_hypers, R_gr_hypers, sensor = "Landsat7") {
#   input <- data                                                                  # keep original input data for sampling.mode
#   
#   # 1. Input A: derived from gap fractions + calculate p
#   data <- data %>% 
#     mutate(
#       cgf_view = Gaps1_my,                                                        # Sensor view (nadir for Landsat)
#       cgf_sun = Gaps2_my + ((img_sza-23)/(38-23))*(Gaps3_my-Gaps2_my),            # Interpolate cgf_sun
#       i0_sun = 1 - cgf_sun,
#       DIFN = 0.066*Gaps1_my + 0.189*Gaps2_my + 0.247*Gaps3_my + 0.249*Gaps4_my +  # based on LAI-2000
#         0.249*Gaps5_my,
#       iD = 1 - DIFN,
#       p = 1 - (iD / LAIt_CIcc_my)                                                # analytical formula based on Stenberg (2007)
#     ) 
#   
#   # 2. Input B: leaf albedo wL
#  
#   # 3. Input C: understory reflectance
# 
#   # 3. Calculation A: canopy scattering coefficient wC = (wL - p*WL) / (1 - p*wL)
#   data <- data %>% mutate(
#     wC_b1 = (wL$b1 - p * wL$b1) / (1 - p * wL$b1),
#     wC_b2 = (wL$b2 - p * wL$b2) / (1 - p * wL$b2),
#     wC_b3 = (wL$b3 - p * wL$b3) / (1 - p * wL$b3),
#     wC_b4 = (wL$b4 - p * wL$b4) / (1 - p * wL$b4),
#     wC_b5 = (wL$b5 - p * wL$b5) / (1 - p * wL$b5),
#     wC_b7 = (wL$b7 - p * wL$b7) / (1 - p * wL$b7)
#   )
#   
#   # 4. Calculation B: fraction of backward scatter Q based on Mottus & Stenberg (2008)
#   data <- data %>% mutate(
#     q = 1 - exp(-0.1684 * LAIt_CIcc_my),                                          # Asymmtery parameter q 
#     Q_b1 = 0.5 + (0.5*q) * ((1 - (p*wL$b1)) / (1 - (p*q*wL$b1))),
#     Q_b2 = 0.5 + (0.5*q) * ((1 - (p*wL$b2)) / (1 - (p*q*wL$b2))),
#     Q_b3 = 0.5 + (0.5*q) * ((1 - (p*wL$b3)) / (1 - (p*q*wL$b3))),
#     Q_b4 = 0.5 + (0.5*q) * ((1 - (p*wL$b4)) / (1 - (p*q*wL$b4))),
#     Q_b5 = 0.5 + (0.5*q) * ((1 - (p*wL$b5)) / (1 - (p*q*wL$b5))),
#     Q_b7 = 0.5 + (0.5*q) * ((1 - (p*wL$b7)) / (1 - (p*q*wL$b7)))
#   )
#   
#   # 5. Main calculation A: BRF black soil
#   data <- data %>% mutate(
#     BRF_b1_bs = Q_b1 * i0_sun * wC_b1,
#     BRF_b2_bs = Q_b2 * i0_sun * wC_b2,
#     BRF_b3_bs = Q_b3 * i0_sun * wC_b3,
#     BRF_b4_bs = Q_b4 * i0_sun * wC_b4,
#     BRF_b5_bs = Q_b5 * i0_sun * wC_b5,
#     BRF_b7_bs = Q_b7 * i0_sun * wC_b7
#   )
#   
#   # 6. Main calculation B: BRF with soil contribution
#   data <- data %>% mutate(
#     BRF_b1_s = (cgf_view * R_gr$b1 * cgf_sun) + (Q_b1 * i0_sun * wC_b1),          # Based on Rautiainen & Stenberg (2005)
#     BRF_b2_s = (cgf_view * R_gr$b2 * cgf_sun) + (Q_b2 * i0_sun * wC_b2),
#     BRF_b3_s = (cgf_view * R_gr$b3 * cgf_sun) + (Q_b3 * i0_sun * wC_b3),
#     BRF_b4_s = (cgf_view * R_gr$b4 * cgf_sun) + (Q_b4 * i0_sun * wC_b4),
#     BRF_b5_s = (cgf_view * R_gr$b5 * cgf_sun) + (Q_b5 * i0_sun * wC_b5),
#     BRF_b7_s = (cgf_view * R_gr$b7 * cgf_sun) + (Q_b7 * i0_sun * wC_b7)
#   )
#   
#   return(data)
#   
# }
# 
