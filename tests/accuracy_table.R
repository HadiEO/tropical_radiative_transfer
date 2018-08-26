# Function to prepare table of RMSE, rRMSE, r, and R2 etc. between 
# simulated and measured BRF by band

source("munge/RMSE.R") # Get RMSE function
source("munge/SEPC.R") # SEPC function


accuracy_table <- function(paras.res) {
  
  nbands <- 5                                                     # Initialize output table
  ETM <- c("b2", "b3", "b4", "b5", "b7")
  acc.table <- tibble(band = ETM, RMSE = rep(999,nbands), rRMSE = rep(999,nbands), bias = rep(999,nbands), SEPC = rep(999,nbands), rSEPC = rep(999,nbands), r = rep(999,nbands), 
                      Landsat.mean = rep(999,nbands), Landsat.sd = rep(999,nbands), Landsat.min = rep(999,nbands), Landsat.max = rep(999,nbands),
                      Paras.mean = rep(999,nbands), Paras.sd = rep(999,nbands), Paras.min = rep(999,nbands), Paras.max = rep(999,nbands))
  
  
  acc.table$RMSE[1] <- with(paras.res, round(RMSE(BRF_b2_s, SR_B2_green), 3))           # RMSE
  acc.table$RMSE[2] <- with(paras.res, round(RMSE(BRF_b3_s, SR_B3_red), 3))
  acc.table$RMSE[3] <- with(paras.res, round(RMSE(BRF_b4_s, SR_B4_nir), 3))
  acc.table$RMSE[4] <- with(paras.res, round(RMSE(BRF_b5_s, SR_B5_swir1), 3))
  acc.table$RMSE[5] <- with(paras.res, round(RMSE(BRF_b7_s, SR_B7_swir2), 3))
  
  acc.table$Landsat.mean[1] <- with(paras.res, round(mean(SR_B2_green), 3))           # Mean measured
  acc.table$Landsat.mean[2] <- with(paras.res, round(mean(SR_B3_red), 3))
  acc.table$Landsat.mean[3] <- with(paras.res, round(mean(SR_B4_nir), 3))
  acc.table$Landsat.mean[4] <- with(paras.res, round(mean(SR_B5_swir1), 3))
  acc.table$Landsat.mean[5] <- with(paras.res, round(mean(SR_B7_swir2), 3))

  acc.table$rRMSE[1] <- with(acc.table, round((RMSE[1] / Landsat.mean[1]) * 100, 1)) # rRMSE normalized to mean Landsat
  acc.table$rRMSE[2] <- with(acc.table, round((RMSE[2] / Landsat.mean[2]) * 100, 1))
  acc.table$rRMSE[3] <- with(acc.table, round((RMSE[3] / Landsat.mean[3]) * 100, 1))
  acc.table$rRMSE[4] <- with(acc.table, round((RMSE[4] / Landsat.mean[4]) * 100, 1))
  acc.table$rRMSE[5] <- with(acc.table, round((RMSE[5] / Landsat.mean[5]) * 100, 1))
  
  acc.table$bias[1] <- with(paras.res, round(mean(BRF_b2_s - SR_B2_green), 3))           # Bias = modelled - measured
  acc.table$bias[2] <- with(paras.res, round(mean(BRF_b3_s - SR_B3_red), 3))
  acc.table$bias[3] <- with(paras.res, round(mean(BRF_b4_s - SR_B4_nir), 3))
  acc.table$bias[4] <- with(paras.res, round(mean(BRF_b5_s - SR_B5_swir1), 3))
  acc.table$bias[5] <- with(paras.res, round(mean(BRF_b7_s - SR_B7_swir2), 3))
  
  acc.table$SEPC[1] <- with(paras.res, round(SEPC(BRF_b2_s, SR_B2_green), 3))           # SEPC
  acc.table$SEPC[2] <- with(paras.res, round(SEPC(BRF_b3_s, SR_B3_red), 3))
  acc.table$SEPC[3] <- with(paras.res, round(SEPC(BRF_b4_s, SR_B4_nir), 3))
  acc.table$SEPC[4] <- with(paras.res, round(SEPC(BRF_b5_s, SR_B5_swir1), 3))
  acc.table$SEPC[5] <- with(paras.res, round(SEPC(BRF_b7_s, SR_B7_swir2), 3))
  
  acc.table$rSEPC[1] <- with(acc.table, round((SEPC[1] / Landsat.mean[1]) * 100, 1)) # rSEPC normalized to mean Landsat
  acc.table$rSEPC[2] <- with(acc.table, round((SEPC[2] / Landsat.mean[2]) * 100, 1))
  acc.table$rSEPC[3] <- with(acc.table, round((SEPC[3] / Landsat.mean[3]) * 100, 1))
  acc.table$rSEPC[4] <- with(acc.table, round((SEPC[4] / Landsat.mean[4]) * 100, 1))
  acc.table$rSEPC[5] <- with(acc.table, round((SEPC[5] / Landsat.mean[5]) * 100, 1))
  

  acc.table$r[1] <- with(paras.res, round(cor(BRF_b2_s, SR_B2_green), 3))  # correlation r
  acc.table$r[2] <- with(paras.res, round(cor(BRF_b3_s, SR_B3_red), 3))
  acc.table$r[3] <- with(paras.res, round(cor(BRF_b4_s, SR_B4_nir), 3))
  acc.table$r[4] <- with(paras.res, round(cor(BRF_b5_s, SR_B5_swir1), 3))
  acc.table$r[5] <- with(paras.res, round(cor(BRF_b7_s, SR_B7_swir2), 3))
  
  acc.table$Landsat.sd[1] <- with(paras.res, round(sd(SR_B2_green), 3))           # Sd measured
  acc.table$Landsat.sd[2] <- with(paras.res, round(sd(SR_B3_red), 3))
  acc.table$Landsat.sd[3] <- with(paras.res, round(sd(SR_B4_nir), 3))
  acc.table$Landsat.sd[4] <- with(paras.res, round(sd(SR_B5_swir1), 3))
  acc.table$Landsat.sd[5] <- with(paras.res, round(sd(SR_B7_swir2), 3))
  
  acc.table$Landsat.min[1] <- with(paras.res, round(min(SR_B2_green), 3))           # Minimum measured
  acc.table$Landsat.min[2] <- with(paras.res, round(min(SR_B3_red), 3))
  acc.table$Landsat.min[3] <- with(paras.res, round(min(SR_B4_nir), 3))
  acc.table$Landsat.min[4] <- with(paras.res, round(min(SR_B5_swir1), 3))
  acc.table$Landsat.min[5] <- with(paras.res, round(min(SR_B7_swir2), 3))
  
  acc.table$Landsat.max[1] <- with(paras.res, round(max(SR_B2_green), 3))           # Maximum measured
  acc.table$Landsat.max[2] <- with(paras.res, round(max(SR_B3_red), 3))
  acc.table$Landsat.max[3] <- with(paras.res, round(max(SR_B4_nir), 3))
  acc.table$Landsat.max[4] <- with(paras.res, round(max(SR_B5_swir1), 3))
  acc.table$Landsat.max[5] <- with(paras.res, round(max(SR_B7_swir2), 3))
  
  # Simulated ##############################################################################################
  
  acc.table$Paras.mean[1] <- with(paras.res, round(mean(BRF_b2_s), 3))           # Mean simulated
  acc.table$Paras.mean[2] <- with(paras.res, round(mean(BRF_b3_s), 3))
  acc.table$Paras.mean[3] <- with(paras.res, round(mean(BRF_b4_s), 3))
  acc.table$Paras.mean[4] <- with(paras.res, round(mean(BRF_b5_s), 3))
  acc.table$Paras.mean[5] <- with(paras.res, round(mean(BRF_b7_s), 3))
  
  acc.table$Paras.sd[1] <- with(paras.res, round(sd(BRF_b2_s), 3))           # Sd simulated
  acc.table$Paras.sd[2] <- with(paras.res, round(sd(BRF_b3_s), 3))
  acc.table$Paras.sd[3] <- with(paras.res, round(sd(BRF_b4_s), 3))
  acc.table$Paras.sd[4] <- with(paras.res, round(sd(BRF_b5_s), 3))
  acc.table$Paras.sd[5] <- with(paras.res, round(sd(BRF_b7_s), 3))
  
  acc.table$Paras.min[1] <- with(paras.res, round(min(BRF_b2_s), 3))           # Minimum simulated
  acc.table$Paras.min[2] <- with(paras.res, round(min(BRF_b3_s), 3))
  acc.table$Paras.min[3] <- with(paras.res, round(min(BRF_b4_s), 3))
  acc.table$Paras.min[4] <- with(paras.res, round(min(BRF_b5_s), 3))
  acc.table$Paras.min[5] <- with(paras.res, round(min(BRF_b7_s), 3))
  
  acc.table$Paras.max[1] <- with(paras.res, round(max(BRF_b2_s), 3))           # Maximum simulated
  acc.table$Paras.max[2] <- with(paras.res, round(max(BRF_b3_s), 3))
  acc.table$Paras.max[3] <- with(paras.res, round(max(BRF_b4_s), 3))
  acc.table$Paras.max[4] <- with(paras.res, round(max(BRF_b5_s), 3))
  acc.table$Paras.max[5] <- with(paras.res, round(max(BRF_b7_s), 3))
  
  return(acc.table)
  
}



comparison_table <- function(basecase, perturbedcase) {
  
  nbands <- 5                                                     # Initialize output table
  ETM <- c("b2", "b3", "b4", "b5", "b7")
  comp.table <- tibble(band = ETM, base.mean = rep(999,nbands), MD = rep(999,nbands), rMD = rep(999,nbands), RMSD = rep(999,nbands), rRMSD = rep(999,nbands),              # r : relative to mean base case simulation
                      pert.mean = rep(999,nbands), pert.sd = rep(999,nbands), pert.min = rep(999,nbands), pert.max = rep(999,nbands))

  comp.table$base.mean[1] <- with(basecase, round(mean(BRF_b2_s), 3))           # Mean base case simulation
  comp.table$base.mean[2] <- with(basecase, round(mean(BRF_b3_s), 3))
  comp.table$base.mean[3] <- with(basecase, round(mean(BRF_b4_s), 3))
  comp.table$base.mean[4] <- with(basecase, round(mean(BRF_b5_s), 3))
  comp.table$base.mean[5] <- with(basecase, round(mean(BRF_b7_s), 3))
  
  comp.table$MD[1] <- round(mean(perturbedcase$BRF_b2_s - basecase$BRF_b2_s), 3)  # Mean difference = perturbed.case - base.case
  comp.table$MD[2] <- round(mean(perturbedcase$BRF_b3_s - basecase$BRF_b3_s), 3)
  comp.table$MD[3] <- round(mean(perturbedcase$BRF_b4_s - basecase$BRF_b4_s), 3)
  comp.table$MD[4] <- round(mean(perturbedcase$BRF_b5_s - basecase$BRF_b5_s), 3)
  comp.table$MD[5] <- round(mean(perturbedcase$BRF_b7_s - basecase$BRF_b7_s), 3)
  
  comp.table$rMD[1] <- with(comp.table, round((MD[1] / base.mean[1]) * 100, 1))   # rMD normalized to mean base case simulation
  comp.table$rMD[2] <- with(comp.table, round((MD[2] / base.mean[2]) * 100, 1))
  comp.table$rMD[3] <- with(comp.table, round((MD[3] / base.mean[3]) * 100, 1))
  comp.table$rMD[4] <- with(comp.table, round((MD[4] / base.mean[4]) * 100, 1))
  comp.table$rMD[5] <- with(comp.table, round((MD[5] / base.mean[5]) * 100, 1))
  
  comp.table$RMSD[1] <- round(RMSE(perturbedcase$BRF_b2_s, basecase$BRF_b2_s), 3)                      # RMSD, but RMSE function
  comp.table$RMSD[2] <- round(RMSE(perturbedcase$BRF_b3_s, basecase$BRF_b3_s), 3)
  comp.table$RMSD[3] <- round(RMSE(perturbedcase$BRF_b4_s, basecase$BRF_b4_s), 3)
  comp.table$RMSD[4] <- round(RMSE(perturbedcase$BRF_b5_s, basecase$BRF_b5_s), 3)
  comp.table$RMSD[5] <- round(RMSE(perturbedcase$BRF_b7_s, basecase$BRF_b7_s), 3)
  
  comp.table$rRMSD[1] <- with(comp.table, round((RMSD[1] / base.mean[1]) * 100, 1)) # rRMSD normalized to mean base case simulation
  comp.table$rRMSD[2] <- with(comp.table, round((RMSD[2] / base.mean[2]) * 100, 1))
  comp.table$rRMSD[3] <- with(comp.table, round((RMSD[3] / base.mean[3]) * 100, 1))
  comp.table$rRMSD[4] <- with(comp.table, round((RMSD[4] / base.mean[4]) * 100, 1))
  comp.table$rRMSD[5] <- with(comp.table, round((RMSD[5] / base.mean[5]) * 100, 1))
  
  comp.table$pert.mean[1] <- round(mean(perturbedcase$BRF_b2_s), 3)           # Mean simulated
  comp.table$pert.mean[2] <- round(mean(perturbedcase$BRF_b3_s), 3)
  comp.table$pert.mean[3] <- round(mean(perturbedcase$BRF_b4_s), 3)
  comp.table$pert.mean[4] <- round(mean(perturbedcase$BRF_b5_s), 3)
  comp.table$pert.mean[5] <- round(mean(perturbedcase$BRF_b7_s), 3)
  
  comp.table$pert.sd[1] <- round(sd(perturbedcase$BRF_b2_s), 3)           # Sd simulated
  comp.table$pert.sd[2] <- round(sd(perturbedcase$BRF_b3_s), 3)
  comp.table$pert.sd[3] <- round(sd(perturbedcase$BRF_b4_s), 3)
  comp.table$pert.sd[4] <- round(sd(perturbedcase$BRF_b5_s), 3)
  comp.table$pert.sd[5] <- round(sd(perturbedcase$BRF_b7_s), 3)
  
  comp.table$pert.min[1] <- round(min(perturbedcase$BRF_b2_s), 3)           # Minimum simulated
  comp.table$pert.min[2] <- round(min(perturbedcase$BRF_b3_s), 3)
  comp.table$pert.min[3] <- round(min(perturbedcase$BRF_b4_s), 3)
  comp.table$pert.min[4] <- round(min(perturbedcase$BRF_b5_s), 3)
  comp.table$pert.min[5] <- round(min(perturbedcase$BRF_b7_s), 3)
  
  comp.table$pert.max[1] <- round(max(perturbedcase$BRF_b2_s), 3)           # Maximum simulated
  comp.table$pert.max[2] <- round(max(perturbedcase$BRF_b3_s), 3)
  comp.table$pert.max[3] <- round(max(perturbedcase$BRF_b4_s), 3)
  comp.table$pert.max[4] <- round(max(perturbedcase$BRF_b5_s), 3)
  comp.table$pert.max[5] <- round(max(perturbedcase$BRF_b7_s), 3)
  
  return(comp.table)
}