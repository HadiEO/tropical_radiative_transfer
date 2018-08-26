source("cache/libPaths.R")
require(xlsx)
require(tidyverse)
require(Rprospect)


# Input leaf biochemicals -------------------------------------------------
prospect_in <- read_csv2("data/leaf/PROSPECT_input_basedonHAWAII.csv")

# N = 1.7 fresh leaves in Jacquemoud et al. (1996)
# todo: if need to use prospect5, check if R function produces the same results as matlab code

# Function ----------------------------------------------------------------
my_prospect <- function(leaf.data = prospect_in, species = "Dipt_mean", N = 1.7, v4 = TRUE) {
  leaf.bio <- leaf.data %>%  dplyr::filter(Group == species) %>%  dplyr::select(Cab, Car, Cw, Cm) %>% mutate(Car = as.numeric(Car))
  if (v4 == FALSE) {
    if (is.na(leaf.bio$Car)) stop("Car measurement not available")
    leaf.spc <- with(leaf.bio, prospect5(N, Cab, Car, Cw, Cm)) # prospect5 uses Carotenoid as well
  } else {
    leaf.spc <- with(leaf.bio, prospect4(N, Cab, Cw, Cm)) 
  }
  
  leaf.spc$Albedo <- leaf.spc$Reflectance + leaf.spc$Transmittance
  leaf.spc <- as_tibble(leaf.spc)
  return(leaf.spc)
}


# Run my_prospect function ------------------------------------------------
wL.DiptMean <- my_prospect(prospect_in, "Dipt_mean", N = 1.7, v4 = TRUE)
# wL.DiptMean.Car <- my_prospect(prospect_in, "Dipt_mean", N = 1.7, v4 = FALSE)
wL.Elaeis <- my_prospect(prospect_in, "Elaeis", N = 1.7, v4 = TRUE)
wL.HawaiiMean <- my_prospect(prospect_in, "Hawaii_mean", N = 1.7, v4 = TRUE)
wL.GlobalMean <- my_prospect(prospect_in, "Global_mean", N = 1.7, v4 = TRUE)
wL.ShoreaMean <- my_prospect(prospect_in, "Shorea_mean", N = 1.7, v4 = TRUE)
wL.Jacq96Mean <- my_prospect(prospect_in, "Jacq1996_mean", N = 1.7, v4 = TRUE)

# Birch leaf albedo
wL.birch <- read_csv2("data/leaf/boreal_leaf_spc.csv") %>% select(Birch)


# Save them
write.csv2(wL.DiptMean, "results/leaf/wL_DiptMean.csv")
write.csv2(wL.Elaeis, "results/leaf/wL_Elaeis.csv")
write.csv2(wL.HawaiiMean, "results/leaf/wL_HawaiiMean.csv")
write.csv2(wL.GlobalMean, "results/leaf/wL_GlobalMean.csv")
write.csv2(wL.ShoreaMean, "results/leaf/wL_ShoreaMean.csv")
write.csv2(wL.Jacq96Mean, "results/leaf/wL_Jacq96Mean.csv")


# Simulate from a multivariate biochemical sampling -----------------------
source("tests/multivariate_sampling.R") # function to generate multivariate correlated samples
# Dipt.bio.sim <- leaf.bio.sim(n = 2500, leaf.data = prospect_in, 
#          mean = "Dipt_mean", sd = "Dipt_sd", # the rows in leaf.data
#          cor = c(0.37, -0.52, -0.67)) # Specifies correlation (Cab,Cw), (Cw,Cm), (Cab,Cm)




# Function to run prospect with many biochemical samples ------------------
# Todo: adjust the function to have N also varied
my_prospect_many <- function(samples, N = 1.7) {
  ptm <- proc.time()
  # Initialize output
  out <- list()
  n <- nrow(samples)
  out$Wavelength <- read_csv2("data/leaf/prospect_wav.csv") %>% .[["x"]]
  out$Reflectance <- array(NA, c(length(out$Wavelength),n))
  out$Transmittance <- array(NA, c(length(out$Wavelength),n))
  out$Albedo <- array(NA, c(length(out$Wavelength),n))
  
  for(i in 1:n){
    prospect_out <- prospect4(N = N, Cab = samples[[i,"Cab"]], Cw = samples[[i,"Cw"]], Cm = samples[[i,"Cm"]])  # Double [[]] to access value out of tibble
    
    out$Reflectance[,i] <- prospect_out[,2]
    out$Transmittance[,i] <- prospect_out[,3]
    out$Albedo[,i] <- prospect_out[,2] + prospect_out[,3]
  }
  return(out)
  print(proc.time() - ptm)
}

# Run the function my_prospect_many
wL.DiptSampling <- my_prospect_many(samples = Dipt.bio.sim, N = 1.7)
saveRDS(wL.DiptSampling, "results/leaf/leaf_sim_Dipt2500.rds") # Save it it takes long time!



# Influence of N on PROSPECT sim ------------------------------------------
prosp_1 <- my_prospect(prospect_in, "Dipt_mean", N = 1, v4 = TRUE)
prosp_1.7 <- my_prospect(prospect_in, "Dipt_mean", N = 1.7, v4 = TRUE)
prosp_2 <- my_prospect(prospect_in, "Dipt_mean", N = 2, v4 = TRUE)
prosp_2.5 <- my_prospect(prospect_in, "Dipt_mean", N = 2.5, v4 = TRUE)
prosp_3 <- my_prospect(prospect_in, "Dipt_mean", N = 3, v4 = TRUE)

R_1 <- spc_resample(prosp_1, "Reflectance", "Wavelength", sensor = "Landsat7")
T_1 <- spc_resample(prosp_1, "Transmittance", "Wavelength", sensor = "Landsat7")
wL_1 <- spc_resample(prosp_1, "Albedo", "Wavelength", sensor = "Landsat7")

R_1.7 <- spc_resample(prosp_1.7, "Reflectance", "Wavelength", sensor = "Landsat7")
T_1.7 <- spc_resample(prosp_1.7, "Transmittance", "Wavelength", sensor = "Landsat7")
wL_1.7 <- spc_resample(prosp_1.7, "Albedo", "Wavelength", sensor = "Landsat7")

R_2 <- spc_resample(prosp_2, "Reflectance", "Wavelength", sensor = "Landsat7")
T_2 <- spc_resample(prosp_2, "Transmittance", "Wavelength", sensor = "Landsat7")
wL_2 <- spc_resample(prosp_2, "Albedo", "Wavelength", sensor = "Landsat7")

R_2.5 <- spc_resample(prosp_2.5, "Reflectance", "Wavelength", sensor = "Landsat7")
T_2.5 <- spc_resample(prosp_2.5, "Transmittance", "Wavelength", sensor = "Landsat7")
wL_2.5 <- spc_resample(prosp_2.5, "Albedo", "Wavelength", sensor = "Landsat7")

R_3 <- spc_resample(prosp_3, "Reflectance", "Wavelength", sensor = "Landsat7")
T_3 <- spc_resample(prosp_3, "Transmittance", "Wavelength", sensor = "Landsat7")
wL_3 <- spc_resample(prosp_3, "Albedo", "Wavelength", sensor = "Landsat7")


# PROSPECT-4 vs PROSPECT-5 ------------------------------------------------
# First check R code of PROSPECT-5 vs matlab
prosp_v5 <- my_prospect(prospect_in, "Dipt_mean", N = 1.7, v4 = FALSE)                                        # PROSPECT-5 R code
prosp_v5_matlab <- read_delim("results/leaf/matlab_prosp5_DiptMean.txt", delim = "\t", col_names = FALSE)     # Maltab code result
colnames(prosp_v5_matlab) <- c("Wavelength", "Reflectance", "Transmittance")                
prosp_v5_matlab <- mutate(prosp_v5_matlab, Albedo = Reflectance + Transmittance)                              # Add Albedo column

R_v5 <- spc_resample(prosp_v5, "Reflectance", "Wavelength", sensor = "Landsat7")                              # Resample to L7
T_v5 <- spc_resample(prosp_v5, "Transmittance", "Wavelength", sensor = "Landsat7")
wL_v5 <- spc_resample(prosp_v5, "Albedo", "Wavelength", sensor = "Landsat7")

R_v5_matlab <- spc_resample(prosp_v5_matlab, "Reflectance", "Wavelength", sensor = "Landsat7")
T_v5_matlab <- spc_resample(prosp_v5_matlab, "Transmittance", "Wavelength", sensor = "Landsat7")
wL_v5_matlab <- spc_resample(prosp_v5_matlab, "Albedo", "Wavelength", sensor = "Landsat7")

prosp_v4 <- my_prospect(prospect_in, "Dipt_mean", N = 1.7, v4 = TRUE)                                          # PROSPECT-4
R_v4 <- spc_resample(prosp_v4, "Reflectance", "Wavelength", sensor = "Landsat7")
T_v4 <- spc_resample(prosp_v4, "Transmittance", "Wavelength", sensor = "Landsat7")
wL_v4 <- spc_resample(prosp_v4, "Albedo", "Wavelength", sensor = "Landsat7")



# Check PROSPECT-4 R code
prosp_v4_matlab <- read_delim("results/leaf/matlab_prosp4_DiptMean.txt", delim = "\t", col_names = FALSE)     # Maltab code result
colnames(prosp_v4_matlab) <- c("Wavelength", "Reflectance", "Transmittance")                
prosp_v4_matlab <- mutate(prosp_v4_matlab, Albedo = Reflectance + Transmittance)    

R_v4_matlab <- spc_resample(prosp_v4_matlab, "Reflectance", "Wavelength", sensor = "Landsat7")
T_v4_matlab <- spc_resample(prosp_v4_matlab, "Transmittance", "Wavelength", sensor = "Landsat7")
wL_v4_matlab <- spc_resample(prosp_v4_matlab, "Albedo", "Wavelength", sensor = "Landsat7")

