wL.Dipt2500 <- readRDS("results/leaf/leaf_sim_Dipt2500.rds")                             # PROSPECT simulations with biochemical sampling

# (1) Add noise expected from PROSPECT forward simulation  ----------------              # Simulate noise based on bias and SEPC in
idx.vis <- which(wL.Dipt2500$Wavelength <= 800)        # VIS (400 - 800 nm)              # Feret et al. (2008) for Hawaii species
idx.ir <- which(wL.Dipt2500$Wavelength > 800)         # IR (800 - 2450 nm)               # SEPC = standard error corrected from bias

prosp.bias <- list(
  Refl = list(vis = 0.008, ir = 0.031),
  Tran = list(vis = 0.002, ir = -0.003)
)

wL.Dipt2500$Refl_prosp.bias <- array(999, dim(wL.Dipt2500$Reflectance))     
wL.Dipt2500$Tran_prosp.bias <- array(999, dim(wL.Dipt2500$Transmittance))

#  First, add bias as constant --------------------------------------------

wL.Dipt2500$Refl_prosp.bias[idx.vis,] <- wL.Dipt2500$Reflectance[idx.vis,] + prosp.bias$Refl$vis       # Refl
wL.Dipt2500$Refl_prosp.bias[idx.ir,] <- wL.Dipt2500$Reflectance[idx.ir,] + prosp.bias$Refl$ir

wL.Dipt2500$Tran_prosp.bias[idx.vis,] <- wL.Dipt2500$Transmittance[idx.vis,] + prosp.bias$Tran$vis   # Tran
wL.Dipt2500$Tran_prosp.bias[idx.ir,] <- wL.Dipt2500$Transmittance[idx.ir,] + prosp.bias$Tran$ir


# Second, add SEPC randomly sampled for each obs (plot) -------------------
set.seed(123)
prosp.sepc <- list(                                                                      
  Refl = list(vis = rnorm(n = ncol(wL.Dipt2500$Reflectance), mean = 0, sd = 0.017),   
              ir =  rnorm(n = ncol(wL.Dipt2500$Reflectance), mean = 0, sd = 0.017)),
  Tran = list(vis = rnorm(n = ncol(wL.Dipt2500$Reflectance), mean = 0, sd = 0.019),
              ir =  rnorm(n = ncol(wL.Dipt2500$Reflectance), mean = 0, sd = 0.017))
)


wL.Dipt2500$Refl_prosp.bias.sepc <- array(999, dim(wL.Dipt2500$Reflectance))     
wL.Dipt2500$Tran_prosp.bias.sepc <- array(999, dim(wL.Dipt2500$Transmittance))



for(i in 1:ncol(wL.Dipt2500$Refl_prosp.bias.sepc)) {
  wL.Dipt2500$Refl_prosp.bias.sepc[idx.vis,i] <- wL.Dipt2500$Refl_prosp.bias[idx.vis,i] + prosp.sepc$Refl$vis[i]    # Refl CHECK if + or -!!!!
  wL.Dipt2500$Refl_prosp.bias.sepc[idx.ir,i] <- wL.Dipt2500$Refl_prosp.bias[idx.ir,i] + prosp.sepc$Refl$ir[i]
  
  wL.Dipt2500$Tran_prosp.bias.sepc[idx.vis,i] <- wL.Dipt2500$Tran_prosp.bias[idx.vis,i] + prosp.sepc$Tran$vis[i]    # Tran
  wL.Dipt2500$Tran_prosp.bias.sepc[idx.ir,i] <- wL.Dipt2500$Tran_prosp.bias[idx.ir,i] + prosp.sepc$Tran$ir[i]
}


# Check negative value ----------------------------------------------------

# If sepc <= -(prosp.bias) causing negative value, pick another random value
temp <- as_tibble(wL.Dipt2500$Tran_prosp.bias.sepc[idx.vis,])                             # Check negative value
temp.min <- temp %>% map_dbl(min)
length(which(temp.min < 0))

for(i in 1:ncol(wL.Dipt2500$Refl_prosp.bias.sepc)) {
  Refl.vis <- wL.Dipt2500$Refl_prosp.bias.sepc[idx.vis,i]                                           # Refl, VIS
  while(any(Refl.vis <= 0)) {
    Refl.vis <- wL.Dipt2500$Refl_prosp.bias.sepc[idx.vis,i]
    sepc <- rnorm(n = 1, mean = 0, sd = 0.017)
    Refl.vis <- Refl.vis + sepc
  }
  wL.Dipt2500$Refl_prosp.bias.sepc[idx.vis,i] <- Refl.vis
}
  
for(i in 1:ncol(wL.Dipt2500$Refl_prosp.bias.sepc)) {
  Refl.ir <- wL.Dipt2500$Refl_prosp.bias.sepc[idx.ir,i]                                           # Refl, IR
  while(any(Refl.ir <= 0)) {
    Refl.ir <- wL.Dipt2500$Refl_prosp.bias.sepc[idx.ir,i]
    sepc <- rnorm(n = 1, mean = 0, sd = 0.017)
    Refl.ir <- Refl.ir + sepc
  }
  wL.Dipt2500$Refl_prosp.bias.sepc[idx.ir,i] <- Refl.ir
}


for(i in 1:ncol(wL.Dipt2500$Tran_prosp.bias.sepc)) {
  Tran.vis <- wL.Dipt2500$Tran_prosp.bias.sepc[idx.vis,i]                                           # Tran, VIS
  while(any(Tran.vis <= 0)) {
    Tran.vis <- wL.Dipt2500$Tran_prosp.bias.sepc[idx.vis,i]
    sepc <- rnorm(n = 1, mean = 0, sd = 0.019)
    Tran.vis <- Tran.vis + sepc
  }
  wL.Dipt2500$Tran_prosp.bias.sepc[idx.vis,i] <- Tran.vis
}

for(i in 1:ncol(wL.Dipt2500$Tran_prosp.bias.sepc)) {
  Tran.ir <- wL.Dipt2500$Tran_prosp.bias.sepc[idx.ir,i]                                           # Tran, IR
  while(any(Tran.ir <= 0)) {
    Tran.ir <- wL.Dipt2500$Tran_prosp.bias.sepc[idx.ir,i]
    sepc <- rnorm(n = 1, mean = 0, sd = 0.017)
    Tran.ir <- Tran.ir + sepc
  }
  wL.Dipt2500$Tran_prosp.bias.sepc[idx.ir,i] <- Tran.ir
}

# Add Albedo --------------------------------------------------------------
wL.Dipt2500$Albedo_prosp.bias.sepc <- array(999, dim(wL.Dipt2500$Albedo))                                           # Add Albedo
wL.Dipt2500$Albedo_prosp.bias.sepc <- wL.Dipt2500$Refl_prosp.bias.sepc + wL.Dipt2500$Tran_prosp.bias.sepc





# Save
saveRDS(wL.Dipt2500, "results/leaf/leaf_sim_Dipt2500_addprospnoise.rds")



# Code to check  ----------------------------------------------------------
with(wL.Dipt2500,
     plot(Reflectance[,1000], Refl_prosp.bias.sepc[,1000], xlim = c(0,1), ylim = c(0,0.6))); abline(0,0.6)

windows()
plot(wL.Dipt2500$Wavelength, wL.Dipt2500$Reflectance[,800], type = "l", ylim = c(0,1))
lines(wL.Dipt2500$Wavelength, wL.Dipt2500$Refl_prosp.bias[,800], col = "red")
lines(wL.Dipt2500$Wavelength, wL.Dipt2500$Refl_prosp.bias[,800], col = "green")

hist(prosp.sepc$Refl$vis)
sd(prosp.sepc$Refl$vis)

windows()
with(wL.Dipt2500,
     plot(Reflectance[,2000], Refl_prosp.bias[,2000], xlim = c(0,1), ylim = c(0,0.6))); abline(0,0.6)

sepc <- array(999, dim(wL.Dipt2500$Reflectance))   
for(i in 1:ncol(wL.Dipt2500$Refl_prosp.bias.sepc[idx.vis,])) {
  sepc[,i] <- wL.Dipt2500$Tran_prosp.bias.sepc[,i] - wL.Dipt2500$Tran_prosp.bias[,i]
}

sepc[,1000]








# (2) Add noise expected in surface reflectance product -------------------
# 0.05*SR + 0.005 based on Claverie et al. (2015)
# But to be added to PARAS BRF of Landsat bands in paras_main.R Case 10



