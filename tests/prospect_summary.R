require(tidyverse)

wL.samples.summary <- function(wL.samples) {
  
  out <-  tibble(wav = wL.samples$Wavelength,                                                            # Wavelength
                 
                 R.mean = apply(wL.samples$Reflectance, 1, mean),                                               # Reflectance
                 R.sd = apply(wL.samples$Reflectance, 1, sd),                                                   
                 R.meanminsd = apply(wL.samples$Reflectance, 1, mean) - apply(wL.samples$Reflectance, 1, sd),
                 R.meanplussd = apply(wL.samples$Reflectance, 1, mean) + apply(wL.samples$Reflectance, 1, sd),
                 R.min = apply(wL.samples$Reflectance, 1, min),
                 R.max = apply(wL.samples$Reflectance, 1, max),
                 
                 T.mean = apply(wL.samples$Transmittance, 1, mean),                                             # Transmittance
                 T.sd = apply(wL.samples$Transmittance, 1, sd),
                 T.meanminsd = apply(wL.samples$Transmittance, 1, mean) - apply(wL.samples$Transmittance, 1, sd),
                 T.meanplussd = apply(wL.samples$Transmittance, 1, mean) + apply(wL.samples$Transmittance, 1, sd),
                 T.min = apply(wL.samples$Transmittance, 1, min),
                 T.max = apply(wL.samples$Transmittance, 1, max),
                 
                 wL.mean = apply(wL.samples$Albedo, 1, mean),                                                    # Albedo
                 wL.sd = apply(wL.samples$Albedo, 1, sd),                                                   
                 wL.meanminsd = apply(wL.samples$Albedo, 1, mean) - apply(wL.samples$Albedo, 1, sd),
                 wL.meanplussd = apply(wL.samples$Albedo, 1, mean) + apply(wL.samples$Albedo, 1, sd),
                 wL.min = apply(wL.samples$Albedo, 1, min),
                 wL.max = apply(wL.samples$Albedo, 1, max),
                 
                 wL.median = apply(wL.samples$Albedo, 1, median),
                 wL.medianminsd = apply(wL.samples$Albedo, 1, median) - apply(wL.samples$Albedo, 1, sd),
                 wL.medianplussd = apply(wL.samples$Albedo, 1, median) + apply(wL.samples$Albedo, 1, sd)
                 
                 
  )
  return(out)
}



# Function to resample PROSPECT simulations (mean etc.) -------------------
source("munge/spc_resample.R")                                                   # Import spectral resampling function

wL.summary.resample <- function(wL.summary) {
  
  out <- array(999, c(6, ncol(wL.summary)))
  out <- as_tibble(out)
    colnames(out) <- colnames(wL.summary)
  
  for(i in 2:ncol(wL.summary)) {   # Column 1 is wav                          
    out[,i] <- t(spc_resample(wL.summary, spc = colnames(wL.summary)[i], wav = "wav", sensor = "Landsat7"))
  }
  
  L7.wav <- c(483, 560, 662, 835, 1648, 2206)  # b1 blue omitted
  out$wav <- L7.wav
  
  return(out)
  
}