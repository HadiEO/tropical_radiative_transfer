source("cache/libPaths.R")
require(xlsx)
require(tidyverse)
require(mgcv)


setwd("C:/LocalUserData/User-data/hadi1/hadi_phd_data/SAFE/soil_spectra")
library(xlsx)
soil_spc <- read.xlsx("selected_soil.xlsx", header=T, sheetName="spectra")

soil_wav <- seq(350, 2500, by=10)

source("C:/Users/hadi1/Dropbox/PhD_Hadi_Aalto/Scripts/repcol_row_function.R")


setwd("C:/LocalUserData/User-data/hadi1/hadi_phd_data/SAFE/RESULTS/highres_fig")
tiff("soil_spectra.tiff", res=1000, height=8, width=12, units="cm", compression="lzw", pointsize=8)

# par(oma = c(0,0,0,0), mai = c(0.3,0.3,0.1,0), 
#     ps = 7, cex = 1, mgp = c(1.3, 0.5, 0), mfrow=c(2,2))

par(oma = c(0,0,0,0), mai = c(0.3,0.3,0.3,0.1), 
    ps = 7, cex = 1, mgp = c(1.3, 0.5, 0), mfrow=c(2,2))


matplot(rep.col(soil_wav, 4), 
        t(soil_spc[soil_spc$Vegetation=="Protected primary Dipterocarp forest", 4:ncol(soil_spc)]),
        type="l", lty=1, xlab="Wavelength (nm)", ylab="Reflectance", main="Protected primary Dipterocarp forest",
        ylim=c(0,1), xaxp=c(400,2400,10))

matplot(rep.col(soil_wav, 6), 
        t(soil_spc[soil_spc$Vegetation=="Primary, Central Kalimantan, Bukit Raya Nature Reserve", 4:ncol(soil_spc)]),
        type="l", lty=1, xlab="Wavelength (nm)", ylab="Reflectance", main="Primary, Central Kalimantan",
        ylim=c(0,1), xaxp=c(400,2400,10))

matplot(rep.col(soil_wav, 5), 
        t(soil_spc[soil_spc$Vegetation=="Secondary forest, about 7 years old", 4:ncol(soil_spc)]),
        type="l", lty=1, xlab="Wavelength (nm)", ylab="Reflectance", main="Secondary forest, about 7 years old",
        ylim=c(0,1), xaxp=c(400,2400,10))

matplot(rep.col(soil_wav, 7), 
        t(soil_spc[soil_spc$Vegetation=="Ecuador, forest cleared, now oil palm and pasture", 4:ncol(soil_spc)]),
        type="l", lty=1, xlab="Wavelength (nm)", ylab="Reflectance", main="Ecuador, forest cleared\nnow oil palm and pasture",
        ylim=c(0,1), xaxp=c(400,2400,10))

dev.off()



# Estimate soil reflectance from BRF vs ECC -------------------------------
data <- read_csv2("results/gaps_spc_field_anc.csv")
data <- mutate(data, scene.Date = as.character(scene.Date))
data <-  data %>%  mutate(ECC_my = (1 - Gaps1_my) * 100) # add ECC


forest <- data %>% dplyr::filter(Biome == "Forest") 
OP <- data %>% dplyr::filter(Biome == "Plantation_Palm") 

# Function
soil.BRF.interc <- function(data) {

  mod.blue <- lm(SR_B1_blue ~ ECC_my, data = data)
  blue <- coef(mod.blue)["(Intercept)"] 
  R2.blue <- summary(mod.blue)$r.squared
  
  mod.green <- lm(SR_B2_green ~ ECC_my, data = data)
  green <- coef(mod.green)["(Intercept)"]
  R2.green <- summary(mod.green)$r.squared
  
  mod.red <- lm(SR_B3_red ~ ECC_my, data = data)
  red <- coef(mod.red)["(Intercept)"] 
  R2.red <- summary(mod.red)$r.squared
  
  mod.nir <- lm(SR_B4_nir ~ ECC_my, data = data)
  nir <- coef(mod.nir)["(Intercept)"] 
  R2.nir <- summary(mod.nir)$r.squared
  
  mod.swir1 <- lm(SR_B5_swir1 ~ ECC_my, data = data)
  swir1 <- coef(mod.swir1)["(Intercept)"] 
  R2.swir1 <- summary(mod.swir1)$r.squared
  
  mod.swir2 <- lm(SR_B7_swir2 ~ ECC_my, data = data)
  swir2 <- coef(mod.swir2)["(Intercept)"] 
  R2.swir2 <- summary(mod.swir2)$r.squared
  
  res <- list(intercept = tibble(b1 = blue, b2 = green, b3 = red, b4 = nir, b5 = swir1, b7 = swir2),
              R2 = tibble(R2.blue, R2.green, R2.red, R2.nir, R2.swir1, R2.swir2))
  return(res)
}
  
soil.interc.forest <- soil.BRF.interc(forest)
soil.interc.OP <- soil.BRF.interc(OP)

write.csv2(soil.interc.forest$intercept, "data/soil/soil_interc_forest.csv")                   # Save
write.csv2(soil.interc.OP$intercept, "data/soil/soil_interc_OP.csv")


