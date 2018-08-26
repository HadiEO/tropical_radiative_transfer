# Tested OK

source("cache/libPaths.R")
require(rgdal); require(rgeos); require(raster); require(xlsx); require(sp);

# require(maptools);require(RStoolbox);require(ggplot2);
# require(rasterVis);require(shapefiles)
# # Import packages for "spectra"
# require(hyperSpec);require(prospectr);require(caret);require(chemometrics)


options(scipen=999) # Disable scientific notation printing


# Convert factor to numeric
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}



# Image directories -------------------------------------------------------
# Needs full path
gen.wd <- "C:/LocalUserData/User-data/hadi1/PHD_RESEARCH/STUDY_II_TROPICS_PARAS/Rproj_TROPICS_PARAS/data/images"  

# SAFE
SAFE.L7.201208 <- "/LE71170572012240-SC20160930102058.tar/clipped"
SAFE.L7.201209 <- "/LE71170572012272-SC20160930102025.tar/clipped"
SAFE.L7.201210 <- "/LE71170572012304-SC20160930102141.tar/clipped"
SAFE.L7.20121217 <- "/LE71170572012352-SC20160930102031.tar/clipped"
SAFE.L7.20121201 <- "/LE71170572012336-SC20160930102236.tar/clipped"
SAFE.L7.201306 <- "/LE71170572013178-SC20160930102108.tar/clipped" # This one is in excel

# Add more plots relaxing time window from Sept 2011
SAFE.L7.20111113 <- "/LE71170572011317-SC20161220110957.tar/clipped"
SAFE.L7.20120421 <- "/LE71170572012112-SC20161220111101.tar/clipped"
SAFE.L7.20120405 <- "/LE71170572012096-SC20161220111111.tar/clipped"
SAFE.L7.20111028 <- "/LE71170572011301-SC20161220110947.tar/clipped"
SAFE.L7.20111215 <- "/LE71170572011349-SC20161220110937.tar/clipped"



# Import shapefile --------------------------------------------------------

shp_folder <- "data/gis"
SAFE.shp <- readOGR(dsn=shp_folder, layer="SAFE_plots_UTM50N") # This has all 194 plots



# Extract spectra --------------------------------------------------------
source("tests/extract_L7_relaxtime_fun.R") # source the function

SAFE.L7.201208.spc <- extract.L7.relaxtime(SAFE.L7.201208, SAFE.shp); SAFE.L7.201208.spc$scene <- "L7.201208" # 111
# SAFE.L7.201209.spc <- extract.L7.relaxtime(SAFE.L7.201209, SAFE.shp); SAFE.L7.201209.spc$scene <- "L7.201209" # no clear plots
# SAFE.L7.201210.spc <- extract.L7.relaxtime(SAFE.L7.201210, SAFE.shp); SAFE.L7.201210.spc$scene <- "L7.201210" # no clear plots
SAFE.L7.20121217.spc <- extract.L7.relaxtime(SAFE.L7.20121217, SAFE.shp); SAFE.L7.20121217.spc$scene <- "L7.20121217" # 74
SAFE.L7.20121201.spc <- extract.L7.relaxtime(SAFE.L7.20121201, SAFE.shp); SAFE.L7.20121201.spc$scene <- "L7.20121201" # 19


# Add more plots relaxing time window from Sept 2011
SAFE.L7.20111113.spc <- extract.L7.relaxtime(SAFE.L7.20111113, SAFE.shp); SAFE.L7.20111113.spc$scene <- "L7.20111113" # 9 plots
SAFE.L7.20120421.spc <- extract.L7.relaxtime(SAFE.L7.20120421, SAFE.shp); SAFE.L7.20120421.spc$scene <- "L7.20120421" # 65 plots
SAFE.L7.20120405.spc <- extract.L7.relaxtime(SAFE.L7.20120405, SAFE.shp); SAFE.L7.20120405.spc$scene <- "L7.20120405" # 6 plots
SAFE.L7.20111028.spc <- extract.L7.relaxtime(SAFE.L7.20111028, SAFE.shp); SAFE.L7.20111028.spc$scene <- "L7.20111028" # 41 plots
SAFE.L7.20111215.spc <- extract.L7.relaxtime(SAFE.L7.20111215, SAFE.shp); SAFE.L7.20111215.spc$scene <- "L7.20111215" # 21 plots

# Save the extracted spectra
spc_list <- list(L7.20111028=SAFE.L7.20111028.spc, L7.20111113=SAFE.L7.20111113.spc, L7.20111215=SAFE.L7.20111215.spc,
                 L7.20120405=SAFE.L7.20120405.spc, L7.20120421=SAFE.L7.20120421.spc, L7.201208=SAFE.L7.201208.spc,
                 L7.20121201=SAFE.L7.20121201.spc, L7.20121217=SAFE.L7.20121217.spc)

saveRDS(spc_list, file="results/SAFE_spc_Sept11_Jan13.rds")
