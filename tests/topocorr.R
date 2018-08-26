
# Topographic correction --------------------------------------------------
.libPaths(c("C:\\Program Files\\R\\R-3.3.1\\library", "\\home.org.aalto.fi\\hadi1\\data\\Documents\\R\\R-3.3.1\\library")) 
library(landsat)
library(raster)

# dem.smooth5 <- slopeasp(dem, smoothing = 5)
# nov4.ccorsmooth <- topocorr(nov4.ar, dem.smooth5$slope, dem.smooth5$aspect,
#                                + sunelev = 26.2, sunazimuth = 159.5, method = "ccorrection")

# Import the DEM ----------------------------------------------------------
setwd("C:/LocalUserData/User-data/hadi1/hadi_phd_data/SAFE/images/DEM/SRTM")
srtm_dem_right <- raster('n04_e117_1arc_v3.tif')
srtm_dem_left <- raster('n04_e116_1arc_v3.tif')
srtm_dem <- mosaic(srtm_dem_right, srtm_dem_left, fun='mean')
# Crop to study area in Arcmap to be safe
writeRaster(srtm_dem, 'srtm_dem_mosaic.tif')
# Import the Arcmap-clipped result
srtm_dem_clip <- raster('srtm_dem_mosaic_clip.tif')
srtm_dem_clip_SpGDF <- as(srtm_dem_clip, 'SpatialGridDataFrame')


# Calculate slope and aspect ----------------------------------------------
# Input SpatialGridDataFrame
srtm_slopeasp <- slopeasp(srtm_dem_clip_SpGDF)
srtm_slope_rst <- raster(srtm_slopeasp$slope)
srtm_asp_rst <- raster(srtm_slopeasp$aspect)
# THIS RESULTS IN WEIRD SLOPE VALUES OF MOSTLY >80 DEG, IS THE DEM SO ERRONEOUS? LET'S COMPUTE SLOPE IN ARCMAP

# Try smoothing
srtm_slopeasp_smth5 <- slopeasp(srtm_dem_clip_SpGDF, smoothing=5)



# Reproject slopeasp to the L7 image
# L7_crs <- CRS('+proj=utm +zone=50 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0')
srtm_slope_UTM <- projectRaster(srtm_slope_rst, to=raster.sr.st, method='ngb') # Get raster.sr.st from test-running  stack_topocorr_extract_L7
srtm_asp_UTM <- projectRaster(srtm_asp_rst, to=raster.sr.st, method='ngb')
srtm_slopeasp_UTM <- list(slope=as(srtm_slope_UTM, 'SpatialGridDataFrame'),
                          aspect=as(srtm_asp_UTM, 'SpatialGridDataFrame'))

# WGS coordinates of final usable plots -----------------------------------
library(xlsx)
SAFE_gaps_spc <- read.xlsx("C:/LocalUserData/User-data/hadi1/hadi_phd_data/SAFE/RESULTS/gaps_spc_final/SAFE_gaps_spc_final_75min10plots.xlsx",
                           sheetIndex=1)

SAFE_use_wgs <- SAFE_gaps_spc[,c("GPS_Lon", "GPS_Lat")]
  
SAFE_use_wgs <- SpatialPoints(SAFE_use_wgs, proj4string=crs(srtm_dem), bbox = NULL)

windows()
plot(srtm_dem_clip)
plot(SAFE_use_wgs, add=T)
# not needed

# Load landsat images subset, convert to SpGDF and apply topocorr ---------
gen.wd <- "C:/LocalUserData/User-data/hadi1/hadi_phd_data/SAFE/images/downloaded_20161001"

SAFE.L7.201208 <- "/SAFE/LE71170572012240-SC20160930102058.tar/clipped"
SAFE.L7.201209 <- "/SAFE/LE71170572012272-SC20160930102025.tar/clipped"
SAFE.L7.201210 <- "/SAFE/LE71170572012304-SC20160930102141.tar/clipped"
SAFE.L7.20121217 <- "/SAFE/LE71170572012352-SC20160930102031.tar/clipped"
SAFE.L7.20121201 <- "/SAFE/LE71170572012336-SC20160930102236.tar/clipped"
SAFE.L7.201306 <- "/SAFE/LE71170572013178-SC20160930102108.tar/clipped" # This one is in excel
# or
# SAFE.L8.201306 <- "/SAFE/LC81170572013154-SC20160930104756.tar/clipped"
SAFE.L8.201307 <- "/SAFE/LC81170572013202-SC20160930104600.tar/clipped"
SAFE.L8.201308 <- "/SAFE/LC81170572013234-SC20160930102836.tar/clipped"



# THE FUNCTION : Single pixel extraction ----------------------------------
topocorr_extract_L7 <- function(img.subdir, sunelev, sunazimuth, topocorrmethod,
                                image.id, site.shp, fieldData.site){
  # Import multiple raster files in a folder
  folder <- paste(c(gen.wd,img.subdir),collapse="") ## Input: img.subdir ## ## gen.wd is global variable ##
  rasterlist <-  list.files(folder, pattern = "\\.tif$", full.names=FALSE) 
  rasterlist.sr <- rasterlist[c(5:10)] # Just SR B1-5,7 layers 
  rasterlist.cf <- rasterlist[1] # CFmask
  
  # Single layer raster of CFmask
  setwd(folder)
  raster.cf <- raster(rasterlist.cf)
  names(raster.cf) <- "CFmask"
  
  # Create raster stack
  # SR
  raster.sr <- list()
  for(i in rasterlist.sr) { assign(unlist(strsplit(i, "[.]"))[1], raster.sr[[i]] <- raster(i)) } 
  raster.sr.st <- stack(raster.sr)
  names(raster.sr.st) <- c("SR_B1_blue", "SR_B2_green", "SR_B3_red", "SR_B4_nir", "SR_B5_swir1", "SR_B7_swir2")
  # Apply scale factor and remove negative and >1
  raster.sr.st <- raster.sr.st * 0.0001
  raster.sr.st[raster.sr.st < 0] <- NA
  raster.sr.st[raster.sr.st > 1] <- NA
  
  # Remove clouds, shadows, water
  raster.sr.st[raster.cf>0] <- NA
  
  # Convert to SpGDF
  srBLUE.SpGDF <- as(raster.sr.st$SR_B1_blue, 'SpatialGridDataFrame') # in UTM
  srGREEN.SpGDF <- as(raster.sr.st$SR_B2_green, 'SpatialGridDataFrame')
  srRED.SpGDF <- as(raster.sr.st$SR_B3_red, 'SpatialGridDataFrame')
  srNIR.SpGDF <- as(raster.sr.st$SR_B4_nir, 'SpatialGridDataFrame')
  srSWIR1.SpGDF <- as(raster.sr.st$SR_B5_swir1, 'SpatialGridDataFrame')
  srSWIR2.SpGDF <- as(raster.sr.st$SR_B7_swir2, 'SpatialGridDataFrame')
  
  # Apply topocorr
  srBLUE.SpGDF.topocorr <- topocorr(srBLUE.SpGDF, srtm_slopeasp_UTM$slope, srtm_slopeasp_UTM$aspect,
                           sunelev=sunelev, sunazimuth=sunazimuth, method=topocorrmethod)
  srGREEN.SpGDF.topocorr <- topocorr(srGREEN.SpGDF, srtm_slopeasp_UTM$slope, srtm_slopeasp_UTM$aspect,
                              sunelev=sunelev, sunazimuth=sunazimuth, method=topocorrmethod)
  srRED.SpGDF.topocorr <- topocorr(srRED.SpGDF, srtm_slopeasp_UTM$slope, srtm_slopeasp_UTM$aspect,
                              sunelev=sunelev, sunazimuth=sunazimuth, method=topocorrmethod)
  srNIR.SpGDF.topocorr <- topocorr(srNIR.SpGDF, srtm_slopeasp_UTM$slope, srtm_slopeasp_UTM$aspect,
                              sunelev=sunelev, sunazimuth=sunazimuth, method=topocorrmethod)
  srSWIR1.SpGDF.topocorr <- topocorr(srSWIR1.SpGDF, srtm_slopeasp_UTM$slope, srtm_slopeasp_UTM$aspect,
                              sunelev=sunelev, sunazimuth=sunazimuth, method=topocorrmethod)
  srSWIR2.SpGDF.topocorr <- topocorr(srSWIR2.SpGDF, srtm_slopeasp_UTM$slope, srtm_slopeasp_UTM$aspect,
                              sunelev=sunelev, sunazimuth=sunazimuth, method=topocorrmethod)
  
  # Make into raster stack
  sr.topocorr.st <- stack(raster(srBLUE.SpGDF.topocorr), raster(srGREEN.SpGDF.topocorr),
                          raster(srRED.SpGDF.topocorr), raster(srNIR.SpGDF.topocorr),
                          raster(srSWIR1.SpGDF.topocorr), raster(srSWIR2.SpGDF.topocorr))
  
  names(sr.topocorr.st) <- c('SRtcorr_B1_blue', 'SRtcorr_B2_green', 'SRtcorr_B3_red', 'SRtcorr_B4_NIR', 'SRtcorr_B5_SWIR1', 'SRtcorr_B7_SWIR2')
  
    # Extract spectra: single coincident pixel
  # Keep only usable plots based on plot ID in fieldData above
  site.shp.sel <- site.shp[(as.character(site.shp@data$SampleID) %in% ## Input: site.shp ##
                              fieldData.site[fieldData.site$Image==image.id, "SampleID"]),] ## Input: image.id & fieldData.site ##
  sppts <- SpatialPoints(site.shp.sel@coords, proj4string=crs(raster.sr.st), bbox = NULL)
  # Extract surface reflectance
  extr.sr.topocorr <- extract(sr.topocorr.st, sppts, method="simple", df=T)
  extr.sr.topocorr$SampleID<- as.character(site.shp.sel@data$SampleID)

  # Insert the extracted SR to fieldData data frame
  fieldData.spc.topocorr <- merge(fieldData.site, extr.sr.topocorr, by="SampleID")

  # Output from function
  return(fieldData.spc.topocorr)
}

topocorr_extract_L8 <- function(img.subdir, sunelev, sunazimuth, topocorrmethod,
                                image.id, site.shp, fieldData.site){
  # Import multiple raster files in a folder
  folder <- paste(c(gen.wd,img.subdir),collapse="") ## Input: img.subdir ## ## gen.wd is global variable ##
  rasterlist <-  list.files(folder, pattern = "\\.tif$", full.names=FALSE) 
  rasterlist.sr <- rasterlist[c(5:10)] # # Just SR B2-7 layers 
  rasterlist.cf <- rasterlist[1] # CFmask
  
  # Single layer raster of CFmask
  setwd(folder)
  raster.cf <- raster(rasterlist.cf)
  names(raster.cf) <- "CFmask"
  
  # Create raster stack
  # SR
  raster.sr <- list()
  for(i in rasterlist.sr) { assign(unlist(strsplit(i, "[.]"))[1], raster.sr[[i]] <- raster(i)) } 
  raster.sr.st <- stack(raster.sr)
  names(raster.sr.st) <- c("SR_B1_blue", "SR_B2_green", "SR_B3_red", "SR_B4_nir", "SR_B5_swir1", "SR_B7_swir2")
  # Apply scale factor and remove negative and >1
  raster.sr.st <- raster.sr.st * 0.0001
  raster.sr.st[raster.sr.st < 0] <- NA
  raster.sr.st[raster.sr.st > 1] <- NA
  
  # Remove clouds, shadows, water
  raster.sr.st[raster.cf>0] <- NA
  
  # Convert to SpGDF
  srBLUE.SpGDF <- as(raster.sr.st$SR_B1_blue, 'SpatialGridDataFrame') # in UTM
  srGREEN.SpGDF <- as(raster.sr.st$SR_B2_green, 'SpatialGridDataFrame')
  srRED.SpGDF <- as(raster.sr.st$SR_B3_red, 'SpatialGridDataFrame')
  srNIR.SpGDF <- as(raster.sr.st$SR_B4_nir, 'SpatialGridDataFrame')
  srSWIR1.SpGDF <- as(raster.sr.st$SR_B5_swir1, 'SpatialGridDataFrame')
  srSWIR2.SpGDF <- as(raster.sr.st$SR_B7_swir2, 'SpatialGridDataFrame')
  
  # Apply topocorr
  srBLUE.SpGDF.topocorr <- topocorr(srBLUE.SpGDF, srtm_slopeasp_UTM$slope, srtm_slopeasp_UTM$aspect,
                                    sunelev=sunelev, sunazimuth=sunazimuth, method=topocorrmethod)
  srGREEN.SpGDF.topocorr <- topocorr(srGREEN.SpGDF, srtm_slopeasp_UTM$slope, srtm_slopeasp_UTM$aspect,
                                     sunelev=sunelev, sunazimuth=sunazimuth, method=topocorrmethod)
  srRED.SpGDF.topocorr <- topocorr(srRED.SpGDF, srtm_slopeasp_UTM$slope, srtm_slopeasp_UTM$aspect,
                                   sunelev=sunelev, sunazimuth=sunazimuth, method=topocorrmethod)
  srNIR.SpGDF.topocorr <- topocorr(srNIR.SpGDF, srtm_slopeasp_UTM$slope, srtm_slopeasp_UTM$aspect,
                                   sunelev=sunelev, sunazimuth=sunazimuth, method=topocorrmethod)
  srSWIR1.SpGDF.topocorr <- topocorr(srSWIR1.SpGDF, srtm_slopeasp_UTM$slope, srtm_slopeasp_UTM$aspect,
                                     sunelev=sunelev, sunazimuth=sunazimuth, method=topocorrmethod)
  srSWIR2.SpGDF.topocorr <- topocorr(srSWIR2.SpGDF, srtm_slopeasp_UTM$slope, srtm_slopeasp_UTM$aspect,
                                     sunelev=sunelev, sunazimuth=sunazimuth, method=topocorrmethod)
  
  # Make into raster stack
  sr.topocorr.st <- stack(raster(srBLUE.SpGDF.topocorr), raster(srGREEN.SpGDF.topocorr),
                          raster(srRED.SpGDF.topocorr), raster(srNIR.SpGDF.topocorr),
                          raster(srSWIR1.SpGDF.topocorr), raster(srSWIR2.SpGDF.topocorr))
  
  names(sr.topocorr.st) <- c('SRtcorr_B1_blue', 'SRtcorr_B2_green', 'SRtcorr_B3_red', 'SRtcorr_B4_NIR', 'SRtcorr_B5_SWIR1', 'SRtcorr_B7_SWIR2')
  
  # Extract spectra: single coincident pixel
  # Keep only usable plots based on plot ID in fieldData above
  site.shp.sel <- site.shp[(as.character(site.shp@data$SampleID) %in% ## Input: site.shp ##
                              fieldData.site[fieldData.site$Image==image.id, "SampleID"]),] ## Input: image.id & fieldData.site ##
  sppts <- SpatialPoints(site.shp.sel@coords, proj4string=crs(raster.sr.st), bbox = NULL)
  # Extract surface reflectance
  extr.sr.topocorr <- extract(sr.topocorr.st, sppts, method="simple", df=T)
  extr.sr.topocorr$SampleID<- as.character(site.shp.sel@data$SampleID)
  
  # Insert the extracted SR to fieldData data frame
  fieldData.spc.topocorr <- merge(fieldData.site, extr.sr.topocorr, by="SampleID")
  
  # Output from function
  return(fieldData.spc.topocorr)
}

# Debug
img.subdir <- SAFE.L7.201208
sunelev <- 61.82055225
sunazimuth <- 76.95121588
topocorrmethod <- "SCS"
# topocorrmethod <- "ccorrection"
image.id = "L7.201208"
site.shp = SAFE.shp
fieldData.site = fieldData.SAFE



# Import field data and point shapefiles ----------------------------------
# Field data
fieldData.SAFE <- read.xlsx("C:/LocalUserData/User-data/hadi1/hadi_phd_data/SAFE/LAI_Malaysia_NoDoublon_usableplots.xls",
                            sheetName ="Just_usable_plots")
fieldData.SAFE <- fieldData.SAFE[1:85,] # Remove NA rows at the end appearing
fieldData.SAFE$SampleName <- as.character(fieldData.SAFE$SampleName)
fieldData.SAFE$SampleID <- as.character(fieldData.SAFE$SampleID)
fieldData.SAFE$Image <- as.character(fieldData.SAFE$Image)

# Shapefile
shp_folder <- "C:/LocalUserData/User-data/hadi1/hadi_phd_data/SAFE/shp"
SAFE.shp <- readOGR(dsn=shp_folder, layer="SAFE_plots_UTM50N") 




# Running the function ----------------------------------------------------
SAFE.L7.201208.spc.topocorr <- topocorr_extract_L7(SAFE.L7.201208, 61.82055225, 76.95121588, 'SCS', 
                                                   "L7.201208", SAFE.shp, fieldData.SAFE)
# Below is undone

# SAFE.L7.201209.spc <- topocorr_extract_L7(SAFE.L7.201209, "L7.201209", SAFE.shp, fieldData.SAFE)
# SAFE.L7.201210.spc <- topocorr_extract_L7(SAFE.L7.201210, "L7.201210", SAFE.shp, fieldData.SAFE)
# SAFE.L7.20121217.spc <- topocorr_extract_L7(SAFE.L7.20121217, "L7.20121217", SAFE.shp, fieldData.SAFE)
# SAFE.L7.20121201.spc <- topocorr_extract_L7(SAFE.L7.20121201, "L7.20121201", SAFE.shp, fieldData.SAFE)
# SAFE.L7.201306.spc <- topocorr_extract_L7(SAFE.L7.201306, "L7.201306", SAFE.shp, fieldData.SAFE)
# 
# SAFE.L8.201307.spc <- topocorr_extract_L8(SAFE.L8.201307, "L8.201307", SAFE.shp, fieldData.SAFE)
# SAFE.L8.201308.spc <- topocorr_extract_L8(SAFE.L8.201308, "L8.201308", SAFE.shp, fieldData.SAFE)
# 
# # Merge all plots in SAFE site
# SAFE.all.spc <- rbind(SAFE.L7.201208.spc, SAFE.L7.201209.spc, SAFE.L7.201210.spc, SAFE.L7.20121217.spc, SAFE.L7.20121201.spc,
#                       SAFE.L7.201306.spc, SAFE.L8.201307.spc, SAFE.L8.201308.spc)
# 
# # Add "sensor" column
# for(i in 1:nrow(SAFE.all.spc)){
#   SAFE.all.spc[i, "Sensor"] <- unlist(strsplit(SAFE.all.spc[i, "Image"], "[.]"))[1]
# }







