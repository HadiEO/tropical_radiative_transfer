require(rgdal); require(rgeos); require(raster); require(xlsx); require(sp);

# Debug (to delete)
# img.subdir = SAFE.L7.201208
# site.shp = SAFE.shp

extract.L7.relaxtime <- function(img.subdir, site.shp){
  # Import multiple raster files in a folder
  folder <- paste(c(gen.wd,img.subdir),collapse="") ## Input: img.subdir ## ## gen.wd is global variable ##

  rasterlist <-  list.files(folder, pattern = "\\.tif$", full.names=TRUE) 
  rasterlist.sr <- rasterlist[c(5:10)] # Just SR B1-5,7 layers 
  rasterlist.toa <- rasterlist[c(17:22)] # Just TOA B1-5,7 layers 
  
  # Single layer raster of CFmask and atmopac
  raster.cf <- raster(rasterlist[1]); names(raster.cf) <- "CFmask"
  raster.atmopac <- raster(rasterlist[4]); names(raster.atmopac) <- "atmopac"
  raster.atmopac <- raster.atmopac * 0.0010 # Apply scale factor
  
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
  # Apply forest mask
  raster.sr.st[raster.cf != 0] <- NA
  # Remove hazy pixels
  raster.sr.st[raster.atmopac > 0.3] <- NA
  # Just to be sure, add the CFmask and atmopac layers
  raster.sr.st <- addLayer(raster.sr.st, raster.cf, raster.atmopac)
  
  # TOAr
  raster.toa <- list()
  for(i in rasterlist.toa) { assign(unlist(strsplit(i, "[.]"))[1], raster.toa[[i]] <- raster(i)) } 
  raster.toa.st <- stack(raster.toa)
  names(raster.toa.st) <- c("TOA_B1_blue", "TOA_B2_green", "TOA_B3_red", "TOA_B4_nir", "TOA_B5_swir1", "TOA_B7_swir2")
  raster.toa.st <- raster.toa.st * 0.0001
  raster.toa.st[raster.toa.st < 0] <- NA
  raster.toa.st[raster.toa.st > 1] <- NA
  # Apply forest mask
  raster.toa.st[raster.cf != 0] <- NA
  # Remove hazy pixels
  raster.toa.st[raster.atmopac > 0.3] <- NA
  
  # Extract spectra: single coincident pixel
  # Keep only usable plots based on plot ID in fieldData above
  sppts <- SpatialPoints(site.shp@coords, proj4string=crs(raster.toa.st), bbox = NULL)
  # Extract surface reflectance
  extr.sr <- extract(raster.sr.st, sppts, method="simple", df=T)
  
  # Stop if zero plots (all hazy)
  if(nrow(extr.sr)==0) stop("Zero clear plots") # Does this work?
  
  extr.sr$SampleID<- as.character(site.shp@data$SampleID)
  # Extract top-of-atmosphere refl  ectance
  extr.toa <- extract(raster.toa.st, sppts, method="simple", df=T)
  extr.toa$SampleID <- as.character(site.shp@data$SampleID)
  
  # Merge extr.sr and extr.toa
  extr <- cbind(extr.toa, extr.sr)
  # Remove NA rows
  extr <- extr[complete.cases(extr),]
  
  # Output from function
  return(extr)
}