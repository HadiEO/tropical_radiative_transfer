source("cache/libPaths.R")

require(xlsx)
require(pracma)

# Update listOfFolderNames
# listOfFolderNames <- read.xlsx("data/forest/listOfFolderNames_usable.xlsx", sheetIndex=1, header=F)
listOfFolderNames <- read.xlsx("D:/SAFE/listOfFolderNames_todoplots_photoschecked.xlsx", sheetIndex=1, header=F)
listOfFolderNames <- read.xlsx("D:/SAFE/listOfFolderNames_todoplots_missed.xlsx", sheetIndex=1, header=F)
# listOfFolderNames <- read.xlsx("E:/SAFE/listOfFolderNames_todoplots_missed.xlsx", sheetIndex=1, header=F)


folder_expmin1 <- as.character(listOfFolderNames[seq(2,nrow(listOfFolderNames),by=3),]) 
folder_expplus1 <- as.character(listOfFolderNames[seq(3,nrow(listOfFolderNames),by=3),]) 
folder_expauto <- as.character(listOfFolderNames[seq(1,nrow(listOfFolderNames),by=3),]) 


read_laiclump <- function(folder_exp = folder_expmin1) {
  
  # Loop through the folder names
  # Store gaps and clumping results here
  mygaps <- data.frame(SampleID=rep(NA,length(folder_exp)), LAI_my=0, Closure75_my=0, Gaps1_my=0, Gaps2_my=0, Gaps3_my=0, Gaps4_my=0, Gaps5_my=0,
                       CLX_my=0, count0_CLX_my=0, CIcc_my=0, count0_CIcc_my=0, LAIt_CLX_my=0, LAIt_CIcc_my=0, LAIt_my=0,
                       LAI_my_new=0)
  
  for (i in 1:length(folder_exp)) { # change exposure here!!!!!!!!!!!!!!!!!!!!
    setwd(folder_exp[i]) # and here !!!!!!!!!!!!!!!!!!!!!!!!!
    SampleID <- strsplit(strsplit(getwd(), '/')[[1]][6], '_')[[1]][2]
    
    if(SampleID=="SAFE") SampleID <- strsplit(strsplit(getwd(), '/')[[1]][6], '_')[[1]][3]
    # NOTE: there are other patterns (e.g., Plot_x2OP) of folder names too hard to account for all, so need to check yeah!
    
    lai_df <- read.xlsx('lai.xls', sheetIndex=1, header=T)
    
    # Read clumping from LAIMIL result, insert into xls based on same plot number in lai_df$Image and 
    # line 1 in LAIMIL output .txt
    laimil_files <- list.files(pattern = "laimil*") # list all laimil.txt
    laimil_df <- data.frame(name=0, CLX=0, CIcc=0)
    for(j in 1:length(laimil_files)){
      laimil_res <- read.table(laimil_files[j], sep="\t", header=FALSE, stringsAsFactors=FALSE,
                               skip=2, fill=TRUE, blank.lines.skip=TRUE)
      laimil_df[j,"name"] <- laimil_res[1,1] # photo name
      laimil_df[j,"CLX"] <- as.numeric(laimil_res[laimil_res$V1=="CLX    ", "V2"])
      laimil_df[j,"CIcc"] <- as.numeric(laimil_res[laimil_res$V1=="CIcc   ", "V2"]) 
    }
    
    # Insert CLX and CIcc into lai_df matching photos
    lai_df$Image <- as.character(lai_df$Image)
    # Create columns to store clumping
    lai_df$laimil_name <- 0
    lai_df$CLX <- 0
    lai_df$CIcc <- 0
    lai_df$LAIt_CLX <- 0
    lai_df$LAIt_CIcc <- 0
    lai_df$LAIt <- 0 # = LAIt_CLX, when CLX=0 thus LAIt_CLX=Inf, replace with LAIt_CIcc
    
    for(k in 1:nrow(lai_df)){
      # Split lai_df$Image 
      photo_id <- strsplit(lai_df[k,"Image"], '_')[[1]][2]
      if(photo_id=="IMG") photo_id <- strsplit(lai_df[k,"Image"], '_')[[1]][3]
      # Match with laimil_df
      for(m in 1:nrow(laimil_df)){
        if(!is.null(strfindi(laimil_df[m,"name"], photo_id, overlap=FALSE))){
          lai_df[k,"laimil_name"] <- laimil_df[m,"name"]
          lai_df[k,"CLX"] <- laimil_df[m,"CLX"]
          lai_df[k,"CIcc"] <- laimil_df[m,"CIcc"]
        }
      }
    }
    
    # Calculate for each photo LAIt = LAI (eff) / clumping
    lai_df$LAIt_CLX <- lai_df$LAI / lai_df$CLX
    lai_df$LAIt_CIcc <- lai_df$LAI / lai_df$CIcc
    lai_df$LAIt <- lai_df$LAIt_CLX
    lai_df[lai_df$LAIt==Inf, "LAIt"] <- lai_df[lai_df$LAIt==Inf, "LAIt_CIcc"] # = LAIt_CLX, when CLX=0 thus LAIt_CLX=Inf, replace with LAIt_CIcc
    # Write to xls
    # write.xlsx(lai_df, "lai_clump.xls")         # Already written, test new way of calculating LAI (Hadi 20170815)
    
    LAI_my <- round(mean(lai_df$LAI), 3)                         # LAIe
    
    Closure75_my <- round(mean(lai_df$Closure75), 3)
    Gaps1_my <- round(mean(lai_df$Gaps1), 3)
    Gaps2_my <- round(mean(lai_df$Gaps2), 3)
    Gaps3_my <- round(mean(lai_df$Gaps3), 3)
    Gaps4_my <- round(mean(lai_df$Gaps4), 3)
    Gaps5_my <- round(mean(lai_df$Gaps5), 3)
    
    ## New calculation of LAIe by averaging gaps over the images *******************************
    angles <- c(pi/180*10.7, pi/180*23.7, pi/180*38.2, pi/180*52.9, pi/180*67.8, pi/180*82.7)
    wis <- sin(angles)
    weight_wis <- wis / sum(wis)
    weight_wis[5] <- weight_wis[5] + weight_wis[6]
    
    weight_wis <- weight_wis[1:5]
    angles <- angles[1:5]
    
    gaps <- c(Gaps1_my, Gaps2_my, Gaps3_my, Gaps4_my, Gaps5_my)
    contacts <- -cos(angles) * log(gaps)
    LAI_my_new <- 2 * (sum(contacts*weight_wis))
    ##
    
    count0_CLX_my <- length(which(lai_df$CLX==0))
    count0_CIcc_my <- length(which(lai_df$CIcc==0))
    
    CLX_my <- round(mean(lai_df$CLX), 3) # Average clumping for all photos in a plot
    CIcc_my <- round(mean(lai_df$CIcc), 3)
    # Change Inf to NA so take mean ignoring NA
    lai_df[lai_df$LAIt_CLX==Inf,"LAIt_CLX"] <- NA
    LAIt_CLX_my <- round(mean(lai_df$LAIt_CLX, na.rm=T), 3)
    
    lai_df[lai_df$LAIt_CIcc==Inf,"LAIt_CIcc"] <- NA
    LAIt_CIcc_my <- round(mean(lai_df$LAIt_CIcc, na.rm=T), 3)
    
    LAIt_my <- round(mean(lai_df$LAIt), 3)
    
    mygaps[i,] <- c(SampleID, as.numeric(LAI_my), as.numeric(Closure75_my), as.numeric(Gaps1_my), as.numeric(Gaps2_my), 
                    as.numeric(Gaps3_my), as.numeric(Gaps4_my), as.numeric(Gaps5_my),
                    as.numeric(CLX_my), as.numeric(count0_CLX_my), as.numeric(CIcc_my), as.numeric(count0_CIcc_my), 
                    as.numeric(LAIt_CLX_my), as.numeric(LAIt_CIcc_my), as.numeric(LAIt_my), as.numeric(LAI_my_new))
    
  } # END
  
  # Just in case if the columns are not numerics
  mygaps$LAI_my <- as.numeric(mygaps$LAI_my)
  mygaps$Closure75_my <- as.numeric(mygaps$Closure75_my)
  mygaps$Gaps1_my <- as.numeric(mygaps$Gaps1_my)
  mygaps$Gaps2_my <- as.numeric(mygaps$Gaps2_my)
  mygaps$Gaps3_my <- as.numeric(mygaps$Gaps3_my)
  mygaps$Gaps4_my <- as.numeric(mygaps$Gaps4_my)
  mygaps$Gaps5_my <- as.numeric(mygaps$Gaps5_my)
  
  mygaps$CLX_my <- as.numeric(mygaps$CLX_my)
  mygaps$count0_CLX_my <- as.numeric(mygaps$count0_CLX_my)
  mygaps$CIcc_my <- as.numeric(mygaps$CIcc_my)
  mygaps$count0_CIcc_my <- as.numeric(mygaps$count0_CIcc_my)
  mygaps$LAIt_CLX_my <- as.numeric(mygaps$LAIt_CLX_my)
  mygaps$LAIt_CIcc_my <- as.numeric(mygaps$LAIt_CIcc_my)
  mygaps$LAIt_my <- as.numeric(mygaps$LAIt_my)
  mygaps$LAI_my_new <- as.numeric(mygaps$LAI_my_new)
  
  return(mygaps)
  
}

#  Safe for different exposures -------------------------------------------
mygaps_expmin <- read_laiclump(folder_expmin1)
mygaps_expplus <- read_laiclump(folder_expplus1) 
mygaps_expauto <- read_laiclump(folder_expauto)    

# Check plot ID for some plots not parsed correctly (there may be more)
# mygaps[mygaps$SampleID=="x2OP1","SampleID"] <- "739"
# mygaps[mygaps$SampleID=="x2OP3","SampleID"] <- "757"

# Save
setwd("C:/LocalUserData/User-data/hadi1/PHD_RESEARCH/STUDY_II_TROPICS_PARAS/Rproj_TROPICS_PARAS")
write.csv2(mygaps_expmin, "results/SAFE_gaps_addplots_expMin.csv", col.names = TRUE)
write.csv2(mygaps_expplus, "results/SAFE_gaps_addplots_expPlus.csv", col.names = TRUE)
write.csv2(mygaps_expauto, "results/SAFE_gaps_addplots_expAuto.csv", col.names = TRUE)


# 4 plots are missing where?
todo_plots <- read.csv2("results/todo_plots.csv") # int
# Found it, somehow empty after unzip
# Save gaps for these 4 plots
setwd("C:/LocalUserData/User-data/hadi1/PHD_RESEARCH/STUDY_II_TROPICS_PARAS/Rproj_TROPICS_PARAS")
write.csv2(mygaps_expmin, "results/SAFE_gaps_missedplots_expMin.csv", col.names = TRUE)
write.csv2(mygaps_expplus, "results/SAFE_gaps_missedplots_expPlus.csv", col.names = TRUE)
write.csv2(mygaps_expauto, "results/SAFE_gaps_missedplots_expAuto.csv", col.names = TRUE)





# Checking missing plots
smallhoriz_plots <- c(623, 753, 751, 747, 727, 726, 725, 719, 718, 717, 716, 715, 714, 713, 712, 711)
smallhoriz_plots <- as.integer(smallhoriz_plots)
addgaps_plots <- mygaps_expmin$SampleID; addgaps_plots <- as.integer(addgaps_plots)

todo_plots$x[which(!todo_plots$x %in% c(smallhoriz_plots, addgaps_plots))] # 612, 613, 616, 617 # They are empty folders