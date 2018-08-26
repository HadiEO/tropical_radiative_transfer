# But some photos are missing!
# Check missing photos


dirs_2012Aug <- read.xlsx("D:/SAFE/listOfFolderNames_SAFE_comb_dircut.xlsx", header = FALSE, sheetName = "2012Aug")
dirs_2012SAFE <- read.xlsx("D:/SAFE/listOfFolderNames_SAFE_comb_dircut.xlsx", header = FALSE, sheetName = "2012SAFE")
dirs_2012last <- read.xlsx("D:/SAFE/listOfFolderNames_SAFE_comb_dircut.xlsx", header = FALSE, sheetName = "2012last")
dirs_2013_14SAFE <- read.xlsx("D:/SAFE/listOfFolderNames_SAFE_comb_dircut.xlsx", header = FALSE, sheetName = "2013_14SAFE")


dirs_2012Aug[,1] <- as.character(dirs_2012Aug[,1])
dirs_2012SAFE[,1] <- as.character(dirs_2012SAFE[,1])
dirs_2012last[,1] <- as.character(dirs_2012last[,1])
dirs_2013_14SAFE[,1] <- as.character(dirs_2013_14SAFE[,1])

dirs_2012Aug <- as_tibble(dirs_2012Aug)
dirs_2012SAFE <- as_tibble(dirs_2012SAFE)
dirs_2012last <- as_tibble(dirs_2012last)
dirs_2013_14SAFE <- as_tibble(dirs_2013_14SAFE) 

get_dirs_SampleID <- function(dirs, idx0, idx1, idx2, pattern.na, pattern.idx2) {
  dirs_SampleID <- vector()
  
  for(i in 1:nrow(dirs)) {
    test <- unlist(dirs[i,1]) 
    split1 <- str_split(test, "\\\\")[[1]][idx0]
    split2 <- str_split(split1, "_")[[1]][idx1]
    
    if(!is.na(split2)) {
      if(!is.null(pattern.na)) {
        if(!is.na(str_match(split2, pattern.na))) split2 <- NA
        }
      }
    
    if(!is.null(idx2)) {
      if(!is.na(str_match(split2, pattern.idx2))) split2 <- str_split(split1, "_")[[1]][idx2]
    }
 
    dirs_SampleID[i] <- split2
  }
  dirs_SampleID <- dirs_SampleID[!is.na(dirs_SampleID)]
  dirs_SampleID <- unique(dirs_SampleID)
  return(dirs_SampleID)
}

dirs_2012Aug_SampleID <- get_dirs_SampleID(dirs = dirs_2012Aug, idx0 = 6,
                                           idx1 = 2,
                                           idx2 = 3,
                                           pattern.na = "^R", pattern.idx2 = "^x")

dirs_2012SAFE_SampleID <- get_dirs_SampleID(dirs = dirs_2012SAFE, idx0 = 5,
                                           idx1 = 3,
                                           idx2 = NULL,
                                           pattern.na = "[ABCDLV]", pattern.idx2 = NULL)

dirs_2012last_SampleID <- get_dirs_SampleID(dirs = dirs_2012last, idx0 = 5,
                                            idx1 = 3,
                                            idx2 = NULL,
                                            pattern.na = NULL, pattern.idx2 = NULL)

dirs_2013_14SAFE_SampleID <- get_dirs_SampleID(dirs = dirs_2013_14SAFE, idx0 = 6,
                                            idx1 = 2,
                                            idx2 = NULL,
                                            pattern.na = "O", pattern.idx2 = NULL)

dirs_all_SampleID <- c(dirs_2012Aug_SampleID,
    dirs_2012SAFE_SampleID,
    dirs_2012last_SampleID,
    dirs_2013_14SAFE_SampleID)

write.csv(dirs_all_SampleID, row.names = FALSE, col.names = FALSE,
  file = "C:/LocalUserData/User-data/hadi1/hadi_phd_data/SAFE/RESULTS/plots_final/dirs_all_SampleID.csv")


plot(1:179, sort(as.integer(dirs_all_SampleID)))

