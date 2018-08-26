source("cache/libPaths.R")
require(rgdal); require(rgeos); require(raster); require(xlsx); require(sp)
require(tidyverse); require(stringr)


# The extracted spectra (atm. opac < 0.3) within no-drought period in Pfeifer et al. 2016
spc_list <- readRDS("results/SAFE_spc_Sept11_Jan13.rds")

# Need the year, month, date of field measurements in field data
SAFE_field_data <- read.xlsx("data/forest/LAI_Malaysia_NoDoublon_usableplots.xls",
                        sheetIndex = 1, header = TRUE)
SAFE_field_data <- as_tibble(SAFE_field_data) %>% mutate(SampleID = as.character(SampleID)) 

# Merge spectra and field data -----------------------------------------------
plots_scenes_ls <- list(
  L7.201208 = merge(SAFE_field_data, spc_list$L7.201208[,-18], by="SampleID"), # Remove duplicate SampleID in col 18
  L7.20121217 = merge(SAFE_field_data, spc_list$L7.20121217[,-18], by="SampleID"),
  L7.20121201 = merge(SAFE_field_data, spc_list$L7.20121201[,-18], by="SampleID"),
  L7.20111113 = merge(SAFE_field_data, spc_list$L7.20111113[,-18], by="SampleID"),
  L7.20120421 = merge(SAFE_field_data, spc_list$L7.20120421[,-18], by="SampleID"),
  L7.20120405 = merge(SAFE_field_data, spc_list$L7.20120405[,-18], by="SampleID"),
  L7.20111028 = merge(SAFE_field_data, spc_list$L7.20111028[,-18], by="SampleID"),
  L7.20111215 = merge(SAFE_field_data, spc_list$L7.20111215[,-18], by="SampleID")
)

saveRDS(plots_scenes_ls, "results/SAFE_spc_field_Sept11_Jan13.rds")


# Reformat the multiple-scenes data (list) into tall data frame
plots_scenes_tall <- plots_scenes_ls %>%  map_df(na.omit)  
plots_scenes_tall <- as_tibble(plots_scenes_tall) 

# Remove field measurements later than January 2013 (after which not known if drought occured)
# Also remove RP plots
plots_scenes_tall <- plots_scenes_tall %>% filter(!grepl("^P", SampleID)) %>%  
  filter(!(Year == 2013 & Month > 1)) %>% 
  filter(Year != 2014)

# How many usable plots scenes Sept2011-Jan2013 
plots_scenes_tall %>% dplyr::select(SampleID) %>% unique() %>% nrow() # 133 plots


#########################################################################################
# How many usable plots in 3 closest-month scenes
#########################################################################################
sel_scenes <- c("L7.201208", "L7.20121201", "L7.20121217")
plots_scenes_tall %>%  filter(scene %in% sel_scenes) %>%
  dplyr::select(SampleID) %>% unique() %>% nrow() # 108 plots

# What forest types?
unique_SampleID <- plots_scenes_tall %>%  filter(scene %in% sel_scenes) %>%
  dplyr::select(SampleID) %>% unique() 
SAFE_field_data %>% filter(SampleID %in% as.vector(unique_SampleID$SampleID)) %>% 
  group_by(Type) %>% summarise(count = n())

# 1 Lightly Logged     8
# 2       Oil Palm    16
# 3        Primary    17
# 4 Salvage Logged    53
# 5   Twice Logged    14

# OK, filter keeping only the 3 scenes
plots_3scenes_tall <- plots_scenes_tall %>% filter(scene %in% sel_scenes)

plots_3scenes_tall  %>%
  dplyr::select(SampleID) %>% unique() %>% nrow() # 108 plots

# Check scene date: ok!
plots_3scenes_tall %>%  group_by(scene) %>% 
  summarise(count = n())

# Save
saveRDS(plots_3scenes_tall, "results/SAFE_spc_field_3scenes_many.rds")

# The selected plots SampleID
plots_SampleID <- plots_3scenes_tall %>%  select(SampleID) %>% unique() 
write.csv2(plots_SampleID, "results/select_plots_SampleID.csv")

# Which SampleID has been processed before in usable plots?
prev_SampleID <- read.csv2("results/prev_usable_plots_SampleID.csv")
prev_SampleID$x <- as.character(prev_SampleID$x)

todo_plots <- plots_SampleID$SampleID[which(!plots_SampleID$SampleID %in% prev_SampleID$x)]
write.csv2(todo_plots, "results/todo_plots.csv")

# Which SampleID in previous usable plots not used anymore?
prev_SampleID$x[which(!prev_SampleID$x %in% plots_SampleID$SampleID)] # 628


#########################################################################################
# But some photos are missing? ----------------------------------------------
#########################################################################################
# Check missing photos in check_missing_photos.R
dirs_SampleID <- read.csv("data/forest/dirs_all_SampleID.csv")
dirs_SampleID <- as_tibble(dirs_SampleID) %>% mutate(x = as.character(x))

mean(plots_SampleID$SampleID %in% dirs_SampleID$x) # 1 i.e. all TRUE no missing photos

# But still need to check for small-horizon photos !!!!!

 
#########################################################################################
# Select spc for plots with multiple scenes ----------------------------------------------
#########################################################################################
# Go to tests/spc_time_vary.R  


