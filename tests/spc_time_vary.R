require(stringr)
require(tidyverse)
require(gridExtra)
require(lubridate)

# Load spc of plots in selected 3 scenes. One plot can have many scenes here.
plots_3scenes_tall <- readRDS("results/SAFE_spc_field_3scenes_many.rds")


#########################################################################################
# Spectral variability in different time window ---------------------------
#########################################################################################
#########################################################################################
# Calculate ---------------------------------------------------------------
#########################################################################################
# plots_groupSampleID <- plots_3scenes_tall %>% 
#   group_by(SampleID) %>% 
#   nest()
# 
# # Test
# # plot_spc <- group_SampleID$data[[1]]
# spc_range <- function(plot_spc, band) {
#   plot_spc %>% select(contains(band)) %>% 
#     map_dbl(function(x) max(range(x)) - min(range(x))) 
# }
# 
# nobs_perplot <- tibble(SampleID = plots_groupSampleID$SampleID,
#                        n = map_int(plots_groupSampleID$data, nrow)) 
# 
# range_BRF <- tibble(SampleID = plots_groupSampleID$SampleID) %>% 
#   mutate(range_RED = map(plots_groupSampleID$data, function(x) spc_range(x, band = "SR_B3_red"))) %>% 
#   mutate(range_NIR = map(plots_groupSampleID$data, function(x) spc_range(x, band = "SR_B4_nir"))) %>% 
#   mutate(range_SWIR1 = map(plots_groupSampleID$data, function(x) spc_range(x, band = "SR_B5_swir1"))) %>% 
#   unnest()
# 
# # windows()
# # par(ps=14, mfrow=c(1,3))
# # hist(range_BRF$range_RED)
# # hist(range_BRF$range_NIR)
# # hist(range_BRF$range_SWIR1)


#########################################################################################
# Visualize ---------------------------------------------------------------
#########################################################################################    
# Fixed
plots_3scenes_tall <- plots_3scenes_tall %>%  mutate(scene = as.factor(scene))
scene_color <- c("black","red","cyan","magenta","brown","dark orange","green","purple") # Keep the colors template for 8 scenes
scene_breaks <- str_c("L7.", c("20111028","20111113","20111215","20120405","20120421","201208","20121201","20121217"))
sc <- scale_color_manual(breaks = scene_breaks, values = scene_color, drop=FALSE)

# Change
plots1 <- 1:50
plots2 <- 51:100
plots3 <- 101:150
plots4 <- 150:193
band_red <- "SR_B3_red"
band_nir <- "SR_B4_nir"
band_swir1 <- "SR_B5_swir1"


plot_spc_scene <- function(spc_tall = plots_3scenes_tall, row = plots1, band = band_nir) {
  
  spc_tall  %>% 
    filter(ID %in% row) %>% 
    ggplot(., aes_string("SampleID", band, colour = "scene")) +
    geom_point() + sc +
    theme(legend.position = "right", axis.text.x = element_text(angle = 45, hjust = 1)) +
    guides(colour = guide_legend(override.aes = list(size=2)))
  
}


# NIR
plots1_nir <- plot_spc_scene(spc_tall = plots_3scenes_tall, row = plots1, band = band_nir)
plots2_nir <- plot_spc_scene(spc_tall = plots_3scenes_tall, row = plots2, band = band_nir)
plots3_nir <- plot_spc_scene(spc_tall = plots_3scenes_tall, row = plots3, band = band_nir)
plots4_nir <- plot_spc_scene(spc_tall = plots_3scenes_tall, row = plots4, band = band_nir)

pdf("graphs/spc_3scenes_NIR.pdf")
grid.arrange(plots1_nir, plots2_nir, plots3_nir, plots4_nir, ncol = 1)
dev.off()

# RED
plots1_red <- plot_spc_scene(spc_tall = plots_3scenes_tall, row = plots1, band = band_red)
plots2_red <- plot_spc_scene(spc_tall = plots_3scenes_tall, row = plots2, band = band_red)
plots3_red <- plot_spc_scene(spc_tall = plots_3scenes_tall, row = plots3, band = band_red)
plots4_red <- plot_spc_scene(spc_tall = plots_3scenes_tall, row = plots4, band = band_red)

pdf("graphs/spc_3scenes_red.pdf")
grid.arrange(plots1_red, plots2_red, plots3_red, plots4_red, ncol = 1)
dev.off()


# SWIR
plots1_swir1 <- plot_spc_scene(spc_tall = plots_3scenes_tall, row = plots1, band = band_swir1)
plots2_swir1 <- plot_spc_scene(spc_tall = plots_3scenes_tall, row = plots2, band = band_swir1)
plots3_swir1 <- plot_spc_scene(spc_tall = plots_3scenes_tall, row = plots3, band = band_swir1)
plots4_swir1 <- plot_spc_scene(spc_tall = plots_3scenes_tall, row = plots4, band = band_swir1)

pdf("graphs/spc_3scenes_swir1.pdf")
grid.arrange(plots1_swir1, plots2_swir1, plots3_swir1, plots4_swir1, ncol = 1)
dev.off()



#########################################################################################
# Deciding final spc to use ---------------------------------------------------------------
######################################################################################### 
# Based on checking the spectra and the image:
# (1) Remove obvious outliers (shadow, checked in the NIR image) : 
# L7.20121217 plot 646, 647, 649, 662, 663, 677, 679, 681, 757, 759, 760, 763
# L7.201208 plot 620, 621

plots_3scenes_tall_rmoutl <- plots_3scenes_tall %>% rowwise() %>% 
  mutate(scene = as.character(scene)) %>%
  filter(!((scene == "L7.20121217") && (SampleID %in%
             c("646","647","649","662","663","677","679","681","757","759","760","763")))) %>% 
  filter(!((scene == "L7.201208") && (SampleID %in% c("620","621"))))



# (2) Then, for the remaining plots with multiple scenes, keep the scene closest in time, except...
# Create date object scene.Date from column scene

plots_3scenes_tall_rmoutl <- plots_3scenes_tall_rmoutl %>% 
  mutate(scene.Date = dplyr::if_else(scene == "L7.201208", ymd("2012-08-27"), 
                                     dplyr::if_else(scene == "L7.20121201", ymd("2012-12-01"), ymd("2012-12-17")))) %>% 
  mutate(field.Date = ymd(str_c(Year, Month, Day, sep = "-")))  %>% # Create date object field.Date from columns Year, Month, Day  
  mutate(diff.Date = abs(field.Date - scene.Date)) # the time difference in days



# x <- plots_3scenes_tall_rmoutl_gr$data[[1]] # testing function
keep_scene_closest <- function(x) {
    arrange(x, diff.Date) %>%  # sort ascending so minimum days difference will be in row 1
    filter(row_number() == 1L) # keep only row 1
}

by_SampleID <- 
  plots_3scenes_tall_rmoutl %>% dplyr::group_by(SampleID) %>% nest() 

plots_3scenes_tall_rmoutl_closest <- by_SampleID %>% 
               mutate(data = map(data, function(y) keep_scene_closest(y))) %>%  unnest()



# Plot to check if undesired spc are gone
# NIR
plots1_nir <- plot_spc_scene(spc_tall = plots_3scenes_tall_rmoutl_closest, row = plots1, band = band_nir)
plots2_nir <- plot_spc_scene(spc_tall = plots_3scenes_tall_rmoutl_closest, row = plots2, band = band_nir)
plots3_nir <- plot_spc_scene(spc_tall = plots_3scenes_tall_rmoutl_closest, row = plots3, band = band_nir)
plots4_nir <- plot_spc_scene(spc_tall = plots_3scenes_tall_rmoutl_closest, row = plots4, band = band_nir)

pdf("graphs/spc_3scenes_NIR_rmoutl.pdf")
grid.arrange(plots1_nir, plots2_nir, plots3_nir, plots4_nir, ncol = 1)
dev.off()

# All good, save the finalized spectra:
saveRDS(plots_3scenes_tall_rmoutl_closest, "results/SAFE_spc_3scenes_108plots.rds")
write.csv2(plots_3scenes_tall_rmoutl_closest, "results/SAFE_spc_3scenes_108plots.csv")

# Check the points (scenes) on the graph is ok = yes!
# plots_3scenes_tall %>% filter(SampleID == "622") %>% select(scene)

# Check some peculiar plots with different NIR
# plots_3scenes_tall %>% filter(SampleID %in% c("646","647","662","663","677","679")) %>% 
#   select(SampleID, LAI_true_v6)    