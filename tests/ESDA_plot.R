require(ggplot2)
require(gridExtra)
source("tests/multiplot.R")


# Load the dataset --------------------------------------------------------
gaps_spc_field_anc <- read_csv2("results/gaps_spc_field_anc.csv")


# Recode forest quality column --------------------------------------------
source("munge/recode_forestqual.R")

# Recode forest Type column as Type.new --------------------------------------------
source("munge/recode_forestType.R")

# Remove poor and very poor forest quality plots ---------------------------------
source("munge/remove_poor_verypoor.R")                                               # Don't run if all plots are assessed



# Call the figure plotting functions --------------------------------------
source("tests/my_ggplot.R")
source("tests/my_ggplot_manuscript.R")
# Note: later when adjusting the figure size, font sizes need to be adjusted

# LAIeff vs LAIt by forest type -------------------------------------------
pdf("graphs/LAIeff_vs_LAIt.pdf") # , height = 2.5, width = 3.5, units = "in"
my_ggplot_1to1(gaps_spc_field_anc, "LAI_my", "LAIt_CIcc_my", "Type", ngroup = 5, pts = 2,
                   "LAI eff", "LAI true", xaxis.lim = c(0,5), yaxis.lim = c(0,5), 
                   xaxis.break = seq(0,4.5,by=0.5), yaxis.break = seq(0,4.5,by=0.5), 
                   text.x = 0.4, text.y = 4.8, 
                   shape.col = c(2:4,6,8,9), legend.pos = c(0.85,0.15))
dev.off()

# LAIt_my vs LAIt_uk by forest type -------------------------------------------
pdf("graphs/LAItmy_vs_LAItuk.pdf") # , height = 2.5, width = 3.5, units = "in"
my_ggplot_1to1(gaps_spc_field_anc, "LAI_true_v6", "LAIt_CIcc_my", "Type", ngroup = 5, pts = 2,
               "LAI true (UK)", "LAI true (MY)", xaxis.lim = c(0,6), yaxis.lim = c(0,6), 
               xaxis.break = seq(0,6,by=0.5), yaxis.break = seq(0,6,by=0.5), 
               text.x = 0.4, text.y = 5.8, 
               shape.col = c(2:4,6,8,9), legend.pos = c(0.85,0.15))
dev.off()

# LAIe_my vs LAIe_uk by forest type -------------------------------------------
pdf("graphs/LAIemy_vs_LAIeuk.pdf") # , height = 2.5, width = 3.5, units = "in"
my_ggplot_1to1(gaps_spc_field_anc, "LAI_eff_v6", "LAI_my", "Type", ngroup = 5, pts = 2,
               "LAI eff (UK)", "LAI eff (MY)", xaxis.lim = c(0,6), yaxis.lim = c(0,6), 
               xaxis.break = seq(0,6,by=0.5), yaxis.break = seq(0,6,by=0.5), 
               text.x = 0.4, text.y = 5.8, 
               shape.col = c(2:4,6,8,9), legend.pos = c(0.85,0.15))
dev.off()

# clumping_my vs clumping_uk by forest type -------------------------------------------
pdf("graphs/clumpmy_vs_clumpuk.pdf") # , height = 2.5, width = 3.5, units = "in"

gaps_spc_field_anc %>% mutate(clump_my = LAI_my / LAIt_CIcc_my, clump_uk = LAI_eff_v6 / LAI_true_v6) %>% 
  my_ggplot_1to1("clump_uk", "clump_my", "Type", ngroup = 5, pts = 2,
               "LAIe/LAIt (UK)", "LAIe/LAIt (MY)", xaxis.lim = c(0,1), yaxis.lim = c(0,1), 
               xaxis.break = seq(0,1,by=0.1), yaxis.break = seq(0,1,by=0.1), 
               text.x = 0.2, text.y = 0.8, 
               shape.col = c(2:4,6,8,9), legend.pos = c(0.85,0.15))
dev.off()


# UK data: LAIe vs CI = LAIe/LAIt -----------------------------------------

UK <- gaps_spc_field_anc %>% mutate(clump_my = LAI_my / LAIt_CIcc_my, clump_uk = LAI_eff_v6 / LAI_true_v6) %>% 
  my_ggplot_lm("LAI_eff_v6", "clump_uk", "Type", ngroup = 5, pts = 2,
               "LAIe", "CI = LAIe / LAIt", xaxis.lim = c(0,5), yaxis.lim = c(0,1), 
               xaxis.break = seq(0,5,by=0.5), yaxis.break = seq(0,1,by=0.1), 
               text.x = 4, text.y = 0.8, 
               shape.col = c(2:4,6,8,9), legend.pos = c(0.85,0.15)) + ggtitle("UK")

MY <- gaps_spc_field_anc %>% mutate(clump_my = LAI_my / LAIt_CIcc_my, clump_uk = LAI_eff_v6 / LAI_true_v6) %>% 
  my_ggplot_lm("LAI_my", "clump_my", "Type", ngroup = 5, pts = 2,
               "LAIe", "CI = LAIe / LAIt", xaxis.lim = c(0,5), yaxis.lim = c(0,1), 
               xaxis.break = seq(0,5,by=0.5), yaxis.break = seq(0,1,by=0.1), 
               text.x = 4, text.y = 0.8, 
               shape.col = c(2:4,6,8,9), legend.pos = c(0.85,0.15)) + ggtitle("MY")

pdf("graphs/LAIe_CI_MY.pdf") # , height = 2.5, width = 3.5, units = "in"
MY
dev.off()

# iD/LAIt vs LAIe -------------------------------------------
pdf("graphs/LAIe_iDdivLAIt.pdf") # , height = 2.5, width = 3.5, units = "in"

paras.res %>% mutate(temp = iD / LAIt_CIcc_my) %>% 
  my_ggplot_lm("LAI_my", "temp", "Type", ngroup = 5, pts = 2,
                 "LAIe", "iD / LAIt", xaxis.lim = c(0,5), yaxis.lim = c(0,0.5), 
                 xaxis.break = seq(0,5,by=0.5), yaxis.break = seq(0,0.5,by=0.1), 
                 text.x = 0.5, text.y = 0.4, 
                 shape.col = c(2:4,6,8,9), legend.pos = c(0.85,0.85))
dev.off()


# p_MY vs p_UK -------------------------------------------
pdf("graphs/p_UKvsMY.pdf") # , height = 2.5, width = 3.5, units = "in"

paras.res %>% mutate(temp = 1 - iD / LAI_true_v6) %>% 
  my_ggplot_1to1("temp", "p", "Type", ngroup = 5, pts = 2,
               "p = 1 - iD/LAIt_uk", "p = 1 - iD/LAIt_my", xaxis.lim = c(0.4,1), yaxis.lim = c(0.4,1), 
               xaxis.break = seq(0.4,1,by=0.1), yaxis.break = seq(0.4,1,by=0.1), 
               text.x = 0.5, text.y = 0.9, 
               shape.col = c(2:4,6,8,9), legend.pos = c(0.85,0.15))
dev.off()





# LAIt vs ECC =  1 - gaps1 ------------------------------------------------
gaps_spc_field_anc <- gaps_spc_field_anc %>%  mutate(ECC_my = (1 - Gaps1_my) * 100)

pdf("graphs/remove_poor_verypoor/ECC_vs_LAIt.pdf") # , height = 2.5, width = 3.5
my_ggplot_lm(gaps_spc_field_anc, "ECC_my", "LAIt_CIcc_my", "Type", ngroup = 5, pts = 2,
               "ECC (%)", "LAI true", xaxis.lim = c(0,100), yaxis.lim = c(0,5), 
               xaxis.break = seq(0,100,by=10), yaxis.break = seq(0,4.75,by=0.5), 
               text.x = 10, text.y = 4, 
               shape.col = c(2:4,6,8,9), legend.pos = c(0.85,0.15))
dev.off()


# ECC vs Clos75   ------------------------------------------------
gaps_spc_field_anc <- gaps_spc_field_anc %>%  mutate(ACC75_my = Closure75_my * 100)


pdf("graphs/ECC_vs_ACC75.pdf") # , height = 2.5, width = 3.5
my_ggplot_lm(gaps_spc_field_anc, "ECC_my", "ACC75_my", "Type", ngroup = 5, pts = 2,
             "ECC (%)", "ACC75 (%)", xaxis.lim = c(0,100), yaxis.lim = c(0,100), 
             xaxis.break = seq(0,100,by=10), yaxis.break = seq(0,100,by=10), 
             text.x = 10, text.y = 4, 
             shape.col = c(2:4,6,8,9), legend.pos = c(0.85,0.15)) + geom_abline(slope = 1, intercept = 0)
dev.off()



# ECC vs BRF by forest type ----------------------------------------------
ECC_BRF_byType <- list(
  green = my_ggplot_lm(gaps_spc_field_anc, "ECC_my", "SR_B2_green", "Type", ngroup = 5, pts = 2,
                        "ECC (%)", "Green BRF", xaxis.lim = c(0,100), yaxis.lim = c(0,0.09), 
                        xaxis.break = seq(0,100,by=10), yaxis.break = seq(0,0.09,by=0.02), 
                        text.x = 10, text.y = 0.08, 
                        shape.col = c(2:4,6,8,9), legend.pos = "right"),
  
  red = my_ggplot_lm(gaps_spc_field_anc, "ECC_my", "SR_B3_red", "Type", ngroup = 5, pts = 2,
                      "ECC (%)", "Red BRF", xaxis.lim = c(0,100), yaxis.lim = c(0,0.065), 
                      xaxis.break = seq(0,100,by=10), yaxis.break = seq(0,0.065,by=0.02), 
                      text.x = 10, text.y = 0.06, 
                      shape.col = c(2:4,6,8,9), legend.pos = "right"),
  
  nir = my_ggplot_lm(gaps_spc_field_anc, "ECC_my", "SR_B4_nir", "Type", ngroup = 5, pts = 2,
                      "ECC (%)", "NIR BRF", xaxis.lim = c(0,100), yaxis.lim = c(0,0.6), 
                      xaxis.break = seq(0,100,by=10), yaxis.break = seq(0,0.6,by=0.1), 
                      text.x = 10, text.y = 0.52, 
                      shape.col = c(2:4,6,8,9), legend.pos = "right"),
  
  swir1 = my_ggplot_lm(gaps_spc_field_anc, "ECC_my", "SR_B5_swir1", "Type", ngroup = 5, pts = 2,
                      "ECC (%)", "SWIR1 BRF", xaxis.lim = c(0,100), yaxis.lim = c(0,0.28), 
                      xaxis.break = seq(0,100,by=10), yaxis.break = seq(0,0.28,by=0.05), 
                      text.x = 10, text.y = 0.25, 
                      shape.col = c(2:4,6,8,9), legend.pos = "right"),
  
  swir2 = my_ggplot_lm(gaps_spc_field_anc, "ECC_my", "SR_B7_swir2", "Type", ngroup = 5, pts = 2,
                        "ECC (%)", "SWIR2 BRF", xaxis.lim = c(0,100), yaxis.lim = c(0,0.12), 
                        xaxis.break = seq(0,100,by=10), yaxis.break = seq(0,0.12,by=0.025), 
                        text.x = 10, text.y = 0.08, 
                        shape.col = c(2:4,6,8,9), legend.pos = "right")
)
  

  # plot <- grid.arrange(red, red, red, red, red, ncol = 3)
# ggsave("graphs/ECC_vs_BRF.pdf", plot)

# For now print the plots (bands) separately, later ask Aarne how to print multiple plots
pdf("graphs/ECC_vs_BRF_swir2.pdf") # , height = 2.5, width = 3.5
ECC_BRF_byType$swir2 # + geom_text(aes(label=SampleID, pos=3), size=3, hjust=-0.5, vjust=0, col="gray")
dev.off()


# Outliers? ---------------------------------------------------------------
# There appear to be some outliers: plot 639, 655, 624, 633, 582
# Which scenes?
outliers <- c(639, 655, 624, 633, 582)
gaps_spc_field_anc %>% filter(SampleID %in% outliers) %>% select(SampleID, scene.Date)
# Checked, plot 655, 624, 633 are cloud/shadow, remove them in the results/gaps_spc_field_anc.csv
# Plot 582, it's not the photos, and it's not clouds/shadow, so keep it



# ECC vs BRF by scene ----------------------------------------------------
ECC_BRF_byScene <- list(
  red = my_ggplot_lm(gaps_spc_field_anc, "ECC_my", "SR_B3_red", "scene.Date", ngroup = 3, pts = 2,
                     "ECC (%)", "Red BRF", xaxis.lim = c(0,100), yaxis.lim = c(0,0.065), 
                     xaxis.break = seq(0,100,by=10), yaxis.break = seq(0,0.065,by=0.02), 
                     text.x = 10, text.y = 0.06, 
                     shape.col = c(2:4), legend.pos = "right"),
  
  nir = my_ggplot_lm(gaps_spc_field_anc, "ECC_my", "SR_B4_nir", "scene.Date", ngroup = 3, pts = 2,
                     "ECC (%)", "NIR BRF", xaxis.lim = c(0,100), yaxis.lim = c(0,0.6), 
                     xaxis.break = seq(0,100,by=10), yaxis.break = seq(0,0.6,by=0.1), 
                     text.x = 10, text.y = 0.52, 
                     shape.col = c(2:4), legend.pos = "right")
)

pdf("graphs/remove_poor_verypoor/ECC_vs_BRF_byScene_red.pdf") # , height = 2.5, width = 3.5
ECC_BRF_byScene$red # + geom_text(aes(label=SampleID, pos=3), size=3, hjust=-0.5, vjust=0, col="gray")
dev.off()



# ECC vs BRF by foresty type & forest quality -------------------------------------------
ECC_BRF_byTypeQual <- list(
  red = my_ggplot_lm_qual(gaps_spc_field_anc, "ECC_my", "SR_B3_red", "Type", "ForestQuality", pts = 2,
                                   "ECC (%)", "Red BRF", xaxis.lim = c(0,100), yaxis.lim = c(0,0.065), 
                                   xaxis.break = seq(0,100,by=10), yaxis.break = seq(0,0.065,by=0.02), 
                                   text.x = 10, text.y = 0.06, legend.pos = "right"),
  nir = my_ggplot_lm_qual(gaps_spc_field_anc, "ECC_my", "SR_B4_nir", "Type", "ForestQuality", pts = 2,
                     "ECC (%)", "NIR BRF", xaxis.lim = c(0,100), yaxis.lim = c(0,0.6), 
                     xaxis.break = seq(0,100,by=10), yaxis.break = seq(0,0.6,by=0.1), 
                     text.x = 10, text.y = 0.52, legend.pos = "right"),
  swir1 = my_ggplot_lm_qual(gaps_spc_field_anc, "ECC_my", "SR_B5_swir1", "Type", "ForestQuality", pts = 2,
                       "ECC (%)", "SWIR1 BRF", xaxis.lim = c(0,100), yaxis.lim = c(0,0.28), 
                       xaxis.break = seq(0,100,by=10), yaxis.break = seq(0,0.28,by=0.05), 
                       text.x = 10, text.y = 0.25, legend.pos = "right")
)
  

pdf("graphs/ECC_vs_BRF_nir_TypeQual.pdf") # , height = 2.5, width = 3.5
ECC_BRF_byTypeQual$nir
dev.off()

# LAI vs BRF by forest type ----------------------------------------------
LAIt_BRF_byType <- list(
  green = my_ggplot_lm(gaps_spc_field_anc, "LAIt_CIcc_my", "SR_B2_green", "Type", ngroup = 5, pts = 2,
                       "LAI true", "Green BRF", xaxis.lim = c(0,5), yaxis.lim = c(0,0.09), 
                       xaxis.break = seq(0,5,by=0.5), yaxis.break = seq(0,0.09,by=0.02), 
                       text.x = 0.5, text.y = 0.06, 
                       shape.col = c(2:4,6,8,9), legend.pos = "right"),
  
  red = my_ggplot_lm(gaps_spc_field_anc, "LAIt_CIcc_my", "SR_B3_red", "Type", ngroup = 5, pts = 2,
                     "LAI true", "Red BRF", xaxis.lim = c(0,5), yaxis.lim = c(0,0.065), 
                     xaxis.break = seq(0,5,by=0.5), yaxis.break = seq(0,0.065,by=0.02), 
                     text.x = 0.5, text.y = 0.06, 
                     shape.col = c(2:4,6,8,9), legend.pos = "right"),
  
  nir = my_ggplot_lm(gaps_spc_field_anc, "LAIt_CIcc_my", "SR_B4_nir", "Type", ngroup = 5, pts = 2,
                     "LAI true", "NIR BRF", xaxis.lim = c(0,5), yaxis.lim = c(0,0.6), 
                     xaxis.break = seq(0,5,by=0.5), yaxis.break = seq(0,0.6,by=0.1), 
                     text.x = 0.5, text.y = 0.52, 
                     shape.col = c(2:4,6,8,9), legend.pos = "right"),
  
  swir1 = my_ggplot_lm(gaps_spc_field_anc, "LAIt_CIcc_my", "SR_B5_swir1", "Type", ngroup = 5, pts = 2,
                       "LAI true", "SWIR1 BRF", xaxis.lim = c(0,5), yaxis.lim = c(0,0.28), 
                       xaxis.break = seq(0,5,by=0.5), yaxis.break = seq(0,0.28,by=0.05), 
                       text.x = 0.5, text.y = 0.25, 
                       shape.col = c(2:4,6,8,9), legend.pos = "right"),
  
  swir2 = my_ggplot_lm(gaps_spc_field_anc, "LAIt_CIcc_my", "SR_B7_swir2", "Type", ngroup = 5, pts = 2,
                       "LAI true", "SWIR2 BRF", xaxis.lim = c(0,5), yaxis.lim = c(0,0.12), 
                       xaxis.break = seq(0,5,by=0.5), yaxis.break = seq(0,0.12,by=0.025), 
                       text.x = 0.5, text.y = 0.08, 
                       shape.col = c(2:4,6,8,9), legend.pos = "right")
)

# LAI vs BRF just with error bar ----------------------------------------------
temp <- gaps_spc_field_anc %>% mutate(
  SR_B2_green_minerr = SR_B2_green - (0.05*SR_B2_green + 0.005),
  SR_B2_green_pluserr = SR_B2_green + (0.05*SR_B2_green + 0.005),
  SR_B3_red_minerr = SR_B3_red - (0.05*SR_B3_red + 0.005),
  SR_B3_red_pluserr = SR_B3_red + (0.05*SR_B3_red + 0.005),
  SR_B4_nir_minerr = SR_B4_nir - (0.05*SR_B4_nir + 0.005),
  SR_B4_nir_pluserr = SR_B4_nir + (0.05*SR_B4_nir + 0.005),
  SR_B5_swir1_minerr = SR_B5_swir1 - (0.05*SR_B5_swir1 + 0.005),
  SR_B5_swir1_pluserr = SR_B5_swir1 + (0.05*SR_B5_swir1 + 0.005),
  SR_B7_swir2_minerr = SR_B7_swir2 - (0.05*SR_B7_swir2 + 0.005),
  SR_B7_swir2_pluserr = SR_B7_swir2 + (0.05*SR_B7_swir2 + 0.005)
)



LAIt_BRF_errbar <- list(
  green = my_ggplot_justerrbar(
    data = gaps_spc_field_anc, xvar = "LAIt_CIcc_my", yvar = "SR_B2_green",
    xaxis.lab = "LAI true", yaxis.lab = "Green BRF", xaxis.lim = c(0,5), yaxis.lim = c(0,0.09), 
    xaxis.break = seq(0,5,by=0.5), yaxis.break = seq(0,0.09,by=0.02), 
    legend.pos = "none", dataerr = temp, xerr = "LAIt_CIcc_my", yerr = "SR_B2_green", 
    yminerr = "SR_B2_green_minerr", ymaxerr = "SR_B2_green_pluserr"),
  
  red = my_ggplot_justerrbar(
    gaps_spc_field_anc, "LAIt_CIcc_my", "SR_B3_red",
     "LAI true", "Red BRF", xaxis.lim = c(0,5), yaxis.lim = c(0,0.065), 
     xaxis.break = seq(0,5,by=0.5), yaxis.break = seq(0,0.065,by=0.02), 
     legend.pos = "none", temp, "LAIt_CIcc_my", "SR_B3_red", "SR_B3_red_minerr", "SR_B3_red_pluserr"),
  
  nir = my_ggplot_justerrbar(
    gaps_spc_field_anc, "LAIt_CIcc_my", "SR_B4_nir",
   "LAI true", "NIR BRF", xaxis.lim = c(0,5), yaxis.lim = c(0,0.6), 
   xaxis.break = seq(0,5,by=0.5), yaxis.break = seq(0,0.6,by=0.1), 
   legend.pos = "none", temp, "LAIt_CIcc_my", "SR_B4_nir", "SR_B4_nir_minerr", "SR_B4_nir_pluserr"),
  
  swir1 = my_ggplot_justerrbar(
    gaps_spc_field_anc, "LAIt_CIcc_my", "SR_B5_swir1",
   "LAI true", "SWIR1 BRF", xaxis.lim = c(0,5), yaxis.lim = c(0,0.28), 
   xaxis.break = seq(0,5,by=0.5), yaxis.break = seq(0,0.28,by=0.05), 
    legend.pos = "none", temp, "LAIt_CIcc_my", "SR_B5_swir1", "SR_B5_swir1_minerr", "SR_B5_swir1_pluserr"),
  
  swir2 = my_ggplot_justerrbar(
    gaps_spc_field_anc, "LAIt_CIcc_my", "SR_B7_swir2",
   "LAI true", "SWIR2 BRF", xaxis.lim = c(0,5), yaxis.lim = c(0,0.12), 
   xaxis.break = seq(0,5,by=0.5), yaxis.break = seq(0,0.12,by=0.025), 
    legend.pos = "none", temp, "LAIt_CIcc_my", "SR_B7_swir2", "SR_B7_swir2_minerr", "SR_B7_swir2_pluserr")
)


     
# plot <- grid.arrange(red, red, red, red, red, ncol = 3)
# ggsave("graphs/ECC_vs_BRF.pdf", plot)

# For now print the plots (bands) separately, later ask Aarne how to print multiple plots
pdf("graphs/remove_poor_verypoor/LAIt_vs_BRF_errbar_swir2.pdf") # , height = 2.5, width = 3.5
LAIt_BRF_errbar$swir2 # + geom_text(aes(label=SampleID, pos=3), size=3, hjust=-0.5, vjust=0, col="gray")
dev.off()



# Height vs BRF by forest type --------------------------------------------
pdf("graphs/Height_vs_BRF_nir.pdf")
ggplot(gaps_spc_field_anc, aes(Height_max_m, SR_B4_nir)) + 
  geom_point(aes(shape = Type, col = Type), cex = 2) + theme_bw()
dev.off()






# Vines vs LAIt -------------------------------------------------------------------

pdf("graphs/Vines_vs_LAIt.pdf") # , height = 2.5, width = 3.5
my_ggplot_lm(gaps_spc_field_anc, "LAIt_CIcc_my", "VinesPerc", "Type", ngroup = 5, pts = 2,
             "LAI true", "Vines (%)", xaxis.lim = c(0,5), yaxis.lim = c(0,100), 
             xaxis.break = seq(0,5,by=0.5), yaxis.break = seq(0,100,by=10), 
             text.x = 4, text.y = 80, 
             shape.col = c(2:4,6,8,9), legend.pos = c(0.15,0.85))
dev.off()




# LAIt vs BRF by foresty type & forest quality -------------------------------------------
LAIt_BRF_byTypeQual <- list(
  green = my_ggplot_lm_qual(gaps_spc_field_anc, "LAIt_CIcc_my", "SR_B2_green", "Type", "ForestQuality", pts = 2,
                          "LAI true", "Green BRF", xaxis.lim = c(0,5), yaxis.lim = c(0,0.08), 
                          xaxis.break = seq(0,5,by=0.5), yaxis.break = seq(0,0.08,by=0.02), 
                          text.x = 0.5, text.y = 0.07, legend.pos = "right"),
  red = my_ggplot_lm_qual(gaps_spc_field_anc, "LAIt_CIcc_my", "SR_B3_red", "Type", "ForestQuality", pts = 2,
                          "LAI true", "Red BRF", xaxis.lim = c(0,5), yaxis.lim = c(0,0.065), 
                          xaxis.break = seq(0,5,by=0.5), yaxis.break = seq(0,0.065,by=0.02), 
                          text.x = 0.5, text.y = 0.06, legend.pos = "right"),
  nir = my_ggplot_lm_qual(gaps_spc_field_anc, "LAIt_CIcc_my", "SR_B4_nir", "Type", "ForestQuality", pts = 2,
                          "LAI true", "NIR BRF", xaxis.lim = c(0,5), yaxis.lim = c(0,0.6), 
                          xaxis.break = seq(0,5,by=0.5), yaxis.break = seq(0,0.6,by=0.1), 
                          text.x = 0.5, text.y = 0.52, legend.pos = "right"),
  swir1 = my_ggplot_lm_qual(gaps_spc_field_anc, "LAIt_CIcc_my", "SR_B5_swir1", "Type", "ForestQuality", pts = 2,
                            "LAI true", "SWIR1 BRF", xaxis.lim = c(0,5), yaxis.lim = c(0,0.28), 
                            xaxis.break = seq(0,5,by=0.5), yaxis.break = seq(0,0.28,by=0.05), 
                            text.x = 0.5, text.y = 0.25, legend.pos = "right")
)


pdf("graphs/LAIt_vs_BRF_green_TypeQual.pdf") # , height = 2.5, width = 3.5
LAIt_BRF_byTypeQual$green # + geom_text(aes(label = SampleID), col = "gray")
dev.off()





# N_deadpieces vs BRF -----------------------------------------------------
pdf("graphs/Ndeadpieces_vs_SWIR1.pdf") # , height = 2.5, width = 3.5
my_ggplot_lm(gaps_spc_field_anc, "N_deadpieces", "SR_B5_swir1", "Type", ngroup = 5, pts = 2,
             "N deadpieces", "Landsat swir1", xaxis.lim = c(0,170), yaxis.lim = c(0,0.28), 
             xaxis.break = seq(0,170,by=30), yaxis.break = seq(0,0.28,by=0.05), 
             text.x = 160, text.y = 0.25, 
             shape.col = c(2:4,6,8,9), legend.pos = c(0.85,0.15))  + geom_text(aes(label = SampleID), col = "gray")
dev.off()




# Summary statistic of field data -----------------------------------------
gaps_spc_field_anc <- gaps_spc_field_anc %>%  mutate(ECC_my = (1 - Gaps1_my) * 100)
gaps_spc_field_anc <- gaps_spc_field_anc %>%  mutate(ACC75_my = Closure75_my * 100)

gaps_spc_field_anc <- gaps_spc_field_anc %>% mutate(Type.new = factor(Type.new, levels = c("Primary", "Lightly Logged", "Twice Logged", "Salvage Logged stage 1", "Salvage Logged stage 2", "Oil Palm")))


FieldData.mean <- gaps_spc_field_anc %>%  group_by(Type.new) %>% dplyr::select(Type.new, ECC_my, ACC75_my, LAI_my, CIcc_my, LAIt_CIcc_my) %>% 
  summarize(ECC = round(mean(ECC_my), 1), 
            ACC75 = round(mean(ACC75_my), 1),
            LAIe = round(mean(LAI_my), 2),
            CI = round(mean(CIcc_my), 2),
            LAIt = round(mean(LAIt_CIcc_my), 2))

FieldData.min <- gaps_spc_field_anc %>%  group_by(Type.new) %>% dplyr::select(Type.new, ECC_my, ACC75_my, LAI_my, CIcc_my, LAIt_CIcc_my) %>% 
  summarize(ECC = round(min(ECC_my), 1), 
            ACC75 = round(min(ACC75_my), 1),
            LAIe = round(min(LAI_my), 2),
            CI = round(min(CIcc_my), 2),
            LAIt = round(min(LAIt_CIcc_my), 2))

FieldData.max <- gaps_spc_field_anc %>%  group_by(Type.new) %>% dplyr::select(Type.new, ECC_my, ACC75_my, LAI_my, CIcc_my, LAIt_CIcc_my) %>% 
  summarize(ECC = round(max(ECC_my), 1), 
            ACC75 = round(max(ACC75_my), 1),
            LAIe = round(max(LAI_my), 2),
            CI = round(max(CIcc_my), 2),
            LAIt = round(max(LAIt_CIcc_my), 2))

FieldData.sd <- gaps_spc_field_anc %>%  group_by(Type.new) %>% dplyr::select(Type.new, ECC_my, ACC75_my, LAI_my, CIcc_my, LAIt_CIcc_my) %>% 
  summarize(ECC = round(sd(ECC_my), 1), 
            ACC75 = round(sd(ACC75_my), 1),
            LAIe = round(sd(LAI_my), 2),
            CI = round(sd(CIcc_my), 2),
            LAIt = round(sd(LAIt_CIcc_my), 2))

FieldData.cv <- gaps_spc_field_anc %>%  group_by(Type.new) %>%  dplyr::select(Type.new, ECC_my, ACC75_my, LAI_my, CIcc_my, LAIt_CIcc_my) %>% 
  summarize(ECC = round(100*sd(ECC_my)/mean(ECC_my), 1), 
            ACC75 = round(100*sd(ACC75_my)/mean(ACC75_my), 1),
            LAIe = round(100*sd(LAI_my)/mean(LAI_my), 2),
            CI = round(100*sd(CIcc_my)/mean(CIcc_my), 2),
            LAIt = round(100*sd(LAIt_CIcc_my)/mean(LAIt_CIcc_my), 2))


write.csv2(FieldData.mean, "results/field_data/field_data_mean.csv")
write.csv2(FieldData.min, "results/field_data/field_data_min.csv")
write.csv2(FieldData.max, "results/field_data/field_data_max.csv")
write.csv2(FieldData.sd, "results/field_data/field_data_sd.csv")
write.csv2(FieldData.cv, "results/field_data/field_data_cv.csv")


# All
gaps_spc_field_anc %>%  dplyr::select(ECC_my, ACC75_my, LAI_my, CIcc_my, LAIt_CIcc_my) %>% 
  summarize(ECC = round(min(ECC_my), 1), 
            ACC75 = round(min(ACC75_my), 1),
            LAIe = round(min(LAI_my), 2),
            CI = round(min(CIcc_my), 2),
            LAIt = round(min(LAIt_CIcc_my), 2))

gaps_spc_field_anc %>%  dplyr::select(ECC_my, ACC75_my, LAI_my, CIcc_my, LAIt_CIcc_my) %>% 
  summarize(ECC = round(max(ECC_my), 1), 
            ACC75 = round(max(ACC75_my), 1),
            LAIe = round(max(LAI_my), 2),
            CI = round(max(CIcc_my), 2),
            LAIt = round(max(LAIt_CIcc_my), 2))

gaps_spc_field_anc %>%  dplyr::select(ECC_my, ACC75_my, LAI_my, CIcc_my, LAIt_CIcc_my) %>% 
  summarize(ECC = round(mean(ECC_my), 1), 
            ACC75 = round(mean(ACC75_my), 1),
            LAIe = round(mean(LAI_my), 2),
            CI = round(mean(CIcc_my), 2),
            LAIt = round(mean(LAIt_CIcc_my), 2))

gaps_spc_field_anc %>%  dplyr::select(ECC_my, ACC75_my, LAI_my, CIcc_my, LAIt_CIcc_my) %>% 
  summarize(ECC = round(sd(ECC_my), 1), 
            ACC75 = round(sd(ACC75_my), 1),
            LAIe = round(sd(LAI_my), 2),
            CI = round(sd(CIcc_my), 2),
            LAIt = round(sd(LAIt_CIcc_my), 2))

gaps_spc_field_anc %>%  dplyr::select(ECC_my, ACC75_my, LAI_my, CIcc_my, LAIt_CIcc_my) %>% 
  summarize(ECC = round(100*sd(ECC_my)/mean(ECC_my), 1), 
            ACC75 = round(100*sd(ACC75_my)/mean(ACC75_my), 1),
            LAIe = round(100*sd(LAI_my)/mean(LAI_my), 2),
            CI = round(100*sd(CIcc_my)/mean(CIcc_my), 2),
            LAIt = round(100*sd(LAIt_CIcc_my)/mean(LAIt_CIcc_my), 2))

# Except Salvage Logged B and Oil Palm
gaps_spc_field_anc %>%  dplyr::filter(!Type.new %in% c("Salvage Logged B", "Oil Palm")) %>% 
  dplyr::select(ECC_my, ACC75_my, LAI_my, CIcc_my, LAIt_CIcc_my) %>% 
  summarize(ECC = round(min(ECC_my), 1), 
            ACC75 = round(min(ACC75_my), 1),
            LAIe = round(min(LAI_my), 2),
            CI = round(min(CIcc_my), 2),
            LAIt = round(min(LAIt_CIcc_my), 2))

gaps_spc_field_anc %>%  dplyr::filter(!Type.new %in% c("Salvage Logged B", "Oil Palm")) %>% 
  dplyr::select(ECC_my, ACC75_my, LAI_my, CIcc_my, LAIt_CIcc_my) %>% 
  summarize(ECC = round(max(ECC_my), 1), 
            ACC75 = round(max(ACC75_my), 1),
            LAIe = round(max(LAI_my), 2),
            CI = round(max(CIcc_my), 2),
            LAIt = round(max(LAIt_CIcc_my), 2))


gaps_spc_field_anc %>%  dplyr::filter(!Type.new %in% c("Salvage Logged stage 2", "Oil Palm")) %>% 
  dplyr::select(ECC_my, ACC75_my, LAI_my, CIcc_my, LAIt_CIcc_my) %>% 
  summarize(ECC = round(mean(ECC_my), 1), 
            ACC75 = round(mean(ACC75_my), 1),
            LAIe = round(mean(LAI_my), 2),
            CI = round(mean(CIcc_my), 2),
            LAIt = round(mean(LAIt_CIcc_my), 2))

gaps_spc_field_anc %>%  dplyr::filter(!Type.new %in% c("Salvage Logged stage 2", "Oil Palm")) %>% 
  dplyr::select(ECC_my, ACC75_my, LAI_my, CIcc_my, LAIt_CIcc_my) %>% 
  summarize(ECC = round(sd(ECC_my), 1), 
            ACC75 = round(sd(ACC75_my), 1),
            LAIe = round(sd(LAI_my), 2),
            CI = round(sd(CIcc_my), 2),
            LAIt = round(sd(LAIt_CIcc_my), 2))

# For manuscript: ECC, ACC75, LAIt, bidirectional gaps --------------------                  # For manuscript ***
pts.cols <- c("magenta", "chartreuse4", "dodgerblue", "dark blue", "brown", "dark orange")
pts.pchs <- c(1,7,5,6,4,2)

gaps_spc_field_anc <- gaps_spc_field_anc %>%  mutate(ECC_my = (1 - Gaps1_my) * 100)
gaps_spc_field_anc <- gaps_spc_field_anc %>%  mutate(ACC75_my = Closure75_my * 100)
source("munge/add_img_sza.R")
gaps_spc_field_anc <- add_img_sza(gaps_spc_field_anc)

gaps_spc_field_anc <- gaps_spc_field_anc %>%  dplyr::mutate(
  cgf_view = Gaps1_my,                                                        
  cgf_sun = Gaps2_my + ((img_sza-23)/(38-23))*(Gaps3_my-Gaps2_my),
  i0_sun = 1 - cgf_sun,
  cgfsun.cgfview = cgf_sun * cgf_view,
  i0sun.cgfview = i0_sun * cgf_view,
  cgfsun.cgfview.plus.i0sun.cgfview = cgfsun.cgfview + i0sun.cgfview
)

temp <- gaps_spc_field_anc %>% mutate(
  Type.new = factor(Type.new, levels = c("Primary", "Lightly Logged", "Twice Logged", "Salvage Logged stage 1", "Salvage Logged stage 2", "Oil Palm")))



# LAIt vs ECC =  1 - gaps1 ------------------------------------------------

temp1 <- my_ggplot_ms(temp, "LAIt_CIcc_my", "ECC_my", "Type.new", ngroup = 6, pts = 2,
             expression(bold(LAI[true])), "ECC (%)", yaxis.lim = c(0,100), xaxis.lim = c(0,5), 
             yaxis.break = seq(0,100,by=20), xaxis.break = seq(0,5,by=1), 
             text.x = 4.9, text.y = 1, 
             shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none",
             text = "(a)")

# ECC vs Clos75   ------------------------------------------------

temp2 <- my_ggplot_1to1_ms(temp, "ACC75_my", "ECC_my", "Type.new", ngroup = 6, pts = 2,
          expression(paste(bold(ACC[75]), bold(" (%)"))), "ECC (%)",  yaxis.lim = c(0,100), xaxis.lim = c(0,100), 
          yaxis.break = seq(0,100,by=20), xaxis.break = seq(0,100,by=20), 
          text.x = 98, text.y = 2, 
          shape.col = pts.cols, shape.pch = pts.pchs, legend.pos =  c(0.3,0.73),
          text = "(b)") 


# # LAIe vs Clos75   ------------------------------------------------
# gaps_spc_field_anc <- gaps_spc_field_anc %>%  mutate(ACC75_my = Closure75_my * 100)
# 
# temp <- gaps_spc_field_anc %>% mutate(
#   Type.new = factor(Type.new, levels = c("Primary", "Lightly Logged", "Twice Logged", "Salvage Logged stage 1", "Salvage Logged stage 2", "Oil Palm")))
# 
# pdf("graphs/FINAL_for_manuscript/LAIe_vs_ACC75.pdf") # , height = 2.5, width = 3.5
# my_ggplot(temp, "ACC75_my", "LAI_my", "Type.new", ngroup = 6, pts = 2,
#                expression(paste(bold(ACC[75]), bold(" (%)"))), "LAIe",  yaxis.lim = c(0,5), xaxis.lim = c(0,100), 
#                yaxis.break = seq(0,5,by=1), xaxis.break = seq(0,100,by=20), 
#                text.x = 10, text.y = 4.5, 
#                shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = c(0.8,0.15)) 
# dev.off()


# LAIt vs cgf.sun*cgf.view & i0.sun*cgf.view ------------------------------------------------

temp3 <- my_ggplot_ms(temp, "LAIt_CIcc_my", "cgfsun.cgfview", "Type.new", ngroup = 6, pts = 2,
               expression(bold(LAI[true])), "Bidirectional gap probability",  # expression(paste(bold(cgf[sun]), bold(" * "), bold(cgf[view])))
               yaxis.lim = c(0,1), xaxis.lim = c(0,5), 
               yaxis.break = seq(0,1,by=0.2), xaxis.break = seq(0,5,by=1), 
               text.x = 4.9, text.y = 0.02, 
               shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none",
               text = "(d)") 

temp4 <- my_ggplot_ms(temp, "LAIt_CIcc_my", "i0sun.cgfview", "Type.new", ngroup = 6, pts = 2,
          expression(bold(LAI[true])), "Canopy direct interception * ",  # expression(paste(bold((1 - cgf[sun])), bold(" * "), bold(cgf[view])))
          yaxis.lim = c(0,1), xaxis.lim = c(0,5), 
          yaxis.break = seq(0,1,by=0.2), xaxis.break = seq(0,5,by=1), 
          text.x = 10, text.y = 90, 
          shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = c(0.37,0.73)) 

temp5 <- my_ggplot_ms(temp, "LAIt_CIcc_my", "Gaps3_my", "Type.new", ngroup = 6, pts = 2,
                      expression(bold(LAI[true])), "Canopy gap fraction",  # expression(paste(bold((1 - cgf[sun])), bold(" * "), bold(cgf[view])))
                      yaxis.lim = c(0,1), xaxis.lim = c(0,5), 
                      yaxis.break = seq(0,1,by=0.2), xaxis.break = seq(0,5,by=1), 
                      text.x = 10, text.y = 90, 
                      shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none") 

temp6 <- my_ggplot_ms(temp, "LAIt_CIcc_my", "ACC75_my", "Type.new", ngroup = 6, pts = 2,
                      expression(bold(LAI[true])), expression(paste(bold(ACC[75]), bold(" (%)"))),  # expression(paste(bold((1 - cgf[sun])), bold(" * "), bold(cgf[view])))
                      yaxis.lim = c(0,100), xaxis.lim = c(0,5), 
                      yaxis.break = seq(0,100,by=20), xaxis.break = seq(0,5,by=1), 
                      text.x = 4.9, text.y = 2, 
                      shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none",
                      text = "(c)") 

# pdf("graphs/FINAL_for_manuscript/canopy_structure.pdf", width = 5.4, height = 5, pointsize = 10) 
# pdf("graphs/FINAL_for_manuscript/canopy_structure_2.pdf", width = 5.4, height = 5, pointsize = 10) 
pdf("graphs/FINAL_for_manuscript/renamed/canopy_structure_2_annotate.pdf", width = 5.4, height = 5, pointsize = 10) 
# multiplot(temp1, temp3, temp2, temp4, cols = 2)
multiplot(temp1, temp6, temp2, temp3, cols = 2)
dev.off()

# FOR MANUSCRIPT: forest structure vs BRF by forest type ----------------------------------------------
ETM_ECCnLAItnLAIe_byType <- list(

  # ECC *********************
  ECC.red = my_ggplot_ms(temp, "ECC_my", "SR_B3_red", "Type.new", ngroup = 6, pts = 2,
                     "ECC (%)", "ETM+ red", xaxis.lim = c(0,100), yaxis.lim = c(0,0.062), 
                     xaxis.break = seq(0,100,by=20), yaxis.break = seq(0,0.065,by=0.02), 
                     text.x = 10, text.y = 0.06, 
                     shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),
  
  ECC.nir = my_ggplot_ms(temp, "ECC_my", "SR_B4_nir", "Type.new", ngroup = 6, pts = 2,
                     "ECC (%)", "ETM+ NIR", xaxis.lim = c(0,100), yaxis.lim = c(0,0.6), 
                     xaxis.break = seq(0,100,by=20), yaxis.break = seq(0,0.6,by=0.1), 
                     text.x = 10, text.y = 0.52, 
                     shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),
  
  ECC.swir1 = my_ggplot_ms(temp, "ECC_my", "SR_B5_swir1", "Type.new", ngroup = 6, pts = 2,
                       "ECC (%)", "ETM+ SWIR1 ", xaxis.lim = c(0,100), yaxis.lim = c(0,0.26), 
                       xaxis.break = seq(0,100,by=20), yaxis.break = seq(0,0.28,by=0.05), 
                       text.x = 10, text.y = 0.25, 
                       shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),
  
  # LAIt *********************
  LAIt.red = my_ggplot_ms(temp, "LAIt_CIcc_my", "SR_B3_red", "Type.new", ngroup = 6, pts = 2,
                     expression(bold(LAI[true])), "ETM+ red", xaxis.lim = c(0,5), yaxis.lim = c(0,0.062), 
                     xaxis.break = seq(0,5,by=1), yaxis.break = seq(0,0.065,by=0.02), 
                     text.x = 0.5, text.y = 0.06, 
                     shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),
  
  LAIt.nir = my_ggplot_ms(temp, "LAIt_CIcc_my", "SR_B4_nir", "Type.new", ngroup = 6, pts = 2,
                     expression(bold(LAI[true])), "ETM+ NIR", xaxis.lim = c(0,5), yaxis.lim = c(0,0.6), 
                     xaxis.break = seq(0,5,by=1), yaxis.break = seq(0,0.6,by=0.1), 
                     text.x = 0.5, text.y = 0.52, 
                     shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),
  
  LAIt.swir1 = my_ggplot_ms(temp, "LAIt_CIcc_my", "SR_B5_swir1", "Type.new", ngroup = 6, pts = 2,
                       expression(bold(LAI[true])), "ETM+ SWIR1", xaxis.lim = c(0,5), yaxis.lim = c(0,0.26), 
                       xaxis.break = seq(0,5,by=1), yaxis.break = seq(0,0.28,by=0.05), 
                       text.x = 0.5, text.y = 0.25, 
                       shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),
  
  # LAIe *********************
  LAIe.red = my_ggplot_ms(temp, "LAI_my", "SR_B3_red", "Type.new", ngroup = 6, pts = 2,
                          expression(bold(LAI[eff])), "ETM+ red", xaxis.lim = c(0,5), yaxis.lim = c(0,0.062), 
                          xaxis.break = seq(0,5,by=1), yaxis.break = seq(0,0.065,by=0.02), 
                          text.x = 0.5, text.y = 0.06, 
                          shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),
  
  LAIe.nir = my_ggplot_ms(temp, "LAI_my", "SR_B4_nir", "Type.new", ngroup = 6, pts = 2,
                          expression(bold(LAI[eff])), "ETM+ NIR", xaxis.lim = c(0,5), yaxis.lim = c(0,0.6), 
                          xaxis.break = seq(0,5,by=1), yaxis.break = seq(0,0.6,by=0.1), 
                          text.x = 0.5, text.y = 0.52, 
                          shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),
  
  LAIe.swir1 = my_ggplot_ms(temp, "LAI_my", "SR_B5_swir1", "Type.new", ngroup = 6, pts = 2,
                            expression(bold(LAI[eff])), "ETM+ SWIR1", xaxis.lim = c(0,5), yaxis.lim = c(0,0.26), 
                            xaxis.break = seq(0,5,by=1), yaxis.break = seq(0,0.28,by=0.05), 
                            text.x = 0.5, text.y = 0.25, 
                            shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),
  
  # cgf.sun * cgf.view *********************
  bidirgap.red = my_ggplot_ms(temp, "cgfsun.cgfview", "SR_B3_red", "Type.new", ngroup = 6, pts = 2,
                          "Bidirectional gap probability", "ETM+ red", xaxis.lim = c(0,0.8), yaxis.lim = c(0,0.062), 
                          xaxis.break = seq(0,0.8,by=0.2), yaxis.break = seq(0,0.065,by=0.02), 
                          text.x = 0.5, text.y = 0.06, 
                          shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),
  
  bidirgap.nir = my_ggplot_ms(temp, "cgfsun.cgfview", "SR_B4_nir", "Type.new", ngroup = 6, pts = 2,
                          "Bidirectional gap probability", "ETM+ NIR", xaxis.lim = c(0,0.8), yaxis.lim = c(0,0.6), 
                          xaxis.break = seq(0,0.8,by=0.2), yaxis.break = seq(0,0.6,by=0.1), 
                          text.x = 0.5, text.y = 0.52, 
                          shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),
  
  bidirgap.swir1 = my_ggplot_ms(temp, "cgfsun.cgfview", "SR_B5_swir1", "Type.new", ngroup = 6, pts = 2,
                            "Bidirectional gap probability", "ETM+ SWIR1", xaxis.lim = c(0,0.8), yaxis.lim = c(0,0.26), 
                            xaxis.break = seq(0,0.8,by=0.2), yaxis.break = seq(0,0.28,by=0.05), 
                            text.x = 0.5, text.y = 0.25, 
                            shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),
  
  # (1 - cgf.sun) * cgf.view *********************
  bidirpluscanintview.red = my_ggplot_ms(temp, "cgfsun.cgfview.plus.i0sun.cgfview", "SR_B3_red", "Type.new", ngroup = 6, pts = 2,
                              "cgfsun*cgfview + i0sun*cgfview", "ETM+ red", xaxis.lim = c(0,1), yaxis.lim = c(0,0.062), 
                              xaxis.break = seq(0,1,by=0.2), yaxis.break = seq(0,0.065,by=0.02), 
                              text.x = 0.5, text.y = 0.06, 
                              shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),
  
  bidirpluscanintview.nir = my_ggplot_ms(temp, "cgfsun.cgfview.plus.i0sun.cgfview", "SR_B4_nir", "Type.new", ngroup = 6, pts = 2,
                              "cgfsun*cgfview + i0sun*cgfview", "ETM+ NIR", xaxis.lim = c(0,1), yaxis.lim = c(0,0.6), 
                              xaxis.break = seq(0,1,by=0.2), yaxis.break = seq(0,0.6,by=0.1), 
                              text.x = 0.5, text.y = 0.52, 
                              shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none"),
  
  bidirpluscanintview.swir1 = my_ggplot_ms(temp, "cgfsun.cgfview.plus.i0sun.cgfview", "SR_B5_swir1", "Type.new", ngroup = 6, pts = 2,
                                "cgfsun*cgfview + i0sun*cgfview", "ETM+ SWIR1", xaxis.lim = c(0,1), yaxis.lim = c(0,0.26), 
                                xaxis.break = seq(0,1,by=0.2), yaxis.break = seq(0,0.28,by=0.05), 
                                text.x = 0.5, text.y = 0.25, 
                                shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "none")
  

)


# Make legend
for_legend <- my_ggplot_ms(temp, "LAI_my", "SR_B5_swir1", "Type.new", ngroup = 6, pts = 2,
                          expression(bold(LAI[eff])), "ETM swir1", xaxis.lim = c(0,5), yaxis.lim = c(0,0.26), 
                          xaxis.break = seq(0,5,by=1), yaxis.break = seq(0,0.28,by=0.05), 
                          text.x = 0.5, text.y = 0.25, 
                          shape.col = pts.cols, shape.pch = pts.pchs, legend.pos = "bottom") +
                          guides(fill=guide_legend(ncol=3))

pdf("graphs/FINAL_for_manuscript/legend.pdf", width = 7.4, height = 2.5, pointsize = 10) 
for_legend
dev.off()


pdf("graphs/FINAL_for_manuscript/ETM_ECCnLAIt.pdf", width = 7.4, height = 4.7, pointsize = 10) 
multiplot(ETM_ECCnLAItnLAIe_byType$ECC.red, ETM_ECCnLAItnLAIe_byType$LAIt.red, ETM_ECCnLAItnLAIe_byType$ECC.nir, 
          ETM_ECCnLAItnLAIe_byType$LAIt.nir, ETM_ECCnLAItnLAIe_byType$ECC.swir1, ETM_ECCnLAItnLAIe_byType$LAIt.swir1,
          cols = 3)
dev.off()

pdf("graphs/FINAL_for_manuscript/ETM_ECCnLAIe.pdf", width = 7.4, height = 4.7, pointsize = 10) 
multiplot(ETM_ECCnLAItnLAIe_byType$ECC.red, ETM_ECCnLAItnLAIe_byType$LAIe.red, ETM_ECCnLAItnLAIe_byType$ECC.nir, 
          ETM_ECCnLAItnLAIe_byType$LAIe.nir, ETM_ECCnLAItnLAIe_byType$ECC.swir1, ETM_ECCnLAItnLAIe_byType$LAIe.swir1,
          cols = 3)
dev.off()


pdf("graphs/FINAL_for_manuscript/ETM_bidirgap_canintview.pdf", width = 7.4, height = 4.7, pointsize = 10) 
multiplot(ETM_ECCnLAItnLAIe_byType$bidirgap.red, ETM_ECCnLAItnLAIe_byType$canopyinterceptview.red, 
          ETM_ECCnLAItnLAIe_byType$bidirgap.nir, ETM_ECCnLAItnLAIe_byType$canopyinterceptview.nir, 
          ETM_ECCnLAItnLAIe_byType$bidirgap.swir1, ETM_ECCnLAItnLAIe_byType$canopyinterceptview.swir1,
          cols = 3)
dev.off()


pdf("graphs/FINAL_for_manuscript/ETM_bidirgappluscanintview.pdf", width = 7.4, height = 2.5, pointsize = 10) 
multiplot(ETM_ECCnLAItnLAIe_byType$bidirpluscanintview.red, ETM_ECCnLAItnLAIe_byType$bidirpluscanintview.nir, 
          ETM_ECCnLAItnLAIe_byType$bidirpluscanintview.swir1, 
          cols = 3)
dev.off()


# Quantify correlation BRF and forest structure ---------------------------

# Level the forest type
temp <- gaps_spc_field_anc %>% mutate(Type.new = factor(Type.new, levels = c("Primary", "Lightly Logged", "Twice Logged", "Salvage Logged stage 1", "Salvage Logged stage 2", "Oil Palm")))
temp <-  temp %>%  mutate(ECC_my = (1 - Gaps1_my) * 100) # add ECC

# Add vegetation indices
temp <- temp %>% mutate(ndvi = (SR_B4_nir-SR_B3_red)/(SR_B4_nir+SR_B3_red),
                        evi = 2.5*((SR_B4_nir-SR_B3_red)/(SR_B4_nir+6*SR_B3_red-7.5*SR_B1_blue+1)),
                        ndmi = (SR_B4_nir-SR_B5_swir1)/(SR_B4_nir+SR_B5_swir1),
                        nbr = (SR_B4_nir-SR_B7_swir2)/(SR_B4_nir+SR_B7_swir2) )



tempcor <- temp %>% group_by(Type.new) %>% dplyr::select(Type.new, LAIt_CIcc_my, SR_B2_green, SR_B3_red, SR_B4_nir, SR_B5_swir1, SR_B7_swir2) %>% 
  summarise(green = round(cor(SR_B2_green, LAIt_CIcc_my), 3),
            red = round(cor(SR_B3_red, LAIt_CIcc_my), 3),
            nir = round(cor(SR_B4_nir, LAIt_CIcc_my), 3),
            swir1 = round(cor(SR_B5_swir1, LAIt_CIcc_my), 3),
            swir2 = round(cor(SR_B7_swir2, LAIt_CIcc_my), 3))

write.csv2(tempcor, "results/cor_Landsat_LAIt.csv")
write.csv2(tempcor, "results/cor_Landsat_ECC.csv")

# By biome
temp %>% group_by(Biome) %>% dplyr::select(Biome, ECC_my, SR_B2_green, SR_B3_red, SR_B4_nir, SR_B5_swir1, SR_B7_swir2) %>% 
  summarise(green = round(cor(SR_B2_green, ECC_my), 3),
            red = round(cor(SR_B3_red, ECC_my), 3),
            nir = round(cor(SR_B4_nir, ECC_my), 3),
            swir1 = round(cor(SR_B5_swir1, ECC_my), 3),
            swir2 = round(cor(SR_B7_swir2, ECC_my), 3))


temp %>% group_by(Biome) %>% dplyr::select(Biome, LAIt_CIcc_my, SR_B2_green, SR_B3_red, SR_B4_nir, SR_B5_swir1, SR_B7_swir2) %>% 
  summarise(green = round(cor(SR_B2_green, LAIt_CIcc_my), 3),
            red = round(cor(SR_B3_red, LAIt_CIcc_my), 3),
            nir = round(cor(SR_B4_nir, LAIt_CIcc_my), 3),
            swir1 = round(cor(SR_B5_swir1, LAIt_CIcc_my), 3),
            swir2 = round(cor(SR_B7_swir2, LAIt_CIcc_my), 3))

# Vegetation indices
temp %>% group_by(Biome) %>% dplyr::select(Biome, ECC_my, ndvi, evi, ndmi, nbr) %>% 
  summarise(ndvi = round(cor(ndvi, ECC_my), 3),
            evi = round(cor(evi, ECC_my), 3),
            ndmi = round(cor(ndmi, ECC_my), 3),
            nbr = round(cor(nbr, ECC_my), 3))

temp %>% group_by(Biome) %>% dplyr::select(Biome, LAIt_CIcc_my, ndvi, evi, ndmi, nbr) %>% 
  summarise(ndvi = round(cor(ndvi, LAIt_CIcc_my), 3),
            evi = round(cor(evi, LAIt_CIcc_my), 3),
            ndmi = round(cor(ndmi, LAIt_CIcc_my), 3),
            nbr = round(cor(nbr, LAIt_CIcc_my), 3))

# Select plots with mean LAIe for example DHP -----------------------------
# Run field data reading in paras_main.R
gaps.spc.field.anc %>% dplyr::filter(Type.new == "Oil Palm") %>% dplyr::filter(LAI_my >= 1.4) %>% dplyr::filter(LAI_my <= 2.5)



#  Check oil palm age -----------------------------------------------------
temp <- gaps.spc.field.anc
# Reclassify Type.new = "Oil Palm" into young and mature oil palm.
# OP1 and OP2 planted in 2006 and OP3 planted in 2000

temp <- temp %>% mutate(Type.new = as.character(Type.new))                                  # if_else doesn't work if data type is "factor"!

temp <- temp %>% 
 mutate(Type.new = dplyr::if_else(Type.new == "Oil Palm" & Block.x %in% c("OP1", "OP2"),
                                   "Oil Palm 2006", Type.new))

temp <- temp %>% 
  mutate(Type.new = dplyr::if_else(Type.new == "Oil Palm" & Block.x == ("OP3"),
                                   "Oil Palm 2000", Type.new))

temp <- temp %>% mutate(Type.new = factor(Type.new, levels = c("Primary", "Lightly Logged", "Twice Logged", "Salvage Logged stage 1", "Salvage Logged stage 2", "Oil Palm 2006", "Oil Palm 2000")))
temp <-  temp %>%  mutate(ECC_my = (1 - Gaps1_my) * 100) # add ECC


tempplot <- list(
  
  LAIt.red = my_ggplot_ms(temp, "LAIt_CIcc_my", "SR_B3_red", "Type.new", ngroup = 7, pts = 2,
                          expression(bold(LAI[true])), "ETM red", xaxis.lim = c(0,5), yaxis.lim = c(0,0.062), 
                          xaxis.break = seq(0,5,by=1), yaxis.break = seq(0,0.065,by=0.02), 
                          text.x = 0.5, text.y = 0.06, 
                          shape.col = c(pts.cols, "red"), shape.pch = c(pts.pchs, 3), legend.pos = "none"),
  
  LAIt.nir = my_ggplot_ms(temp, "LAIt_CIcc_my", "SR_B4_nir", "Type.new", ngroup = 7, pts = 2,
                          expression(bold(LAI[true])), "ETM nir", xaxis.lim = c(0,5), yaxis.lim = c(0,0.6), 
                          xaxis.break = seq(0,5,by=1), yaxis.break = seq(0,0.6,by=0.1), 
                          text.x = 0.5, text.y = 0.52, 
                          shape.col = c(pts.cols, "red"), shape.pch = c(pts.pchs, 3), legend.pos = "none"),
  
  LAIt.swir1 = my_ggplot_ms(temp, "LAIt_CIcc_my", "SR_B5_swir1", "Type.new", ngroup = 7, pts = 2,
                            expression(bold(LAI[true])), "ETM swir1", xaxis.lim = c(0,5), yaxis.lim = c(0,0.26), 
                            xaxis.break = seq(0,5,by=1), yaxis.break = seq(0,0.28,by=0.05), 
                            text.x = 0.5, text.y = 0.25, 
                            shape.col = c(pts.cols, "red"), shape.pch = c(pts.pchs, 3), legend.pos = "none"),
  
  LAIe.red = my_ggplot_ms(temp, "LAI_my", "SR_B3_red", "Type.new", ngroup = 7, pts = 2,
                          expression(bold(LAI[eff])), "ETM red", xaxis.lim = c(0,5), yaxis.lim = c(0,0.062), 
                          xaxis.break = seq(0,5,by=1), yaxis.break = seq(0,0.065,by=0.02), 
                          text.x = 0.5, text.y = 0.06, 
                          shape.col = c(pts.cols, "red"), shape.pch = c(pts.pchs, 3), legend.pos = "none"),
  
  LAIe.nir = my_ggplot_ms(temp, "LAI_my", "SR_B4_nir", "Type.new", ngroup = 7, pts = 2,
                          expression(bold(LAI[eff])), "ETM nir", xaxis.lim = c(0,5), yaxis.lim = c(0,0.6), 
                          xaxis.break = seq(0,5,by=1), yaxis.break = seq(0,0.6,by=0.1), 
                          text.x = 0.5, text.y = 0.52, 
                          shape.col = c(pts.cols, "red"), shape.pch = c(pts.pchs, 3), legend.pos = "none"),
  
  LAIe.swir1 = my_ggplot_ms(temp, "LAI_my", "SR_B5_swir1", "Type.new", ngroup = 7, pts = 2,
                            expression(bold(LAI[eff])), "ETM swir1", xaxis.lim = c(0,5), yaxis.lim = c(0,0.26), 
                            xaxis.break = seq(0,5,by=1), yaxis.break = seq(0,0.28,by=0.05), 
                            text.x = 0.5, text.y = 0.25, 
                            shape.col = c(pts.cols, "red"), shape.pch = c(pts.pchs, 3), legend.pos = "none")
  
)



pdf("graphs/FINAL_for_manuscript/ETM_LAIe_oilpalm_age.pdf", width = 7, height = 3, pointsize = 10) 
multiplot(tempplot$LAIe.red, tempplot$LAIe.nir, tempplot$LAIe.swir1,
          cols = 3)
dev.off()



# Get coordinates (lat lon) of 88 plots used in this study ----------------
# SampleID in temp is int, in SAFE.data.ori is char
SAFE.data.ori <- read_csv2("data/forest/LAI_Malaysia_NoDoublon.csv")
temp <- temp %>%  mutate(SampleID = as.character(SampleID))
SAFE.data.ori.used <- SAFE.data.ori %>% dplyr::filter(SampleID %in% temp[["SampleID"]])

# Add lat lon columns to temp
temp$lon <- SAFE.data.ori.used$GPS_Lon
temp$lat <- SAFE.data.ori.used$GPS_Lat

write.csv2(temp, "results/temp/temp_withLatLon.csv")




# Statistical tests of canopy structure differences -----------------------
# (1) Test homogeneity of variances with Bartlett test, if variances not the same, apply Welsh correction
# (2) If variance is the same, do ANOVA and Tukey's HSD test

# We test response variables: ECC, ACC75, LAIe, LAIt, CI












