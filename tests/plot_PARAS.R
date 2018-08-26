require(ggplot2)
require(tidyverse)
require(stringr)

# Call the figure plotting functions --------------------------------------
source("tests/my_ggplot.R")
# Note: later when adjusting the figure size, font sizes need to be adjusted

# Load PARAS simulation results -------------------------------------------
paras.res.Case1 <- read_csv2("results/PARAS/remove_poor_verypoor/Case1_DiptPrim_ElaePalm_mean.csv")
paras.res.Case1 <-  paras.res.Case1 %>%  mutate(ECC_my = (1 - Gaps1_my) * 100) # add ECC

paras.res.Case2.raw <- readRDS("results/PARAS/remove_poor_verypoor/Dipt2500.rds") # Raw because needs to be processed into Table

paras.res.Case3 <- read_csv2("results/PARAS/Case3_DiptDiptr_DiptDiptr.csv")
paras.res.Case3 <-  paras.res.Case3 %>%  mutate(ECC_my = (1 - Gaps1_my) * 100) # add ECC

paras.res.Case4 <- read_csv2("results/PARAS/Case4_DiptHerb_DiptHerb.csv")
paras.res.Case4 <-  paras.res.Case4 %>%  mutate(ECC_my = (1 - Gaps1_my) * 100) # add ECC

paras.res.Case5 <- read_csv2("results/PARAS/Case5_HawaiiHerb_HawaiiHerb.csv")
paras.res.Case5 <-  paras.res.Case5 %>%  mutate(ECC_my = (1 - Gaps1_my) * 100) # add ECC

paras.res.Case6 <- read_csv2("results/PARAS/Case6_DiptPrim_ElaePalm_soilMin.csv")
paras.res.Case6 <-  paras.res.Case6 %>%  mutate(ECC_my = (1 - Gaps1_my) * 100) # add ECC

paras.res.Case7 <- read_csv2("results/PARAS/Case7_case1_pUK_LAItUK.csv")
paras.res.Case7 <-  paras.res.Case7 %>%  mutate(ECC_my = (1 - Gaps1_my) * 100) # add ECC

paras.res.Case8.raw <- readRDS("results/PARAS/Case8_case4with2500samples.rds")

paras.res.Case9 <- read_csv2("results/PARAS/Case9_case1_lDiptElae_sHerbrich.csv")
paras.res.Case9 <-  paras.res.Case9 %>%  mutate(ECC_my = (1 - Gaps1_my) * 100) # add ECC

paras.res.Case10a.raw <- readRDS("results/PARAS/remove_poor_verypoor/Case10a_case2_prospErr.rds")

paras.res.Case11 <- read_csv2("results/PARAS/Case11_case4_withLAItUK.csv")
paras.res.Case11 <-  paras.res.Case11 %>%  mutate(ECC_my = (1 - Gaps1_my) * 100) # add ECC

paras.res.Case12 <- read_csv2("results/PARAS/Case12_case4_Qplus20%.csv")
paras.res.Case12 <-  paras.res.Case12 %>%  mutate(ECC_my = (1 - Gaps1_my) * 100) # add ECC

paras.res.Case13 <- read_csv2("results/PARAS/Case13_case4_wood11%.csv")
paras.res.Case13 <-  paras.res.Case13 %>%  mutate(ECC_my = (1 - Gaps1_my) * 100) # add ECC

paras.res.Case15 <- read_csv2("results/PARAS/remove_poor_verypoor/Case15_case1_newq.csv")
paras.res.Case15 <-  paras.res.Case15 %>%  mutate(ECC_my = (1 - Gaps1_my) * 100) # add ECC

paras.res.Case16 <- read_csv2("results/PARAS/remove_poor_verypoor/Case16_case1_newq.csv")
paras.res.Case16 <-  paras.res.Case16 %>%  mutate(ECC_my = (1 - Gaps1_my) * 100) # add ECC

paras.res.Case17 <- read_csv2("results/PARAS/remove_poor_verypoor/Case17_newq_wLDipt_RgPrim.csv")
paras.res.Case17 <-  paras.res.Case17 %>%  mutate(ECC_my = (1 - Gaps1_my) * 100) # add ECC

paras.res.Case18 <- read_csv2("results/PARAS/remove_poor_verypoor/Case18_oldq_wLDipt_soilinterc.csv")
paras.res.Case18 <-  paras.res.Case18 %>%  mutate(ECC_my = (1 - Gaps1_my) * 100) # add ECC

paras.res.Case19 <- read_csv2("results/PARAS/remove_poor_verypoor/Case19_newestq_wLDipt_RgPrim.csv")
paras.res.Case19 <-  paras.res.Case19 %>%  mutate(ECC_my = (1 - Gaps1_my) * 100) # add ECC

paras.res.Case20 <- read_csv2("results/PARAS/remove_poor_verypoor/Case20_newestq_wLDipt_soilinterc.csv")
paras.res.Case20 <-  paras.res.Case20 %>%  mutate(ECC_my = (1 - Gaps1_my) * 100) # add ECC

paras.res.Case21 <- read_csv2("results/PARAS/remove_poor_verypoor/Case21_newestq_wLDipt_soilmeasPrimPalm.csv")
paras.res.Case21 <-  paras.res.Case21 %>%  mutate(ECC_my = (1 - Gaps1_my) * 100) # add ECC

paras.res.Case22 <- read_csv2("results/PARAS/remove_poor_verypoor/Case22_oldq_wLDipt_soilmeasPrimPalm.csv")
paras.res.Case22 <-  paras.res.Case22 %>%  mutate(ECC_my = (1 - Gaps1_my) * 100) # add ECC

paras.res.Case24 <- read_csv2("results/PARAS/remove_poor_verypoor/Case24_oldq_wLDipt_soilLitter.csv")
paras.res.Case24 <-  paras.res.Case24 %>%  mutate(ECC_my = (1 - Gaps1_my) * 100) # add ECC

# Plot Landsat vs PARAS by Forest Type -----------------------------------------------------
# Make this also a function
# out.dir <- "graphs/PARAS/Case_1/bs"
# title.bs = "Case 1 black soil"
# title = "Case 1"

print.paras.graphs <- function (paras.res.Case, out.dir, out.dir.bs, title, title.bs) {
  
  # (a) Black soil --------------------------------------------------------------
  myplot_PARASvsLandsat(paras.res.Case, black.soil = TRUE, xaxis.lims = list(green=c(0,0.6), red=c(0,0.6), nir=c(0,0.6), swir1=c(0,0.6), swir2=c(0,0.6)),
                        yaxis.lims = list(green=c(0,0.6), red=c(0,0.6), nir=c(0,0.6), swir1=c(0,0.6), swir2=c(0,0.6)),
                        xaxis.breaks = list(green=seq(0,0.6,0.1), red=seq(0,0.6,0.1), nir=seq(0,0.6,0.1), swir1=seq(0,0.6,0.1), swir2=seq(0,0.6,0.1)), 
                        yaxis.breaks = list(green=seq(0,0.6,0.1), red=seq(0,0.6,0.1), nir=seq(0,0.6,0.1), swir1=seq(0,0.6,0.1), swir2=seq(0,0.6,0.1)),
                        text.xs = list(green=0.5, red=0.5, nir=0.5, swir1=0.5, swir2=0.5), 
                        text.ys = list(green=0.5, red=0.5, nir=0.5, swir1=0.5, swir2=0.5),
                        out.dir = out.dir.bs, title = title.bs)
  
  
  # (b) With soil ---------------------------------------------------------------
  myplot_PARASvsLandsat(paras.res.Case, black.soil = FALSE, xaxis.lims = list(green=c(0,0.6), red=c(0,0.6), nir=c(0,0.6), swir1=c(0,0.6), swir2=c(0,0.6)),
                        yaxis.lims = list(green=c(0,0.6), red=c(0,0.6), nir=c(0,0.6), swir1=c(0,0.6), swir2=c(0,0.6)),
                        xaxis.breaks = list(green=seq(0,0.6,0.1), red=seq(0,0.6,0.1), nir=seq(0,0.6,0.1), swir1=seq(0,0.6,0.1), swir2=seq(0,0.6,0.1)), 
                        yaxis.breaks = list(green=seq(0,0.6,0.1), red=seq(0,0.6,0.1), nir=seq(0,0.6,0.1), swir1=seq(0,0.6,0.1), swir2=seq(0,0.6,0.1)),
                        text.xs = list(green=0.5, red=0.5, nir=0.5, swir1=0.5, swir2=0.5), 
                        text.ys = list(green=0.5, red=0.5, nir=0.5, swir1=0.5, swir2=0.5),
                        out.dir = out.dir, title = title)
  
  
  
  
  # Plot forest variable vs Landsat & PARAS by forest type ------------------
  
  myplot_PARASvsLandsat_forvar(paras.res.Case, "LAIt_CIcc_my", 
                        xaxis.lims = list(green=c(0,5), red=c(0,5), nir=c(0,5), swir1=c(0,5), swir2=c(0,5)),
                        yaxis.lims = list(green=c(0,0.3), red=c(0,0.4), nir=c(0,0.6), swir1=c(0,0.6), swir2=c(0,0.4)),
                        xaxis.breaks = list(green=seq(0,5,0.5), red=seq(0,5,0.5), nir=seq(0,5,0.5), swir1=seq(0,5,0.5), swir2=seq(0,5,0.5)), 
                        yaxis.breaks = list(green=seq(0,0.3,0.05), red=seq(0,0.4,0.05), nir=seq(0,0.6,0.1), swir1=seq(0,0.6,0.1), swir2=seq(0,0.4,0.05)),
                        text.xs = list(green=4, red=4, nir=4, swir1=4, swir2=4), 
                        text.ys = list(green=0.8, red=0.8, nir=0.8, swir1=0.8, swir2=0.8),
                        out.dir = out.dir, title = title, xlab = "LAI true", black.soil = FALSE)
  
  myplot_PARASvsLandsat_forvar(paras.res.Case, "ECC_my",                                      # ECC
                        xaxis.lims = list(green=c(0,100), red=c(0,100), nir=c(0,100), swir1=c(0,100), swir2=c(0,100)),
                        yaxis.lims = list(green=c(0,0.3), red=c(0,0.4), nir=c(0,0.6), swir1=c(0,0.6), swir2=c(0,0.4)),
                        xaxis.breaks = list(green=seq(0,100,20), red=seq(0,100,20), nir=seq(0,100,20), swir1=seq(0,100,20), swir2=seq(0,100,20)), 
                        yaxis.breaks = list(green=seq(0,0.3,0.05), red=seq(0,0.4,0.05), nir=seq(0,0.6,0.1), swir1=seq(0,0.6,0.1), swir2=seq(0,0.4,0.05)),
                        text.xs = list(green=4, red=4, nir=4, swir1=4, swir2=4), 
                        text.ys = list(green=0.8, red=0.8, nir=0.8, swir1=0.8, swir2=0.8),
                        out.dir = out.dir, title = title, xlab = "ECC (%)", black.soil = FALSE)
  
  
  # Black soil
  myplot_PARASvsLandsat_forvar(paras.res.Case, "LAIt_CIcc_my", 
                        xaxis.lims = list(green=c(0,5), red=c(0,5), nir=c(0,5), swir1=c(0,5), swir2=c(0,5)),
                        yaxis.lims = list(green=c(0,0.3), red=c(0,0.4), nir=c(0,0.6), swir1=c(0,0.6), swir2=c(0,0.4)),
                        xaxis.breaks = list(green=seq(0,5,0.5), red=seq(0,5,0.5), nir=seq(0,5,0.5), swir1=seq(0,5,0.5), swir2=seq(0,5,0.5)), 
                        yaxis.breaks = list(green=seq(0,0.3,0.05), red=seq(0,0.4,0.05), nir=seq(0,0.6,0.1), swir1=seq(0,0.6,0.1), swir2=seq(0,0.4,0.05)),
                        text.xs = list(green=4, red=4, nir=4, swir1=4, swir2=4), 
                        text.ys = list(green=0.8, red=0.8, nir=0.8, swir1=0.8, swir2=0.8),
                        out.dir = out.dir.bs, title = title.bs, xlab = "LAI true", black.soil = TRUE)
  
  myplot_PARASvsLandsat_forvar(paras.res.Case, "ECC_my",                                      # ECC
                        xaxis.lims = list(green=c(0,100), red=c(0,100), nir=c(0,100), swir1=c(0,100), swir2=c(0,100)),
                        yaxis.lims = list(green=c(0,0.3), red=c(0,0.4), nir=c(0,0.6), swir1=c(0,0.6), swir2=c(0,0.4)),
                        xaxis.breaks = list(green=seq(0,100,20), red=seq(0,100,20), nir=seq(0,100,20), swir1=seq(0,100,20), swir2=seq(0,100,20)), 
                        yaxis.breaks = list(green=seq(0,0.3,0.05), red=seq(0,0.4,0.05), nir=seq(0,0.6,0.1), swir1=seq(0,0.6,0.1), swir2=seq(0,0.4,0.05)),
                        text.xs = list(green=4, red=4, nir=4, swir1=4, swir2=4), 
                        text.ys = list(green=0.8, red=0.8, nir=0.8, swir1=0.8, swir2=0.8),
                        out.dir = out.dir.bs, title = title.bs, xlab = "ECC (%)", black.soil = TRUE)
  
  return(1)
}     # Function to print graphs pdf


print.paras.graphs(paras.res.Case1, "graphs/remove_poor_verypoor/PARAS/Case_1", "graphs/remove_poor_verypoor/PARAS/Case_1/bs", "Case 1", "Case 1 BS")
print.paras.graphs(paras.res.Case3, "graphs/PARAS/Case_3", "graphs/PARAS/Case_3/bs", "Case 3", "Case 3 BS")
print.paras.graphs(paras.res.Case4, "graphs/PARAS/Case_4", "graphs/PARAS/Case_4/bs", "Case 4", "Case 4 BS")
print.paras.graphs(paras.res.Case5, "graphs/PARAS/Case_5", "graphs/PARAS/Case_5/bs", "Case 5", "Case 5 BS")
print.paras.graphs(paras.res.Case6, "graphs/PARAS/Case_6", "graphs/PARAS/Case_6/bs", "Case 6", "Case 6 BS")
print.paras.graphs(paras.res.Case7, "graphs/PARAS/Case_7", "graphs/PARAS/Case_7/bs", "Case 7", "Case 7 BS")
print.paras.graphs(paras.res.Case9, "graphs/PARAS/Case_9", "graphs/PARAS/Case_9/bs", "Case 9", "Case 9 BS")
print.paras.graphs(paras.res.Case11, "graphs/PARAS/Case_11", "graphs/PARAS/Case_11/bs", "Case 11", "Case 11 BS")
print.paras.graphs(paras.res.Case12, "graphs/PARAS/Case_12", "graphs/PARAS/Case_12/bs", "Case 12", "Case 12 BS")
print.paras.graphs(paras.res.Case13, "graphs/PARAS/Case_13", "graphs/PARAS/Case_13/bs", "Case 13", "Case 13 BS")
print.paras.graphs(paras.res.Case15, "graphs/remove_poor_verypoor/PARAS/Case_15", "graphs/remove_poor_verypoor/PARAS/Case_15/bs", "Case 15", "Case 15 BS")
print.paras.graphs(paras.res.Case16, "graphs/remove_poor_verypoor/PARAS/Case_16", "graphs/remove_poor_verypoor/PARAS/Case_16/bs", "Case 16", "Case 16 BS")

print.paras.graphs(paras.res.Case17, "graphs/remove_poor_verypoor/PARAS/Case_17", "graphs/remove_poor_verypoor/PARAS/Case_17/bs", "Case 17", "Case 17 BS")
print.paras.graphs(paras.res.Case18, "graphs/remove_poor_verypoor/PARAS/Case_18", "graphs/remove_poor_verypoor/PARAS/Case_18/bs", "Case 18", "Case 18 BS")
print.paras.graphs(paras.res.Case19, "graphs/remove_poor_verypoor/PARAS/Case_19", "graphs/remove_poor_verypoor/PARAS/Case_19/bs", "Case 19", "Case 19 BS")
print.paras.graphs(paras.res.Case20, "graphs/remove_poor_verypoor/PARAS/Case_20", "graphs/remove_poor_verypoor/PARAS/Case_20/bs", "Case 20", "Case 20 BS")
print.paras.graphs(paras.res.Case21, "graphs/remove_poor_verypoor/PARAS/Case_21", "graphs/remove_poor_verypoor/PARAS/Case_21/bs", "Case 21", "Case 21 BS")
print.paras.graphs(paras.res.Case22, "graphs/remove_poor_verypoor/PARAS/Case_22", "graphs/remove_poor_verypoor/PARAS/Case_22/bs", "Case 22", "Case 22 BS")
print.paras.graphs(paras.res.Case24, "graphs/remove_poor_verypoor/PARAS/Case_24", "graphs/remove_poor_verypoor/PARAS/Case_24/bs", "Case 24", "Case 24 BS")


# Plot PARAS sampling -----------------------------------------------------
# A function to summarize PARAS sampling ----------------------------------
source("tests/paras_summary.R")
source("tests/add_SR_error.R")
paras.res.Case2 <- paras.summary(paras.res.Case2.raw)
paras.res.Case2 <-  paras.res.Case2 %>%  mutate(ECC_my = (1 - Gaps1_my) * 100) # add ECC

paras.res.Case8 <- paras.summary(paras.res.Case8.raw)
paras.res.Case8 <-  paras.res.Case8 %>%  mutate(ECC_my = (1 - Gaps1_my) * 100) # add ECC

paras.res.Case10a <- paras.summary(paras.res.Case10a.raw)
paras.res.Case10a <-  paras.res.Case10a %>%  mutate(ECC_my = (1 - Gaps1_my) * 100) # add ECC
paras.res.Case10a <-  add_SR_error(paras.res.Case10a)                              # ad SR error

paras.res.Case10b <- paras.summary.addSRnoise(paras.res.Case10a.raw)
paras.res.Case10b <-  paras.res.Case10b %>%  mutate(ECC_my = (1 - Gaps1_my) * 100) # add ECC


# Function to print Figures with error bars
print.paras.graphs.errbar <- function (paras.res.Case, out.dir, out.dir.bs, title, title.bs) {   # Function to print graphs pdf
  
  # Plot forest variable vs Landsat & PARAS by forest type ------------------
  
  myplot_PARASvsLandsat_forvar_errbar(paras.res.Case, "LAIt_CIcc_my", 
                               xaxis.lims = list(green=c(0,5), red=c(0,5), nir=c(0,5), swir1=c(0,5), swir2=c(0,5)),
                               yaxis.lims = list(green=c(-0.05,0.3), red=c(-0.05,0.4), nir=c(-0.05,0.6), swir1=c(-0.05,0.6), swir2=c(-0.05,0.4)),
                               xaxis.breaks = list(green=seq(0,5,0.5), red=seq(0,5,0.5), nir=seq(0,5,0.5), swir1=seq(0,5,0.5), swir2=seq(0,5,0.5)), 
                               yaxis.breaks = list(green=seq(0,0.3,0.05), red=seq(0,0.4,0.05), nir=seq(0,0.6,0.1), swir1=seq(0,0.6,0.1), swir2=seq(0,0.4,0.05)),
                               text.xs = list(green=4, red=4, nir=4, swir1=4, swir2=4), 
                               text.ys = list(green=0.8, red=0.8, nir=0.8, swir1=0.8, swir2=0.8),
                               out.dir = out.dir, title = title, xlab = "LAI true", black.soil = FALSE)
  
  myplot_PARASvsLandsat_forvar_errbar(paras.res.Case, "ECC_my",                                      # ECC
                               xaxis.lims = list(green=c(0,100), red=c(0,100), nir=c(0,100), swir1=c(0,100), swir2=c(0,100)),
                               yaxis.lims = list(green=c(-0.05,0.3), red=c(-0.05,0.4), nir=c(-0.05,0.6), swir1=c(-0.05,0.6), swir2=c(-0.05,0.4)),
                               xaxis.breaks = list(green=seq(0,100,20), red=seq(0,100,20), nir=seq(0,100,20), swir1=seq(0,100,20), swir2=seq(0,100,20)), 
                               yaxis.breaks = list(green=seq(0,0.3,0.05), red=seq(0,0.4,0.05), nir=seq(0,0.6,0.1), swir1=seq(0,0.6,0.1), swir2=seq(0,0.4,0.05)),
                               text.xs = list(green=4, red=4, nir=4, swir1=4, swir2=4), 
                               text.ys = list(green=0.8, red=0.8, nir=0.8, swir1=0.8, swir2=0.8),
                               out.dir = out.dir, title = title, xlab = "ECC (%)", black.soil = FALSE)
  
  
  # Black soil
  myplot_PARASvsLandsat_forvar_errbar(paras.res.Case, "LAIt_CIcc_my", 
                               xaxis.lims = list(green=c(0,5), red=c(0,5), nir=c(0,5), swir1=c(0,5), swir2=c(0,5)),
                               yaxis.lims = list(green=c(-0.05,0.3), red=c(-0.05,0.4), nir=c(-0.05,0.6), swir1=c(-0.05,0.6), swir2=c(-0.05,0.4)),
                               xaxis.breaks = list(green=seq(0,5,0.5), red=seq(0,5,0.5), nir=seq(0,5,0.5), swir1=seq(0,5,0.5), swir2=seq(0,5,0.5)), 
                               yaxis.breaks = list(green=seq(0,0.3,0.05), red=seq(0,0.4,0.05), nir=seq(0,0.6,0.1), swir1=seq(0,0.6,0.1), swir2=seq(0,0.4,0.05)),
                               text.xs = list(green=4, red=4, nir=4, swir1=4, swir2=4), 
                               text.ys = list(green=0.8, red=0.8, nir=0.8, swir1=0.8, swir2=0.8),
                               out.dir = out.dir.bs, title = title.bs, xlab = "LAI true", black.soil = TRUE)
  
  myplot_PARASvsLandsat_forvar_errbar(paras.res.Case, "ECC_my",                                      # ECC
                               xaxis.lims = list(green=c(0,100), red=c(0,100), nir=c(0,100), swir1=c(0,100), swir2=c(0,100)),
                               yaxis.lims = list(green=c(-0.05,0.3), red=c(-0.05,0.4), nir=c(-0.05,0.6), swir1=c(-0.05,0.6), swir2=c(-0.05,0.4)),
                               xaxis.breaks = list(green=seq(0,100,20), red=seq(0,100,20), nir=seq(0,100,20), swir1=seq(0,100,20), swir2=seq(0,100,20)), 
                               yaxis.breaks = list(green=seq(0,0.3,0.05), red=seq(0,0.4,0.05), nir=seq(0,0.6,0.1), swir1=seq(0,0.6,0.1), swir2=seq(0,0.4,0.05)),
                               text.xs = list(green=4, red=4, nir=4, swir1=4, swir2=4), 
                               text.ys = list(green=0.8, red=0.8, nir=0.8, swir1=0.8, swir2=0.8),
                               out.dir = out.dir.bs, title = title.bs, xlab = "ECC (%)", black.soil = TRUE)
  
  return(1)
}       

# Limit LAI
print.paras.graphs.errbar2 <- function (paras.res.Case, out.dir, out.dir.bs, title, title.bs) {   # Function to print graphs pdf
  
  # Plot forest variable vs Landsat & PARAS by forest type ------------------
  
  myplot_PARASvsLandsat_forvar_errbar(paras.res.Case, "LAIt_CIcc_my", 
                                      xaxis.lims = list(green=c(2.5,4.75), red=c(2.5,4.75), nir=c(2.5,4.75), swir1=c(2.5,4.75), swir2=c(2.5,4.75)),
                                      yaxis.lims = list(green=c(0,0.075), red=c(0,0.06), nir=c(0,0.6), swir1=c(0,0.25), swir2=c(0,0.09)),
                                      xaxis.breaks = list(green=seq(2.5,4.75,0.5), red=seq(2.5,4.75,0.5), nir=seq(2.5,4.75,0.5), swir1=seq(2.5,4.75,0.5), swir2=seq(2.5,4.75,0.5)), 
                                      yaxis.breaks = list(green=seq(0,0.075,0.02), red=seq(0,0.06,0.02), nir=seq(0,0.6,0.1), swir1=seq(0,0.25,0.05), swir2=seq(0,0.09,0.02)),
                                      text.xs = list(green=4, red=4, nir=4, swir1=4, swir2=4), 
                                      text.ys = list(green=0.8, red=0.8, nir=0.8, swir1=0.8, swir2=0.8),
                                      out.dir = out.dir, title = title, xlab = "LAI true", black.soil = FALSE)
  
  myplot_PARASvsLandsat_forvar_errbar(paras.res.Case, "ECC_my",                                      # ECC
                                      xaxis.lims = list(green=c(0,100), red=c(0,100), nir=c(0,100), swir1=c(0,100), swir2=c(0,100)),
                                      yaxis.lims = list(green=c(0,0.075), red=c(0,0.06), nir=c(0,0.6), swir1=c(0,0.25), swir2=c(0,0.09)),
                                      xaxis.breaks = list(green=seq(0,100,20), red=seq(0,100,20), nir=seq(0,100,20), swir1=seq(0,100,20), swir2=seq(0,100,20)), 
                                      yaxis.breaks = list(green=seq(0,0.075,0.02), red=seq(0,0.06,0.02), nir=seq(0,0.6,0.1), swir1=seq(0,0.25,0.05), swir2=seq(0,0.09,0.02)),
                                      text.xs = list(green=4, red=4, nir=4, swir1=4, swir2=4), 
                                      text.ys = list(green=0.8, red=0.8, nir=0.8, swir1=0.8, swir2=0.8),
                                      out.dir = out.dir, title = title, xlab = "ECC (%)", black.soil = FALSE)
  
  
  # Black soil
  myplot_PARASvsLandsat_forvar_errbar(paras.res.Case, "LAIt_CIcc_my", 
                                      xaxis.lims = list(green=c(2.5,4.75), red=c(2.5,4.75), nir=c(2.5,4.75), swir1=c(2.5,4.75), swir2=c(2.5,4.75)),
                                      yaxis.lims = list(green=c(0,0.075), red=c(0,0.06), nir=c(0,0.6), swir1=c(0,0.25), swir2=c(0,0.09)),
                                      xaxis.breaks = list(green=seq(2.5,4.75,0.5), red=seq(2.5,4.75,0.5), nir=seq(2.5,4.75,0.5), swir1=seq(2.5,4.75,0.5), swir2=seq(2.5,4.75,0.5)), 
                                      yaxis.breaks = list(green=seq(0,0.075,0.02), red=seq(0,0.06,0.02), nir=seq(0,0.6,0.1), swir1=seq(0,0.25,0.05), swir2=seq(0,0.09,0.02)),
                                      text.xs = list(green=4, red=4, nir=4, swir1=4, swir2=4), 
                                      text.ys = list(green=0.8, red=0.8, nir=0.8, swir1=0.8, swir2=0.8),
                                      out.dir = out.dir.bs, title = title.bs, xlab = "LAI true", black.soil = TRUE)
  
  myplot_PARASvsLandsat_forvar_errbar(paras.res.Case, "ECC_my",                                      # ECC
                                      xaxis.lims = list(green=c(0,100), red=c(0,100), nir=c(0,100), swir1=c(0,100), swir2=c(0,100)),
                                      yaxis.lims = list(green=c(0,0.075), red=c(0,0.06), nir=c(0,0.6), swir1=c(0,0.25), swir2=c(0,0.09)),
                                      xaxis.breaks = list(green=seq(0,100,20), red=seq(0,100,20), nir=seq(0,100,20), swir1=seq(0,100,20), swir2=seq(0,100,20)), 
                                      yaxis.breaks = list(green=seq(0,0.075,0.02), red=seq(0,0.06,0.02), nir=seq(0,0.6,0.1), swir1=seq(0,0.25,0.05), swir2=seq(0,0.09,0.02)),
                                      text.xs = list(green=4, red=4, nir=4, swir1=4, swir2=4), 
                                      text.ys = list(green=0.8, red=0.8, nir=0.8, swir1=0.8, swir2=0.8),
                                      out.dir = out.dir.bs, title = title.bs, xlab = "ECC (%)", black.soil = TRUE)
  
  return(1)
}       


print.paras.graphs.2errbar <- function (paras.res.Case, out.dir, out.dir.bs, title, title.bs) {   # Function to print graphs pdf
  
  # Plot forest variable vs Landsat & PARAS by forest type ------------------
  
  myplot_PARASvsLandsat_forvar_2errbar(paras.res.Case, "LAIt_CIcc_my", 
                                      xaxis.lims = list(green=c(2.5,4.75), red=c(2.5,4.75), nir=c(2.5,4.75), swir1=c(2.5,4.75), swir2=c(2.5,4.75)),
                                      yaxis.lims = list(green=c(0,0.075), red=c(0,0.06), nir=c(0,0.6), swir1=c(0,0.25), swir2=c(0,0.09)),
                                      xaxis.breaks = list(green=seq(2.5,4.75,0.5), red=seq(2.5,4.75,0.5), nir=seq(2.5,4.75,0.5), swir1=seq(2.5,4.75,0.5), swir2=seq(2.5,4.75,0.5)), 
                                      yaxis.breaks = list(green=seq(0,0.075,0.02), red=seq(0,0.06,0.02), nir=seq(0,0.6,0.1), swir1=seq(0,0.25,0.05), swir2=seq(0,0.09,0.02)),
                                      text.xs = list(green=4, red=4, nir=4, swir1=4, swir2=4), 
                                      text.ys = list(green=0.8, red=0.8, nir=0.8, swir1=0.8, swir2=0.8),
                                      out.dir = out.dir, title = title, xlab = "LAI true", black.soil = FALSE)
  
  myplot_PARASvsLandsat_forvar_2errbar(paras.res.Case, "ECC_my",                                      # ECC
                                      xaxis.lims = list(green=c(0,100), red=c(0,100), nir=c(0,100), swir1=c(0,100), swir2=c(0,100)),
                                      yaxis.lims = list(green=c(0,0.075), red=c(0,0.06), nir=c(0,0.6), swir1=c(0,0.25), swir2=c(0,0.09)),
                                      xaxis.breaks = list(green=seq(0,100,20), red=seq(0,100,20), nir=seq(0,100,20), swir1=seq(0,100,20), swir2=seq(0,100,20)), 
                                      yaxis.breaks = list(green=seq(0,0.075,0.02), red=seq(0,0.06,0.02), nir=seq(0,0.6,0.1), swir1=seq(0,0.25,0.05), swir2=seq(0,0.09,0.02)),
                                      text.xs = list(green=4, red=4, nir=4, swir1=4, swir2=4), 
                                      text.ys = list(green=0.8, red=0.8, nir=0.8, swir1=0.8, swir2=0.8),
                                      out.dir = out.dir, title = title, xlab = "ECC (%)", black.soil = FALSE)
  
  
  # Black soil
  myplot_PARASvsLandsat_forvar_2errbar(paras.res.Case, "LAIt_CIcc_my", 
                                      xaxis.lims = list(green=c(2.5,4.75), red=c(2.5,4.75), nir=c(2.5,4.75), swir1=c(2.5,4.75), swir2=c(2.5,4.75)),
                                      yaxis.lims = list(green=c(0,0.075), red=c(0,0.06), nir=c(0,0.6), swir1=c(0,0.25), swir2=c(0,0.09)),
                                      xaxis.breaks = list(green=seq(2.5,4.75,0.5), red=seq(2.5,4.75,0.5), nir=seq(2.5,4.75,0.5), swir1=seq(2.5,4.75,0.5), swir2=seq(2.5,4.75,0.5)), 
                                      yaxis.breaks = list(green=seq(0,0.075,0.02), red=seq(0,0.06,0.02), nir=seq(0,0.6,0.1), swir1=seq(0,0.25,0.05), swir2=seq(0,0.09,0.02)),
                                      text.xs = list(green=4, red=4, nir=4, swir1=4, swir2=4), 
                                      text.ys = list(green=0.8, red=0.8, nir=0.8, swir1=0.8, swir2=0.8),
                                      out.dir = out.dir.bs, title = title.bs, xlab = "LAI true", black.soil = TRUE)
  
  myplot_PARASvsLandsat_forvar_2errbar(paras.res.Case, "ECC_my",                                      # ECC
                                      xaxis.lims = list(green=c(0,100), red=c(0,100), nir=c(0,100), swir1=c(0,100), swir2=c(0,100)),
                                      yaxis.lims = list(green=c(0,0.075), red=c(0,0.06), nir=c(0,0.6), swir1=c(0,0.25), swir2=c(0,0.09)),
                                      xaxis.breaks = list(green=seq(0,100,20), red=seq(0,100,20), nir=seq(0,100,20), swir1=seq(0,100,20), swir2=seq(0,100,20)), 
                                      yaxis.breaks = list(green=seq(0,0.075,0.02), red=seq(0,0.06,0.02), nir=seq(0,0.6,0.1), swir1=seq(0,0.25,0.05), swir2=seq(0,0.09,0.02)),
                                      text.xs = list(green=4, red=4, nir=4, swir1=4, swir2=4), 
                                      text.ys = list(green=0.8, red=0.8, nir=0.8, swir1=0.8, swir2=0.8),
                                      out.dir = out.dir.bs, title = title.bs, xlab = "ECC (%)", black.soil = TRUE)
  
  return(1)
}       

print.paras.graphs.errbar.minmax <- function (paras.res.Case, out.dir, out.dir.bs, title, title.bs) {   # Function to print graphs pdf
  
  # Plot forest variable vs Landsat & PARAS by forest type ------------------
  
  myplot_PARASvsLandsat_forvar_errbar_minmax(paras.res.Case, "LAIt_CIcc_my", 
                                      xaxis.lims = list(green=c(0,5), red=c(0,5), nir=c(0,5), swir1=c(0,5), swir2=c(0,5)),
                                      yaxis.lims = list(green=c(-0.05,0.3), red=c(-0.05,0.4), nir=c(-0.05,0.6), swir1=c(-0.05,0.6), swir2=c(-0.05,0.4)),
                                      xaxis.breaks = list(green=seq(0,5,0.5), red=seq(0,5,0.5), nir=seq(0,5,0.5), swir1=seq(0,5,0.5), swir2=seq(0,5,0.5)), 
                                      yaxis.breaks = list(green=seq(0,0.3,0.05), red=seq(0,0.4,0.05), nir=seq(0,0.6,0.1), swir1=seq(0,0.6,0.1), swir2=seq(0,0.4,0.05)),
                                      text.xs = list(green=4, red=4, nir=4, swir1=4, swir2=4), 
                                      text.ys = list(green=0.8, red=0.8, nir=0.8, swir1=0.8, swir2=0.8),
                                      out.dir = out.dir, title = title, xlab = "LAI true", black.soil = FALSE)
  
  myplot_PARASvsLandsat_forvar_errbar_minmax(paras.res.Case, "ECC_my",                                      # ECC
                                      xaxis.lims = list(green=c(0,100), red=c(0,100), nir=c(0,100), swir1=c(0,100), swir2=c(0,100)),
                                      yaxis.lims = list(green=c(-0.05,0.3), red=c(-0.05,0.4), nir=c(-0.05,0.6), swir1=c(-0.05,0.6), swir2=c(-0.05,0.4)),
                                      xaxis.breaks = list(green=seq(0,100,20), red=seq(0,100,20), nir=seq(0,100,20), swir1=seq(0,100,20), swir2=seq(0,100,20)), 
                                      yaxis.breaks = list(green=seq(0,0.3,0.05), red=seq(0,0.4,0.05), nir=seq(0,0.6,0.1), swir1=seq(0,0.6,0.1), swir2=seq(0,0.4,0.05)),
                                      text.xs = list(green=4, red=4, nir=4, swir1=4, swir2=4), 
                                      text.ys = list(green=0.8, red=0.8, nir=0.8, swir1=0.8, swir2=0.8),
                                      out.dir = out.dir, title = title, xlab = "ECC (%)", black.soil = FALSE)
  
  
  # Black soil
  myplot_PARASvsLandsat_forvar_errbar_minmax(paras.res.Case, "LAIt_CIcc_my", 
                                      xaxis.lims = list(green=c(0,5), red=c(0,5), nir=c(0,5), swir1=c(0,5), swir2=c(0,5)),
                                      yaxis.lims = list(green=c(-0.05,0.3), red=c(-0.05,0.4), nir=c(-0.05,0.6), swir1=c(-0.05,0.6), swir2=c(-0.05,0.4)),
                                      xaxis.breaks = list(green=seq(0,5,0.5), red=seq(0,5,0.5), nir=seq(0,5,0.5), swir1=seq(0,5,0.5), swir2=seq(0,5,0.5)), 
                                      yaxis.breaks = list(green=seq(0,0.3,0.05), red=seq(0,0.4,0.05), nir=seq(0,0.6,0.1), swir1=seq(0,0.6,0.1), swir2=seq(0,0.4,0.05)),
                                      text.xs = list(green=4, red=4, nir=4, swir1=4, swir2=4), 
                                      text.ys = list(green=0.8, red=0.8, nir=0.8, swir1=0.8, swir2=0.8),
                                      out.dir = out.dir.bs, title = title.bs, xlab = "LAI true", black.soil = TRUE)
  
  myplot_PARASvsLandsat_forvar_errbar_minmax(paras.res.Case, "ECC_my",                                      # ECC
                                      xaxis.lims = list(green=c(0,100), red=c(0,100), nir=c(0,100), swir1=c(0,100), swir2=c(0,100)),
                                      yaxis.lims = list(green=c(-0.05,0.3), red=c(-0.05,0.4), nir=c(-0.05,0.6), swir1=c(-0.05,0.6), swir2=c(-0.05,0.4)),
                                      xaxis.breaks = list(green=seq(0,100,20), red=seq(0,100,20), nir=seq(0,100,20), swir1=seq(0,100,20), swir2=seq(0,100,20)), 
                                      yaxis.breaks = list(green=seq(0,0.3,0.05), red=seq(0,0.4,0.05), nir=seq(0,0.6,0.1), swir1=seq(0,0.6,0.1), swir2=seq(0,0.4,0.05)),
                                      text.xs = list(green=4, red=4, nir=4, swir1=4, swir2=4), 
                                      text.ys = list(green=0.8, red=0.8, nir=0.8, swir1=0.8, swir2=0.8),
                                      out.dir = out.dir.bs, title = title.bs, xlab = "ECC (%)", black.soil = TRUE)
  
  return(1)
}       

# Run the function
print.paras.graphs.errbar(paras.res.Case2, "graphs/remove_poor_verypoor/PARAS/Case_2", "graphs/remove_poor_verypoor/PARAS/Case_2/bs", "Case 2", "Case 2 BS")

print.paras.graphs.errbar(paras.res.Case8, "graphs/PARAS/Case_8", "graphs/PARAS/Case_8/bs", "Case 8", "Case 8 BS")

print.paras.graphs.errbar.minmax(paras.res.Case8, "graphs/PARAS/Case_8_minmax", "graphs/PARAS/Case_8_minmax/bs", "Case 8 minmax", "Case 8 BS minmax")

print.paras.graphs.errbar2(paras.res.Case10a, "graphs/remove_poor_verypoor/PARAS/Case_10a", "graphs/remove_poor_verypoor/PARAS/Case_10a/bs", "Case 10a", "Case 10a BS")
print.paras.graphs.2errbar(paras.res.Case10a, "graphs/remove_poor_verypoor/PARAS/Case_10a_2errbar", "graphs/remove_poor_verypoor/PARAS/Case_10a_2errbar/bs", "Case 10a", "Case 10a BS")



print.paras.graphs.errbar(paras.res.Case10b, "graphs/remove_poor_verypoor/PARAS/Case_10b", "graphs/remove_poor_verypoor/PARAS/Case_10b/bs", "Case 10b", "Case 10b BS")



#######################################################################################################
# FOR MANUSCRIPT ----------------------------------------------------------
#######################################################################################################
source("tests/my_ggplot_manuscript.R")
source("tests/multiplot.R")

paras.res.FinalCase.sPrim <- read_csv2("results/PARAS/remove_poor_verypoor/FinalCase_lDipt_sPrim_mean_goodforest.csv")
paras.res.FinalCase.sLitt <- read_csv2("results/PARAS/remove_poor_verypoor/FinalCase_lDipt_sLitt_mean_goodforest.csv")

prosail.res.closCan.sDef <- read_csv2("results/PROSAIL/closCan_DiptMean_defRsoil.csv")
prosail.res.closCan.sDef.LAIe <- read_csv2("results/PROSAIL/closCan_DiptMean_defRsoil_LAIe.csv")
prosail.res.closCan.sDef.hspot025.ALA45 <- read_csv2("results/PROSAIL/closCan_DiptMean_defRsoil_hspot025_ALA45.csv")
prosail.res.closCan.sDef.ALA45 <- read_csv2("results/PROSAIL/closCan_DiptMean_defRsoil_ALA45.csv")

source("munge/point_pch_col_forestType.R")

temp <- prosail.res.closCan.sDef.ALA45                      # temp ********************************************************

# Recode forest type
temp <- temp %>% 
  mutate(Type.new = dplyr::if_else(Type == "Salvage Logged" & ForestQuality %in% c("Very poor", "Poor"),
                                   "Salvage Logged stage 2", Type))

temp <- temp %>% 
  mutate(Type.new = dplyr::if_else(Type == "Salvage Logged" & !ForestQuality %in% c("Very poor", "Poor"),
                                   "Salvage Logged stage 1", Type.new))
# Level the forest type
temp <- temp %>% mutate(Type.new = factor(Type.new, levels = c("Primary", "Lightly Logged", "Twice Logged", "Salvage Logged stage 1")))

temp <- as.data.frame(temp)   

# Plot forest variable vs Landsat & PARAS by forest type ------------------
source("tests/my_ggplot_manuscript.R")
tempplot <- myplot_PARASvsLandsat_forvar_ms(temp, 
                                            forest.var = "LAIt_CIcc_my", 
                                            shape.col = c("dark orange", "dark blue"), 
                                            shape.pch = pts.pchs[1:4], 
                                            xlab = expression(bold(LAI[true])))
                            

# pdf("graphs/FINAL_for_manuscript/PARASvsETM_LAIt_sPrim.pdf", width = 5.5, height = 5, pointsize = 10)
# pdf("graphs/FINAL_for_manuscript/PARASvsETM_LAIt_sLitt.pdf", width = 5, height = 5, pointsize = 10) 
# pdf("graphs/FINAL_for_manuscript/PARASvsETM_LAIt_sPrim_REV1.pdf", width = 5.5, height = 5, pointsize = 10)
# pdf("graphs/PROSAIL/prosail_closCan_DiptMean_defRsoil.pdf", width = 5.5, height = 5, pointsize = 10)
# pdf("graphs/PROSAIL/prosail_closCan_DiptMean_defRsoil_LAIe.pdf", width = 5.5, height = 5, pointsize = 10)
# pdf("graphs/PROSAIL/prosail_closCan_DiptMean_defRsoil_hspot025_ALA45.pdf", width = 5.5, height = 5, pointsize = 10)
pdf("graphs/PROSAIL/prosail_closCan_DiptMean_defRsoil_ALA45.pdf", width = 5.5, height = 5, pointsize = 10)

multiplot(tempplot$green, tempplot$nir, tempplot$red, tempplot$swir1, cols = 2)
dev.off()

# paras.res = temp                                                          # debug debug bugggg
# forest.var = "LAIt_CIcc_my"
# xaxis.lims <- list(green=c(0,5))
# xaxis.breaks <- list(green=seq(0,5,1))
# yaxis.lims <- list(green=c(0,1))
# yaxis.breaks <- list(green=seq(0,1,0.1))
# shape.col = c("magenta", "blue")
# shape.pch = pts.pchs
# xlab = expression(bold(LAI[true]))


######################################################################################################
# FOR MANUSCRIPT: Landsat vs PARAS with wL variability --------------------
######################################################################################################
# print.paras.graphs.errbar2
source("tests/paras_summary.R")

paras.res.CaseFinal.raw <- readRDS("results/PARAS/remove_poor_verypoor/FinalCase_lDiptSampling_sPrim_mean_goodforest.rds")

temp <- paras.summary(paras.res.CaseFinal.raw)                                                  # temp
temp <- as.data.frame(temp)

# source("tests/my_ggplot_manuscript.R")                                                          # source this if ggplot setting changed!
# tempplot <- myplot_PARASvsLandsat_forvar_errbar_minmax_ms(temp, "LAIt_CIcc_my", 
#                                               xlab = expression(bold(LAI[true])), 
#                                               shape.pch = pts.pchs[1:4], 
#                                               shape.col = c("dark orange", "dark blue"))

source("tests/my_ggplot_manuscript.R")       
tempplot <- myplot_PARASvsLandsat_forvar_errbar_1std_ms(temp, "LAIt_CIcc_my",                             # error bar = 1std BRF
                                                          xlab = expression(bold(LAI[true])), 
                                                          shape.pch = pts.pchs[1:4], 
                                                          shape.col = c("dark orange", "dark blue"),
                                                          pts.cex = 1.5)


# pdf("graphs/FINAL_for_manuscript/PARASvsETM_varywLLAIt_sPrim.pdf", width = 5.5, height = 5, pointsize = 10)
pdf("graphs/FINAL_for_manuscript/PARASvsETM_varywLLAIt_sPrim_REV1.pdf", width = 5.5, height = 5, pointsize = 10)
multiplot(tempplot$green, tempplot$nir, tempplot$red, tempplot$swir1, cols = 2)
dev.off()



