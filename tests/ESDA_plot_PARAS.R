require(ggplot2)
require(tidyverse)

# Call the figure plotting functions --------------------------------------
source("tests/my_ggplot.R")
# Note: later when adjusting the figure size, font sizes need to be adjusted

# Load PARAS simulation results -------------------------------------------
paras.res <- read_csv2("results/PARAS/remove_poor_verypoor/Case1_DiptPrim_ElaePalm_mean.csv")                  # Separate leaf albedo for Diptero and OP
paras.res <-  paras.res %>%  mutate(ECC_my = (1 - Gaps1_my) * 100) # add ECC

paras.res.Case4 <- read_csv2("results/PARAS/remove_poor_verypoor/Case4_DiptHerb_DiptHerb.csv")             # Same leaf albedo
paras.res.Case4 <-  paras.res.Case4 %>%  mutate(ECC_my = (1 - Gaps1_my) * 100) # add ECC

paras.res.Case20 <- read_csv2("results/PARAS/remove_poor_verypoor/Case20_newestq_wLDipt_soilinterc.csv")             # Same leaf albedo
paras.res.Case20 <-  paras.res.Case20 %>%  mutate(ECC_my = (1 - Gaps1_my) * 100) # add ECC

paras.res.Case22 <- read_csv2("results/PARAS/remove_poor_verypoor/Case22_oldq_wLDipt_soilmeasPrimPalm.csv")
paras.res.Case22 <-  paras.res.Case22 %>%  mutate(ECC_my = (1 - Gaps1_my) * 100) # add ECC


# Check gap fractions derived variables -----------------------------------
# p vs ECC, p vs LAIeff, p vs LAIt
pdf("graphs/p_vs_ECC.pdf") # , height = 2.5, width = 3.5                           # p vs ECC
my_ggplot_lm(paras.res, "ECC_my", "p", "Type", ngroup = 5, pts = 2,
             "ECC (%)", "p", xaxis.lim = c(0,100), yaxis.lim = c(0,1), 
             xaxis.break = seq(0,100,by=10), yaxis.break = seq(0,1,by=0.1), 
             text.x = 10, text.y = 0.2, 
             shape.col = c(2:4,6,8,9), legend.pos = c(0.85,0.15))
dev.off()


pdf("graphs/p_vs_LAIt.pdf") # , height = 2.5, width = 3.5                          # p vs LAIt
my_ggplot_lm(paras.res, "LAIt_CIcc_my", "p", "Type", ngroup = 5, pts = 2,
             "LAI true", "p", xaxis.lim = c(0,5), yaxis.lim = c(0,1), 
             xaxis.break = seq(0,5,by=0.5), yaxis.break = seq(0,1,by=0.1), 
             text.x = 0.5, text.y = 0.2, 
             shape.col = c(2:4,6,8,9), legend.pos = c(0.85,0.15))
dev.off()


pdf("graphs/p_vs_LAIe.pdf") # , height = 2.5, width = 3.5                          # p vs LAI eff
my_ggplot_lm(paras.res, "LAI_my", "p", "Type", ngroup = 5, pts = 2,
             "LAI eff", "p", xaxis.lim = c(0,5), yaxis.lim = c(0,1), 
             xaxis.break = seq(0,5,by=0.5), yaxis.break = seq(0,1,by=0.1), 
             text.x = 0.5, text.y = 0.2, 
             shape.col = c(2:4,6,8,9), legend.pos = c(0.85,0.15))
dev.off()

pdf("graphs/DIFN_vs_LAIe.pdf") # , height = 2.5, width = 3.5                          # DIFN vs LAI eff
my_ggplot_lm(paras.res, "LAI_my", "DIFN", "Type", ngroup = 5, pts = 2,
             "LAI eff", "DIFN", xaxis.lim = c(0,5), yaxis.lim = c(0,1), 
             xaxis.break = seq(0,5,by=0.5), yaxis.break = seq(0,1,by=0.1), 
             text.x = 0.5, text.y = 0.2, 
             shape.col = c(2:4,6,8,9), legend.pos = c(0.85,0.9))
dev.off()

pdf("graphs/DIFN_vs_LAIt.pdf") # , height = 2.5, width = 3.5                          # DIFN vs LAI true
my_ggplot_lm(paras.res, "LAIt_CIcc_my", "DIFN", "Type", ngroup = 5, pts = 2,
             "LAI true", "DIFN", xaxis.lim = c(0,5), yaxis.lim = c(0,1), 
             xaxis.break = seq(0,5,by=0.5), yaxis.break = seq(0,1,by=0.1), 
             text.x = 0.5, text.y = 0.2, 
             shape.col = c(2:4,6,8,9), legend.pos = c(0.85,0.9))
dev.off()

scale01 <- function(x) out <- (x - min(x)) / (max(x) - min(x))

scaled <- paras.res %>% mutate(scaled.LAIt = scale01(LAIt_CIcc_my), 
                                 scaled.DIFN = scale01(DIFN))
pdf("graphs/DIFN_vs_LAIt_scaled.pdf") # , height = 2.5, width = 3.5              # scaled DIFN vs LAI true
my_ggplot_lm(scaled, "scaled.LAIt", "scaled.DIFN", "Type", ngroup = 5, pts = 2,
             "LAI true (scaled)", "DIFN (scaled)", xaxis.lim = c(-0.1,1.1), yaxis.lim = c(-0.1,1.1), 
             xaxis.break = seq(-0.1,1.1,by=0.1), yaxis.break = seq(-0.1,1.1,by=0.1), 
             text.x = 0.2, text.y = 0.8, 
             shape.col = c(2:4,6,8,9), legend.pos = c(0.85,0.9)) + geom_abline(slope = -1, intercept = 1)
dev.off()




pdf("graphs/CIcc_vs_LAIt.pdf") # , height = 2.5, width = 3.5                          # clumping vs LAI true
my_ggplot_lm(paras.res, "LAIt_CIcc_my", "CIcc_my", "Type", ngroup = 5, pts = 2,
             "LAI true", "CI_cc", xaxis.lim = c(0,5), yaxis.lim = c(0,1), 
             xaxis.break = seq(0,5,by=0.5), yaxis.break = seq(0,1,by=0.1), 
             text.x = 0.5, text.y = 0.2, 
             shape.col = c(2:4,6,8,9), legend.pos = c(0.85,0.2))
dev.off()


pdf("graphs/CIcc_vs_LAIe.pdf") # , height = 2.5, width = 3.5                          # clumping vs LAI eff
my_ggplot_lm(paras.res, "LAI_my", "CIcc_my", "Type", ngroup = 5, pts = 2,
             "LAI eff", "CI_cc", xaxis.lim = c(0,5), yaxis.lim = c(0,1), 
             xaxis.break = seq(0,5,by=0.5), yaxis.break = seq(0,1,by=0.1), 
             text.x = 0.5, text.y = 0.2, 
             shape.col = c(2:4,6,8,9), legend.pos = c(0.85,0.2))
dev.off()




# LAIe_uk vs p calculated with LAIt_uk ------------------------------------
# How CAN-EYE calculates LAIe using gap fractions 15-60 deg?

pdf("graphs/p_vs_LAIe_UK.pdf") # , height = 2.5, width = 3.5                          # p vs LAI eff
paras.res %>% mutate(p_uk = 1 - (iD / LAI_true_v6)) %>% 
  
  my_ggplot_lm("LAI_eff_v6", "p_uk", "Type", ngroup = 5, pts = 2,
             "LAI eff (UK)", "p (= 1 - iD_my/LAIt_uk)", xaxis.lim = c(0,5), yaxis.lim = c(0,1), 
             xaxis.break = seq(0,5,by=0.5), yaxis.break = seq(0,1,by=0.1), 
             text.x = 0.5, text.y = 0.2, 
             shape.col = c(2:4,6,8,9), legend.pos = c(0.85,0.15))
dev.off()


# DASF from DHP variables --------------------------------------------------------------------
# rho escape probability
pdf("graphs/remove_poor_verypoor/DASF_Qi0_Case1.pdf") # , height = 2.5, width = 3.5                          # DASF=Q*i0 vs (1-p)
paras.res %>% mutate(rho = 1 - p, abs.ln.rho = abs(log(rho)),
                     DASF = Q_b4 * i0_sun, abs.ln.DASF = abs(log(DASF))) %>% 
  my_ggplot_lm("abs.ln.DASF", "abs.ln.rho", "Type", ngroup = 5, pts = 2,
               "|ln(DASF=Qnir*i0)|", "|ln(1-p)|", xaxis.lim = c(0.4,2.4), yaxis.lim = c(1,1.62), 
               xaxis.break = seq(0.4,2.4,by=0.2), yaxis.break = seq(1,1.62,by=0.1), 
               text.x = 10, text.y = 10, 
               shape.col = c(2:4,6,8,9), legend.pos = c(0.85,0.15)) # + geom_text(aes(label = SampleID))
dev.off()


pdf("graphs/DASF_Qi0_vsLAIt_Case1.pdf") # , height = 2.5, width = 3.5                          # DASF=Q*i0 vs LAIt
paras.res.Case4 %>% mutate(rho = 1 - p, abs.ln.rho = abs(log(rho)),
                     DASF = Q_b4 * i0_sun, abs.ln.DASF = abs(log(DASF))) %>% 
  my_ggplot_lm("DASF", "LAIt_CIcc_my", "Type", ngroup = 5, pts = 2,
               "DASF = Qnir*i0", "LAI true", xaxis.lim = c(0,0.65), yaxis.lim = c(0,5), 
               xaxis.break = seq(0,0.65,by=0.1), yaxis.break = seq(0,5,by=0.5), 
               text.x = 0.05, text.y = 4.5, 
               shape.col = c(2:4,6,8,9), legend.pos = c(0.85,0.15)) # + geom_text(aes(label = SampleID))
dev.off()


DASF.Case1 = paras.res$Q_b4 * paras.res$i0_sun
DASF.Case4 = paras.res.Case4$Q_b4 * paras.res.Case4$i0_sun
windows()
plot(DASF.Case1, DASF.Case4); abline(0,1)



# DASF from simulated BRF -------------------------------------------------

ratio = paras.res$SR_B4_nir / wL_DiptMean_L7$b4
windows()
plot(paras.res$SR_B4_nir, ratio)


# LAIt vs i0 and cgf_nadir ------------------------------------------------
LAIt.vs.i0 <- my_ggplot_lm(paras.res, "LAIt_CIcc_my", "i0_sun", "Type", ngroup = 5, pts = 2,
                           "LAI true", "i0", xaxis.lim = c(0,5), yaxis.lim = c(0,1), 
                           xaxis.break = seq(0,5,by=1), yaxis.break = seq(0,1,by=0.1), 
                           text.x = 4, text.y = 0.1, 
                           shape.col = c(2:4,6,8,9), legend.pos = "none")

LAIt.vs.cgfnadir <- my_ggplot_lm(paras.res, "LAIt_CIcc_my", "cgf_view", "Type", ngroup = 5, pts = 2,
                           "LAI true", "cgf.nadir", xaxis.lim = c(0,5), yaxis.lim = c(0,1), 
                           xaxis.break = seq(0,5,by=1), yaxis.break = seq(0,1,by=0.1), 
                           text.x = 1, text.y = 0.2, 
                           shape.col = c(2:4,6,8,9), legend.pos = "none")

LAIt.vs.i0xcgfnadir <- paras.res %>% mutate(temp = i0_sun * cgf_view) %>% 
  my_ggplot_lm("LAIt_CIcc_my", "temp", "Type", ngroup = 5, pts = 2,
                           "LAI true", "i0 * cgf.nadir", xaxis.lim = c(0,5), yaxis.lim = c(0,1), 
                           xaxis.break = seq(0,5,by=1), yaxis.break = seq(0,1,by=0.1), 
                           text.x = 1, text.y = 0.8, 
                           shape.col = c(2:4,6,8,9), legend.pos = "none")

source("tests/multiplot.R")
pdf("graphs/remove_poor_verypoor/LAIt_i0_cgfnadir.pdf")  
multiplot(LAIt.vs.i0, LAIt.vs.cgfnadir, LAIt.vs.i0xcgfnadir, cols = 2)
dev.off()




# Q vs LAI ----------------------------------------------------------------
pdf("graphs/Q_LAIt_Case1.pdf")
paras.res %>%  dplyr::select(LAIt_CIcc_my, Q_b2, Q_b3, Q_b4, Q_b5, Q_b7, Type) %>% collect %>% 
  melt(., id.vars = c("LAIt_CIcc_my", "Type")) %>% collect %>%  
  my_ggplot_spl_nolabel(., xvar = "LAIt_CIcc_my", yvar = "value", shp.group = "Type", col.group = "variable", ngroup = 5, pts = 2, 
                "LAI true", "Q", xaxis.lim = c(0,5), yaxis.lim = c(0.5,0.8), 
                xaxis.break = seq(0,5,0.5), yaxis.break = seq(0,1,0.1),
                text.x = 4.5, text.y = 0.9,
                shape.col = c("magenta", "blue", "brown", "dark orange", "green"), legend.pos = "right")
dev.off()

pdf("graphs/Q_LAIt_Case4.pdf")
paras.res.Case4 %>%  dplyr::select(LAIt_CIcc_my, Q_b2, Q_b3, Q_b4, Q_b5, Q_b7, Type) %>% collect %>% 
  melt(., id.vars = c("LAIt_CIcc_my", "Type")) %>% collect %>%  
  my_ggplot_spl_nolabel(., xvar = "LAIt_CIcc_my", yvar = "value", shp.group = "Type", col.group = "variable", ngroup = 5, pts = 2, 
                        "LAI true", "Q", xaxis.lim = c(0,5), yaxis.lim = c(0.4,0.8), 
                        xaxis.break = seq(0,5,0.5), yaxis.break = seq(0,1,0.1),
                        text.x = 4.5, text.y = 0.9,
                        shape.col = c("magenta", "blue", "brown", "dark orange", "green"), legend.pos = "right")
dev.off()

pdf("graphs/Q_LAIt_Case20_newq.pdf")
paras.res.Case20 %>%  dplyr::select(LAIt_CIcc_my, Q_b2, Q_b3, Q_b4, Q_b5, Q_b7, Type) %>% collect %>% 
  melt(., id.vars = c("LAIt_CIcc_my", "Type")) %>% collect %>%  
  my_ggplot_spl_nolabel(., xvar = "LAIt_CIcc_my", yvar = "value", shp.group = "Type", col.group = "variable", ngroup = 5, pts = 2, 
                        "LAI true", "Q", xaxis.lim = c(0,5), yaxis.lim = c(0.4,0.8), 
                        xaxis.break = seq(0,5,0.5), yaxis.break = seq(0,1,0.1),
                        text.x = 4.5, text.y = 0.9,
                        shape.col = c("magenta", "blue", "brown", "dark orange", "green"), legend.pos = "right")
dev.off()

pdf("graphs/Q_oldq_vs_newq.pdf")                        # Q with old q vs new q
 
par(mfrow=c(2,2), ps = 16)
plot(paras.res.Case4$LAIt_CIcc_my, paras.res.Case4$Q_b2, xlim = c(0,5), ylim = c(0.4, 0.8), 
     col = "dark blue", xlab = "LAI true", ylab = "Q_b2 (green)")
points(paras.res.Case20$LAIt_CIcc_my, paras.res.Case20$Q_b2, col = "magenta")
legend("topleft", c("Old q", "New q"), pch = c(1,1), col = c("dark blue", "magenta"))

plot(paras.res.Case4$LAIt_CIcc_my, paras.res.Case4$Q_b4, xlim = c(0,5), ylim = c(0.4, 0.8), 
     col = "dark blue", xlab = "LAI true", ylab = "Q_b4 (nir)")
points(paras.res.Case20$LAIt_CIcc_my, paras.res.Case20$Q_b4, col = "magenta")
legend("topleft", c("Old q", "New q"), pch = c(1,1), col = c("dark blue", "magenta"))

plot(paras.res.Case4$LAIt_CIcc_my, paras.res.Case4$Q_b3, xlim = c(0,5), ylim = c(0.4, 0.8), 
     col = "dark blue", xlab = "LAI true", ylab = "Q_b3 (red)")
points(paras.res.Case20$LAIt_CIcc_my, paras.res.Case20$Q_b3, col = "magenta")
legend("topleft", c("Old q", "New q"), pch = c(1,1), col = c("dark blue", "magenta"))

plot(paras.res.Case4$LAIt_CIcc_my, paras.res.Case4$Q_b5, xlim = c(0,5), ylim = c(0.4, 0.8), 
     col = "dark blue", xlab = "LAI true", ylab = "Q_b5 (swir1)")
points(paras.res.Case20$LAIt_CIcc_my, paras.res.Case20$Q_b5, col = "magenta")
legend("topleft", c("Old q", "New q"), pch = c(1,1), col = c("dark blue", "magenta"))

dev.off()


# wC vs BRF ----------------------------------------------------------------
# Case 1 ###################################################################################################
pdf("graphs/wC_SR_Case1_green.pdf")                                                    # Green
my_ggplot_lm(paras.res, "wC_b2", "SR_B2_green", "Type", ngroup = 5, pts = 2,
             "wC green", "Landsat green", xaxis.lim = c(0,0.2), yaxis.lim = c(0,0.1), 
             xaxis.break = seq(0,0.2,by=0.05), yaxis.break = seq(0,0.1,by=0.02), 
             text.x = 0.15, text.y = 0.085, 
             shape.col = c(2:4,6,8,9), legend.pos = c(0.85,0.2))
dev.off()

pdf("graphs/wC_SR_Case1_red.pdf")                                                       # Red
my_ggplot_lm(paras.res, "wC_b3", "SR_B3_red", "Type", ngroup = 5, pts = 2,
             "wC red", "Landsat red", xaxis.lim = c(0,0.1), yaxis.lim = c(0,0.1), 
             xaxis.break = seq(0,0.1,by=0.02), yaxis.break = seq(0,0.1,by=0.02), 
             text.x = 0.07, text.y = 0.085, 
             shape.col = c(2:4,6,8,9), legend.pos = c(0.85,0.2))
dev.off()


pdf("graphs/wC_SR_Case1_nir.pdf")                                                       # NIR
my_ggplot_lm(paras.res, "wC_b4", "SR_B4_nir", "Type", ngroup = 5, pts = 2,
             "wC nir", "Landsat nir", xaxis.lim = c(0,0.8), yaxis.lim = c(0,0.6), 
             xaxis.break = seq(0,0.8,by=0.1), yaxis.break = seq(0,0.6,by=0.1), 
             text.x = 0.15, text.y = 0.5, 
             shape.col = c(2:4,6,8,9), legend.pos = c(0.2,0.2))
dev.off()


pdf("graphs/wC_SR_Case1_swir1.pdf")                                                       # Red
my_ggplot_lm(paras.res, "wC_b5", "SR_B5_swir1", "Type", ngroup = 5, pts = 2,
             "wC swir1", "Landsat swir1", xaxis.lim = c(0,0.35), yaxis.lim = c(0,0.3), 
             xaxis.break = seq(0,0.35,by=0.05), yaxis.break = seq(0,0.3,by=0.05), 
             text.x = 0.3, text.y = 0.25, 
             shape.col = c(2:4,6,8,9), legend.pos = c(0.2,0.2))
dev.off()

# Case 4 ###################################################################################################
pdf("graphs/wC_SR_Case4_green.pdf")                                                    # Green
my_ggplot_lm(paras.res.Case4, "wC_b2", "SR_B2_green", "Type", ngroup = 5, pts = 2,
             "wC green", "Landsat green", xaxis.lim = c(0,0.2), yaxis.lim = c(0,0.1), 
             xaxis.break = seq(0,0.2,by=0.05), yaxis.break = seq(0,0.1,by=0.02), 
             text.x = 0.15, text.y = 0.085, 
             shape.col = c(2:4,6,8,9), legend.pos = c(0.85,0.2))
dev.off()

pdf("graphs/wC_SR_Case4_red.pdf")                                                       # Red
my_ggplot_lm(paras.res.Case4, "wC_b3", "SR_B3_red", "Type", ngroup = 5, pts = 2,
             "wC red", "Landsat red", xaxis.lim = c(0,0.1), yaxis.lim = c(0,0.1), 
             xaxis.break = seq(0,0.1,by=0.02), yaxis.break = seq(0,0.1,by=0.02), 
             text.x = 0.07, text.y = 0.085, 
             shape.col = c(2:4,6,8,9), legend.pos = c(0.85,0.2))
dev.off()

pdf("graphs/wC_SR_Case4_nir.pdf")                                                       # NIR
my_ggplot_lm(paras.res.Case4, "wC_b4", "SR_B4_nir", "Type", ngroup = 5, pts = 2,
             "wC nir", "Landsat nir", xaxis.lim = c(0,0.8), yaxis.lim = c(0,0.6), 
             xaxis.break = seq(0,0.8,by=0.1), yaxis.break = seq(0,0.6,by=0.1), 
             text.x = 0.15, text.y = 0.5, 
             shape.col = c(2:4,6,8,9), legend.pos = c(0.2,0.2))
dev.off()


pdf("graphs/wC_SR_Case4_swir1.pdf")                                                       # Red
my_ggplot_lm(paras.res.Case4, "wC_b5", "SR_B5_swir1", "Type", ngroup = 5, pts = 2,
             "wC swir1", "Landsat swir1", xaxis.lim = c(0,0.35), yaxis.lim = c(0,0.3), 
             xaxis.break = seq(0,0.35,by=0.05), yaxis.break = seq(0,0.3,by=0.05), 
             text.x = 0.3, text.y = 0.25, 
             shape.col = c(2:4,6,8,9), legend.pos = c(0.2,0.2))
dev.off()



# Different q formulation -------------------------------------------------
temp <- paras.res
temp$q <- 1 - exp(-0.1684 * temp$LAIt_CIcc_my)
temp$new.q <- 1/2 * (1/temp$DIFN)

windows()
plot(temp$q, temp$new.q, xlab = "q = 1-exp(-0.1684*LAIt)", ylab = "q = 1/2*(1/DIFN)")



# BRF vs intermediate/derived inputs (p, q, Q, wC) ------------------------
temp <- paras.res.Case22 %>% dplyr::filter(Biome == "Forest")      # Just forest plots
temp %>%  dplyr::select(q, BRF_b2_s, BRF_b3_s, BRF_b4_s, BRF_b5_s, BRF_b7_s) %>%  cor() %>% round(3)
round(cor(temp$Q_b2, temp$BRF_b2_s), 3)

pdf("graphs/sensitivity/BRF_p_forest.pdf")                                                    # BRF vs p
temp %>%  dplyr::select(p, BRF_b2_s, BRF_b3_s, BRF_b4_s, BRF_b5_s, BRF_b7_s) %>% collect %>% 
  melt(., id.vars = "p") %>% collect %>%  
  my_ggplot_spl_nolabel(., xvar = "p", yvar = "value", shp.group = "variable", col.group = "variable", ngroup = 5, pts = 2, 
                        "p", "BRF", xaxis.lim = c(0.65,0.85), yaxis.lim = c(0,0.5), 
                        xaxis.break = seq(0.65,0.85,0.05), yaxis.break = seq(0,0.5,0.1),
                        text.x = 0.7, text.y = 0.45,
                        shape.col = c("magenta", "blue", "brown", "dark orange", "green"), legend.pos = "right") +
  geom_text(x = 0.75, y = 0.45, label = "r -0.363 (b2); -0.283 (b3); -0.269 (b4); -0.700 (b5); -0.482 (b7)", color = "black")
dev.off()


pdf("graphs/sensitivity/BRF_oldq_forest.pdf")                                                    # BRF vs q
temp %>%  dplyr::select(q, BRF_b2_s, BRF_b3_s, BRF_b4_s, BRF_b5_s, BRF_b7_s) %>% collect %>% 
  melt(., id.vars = "q") %>% collect %>%  
  my_ggplot_spl_nolabel(., xvar = "q", yvar = "value", shp.group = "variable", col.group = "variable", ngroup = 5, pts = 2, 
                        "Old q", "BRF", xaxis.lim = c(0.3,0.6), yaxis.lim = c(0,0.5), 
                        xaxis.break = seq(0.3,0.6,0.05), yaxis.break = seq(0,0.5,0.1),
                        text.x = 0.45, text.y = 0.45,
                        shape.col = c("magenta", "blue", "brown", "dark orange", "green"), legend.pos = "right") +
  geom_text(x = 0.45, y = 0.45, label = "r -0.590 (b2); -0.522 (b3); -0.059 (b4); -0.827 (b5); -0.685 (b7)", color = "black")
dev.off()

pdf("graphs/sensitivity/BRF_oldbigQ_forest.pdf")                                                    # BRF vs Q
par(ps = 12, mfrow = c(2,3))
plot(temp$Q_b2, temp$BRF_b2_s, xlab = "Q", ylab = expression(BRF[paras]), main = "b2 (r -0.592)")
plot(temp$Q_b3, temp$BRF_b3_s, xlab = "Q", ylab = expression(BRF[paras]), main = "b3 (r -0.523)")
plot(temp$Q_b4, temp$BRF_b4_s, xlab = "Q", ylab = expression(BRF[paras]), main = "b4 (r 0.057)")
plot(temp$Q_b5, temp$BRF_b5_s, xlab = "Q", ylab = expression(BRF[paras]), main = "b5 (r -0.841)")
plot(temp$Q_b7, temp$BRF_b7_s, xlab = "Q", ylab = expression(BRF[paras]), main = "b7 (r -0.691)")
dev.off()

