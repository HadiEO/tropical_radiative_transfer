# Plot sobol indices (first-order S and total ST) ====================================================================


# with PROSPECT inputs ----------------------------------------------------

# sobolSI <- readRDS("R_GSA/result/sobolSI_paras_varymaininput_1000.rds")
# sobolSI <- readRDS("results/sensitivity/sobolSI_paras_varymaininput_20000_par.rds")
sobolSI <- readRDS("results/sensitivity/sobolSI_paras_varymaininput_20000_HAWAII.rds")

varnames <-  c("LAIe", "CI", "DIFN", "i0", "cgf.view", "N", "Cab", "Cw", "Cm", "Rg.b1", "Rg.b2", "Rg.b3", "Rg.b4", "Rg.b5", "Rg.b7")


S <- sobolSI$Sfct; S <- as_tibble(S); colnames(S) <- varnames                          # Run from here
ST <- sobolSI$Tfct; ST <- as_tibble(ST); colnames(ST) <- varnames
ETM <- hsdar::get.sensor.characteristics("Landsat7"); wav <- (ETM$lb + ETM$ub) / 2


pdf("graphs/sensitivity/SI_varymain_20000_HAWAII.pdf")

par(mfrow = c(2,1), ps = 12, las = 1, mar=c(5.1, 4.1, 4.1, 12.1), xpd=FALSE)
# Si # =============================================================================================
plot(wav, S$LAIe, type = 'b', col = 'dark blue', ylim = c(-0.12, 1), xaxp = c(400,2400,10), lwd = 1,              # First-effect SI
     main = "First-order SI (n = 20,000)\ntropical forest plots (LAIe = 2.17-3.48)", xlab = "Wavelength (nm)", ylab = "Si")
# abline(h = 0, lty = 2, col = "grey20")
points(wav, S$CI, col = 'chartreuse4', lwd = 1, type = "b")
points(wav, S$DIFN, col = 'dark orange', lwd = 1, type = "b")
points(wav, S$i0, col = 'magenta', lwd = 1, type = "b")
points(wav, S$cgf.view, col = 'brown', lwd = 1, type = "b")

points(wav, S$N, col = 'dark blue', lwd = 1, lty = 2, type = "b", pch = 2)
points(wav, S$Cab, col = 'chartreuse4', lwd = 1, lty = 2, type = "b", pch = 2)
points(wav, S$Cw, col = 'dark orange', lwd = 1, lty = 2, type = "b", pch = 2)
points(wav, S$Cm, col = 'magenta', lwd = 1, lty = 2, type = "b", pch = 2)

points(wav, c(S$Rg.b1[1], S$Rg.b2[2], S$Rg.b3[3], S$Rg.b4[4], S$Rg.b5[5], S$Rg.b7[6]),   # Rg
       col = 'brown', lwd = 1, lty = 2, type = "b", pch = 2)            

cols <- c("dark blue", "chartreuse4", "dark orange", "magenta", "brown",
          "dark blue", "chartreuse4", "dark orange", "magenta", "brown")
legend("topright", c(varnames[1:9], "Rg"), col = cols, 
       lwd = rep(1,10), lty = c(rep(1,5), rep(2,5)), pch = c(rep(1,5), rep(2,5)), 
       ncol = 2, xpd = TRUE, inset = c(-0.65,0), bty = "n")

# STi # ==============================================================================================
plot(wav, ST$LAIe, type = 'b', col = 'dark blue', ylim = c(-0.12, 1), xaxp = c(400,2400,10), lwd = 1,              # First-effect STI
     main = "Total SI (n = 20,000)\ntropical forest plots (LAIe = 2.17-3.48)", xlab = "Wavelength (nm)", ylab = "STi")
abline(h = 0, lty = 2, col = "grey20")
points(wav, ST$CI, col = 'chartreuse4', lwd = 1, type = "b")
points(wav, ST$DIFN, col = 'dark orange', lwd = 1, type = "b")
points(wav, ST$i0, col = 'magenta', lwd = 1, type = "b")
points(wav, ST$cgf.view, col = 'brown', lwd = 1, type = "b")

points(wav, ST$N, col = 'dark blue', lwd = 1, lty = 2, type = "b", pch = 2)
points(wav, ST$Cab, col = 'chartreuse4', lwd = 1, lty = 2, type = "b", pch = 2)
points(wav, ST$Cw, col = 'dark orange', lwd = 1, lty = 2, type = "b", pch = 2)
points(wav, ST$Cm, col = 'magenta', lwd = 1, lty = 2, type = "b", pch = 2)

points(wav, c(ST$Rg.b1[1], ST$Rg.b2[2], ST$Rg.b3[3], ST$Rg.b4[4], ST$Rg.b5[5], ST$Rg.b7[6]),   # Rg
       col = 'brown', lwd = 1, lty = 2, type = "b", pch = 2)            

legend("topright", c(varnames[1:9], "Rg"), col = cols, 
       lwd = rep(1,10), lty = c(rep(1,5), rep(2,5)), pch = c(rep(1,5), rep(2,5)),
       ncol = 2, xpd = TRUE, inset = c(-0.65,0), bty = "n")

dev.off()



# with wL inputs ----------------------------------------------------------

# sobolSI <- readRDS("results/sensitivity/sobolSI_paras_varymaininput_20000_wL.rds")    # Switch the data here
# sobolSI <- readRDS("results/sensitivity/sobolSI_paras_varymaininput_20000_BOREAL.rds")  # Boreal
# sobolSI <- readRDS("results/sensitivity/sobolSI_paras_varymaininput_20000_BOREAL_limLAI.rds") # Boreal + restrict LAIe
# sobolSI <- readRDS("results/sensitivity/sobolSI_paras_varymaininput_20000_wL_Spruce4Diptero.rds")
# sobolSI <- readRDS("results/sensitivity/sobolSI_paras_varymaininput_20000_wL_rowwise_globcorr.rds")
# sobolSI <- readRDS("results/sensitivity/sobolSI_paras_varymaininput_20000_wL_rowwise_globcorr_1std.rds")
sobolSI <- readRDS("results/sensitivity/sobolSI_paras_varymaininput_20000_wL_lessinput.rds")


varnames <-  c("LAIe", "CI", "DIFN", "i0", "cgf.view", 
               "wL.b1", "wL.b2", "wL.b3", "wL.b4", "wL.b5", "wL.b7", 
               "Rg.b1", "Rg.b2", "Rg.b3", "Rg.b4", "Rg.b5", "Rg.b7")


S <- sobolSI$Sfct; S <- as_tibble(S); colnames(S) <- varnames                          # Run from here
ST <- sobolSI$Tfct; ST <- as_tibble(ST); colnames(ST) <- varnames
ETM <- hsdar::get.sensor.characteristics("Landsat7"); wav <- (ETM$lb + ETM$ub) / 2

# pdf("graphs/sensitivity/SI_varymain_20000_wL.pdf")
# pdf("graphs/sensitivity/SI_varymain_20000_wL_BOREAL.pdf")
# pdf("graphs/sensitivity/SI_varymain_20000_wL_BOREAL_limLAI.pdf")
# pdf("graphs/sensitivity/SI_varymain_20000_wL_Spruce4Diptero.pdf")
pdf("graphs/sensitivity/SI_varymain_20000_wL_rowwise_globcorr_1std.pdf")

par(mfrow = c(2,1), ps = 12, las = 1, mar=c(5.1, 4.1, 4.1, 12.1), xpd=FALSE)
# Si # =============================================================================================
plot(wav, S$LAIe, type = 'b', col = 'dark blue', ylim = c(-0.12, 1), xaxp = c(400,2400,10), lwd = 1,              # First-effect SI
     main = "First-order SI (n = 20,000)\ntropical forest plots (LAIe = 2.17-3.48), rowwise-globcorr-1std", xlab = "Wavelength (nm)", ylab = "Si")
# abline(h = 0, lty = 2, col = "grey20")
points(wav, S$CI, col = 'dodgerblue', lwd = 1, type = "b")
points(wav, S$DIFN, col = 'dark orange', lwd = 1, type = "b")
points(wav, S$i0, col = 'magenta', lwd = 1, type = "b")
points(wav, S$cgf.view, col = 'brown', lwd = 1, type = "b")

points(wav, c(S$wL.b1[1],  S$wL.b2[2], S$wL.b3[3], S$wL.b4[4], S$wL.b5[5], S$wL.b7[6]),           # wL
       col = 'chartreuse4', lwd = 1, lty = 2, type = "b", pch = 2)       

points(wav, c(S$Rg.b1[1], S$Rg.b2[2], S$Rg.b3[3], S$Rg.b4[4], S$Rg.b5[5], S$Rg.b7[6]),            # Rg
       col = 'grey40', lwd = 1, lty = 2, type = "b", pch = 2)            

cols <- c("dark blue", "dodgerblue", "dark orange", "magenta", "brown",
          "chartreuse4", "grey40")
legend("topright", c(varnames[1:5], "wL", "Rg"), col = cols, 
       lwd = rep(1,7), lty = c(rep(1, 5), 2, 2), pch = c(rep(1,5), 2, 2), 
       ncol = 2, xpd = TRUE, inset = c(-0.65,0), bty = "n")

# STi # ==============================================================================================
plot(wav, ST$LAIe, type = 'b', col = 'dark blue', ylim = c(-0.12, 1), xaxp = c(400,2400,10), lwd = 1,              # First-effect SI
     main = "Total SI (n = 20,000)\ntropical forest plots (LAIe = 2.17-3.48), rowwise-globcorr-1std", xlab = "Wavelength (nm)", ylab = "STi")
# abline(h = 0, lty = 2, col = "grey20")
points(wav, ST$CI, col = 'dodgerblue', lwd = 1, type = "b")
points(wav, ST$DIFN, col = 'dark orange', lwd = 1, type = "b")
points(wav, ST$i0, col = 'magenta', lwd = 1, type = "b")
points(wav, ST$cgf.view, col = 'brown', lwd = 1, type = "b")

points(wav, c(ST$wL.b1[1],  ST$wL.b2[2], ST$wL.b3[3], ST$wL.b4[4], ST$wL.b5[5], ST$wL.b7[6]),           # wL
       col = 'chartreuse4', lwd = 1, lty = 2, type = "b", pch = 2)       

points(wav, c(ST$Rg.b1[1], ST$Rg.b2[2], ST$Rg.b3[3], ST$Rg.b4[4], ST$Rg.b5[5], ST$Rg.b7[6]),            # Rg
       col = 'grey40', lwd = 1, lty = 2, type = "b", pch = 2)     

cols <- c("dark blue", "dodgerblue", "dark orange", "magenta", "brown",
         "chartreuse4", "grey40")
legend("topright", c(varnames[1:5], "wL", "Rg"), col = cols, 
       lwd = rep(1,7), lty = c(rep(1, 5), 2, 2), pch = c(rep(1,5), 2, 2), 
       ncol = 2, xpd = TRUE, inset = c(-0.65,0), bty = "n")

dev.off()




# with wL, less inputs ----------------------------------------------------                                 # FINAL

sobolSI <- readRDS("results/sensitivity/sobolSI_paras_varymaininput_20000_wL_lessinput_minmaxwL.rds")
# sobolSI <- readRDS("results/sensitivity/sobolSI_paras_varymaininput_20000_wL_lessinput_1sdwL.rds")


varnames <-  c("LAIe", "i0", "cgf.view", 
               "wL.b1", "wL.b2", "wL.b3", "wL.b4", "wL.b5", "wL.b7", 
               "Rg.b1", "Rg.b2", "Rg.b3", "Rg.b4", "Rg.b5", "Rg.b7")


S <- sobolSI$Sfct; S <- as_tibble(S); colnames(S) <- varnames                          # Run from here
ST <- sobolSI$Tfct; ST <- as_tibble(ST); colnames(ST) <- varnames
# ETM <- hsdar::get.sensor.characteristics("Landsat7"); wav <- (ETM$lb + ETM$ub) / 2
wav <- c(485, 570, 660, 840, 1650, 2220)

# pdf("graphs/sensitivity/SI_varymain_20000_wL_lessinput_1sdwL.pdf")
pdf("graphs/sensitivity/SI_varymain_20000_wL_lessinput_minmaxwL.pdf")

par(mfrow = c(2,1), ps = 12, las = 1, mar=c(5.1, 4.1, 4.1, 12.1), xpd=FALSE)
# Si # =============================================================================================
plot(wav, S$LAIe, type = 'b', col = 'dark blue', ylim = c(-0.12, 1), xaxp = c(400,2400,10), lwd = 1,              # First-effect SI
     main = "First-order SI (n = 20,000)\ntropical forest plots (LAIe = 2.17-3.48), HAWAII % range & globcor", xlab = "Wavelength (nm)", ylab = "Si")
# abline(h = 0, lty = 2, col = "grey20")
points(wav, S$i0, col = 'magenta', lwd = 1, type = "b")
points(wav, S$cgf.view, col = 'brown', lwd = 1, type = "b")

points(wav, c(S$wL.b1[1],  S$wL.b2[2], S$wL.b3[3], S$wL.b4[4], S$wL.b5[5], S$wL.b7[6]),           # wL
       col = 'chartreuse4', lwd = 1, lty = 2, type = "b", pch = 2)       

points(wav, c(S$Rg.b1[1], S$Rg.b2[2], S$Rg.b3[3], S$Rg.b4[4], S$Rg.b5[5], S$Rg.b7[6]),            # Rg
       col = 'grey40', lwd = 1, lty = 2, type = "b", pch = 2)            

cols <- c("dark blue", "magenta", "brown",
          "chartreuse4", "grey40")
legend("topright", c(varnames[1:3], "wL", "Rg"), col = cols, 
       lwd = rep(1,5), lty = c(rep(1, 3), 2, 2), pch = c(rep(1,3), 2, 2), 
       ncol = 2, xpd = TRUE, inset = c(-0.65,0), bty = "n")

# STi # ==============================================================================================
plot(wav, ST$LAIe, type = 'b', col = 'dark blue', ylim = c(-0.12, 1), xaxp = c(400,2400,10), lwd = 1,              # First-effect SI
     main = "Total SI (n = 20,000)\ntropical forest plots (LAIe = 2.17-3.48), HAWAII % range & globcor", xlab = "Wavelength (nm)", ylab = "STi")
# abline(h = 0, lty = 2, col = "grey20")
points(wav, ST$i0, col = 'magenta', lwd = 1, type = "b")
points(wav, ST$cgf.view, col = 'brown', lwd = 1, type = "b")

points(wav, c(ST$wL.b1[1],  ST$wL.b2[2], ST$wL.b3[3], ST$wL.b4[4], ST$wL.b5[5], ST$wL.b7[6]),           # wL
       col = 'chartreuse4', lwd = 1, lty = 2, type = "b", pch = 2)       

points(wav, c(ST$Rg.b1[1], ST$Rg.b2[2], ST$Rg.b3[3], ST$Rg.b4[4], ST$Rg.b5[5], ST$Rg.b7[6]),            # Rg
       col = 'grey40', lwd = 1, lty = 2, type = "b", pch = 2)     

cols <- c("dark blue", "magenta", "brown",
          "chartreuse4", "grey40")
legend("topright", c(varnames[1:3], "wL", "Rg"), col = cols, 
       lwd = rep(1,5), lty = c(rep(1, 3), 2, 2), pch = c(rep(1,3), 2, 2), 
       ncol = 2, xpd = TRUE, inset = c(-0.65,0), bty = "n")

dev.off()





# Plot SI of PROSPECT -----------------------------------------------------

sobolSI <- readRDS("results/sensitivity/prospect_20000_finalHawaiiMinMax_SI.rds")

varnames <-  c("N", "Cab", "Cw", "Cm")

S <- sobolSI$Sfct; S <- as_tibble(S); colnames(S) <- varnames                         
ST <- sobolSI$Tfct; ST <- as_tibble(ST); colnames(ST) <- varnames
wav <- c(485, 570, 660, 840, 1650, 2220)

pdf("graphs/sensitivity/SI_PROSPECT_20000_finalHawaiiMinMax.pdf")

par(mfrow = c(2,1), ps = 12, las = 1, mar=c(5.1, 4.1, 4.1, 12.1), xpd=FALSE)
# Si # =============================================================================================
plot(wav, S$N, type = 'b', col = 'dark blue', ylim = c(-0.12, 1), xaxp = c(400,2400,10), lwd = 1,              # First-effect SI
     main = "First-order SI (n = 20,000)\nDiptero. leaf with HAWAII range", xlab = "Wavelength (nm)", ylab = "Si")
points(wav, S$Cab, col = 'magenta', lwd = 1, type = "b")
points(wav, S$Cw, col = 'brown', lwd = 1, type = "b")
points(wav, S$Cm, col = 'chartreuse4', lwd = 1, type = "b")

cols <- c("dark blue", "magenta", "brown", "chartreuse4")
legend("topright", varnames, col = cols, 
       lwd = rep(1,4), lty = rep(1,4), pch = rep(1,4), 
       ncol = 1, xpd = TRUE, inset = c(-0.65,0), bty = "n")

# STi # ==============================================================================================
plot(wav, ST$N, type = 'b', col = 'dark blue', ylim = c(-0.12, 1), xaxp = c(400,2400,10), lwd = 1,              # First-effect SI
     main = "Total SI (n = 20,000)\n\nDiptero. leaf with HAWAII range", xlab = "Wavelength (nm)", ylab = "STi")
points(wav, ST$Cab, col = 'magenta', lwd = 1, type = "b")
points(wav, ST$Cw, col = 'brown', lwd = 1, type = "b")
points(wav, ST$Cm, col = 'chartreuse4', lwd = 1, type = "b")

cols <- c("dark blue", "magenta", "brown", "chartreuse4")
legend("topright", varnames, col = cols, 
       lwd = rep(1,4), lty = rep(1,4), pch = rep(1,4), 
       ncol = 1, xpd = TRUE, inset = c(-0.65,0), bty = "n")


dev.off()


# Plot SI of PROSPECT (no N) ----------------------------------------------

sobolSI <- readRDS("results/sensitivity/prospectfixN_20000_finalHawaiiMinMax_SI.rds")

varnames <-  c("Cab", "Cw", "Cm")

S <- sobolSI$Sfct; S <- as_tibble(S); colnames(S) <- varnames                         
ST <- sobolSI$Tfct; ST <- as_tibble(ST); colnames(ST) <- varnames
wav <- c(485, 570, 660, 840, 1650, 2220)

pdf("graphs/sensitivity/SI_PROSPECT_fixN_20000_finalHawaiiMinMax.pdf")

par(mfrow = c(2,1), ps = 12, las = 1, mar=c(5.1, 4.1, 4.1, 12.1), xpd=FALSE)
# Si # =============================================================================================
plot(wav, S$Cab, type = 'b', col = 'chartreuse4', ylim = c(-0.12, 1), xaxp = c(400,2400,10), lwd = 1,              # First-effect SI
     main = "First-order SI (n = 20,000)\nDiptero. leaf with HAWAII range", xlab = "Wavelength (nm)", ylab = "Si")
points(wav, S$Cw, col = 'magenta', lwd = 1, type = "b")
points(wav, S$Cm, col = 'brown', lwd = 1, type = "b")

cols <- c("chartreuse4", "magenta", "brown")
legend("topright", varnames, col = cols, 
       lwd = rep(1,3), lty = rep(1,3), pch = rep(1,3), 
       ncol = 1, xpd = TRUE, inset = c(-0.65,0), bty = "n")

# STi # ==============================================================================================
plot(wav, ST$Cab, type = 'b', col = 'chartreuse4', ylim = c(-0.12, 1), xaxp = c(400,2400,10), lwd = 1,              # Total SI
     main = "Total SI (n = 20,000)\nDiptero. leaf with HAWAII range", xlab = "Wavelength (nm)", ylab = "STi")
points(wav, ST$Cw, col = 'magenta', lwd = 1, type = "b")
points(wav, ST$Cm, col = 'brown', lwd = 1, type = "b")

cols <- c("chartreuse4", "magenta", "brown")
legend("topright", varnames, col = cols, 
       lwd = rep(1,3), lty = rep(1,3), pch = rep(1,3), 
       ncol = 1, xpd = TRUE, inset = c(-0.65,0), bty = "n")


dev.off()


# FOR MANUSCRIPT: with wL, less inputs ----------------------------------------------------                                 # FINAL
# Just show total SI, for (a) min-max wL; (b) 1 std wL. No blue
# 1.5 column, 1 row, legend on top


sobolSI.minmaxwL <- readRDS("results/sensitivity/sobolSI_paras_varymaininput_20000_wL_lessinput_minmaxwL.rds")
sobolSI.stdwL <- readRDS("results/sensitivity/sobolSI_paras_varymaininput_20000_wL_lessinput_1sdwL.rds")

sobolSI.minmaxwL.b2b3Asner <- readRDS("results/sensitivity/sobolSI_paras_varymaininput_20000_wL_lessinput_minmaxwL_b2b3Asner.rds")


varnames <-  c("LAIe", "i0", "cgf.view", 
               "wL.b1", "wL.b2", "wL.b3", "wL.b4", "wL.b5", "wL.b7", 
               "Rg.b1", "Rg.b2", "Rg.b3", "Rg.b4", "Rg.b5", "Rg.b7")

ST.minmaxwL <- sobolSI.minmaxwL$Tfct; ST.minmaxwL <- as_tibble(ST.minmaxwL); colnames(ST.minmaxwL) <- varnames
ST.stdwL <- sobolSI.stdwL$Tfct; ST.stdwL <- as_tibble(ST.stdwL); colnames(ST.stdwL) <- varnames
ST.minmaxwL.b2b3Asner <- sobolSI.minmaxwL.b2b3Asner$Tfct; ST.minmaxwL.b2b3Asner <- as_tibble(ST.minmaxwL.b2b3Asner); colnames(ST.minmaxwL.b2b3Asner) <- varnames

wav <- c(485, 570, 660, 840, 1650, 2220)

pdf("graphs/FINAL_for_manuscript/renamed/sobol_sensitivity_paras_noLine.pdf", width = 7, height = 3, pointsize = 10)
# pdf("graphs/FINAL_for_manuscript/sobol_sensitivity_paras_ok_minmax_b2b3Asner.pdf", width = 7, height = 3, pointsize = 10)


par(mfrow = c(1,2), oma = c(0, 0, 0, 0), mai = c(0.45, 0.35, 0.1, 0.1),
   ps = 10, mgp = c(1.7, 0.5, 0), mar =c(2.7, 2.5, 0.5, 0.5))

# ST.minmaxwL
plot(wav[-1], ST.minmaxwL$LAIe[-1], type = 'p', col = 'dodgerblue', ylim = c(0, 1), xaxp = c(400,2400,10), lwd = 1,              
     main = "", xlab = "Wavelength (nm)", ylab = "Contribution")
points(wav[-1], ST.minmaxwL$i0[-1], col = 'magenta', lwd = 1, type = "p")
points(wav[-1], ST.minmaxwL$cgf.view[-1], col = 'brown', lwd = 1, type = "p")
points(wav[-1], c(ST.minmaxwL$wL.b2[2], ST.minmaxwL$wL.b3[3], ST.minmaxwL$wL.b4[4], ST.minmaxwL$wL.b5[5], ST.minmaxwL$wL.b7[6]),           # wL
       col = 'chartreuse4', lwd = 1, lty = 2, type = "p", pch = 2)       
points(wav[-1], c(ST.minmaxwL$Rg.b2[2], ST.minmaxwL$Rg.b3[3], ST.minmaxwL$Rg.b4[4], ST.minmaxwL$Rg.b5[5], ST.minmaxwL$Rg.b7[6]),            # Rg
       col = 'grey40', lwd = 1, lty = 2, type = "p", pch = 2) 
text(2200, 1.0, "(a)", font = 2)

# ST.stdwL
plot(wav[-1], ST.stdwL$LAIe[-1], type = 'p', col = 'dodgerblue', ylim = c(0, 1), xaxp = c(400,2400,10), lwd = 1,
     main = "", xlab = "Wavelength (nm)", ylab = "Contribution")
points(wav[-1], ST.stdwL$i0[-1], col = 'magenta', lwd = 1, type = "p")
points(wav[-1], ST.stdwL$cgf.view[-1], col = 'brown', lwd = 1, type = "p")
points(wav[-1], c(ST.stdwL$wL.b2[2], ST.stdwL$wL.b3[3], ST.stdwL$wL.b4[4], ST.stdwL$wL.b5[5], ST.stdwL$wL.b7[6]),           # wL
       col = 'chartreuse4', lwd = 1, lty = 2, type = "p", pch = 2)
points(wav[-1], c(ST.stdwL$Rg.b2[2], ST.stdwL$Rg.b3[3], ST.stdwL$Rg.b4[4], ST.stdwL$Rg.b5[5], ST.stdwL$Rg.b7[6]),            # Rg
       col = 'grey40', lwd = 1, lty = 2, type = "p", pch = 2)
text(2200, 1.0, "(b)", font = 2)

# # ST.minmaxwL.b2b3Asner
# plot(wav[-1], ST.minmaxwL.b2b3Asner$LAIe[-1], type = 'p', col = 'dodgerblue', ylim = c(0, 1), xaxp = c(400,2400,10), lwd = 1,              
#      main = "", xlab = "Wavelength (nm)", ylab = "Contribution")
# points(wav[-1], ST.minmaxwL.b2b3Asner$i0[-1], col = 'magenta', lwd = 1, type = "p")
# points(wav[-1], ST.minmaxwL.b2b3Asner$cgf.view[-1], col = 'brown', lwd = 1, type = "p")
# points(wav[-1], c(ST.minmaxwL.b2b3Asner$wL.b2[2], ST.minmaxwL.b2b3Asner$wL.b3[3], ST.minmaxwL.b2b3Asner$wL.b4[4], ST.minmaxwL.b2b3Asner$wL.b5[5], ST.minmaxwL.b2b3Asner$wL.b7[6]),           # wL
#        col = 'chartreuse4', lwd = 1, lty = 2, type = "p", pch = 2)       
# points(wav[-1], c(ST.minmaxwL.b2b3Asner$Rg.b2[2], ST.minmaxwL.b2b3Asner$Rg.b3[3], ST.minmaxwL.b2b3Asner$Rg.b4[4], ST.minmaxwL.b2b3Asner$Rg.b5[5], ST.minmaxwL.b2b3Asner$Rg.b7[6]),            # Rg
#        col = 'grey40', lwd = 1, lty = 2, type = "p", pch = 2)     


par(oma = c(0,0,0,0), mar = c(0,0,0,0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
cols <- c("dodgerblue", "magenta", "brown",
          "chartreuse4", "grey40")
# legend("topleft", xpd = TRUE, legend = c(expression(LAI[eff]), expression(paste(i[0], " = 1 - ", cgf[sun], "  ")), expression(cgf[view]), "leaf albedo", "ground refl."), 
#        col = cols, pch = c(rep(1,3), 2, 2),
#         lty = c(rep(1, 3), 2, 2),  lwd = rep(1,5), ncol = 2, cex = 1,  inset = c(0.13,-0.001), bty = "n", x.intersp = 0.5)

legend("topleft", xpd = TRUE, legend = c(expression(LAI[eff]), expression(paste(i[0], " = 1 - ", cgf(theta[sun]), "  ")), expression(cgf(theta[view])), "leaf albedo", "ground refl."), 
       col = cols, pch = c(rep(1,3), 2, 2),
       # lty = c(rep(1, 3), 2, 2),  lwd = rep(1,5), 
       ncol = 2, cex = 1,  inset = c(0.13,-0.001), bty = "n", x.intersp = 0.5)


dev.off()


# Still for manuscript, x axis band name ----------------------------------

sobolSI.minmaxwL <- readRDS("results/sensitivity/sobolSI_paras_varymaininput_20000_wL_lessinput_minmaxwL.rds")
sobolSI.stdwL <- readRDS("results/sensitivity/sobolSI_paras_varymaininput_20000_wL_lessinput_1sdwL.rds")

varnames <-  c("LAIe", "i0", "cgf.view", 
               "wL.b1", "wL.b2", "wL.b3", "wL.b4", "wL.b5", "wL.b7", 
               "Rg.b1", "Rg.b2", "Rg.b3", "Rg.b4", "Rg.b5", "Rg.b7")

wav.label <- c("G", "R", "NIR", "SWIR1", "SWIR2")
wav <- 0:5

ST.minmaxwL <- sobolSI.minmaxwL$Tfct; ST.minmaxwL <- as_tibble(ST.minmaxwL); colnames(ST.minmaxwL) <- varnames
ST.stdwL <- sobolSI.stdwL$Tfct; ST.stdwL <- as_tibble(ST.stdwL); colnames(ST.stdwL) <- varnames
ST.minmaxwL.b2b3Asner <- sobolSI.minmaxwL.b2b3Asner$Tfct; ST.minmaxwL.b2b3Asner <- as_tibble(ST.minmaxwL.b2b3Asner); colnames(ST.minmaxwL.b2b3Asner) <- varnames

# Make into %
ST.minmaxwL <- ST.minmaxwL * 100
ST.stdwL <- ST.stdwL * 100


pdf("graphs/FINAL_for_manuscript/renamed/sobol_sensitivity_paras_noLine_xBandName.pdf", width = 7, height = 3, pointsize = 10)


par(mfrow = c(1,2), oma = c(0, 0, 0, 0), mai = c(0.45, 0.35, 0.1, 0.1),
    ps = 10, mgp = c(1.7, 0.5, 0), mar =c(2.7, 2.6, 0.5, 0.75))   # mar =c(2.7, 2.5, 0.5, 0.5)

# ST.minmaxwL
plot(wav[-1], ST.minmaxwL$LAIe[-1], type = 'p', col = 'dodgerblue', ylim = c(0, 100), xaxt = "n", lwd = 1,              
     main = "", xlab = "ETM+ band", ylab = "Contribution (%)")
points(wav[-1], ST.minmaxwL$i0[-1], col = 'dark orange', lwd = 1, type = "p")
points(wav[-1], ST.minmaxwL$cgf.view[-1], col = 'brown', lwd = 1, type = "p")
points(wav[-1], c(ST.minmaxwL$wL.b2[2], ST.minmaxwL$wL.b3[3], ST.minmaxwL$wL.b4[4], ST.minmaxwL$wL.b5[5], ST.minmaxwL$wL.b7[6]),           # wL
       col = 'chartreuse4', lwd = 1, lty = 2, type = "p", pch = 2)       
points(wav[-1], c(ST.minmaxwL$Rg.b2[2], ST.minmaxwL$Rg.b3[3], ST.minmaxwL$Rg.b4[4], ST.minmaxwL$Rg.b5[5], ST.minmaxwL$Rg.b7[6]),            # Rg
       col = 'grey40', lwd = 1, lty = 2, type = "p", pch = 2) 
axis(1, at = 1:5, labels = wav.label) 
text(5, 100, "(a)", font = 2)

# ST.stdwL
plot(wav[-1], ST.stdwL$LAIe[-1], type = 'p', col = 'dodgerblue', ylim = c(0, 100), xaxt = "n", lwd = 1,
     main = "", xlab = "ETM+ band", ylab = "Contribution (%)")
points(wav[-1], ST.stdwL$i0[-1], col = 'dark orange', lwd = 1, type = "p")
points(wav[-1], ST.stdwL$cgf.view[-1], col = 'brown', lwd = 1, type = "p")
points(wav[-1], c(ST.stdwL$wL.b2[2], ST.stdwL$wL.b3[3], ST.stdwL$wL.b4[4], ST.stdwL$wL.b5[5], ST.stdwL$wL.b7[6]),           # wL
       col = 'chartreuse4', lwd = 1, lty = 2, type = "p", pch = 2)
points(wav[-1], c(ST.stdwL$Rg.b2[2], ST.stdwL$Rg.b3[3], ST.stdwL$Rg.b4[4], ST.stdwL$Rg.b5[5], ST.stdwL$Rg.b7[6]),            # Rg
       col = 'grey40', lwd = 1, lty = 2, type = "p", pch = 2)
axis(1, at = 1:5, labels = wav.label) 
text(5, 100, "(b)", font = 2)


par(oma = c(0,0,0,0), mar = c(0,0,0,0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
cols <- c("dodgerblue", "dark orange", "brown",
          "chartreuse4", "grey40")

legend("topleft", xpd = TRUE, legend = c(expression(LAI[eff]), expression(paste(i[0], " = 1 - ", cgf(theta[sun]), "  ")), expression(cgf(theta[view])), "leaf albedo", "ground refl."), 
       col = cols, pch = c(rep(1,3), 2, 2),
       ncol = 2, cex = 1,  inset = c(0.13,-0.001), x.intersp = 0.5, bty = "n")  


dev.off()
