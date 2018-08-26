# Copy updated script from Z


#########################################################################################################
# Case (1) dependent variables (Shapley effect) ----------------------------------------------
########################################################################################################## 
# PURPOSE: to restrict the input parameter space based on their inter-correlation
# also, to quantify sensitivity to intermediate inputs: p, q, Q, wC
# Alternative, <========================================================================================= PCC
# from Case (2) sobol samples, calculate Partial Correlation Coefficients (sensitivity::pcc),
# <== so check relation BRF vs (p, q, Q, wC)
sobolSI.paras.varymaininput <- readRDS("results/sensitivity/sobolSI_paras_varymaininput_20000_par.rds")
y_interm_varymaininput <- readRDS("results/sensitivity/y_interm_varymaininput_20000.rds")
y_interm_varymaininput <- as_tibble(y_interm_varymaininput)


design.mat <- as_data_frame(sobolSI.paras.varymaininput$X)
# Add intermediate inputs (LAIt, p, q, wL.bx, wC.bx, Q ?
design.mat.interm <- list( b1 = mutate(design.mat, y_interm_varymaininput))

# pccr.paras.varymaininput <- pcc(X = as_data_frame(sobolSI.paras.varymaininput$X), y = sobolSI.paras.varymaininput$y,
#                                rank = TRUE)
# barplot(abs(pccr.paras.varymaininput$PRCC$original), names.arg = rownames(pccr.paras.varymaininput$PRCC), 
#         xlab = "PROSPECT-PARAS inputs", ylab = "|PRCC|", ylim = c(0,1), 
#         main = "Partial rank correlation coefficient\nPROSPECT-PARAS (340,000 model evaluations)")
# # Note: no major difference between PCC and PRCC

pcc.paras.varymaininput <- list( BRF.b1 = pcc(X = design.mat, y = y_interm_varymaininput$BRF.b1,
                                         rank = FALSE),
                                 BRF.b2 = pcc(X = design.mat, y = y_interm_varymaininput$BRF.b2,
                                             rank = FALSE),
                                 BRF.b3 = pcc(X = design.mat, y = y_interm_varymaininput$BRF.b3,
                                             rank = FALSE),
                                 BRF.b4 = pcc(X = design.mat, y = y_interm_varymaininput$BRF.b4,
                                             rank = FALSE),
                                 BRF.b5 = pcc(X = design.mat, y = y_interm_varymaininput$BRF.b5,
                                             rank = FALSE),
                                 BRF.b7 = pcc(X = design.mat, y = y_interm_varymaininput$BRF.b7,
                                             rank = FALSE) )

pccjust.paras.varymaininput <- llply(pcc.paras.varymaininput, function(x) x$PCC) %>% 
  llply(function(x) cbind(x, var = rownames(x))) %>% llply(as_tibble)

# Keep respective band for Rg  
pccjust.paras.varymaininput$BRF.b1 <- pccjust.paras.varymaininput$BRF.b1 %>% 
  dplyr::filter(! var %in% c("Rg.b2", "Rg.b3", "Rg.b4", "Rg.b5", "Rg.b7"))

pccjust.paras.varymaininput$BRF.b2 <- pccjust.paras.varymaininput$BRF.b2 %>% 
  dplyr::filter(! var %in% c("Rg.b1", "Rg.b3", "Rg.b4", "Rg.b5", "Rg.b7"))

pccjust.paras.varymaininput$BRF.b3 <- pccjust.paras.varymaininput$BRF.b3 %>% 
  dplyr::filter(! var %in% c("Rg.b1", "Rg.b2", "Rg.b4", "Rg.b5", "Rg.b7"))

pccjust.paras.varymaininput$BRF.b4 <- pccjust.paras.varymaininput$BRF.b4 %>% 
  dplyr::filter(! var %in% c("Rg.b1", "Rg.b2", "Rg.b3", "Rg.b5", "Rg.b7"))

pccjust.paras.varymaininput$BRF.b5 <- pccjust.paras.varymaininput$BRF.b5 %>% 
  dplyr::filter(! var %in% c("Rg.b1", "Rg.b2", "Rg.b3", "Rg.b4", "Rg.b7"))

pccjust.paras.varymaininput$BRF.b7 <- pccjust.paras.varymaininput$BRF.b7 %>% 
  dplyr::filter(! var %in% c("Rg.b1", "Rg.b2", "Rg.b3", "Rg.b4", "Rg.b5"))

saveRDS(pccjust.paras.varymaininput, "results/sensitivity/PCC_340000.rds")

pcc.varnames <- c("LAIe", "CI", "DIFN", "i0", "cgf.view", "N", "Cab", "Cw", "Cm", "Rg")
  
pdf("graphs/sensitivity/sobol_PCC_forest_340000.pdf")
par(ps = 12, mfrow = c(3,2))
barplot(abs(pccjust.paras.varymaininput$BRF.b1$original), names.arg = pcc.varnames,
        ylab = "|PCC|", ylim = c(0,1), main = "B1", las = 2); 
barplot(abs(pccjust.paras.varymaininput$BRF.b2$original), names.arg = pcc.varnames,
        ylab = "|PCC|", ylim = c(0,1), main = "B2", las = 2)
barplot(abs(pccjust.paras.varymaininput$BRF.b3$original), names.arg = pcc.varnames,
        ylab = "|PCC|", ylim = c(0,1), main = "B3", las = 2)
barplot(abs(pccjust.paras.varymaininput$BRF.b4$original), names.arg = pcc.varnames,
        ylab = "|PCC|", ylim = c(0,1), main = "B4", las = 2)
barplot(abs(pccjust.paras.varymaininput$BRF.b5$original), names.arg = pcc.varnames,
        ylab = "|PCC|", ylim = c(0,1), main = "B5", las = 2)
barplot(abs(pccjust.paras.varymaininput$BRF.b7$original), names.arg = pcc.varnames,
        ylab = "|PCC|", ylim = c(0,1), main = "B7", las = 2)
dev.off()

# Caution: PCC measures seem not "uniquely" representing one input, for example N & Rg has quite large contribution !

# Back to Shapley <=========================================================================== back to Shapley
# go to tests/prepare_data_PARAS_sensitivity_shapley.R

paras.shapley <- function(cgf.view, Rg, i0, Q, wC) (cgf.view * Rg * (1 - i0)) + (Q * i0 * wC)

x <- shapleyPermEx(model = modlin, Xall=Xall, Xset=Xset, d=d, Nv=1e4, No = 1e3, Ni = 3)
print(x)
plot(x)







