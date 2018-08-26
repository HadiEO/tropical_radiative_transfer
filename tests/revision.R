
paras.res.FinalCase.sPrim <- read_csv2("results/PARAS/remove_poor_verypoor/FinalCase_lDipt_sPrim_mean_goodforest.csv")

wL_DiptMean_L7 <- read_csv2("results/leaf/wL_DiptMean_L7.csv")

pw <- paras.res.FinalCase.sPrim$p * wL_DiptMean_L7$b4
summary(pw)   # 0.606 - 0.690



source("tests/paras_summary.R")

paras.res.CaseFinal.raw <- readRDS("results/PARAS/remove_poor_verypoor/FinalCase_lDiptSampling_sPrim_mean_goodforest.rds")

temp <- paras.summary(paras.res.CaseFinal.raw)                                                  # temp
temp <- as.data.frame(temp)

(temp$BRF_b4_s.sd/temp$BRF_b4_s.mean)*100




# In Samanta et al. (2012)


sobolSI.minmaxwL <- readRDS("results/sensitivity/sobolSI_paras_varymaininput_20000_wL_lessinput_minmaxwL.rds")
sobolSI.stdwL <- readRDS("results/sensitivity/sobolSI_paras_varymaininput_20000_wL_lessinput_1sdwL.rds")
names(sobolSI.minmaxwL)

varnames <-  c("LAIe", "i0", "cgf.view", 
               "wL.b1", "wL.b2", "wL.b3", "wL.b4", "wL.b5", "wL.b7", 
               "Rg.b1", "Rg.b2", "Rg.b3", "Rg.b4", "Rg.b5", "Rg.b7")

ST.minmaxwL <- sobolSI.minmaxwL$Tfct; ST.minmaxwL <- as_tibble(ST.minmaxwL); colnames(ST.minmaxwL) <- varnames
ST.stdwL <- sobolSI.stdwL$Tfct; ST.stdwL <- as_tibble(ST.stdwL); colnames(ST.stdwL) <- varnames

# 1st order SI
S.minmaxwL <- sobolSI.minmaxwL$Sfct; S.minmaxwL <- as_tibble(S.minmaxwL); colnames(S.minmaxwL) <- varnames
S.stdwL <- sobolSI.stdwL$Sfct; S.stdwL <- as_tibble(S.stdwL); colnames(S.stdwL) <- varnames
S.minmaxwL
