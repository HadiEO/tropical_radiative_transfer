require(tidyverse)
source("tests/accuracy_table.R")


# Case 4 ------------------------------------------------------------------
paras.res.Case4 <- read_csv2("results/PARAS/Case4_DiptHerb_DiptHerb.csv")
paras.res.Case4 <-  paras.res.Case4 %>%  mutate(ECC_my = (1 - Gaps1_my) * 100) # add ECC

Case4.acc <- accuracy_table(paras.res.Case4) # %>% write.csv2("results/PARAS/accuracy/Case_4/Case4_all.csv")

Case4.acc.OP <- paras.res.Case4 %>% dplyr::filter(Type == "Oil Palm") %>% accuracy_table # %>% write.csv2("results/PARAS/accuracy/Case_4/Case4_OP.csv")
Case4.acc.Light <- paras.res.Case4 %>% dplyr::filter(Type == "Lightly Logged") %>% accuracy_table # %>% write.csv2("results/PARAS/accuracy/Case_4/Case4_Light.csv")
Case4.acc.Primary <- paras.res.Case4 %>% dplyr::filter(Type == "Primary") %>% accuracy_table # %>% write.csv2("results/PARAS/accuracy/Case_4/Case4_Primary.csv")
Case4.acc.Salvage <- paras.res.Case4 %>% dplyr::filter(Type == "Salvage Logged") %>% accuracy_table # %>% write.csv2("results/PARAS/accuracy/Case_4/Case4_Salvage.csv")
Case4.acc.Twice <- paras.res.Case4 %>% dplyr::filter(Type == "Twice Logged") %>% accuracy_table # %>% write.csv2("results/PARAS/accuracy/Case_4/Case4_Twice.csv")


# Plot RMSE, rRMSE, Bias, SEPC, rSEPC
dat <- rbind(Case4.acc, Case4.acc.OP, Case4.acc.Salvage, Case4.acc.Twice, Case4.acc.Light, Case4.acc.Primary)
Type <- c("All (89)", "Oil Palm (13)", "Salvage Logged (49)", "Twice Logged (14)", "Lightly Logged (8)", "Primary (5)")
dat$Type <- factor(rep(Type, each = 5))
dat$band <- as.factor(dat$band)

pdf("results/PARAS/accuracy/Case_4/rRMSE.pdf")                                        # rRMSE
ggplot(data = dat, aes(x = band, y = rRMSE, fill = Type)) + 
  geom_bar(stat = "identity", position = position_dodge()) + ggtitle("Case 4")
dev.off()

pdf("results/PARAS/accuracy/Case_4/RMSE.pdf")                                        # RMSE
ggplot(data = dat, aes(x = band, y = RMSE, fill = Type)) + 
  geom_bar(stat = "identity", position = position_dodge()) + ggtitle("Case 4")
dev.off()

pdf("results/PARAS/accuracy/Case_4/Bias.pdf")                                        # Bias
ggplot(data = dat, aes(x = band, y = bias, fill = Type)) + 
  geom_bar(stat = "identity", position = position_dodge()) + ggtitle("Case 4")
dev.off()

pdf("results/PARAS/accuracy/Case_4/SEPC.pdf")                                        # SEPC
ggplot(data = dat, aes(x = band, y = SEPC, fill = Type)) + 
  geom_bar(stat = "identity", position = position_dodge()) + ggtitle("Case 4")
dev.off()

pdf("results/PARAS/accuracy/Case_4/rSEPC.pdf")                                        # rSEPC
ggplot(data = dat, aes(x = band, y = rSEPC, fill = Type)) + 
  geom_bar(stat = "identity", position = position_dodge()) + ggtitle("Case 4")
dev.off()




# Case 1 ------------------------------------------------------------------
paras.res.Case1 <- read_csv2("results/PARAS/remove_poor_verypoor/Case1_DiptPrim_ElaePalm_mean.csv")
paras.res.Case1 <-  paras.res.Case1 %>%  mutate(ECC_my = (1 - Gaps1_my) * 100) # add ECC

Case1.acc <- accuracy_table(paras.res.Case1)  
write.csv2(Case1.acc, "results/PARAS/remove_poor_verypoor/accuracy/Case_1/Case1_all.csv")

Case1.acc.OP <- paras.res.Case1 %>% dplyr::filter(Type == "Oil Palm") %>% accuracy_table
write.csv2(Case1.acc.OP, "results/PARAS/remove_poor_verypoor/accuracy/Case_1/Case1_OP.csv")

Case1.acc.Light <- paras.res.Case1 %>% dplyr::filter(Type == "Lightly Logged") %>% accuracy_table
write.csv2(Case1.acc.Light, "results/PARAS/remove_poor_verypoor/accuracy/Case_1/Case1_Light.csv")

Case1.acc.Primary <- paras.res.Case1 %>% dplyr::filter(Type == "Primary") %>% accuracy_table
write.csv2(Case1.acc.Primary, "results/PARAS/remove_poor_verypoor/accuracy/Case_1/Case1_Primary.csv")

Case1.acc.Salvage <- paras.res.Case1 %>% dplyr::filter(Type == "Salvage Logged") %>% accuracy_table
write.csv2(Case1.acc.Salvage, "results/PARAS/remove_poor_verypoor/accuracy/Case_1/Case1_Salvage.csv")

Case1.acc.Twice <- paras.res.Case1 %>% dplyr::filter(Type == "Twice Logged") %>% accuracy_table
write.csv2(Case1.acc.Twice, "results/PARAS/remove_poor_verypoor/accuracy/Case_1/Case1_Twice.csv")


# Plot RMSE, rRMSE, Bias, SEPC, rSEPC
dat <- rbind(Case1.acc, Case1.acc.OP, Case1.acc.Salvage, Case1.acc.Twice, Case1.acc.Light, Case1.acc.Primary)
Type <- c("All (65)", "Oil Palm (13)", "Salvage Logged (26)", "Twice Logged (13)", "Lightly Logged (8)", "Primary (5)")
dat$Type <- factor(rep(Type, each = 5))
dat$band <- as.factor(dat$band)

pdf("results/PARAS/remove_poor_verypoor/accuracy/Case_1/rRMSE.pdf")                                        # rRMSE
ggplot(data = dat, aes(x = band, y = rRMSE, fill = Type)) + 
  geom_bar(stat = "identity", position = position_dodge()) + ggtitle("Case 1")
dev.off()

pdf("results/PARAS/remove_poor_verypoor/accuracy/Case_1/RMSE.pdf")                                        # RMSE
ggplot(data = dat, aes(x = band, y = RMSE, fill = Type)) + 
  geom_bar(stat = "identity", position = position_dodge()) + ggtitle("Case 1")
dev.off()

pdf("results/PARAS/remove_poor_verypoor/accuracy/Case_1/Bias.pdf")                                        # Bias
ggplot(data = dat, aes(x = band, y = bias, fill = Type)) + 
  geom_bar(stat = "identity", position = position_dodge()) + ggtitle("Case 1")
dev.off()

pdf("results/PARAS/remove_poor_verypoor/accuracy/Case_1/SEPC.pdf")                                        # SEPC
ggplot(data = dat, aes(x = band, y = SEPC, fill = Type)) + 
  geom_bar(stat = "identity", position = position_dodge()) + ggtitle("Case 1")
dev.off()

pdf("results/PARAS/remove_poor_verypoor/accuracy/Case_1/rSEPC.pdf")                                        # rSEPC
ggplot(data = dat, aes(x = band, y = rSEPC, fill = Type)) + 
  geom_bar(stat = "identity", position = position_dodge()) + ggtitle("Case 1")
dev.off()



# Final case ------------------------------------------------------------------
paras.res.CaseFinal <- read_csv2("results/PARAS/remove_poor_verypoor/FinalCase_lDipt_sPrim_mean_goodforest.csv")
# paras.res.CaseFinal <-  paras.res.CaseFinal %>%  mutate(ECC_my = (1 - Gaps1_my) * 100) # add ECC
source("munge/point_pch_col_forestType.R")

temp <- paras.res.CaseFinal                      # temp ********************************************************

# Recode forest type
temp <- temp %>% 
  mutate(Type.new = dplyr::if_else(Type == "Salvage Logged" & ForestQuality %in% c("Very poor", "Poor"),
                                   "Salvage Logged stage 2", Type))

temp <- temp %>% 
  mutate(Type.new = dplyr::if_else(Type == "Salvage Logged" & !ForestQuality %in% c("Very poor", "Poor"),
                                   "Salvage Logged stage 1", Type.new))
# Level the forest type
temp <- temp %>% mutate(Type.new = factor(Type.new, levels = c("Primary", "Lightly Logged", "Twice Logged", "Salvage Logged stage 1")))


CaseFinal.acc <- accuracy_table(temp)  
write.csv2(CaseFinal.acc, "results/PARAS/remove_poor_verypoor/accuracy/Case_final/CaseFinal_all.csv")

CaseFinal.acc.Light <- temp %>% dplyr::filter(Type.new == "Lightly Logged") %>% accuracy_table
write.csv2(CaseFinal.acc.Light, "results/PARAS/remove_poor_verypoor/accuracy/Case_final/CaseFinal_Light.csv")

CaseFinal.acc.Primary <- temp %>% dplyr::filter(Type.new == "Primary") %>% accuracy_table
write.csv2(CaseFinal.acc.Primary, "results/PARAS/remove_poor_verypoor/accuracy/Case_final/CaseFinal_Primary.csv")

CaseFinal.acc.Salvage1 <- temp %>% dplyr::filter(Type.new == "Salvage Logged stage 1") %>% accuracy_table
write.csv2(CaseFinal.acc.Salvage1, "results/PARAS/remove_poor_verypoor/accuracy/Case_final/CaseFinal_Salvage1.csv")

CaseFinal.acc.Twice <- temp %>% dplyr::filter(Type.new == "Twice Logged") %>% accuracy_table
write.csv2(CaseFinal.acc.Twice, "results/PARAS/remove_poor_verypoor/accuracy/Case_final/CaseFinal_Twice.csv")


# Plot RMSE, rRMSE, Bias, SEPC, rSEPC
dat <- rbind(CaseFinal.acc, CaseFinal.acc.OP, CaseFinal.acc.Salvage, CaseFinal.acc.Twice, CaseFinal.acc.Light, CaseFinal.acc.Primary)
Type <- c("All (65)", "Oil Palm (13)", "Salvage Logged (26)", "Twice Logged (13)", "Lightly Logged (8)", "Primary (5)")
dat$Type <- factor(rep(Type, each = 5))
dat$band <- as.factor(dat$band)

pdf("results/PARAS/remove_poor_verypoor/accuracy/Case_final/rRMSE.pdf")                                        # rRMSE
ggplot(data = dat, aes(x = band, y = rRMSE, fill = Type)) + 
  geom_bar(stat = "identity", position = position_dodge()) + ggtitle("Case 1")
dev.off()

pdf("results/PARAS/remove_poor_verypoor/accuracy/Case_final/RMSE.pdf")                                        # RMSE
ggplot(data = dat, aes(x = band, y = RMSE, fill = Type)) + 
  geom_bar(stat = "identity", position = position_dodge()) + ggtitle("Case 1")
dev.off()

pdf("results/PARAS/remove_poor_verypoor/accuracy/Case_final/Bias.pdf")                                        # Bias
ggplot(data = dat, aes(x = band, y = bias, fill = Type)) + 
  geom_bar(stat = "identity", position = position_dodge()) + ggtitle("Case 1")
dev.off()

pdf("results/PARAS/remove_poor_verypoor/accuracy/Case_final/SEPC.pdf")                                        # SEPC
ggplot(data = dat, aes(x = band, y = SEPC, fill = Type)) + 
  geom_bar(stat = "identity", position = position_dodge()) + ggtitle("Case 1")
dev.off()

pdf("results/PARAS/remove_poor_verypoor/accuracy/Case_final/rSEPC.pdf")                                        # rSEPC
ggplot(data = dat, aes(x = band, y = rSEPC, fill = Type)) + 
  geom_bar(stat = "identity", position = position_dodge()) + ggtitle("Case 1")
dev.off()

