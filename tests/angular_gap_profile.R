gaps_spc_field_anc <- read_csv2("results/gaps_spc_field_anc.csv")

# Recode forest quality column --------------------------------------------
source("munge/recode_forestqual.R")

# Recode forest Type column as Type.new --------------------------------------------
source("munge/recode_forestType.R")


# Calculate shape functions as in AFM 2016 (2015) 1-3 ---------------------
# For each VZA (ring), shpfun = (-2*ln(Gaps_my)*cos(ring))/LAI_eff_my

gaps_spc_field_anc <- gaps_spc_field_anc %>% 
  mutate(shpfun_Gaps1 = (-2*log(Gaps1_my)*cos(10.7*pi/180))/LAI_my, # log computes ln, angles follow Lauri's script for DHP, differ from LAI-2000
         shpfun_Gaps2 = (-2*log(Gaps2_my)*cos(23.7*pi/180))/LAI_my,
         shpfun_Gaps3 = (-2*log(Gaps3_my)*cos(38.2*pi/180))/LAI_my,
         shpfun_Gaps4 = (-2*log(Gaps4_my)*cos(52.9*pi/180))/LAI_my,
         shpfun_Gaps5 = (-2*log(Gaps5_my)*cos(67.8*pi/180))/LAI_my
         )


# PLOT ANGULAR PROFILE (mean + sd) ----------------------------------------------------
Primary <- gaps_spc_field_anc %>% dplyr::filter(Type.new == "Primary")
LightlyLogged <- gaps_spc_field_anc %>% dplyr::filter(Type.new == "Lightly Logged")
SalvageLogged.A <- gaps_spc_field_anc %>% dplyr::filter(Type.new == "Salvage Logged stage 1")
SalvageLogged.B <- gaps_spc_field_anc %>% dplyr::filter(Type.new == "Salvage Logged stage 2")
TwiceLogged <- gaps_spc_field_anc %>% dplyr::filter(Type.new == "Twice Logged")
OilPalm <- gaps_spc_field_anc %>% dplyr::filter(Type.new == "Oil Palm")



# Function to plot angular profile
# Need to reshape the data frame
plot_angular_gaps <- function(data, text.x, text.y, mytext){
  data_cols <- as.data.frame(t(data[,c("Gaps1_my", "Gaps2_my", "Gaps3_my",
                                                                   "Gaps4_my", "Gaps5_my")]))
  colnames(data_cols) <- data$SampleID
  
  data_meansd <- data.frame(Gaps_mean=rowMeans(data_cols),
                                          Gaps_sd=apply(data_cols, 1, sd),
                                          Gaps_min=apply(data_cols, 1, min),
                                          Gaps_max=apply(data_cols, 1, max))
  data_meansd$name <- factor(x=c("Gaps1", "Gaps2", "Gaps3",
                                               "Gaps4", "Gaps5"), levels=c("Gaps1", "Gaps2", "Gaps3",
                                                                           "Gaps4", "Gaps5"))

  ring.center <- c(7, 23, 38, 53, 68)
  with(data_meansd, plot(ring.center, Gaps_mean, type='b', pch=19, xlab="View zenith angle (degrees)", ylab="Canopy gap fraction", ylim=c(0,1), xlim=c(0,80)))
  with(data_meansd, arrows(ring.center, Gaps_mean-Gaps_sd, ring.center, Gaps_mean+Gaps_sd, length=0.05, angle=90, code=3))
  points(ring.center, data_meansd$Gaps_min, pch=2)
  points(ring.center, data_meansd$Gaps_max, pch=6)
  text(text.x, text.y, mytext, cex=1.2)
}


# tiff("angular_gaps.tiff", res=1000, height=12, width=17, units="cm", compression="lzw", pointsize=8)

pdf("graphs/FINAL_for_manuscript/angular_gaps_types.pdf", width=7.4, height=5, pointsize=10)                    # [For manuscript] *************

par(oma = c(0,0,0,0), mai = c(0.45,0.35,0.1,0.1), 
    ps = 10, mgp = c(1.7, 0.5, 0), mfrow=c(2,3))                       # mgp = c(1.3, 0.5, 0)
plot_angular_gaps(Primary, 40, 0.95, mytext="Primary (N = 5)\nACC75 mean (std): 90.5% (0.7%)")
plot_angular_gaps(LightlyLogged, 40, 0.95, mytext="Lightly logged (N = 8)\nACC75: 90.9% (2.4%)")
plot_angular_gaps(TwiceLogged, 40, 0.95, mytext="Twice logged (N = 13)\nACC75: 90.9% (1.9%)")
plot_angular_gaps(SalvageLogged.A, 40, 0.95, mytext="Salvage logged stage 1 (N = 26)\nACC75: 88.0% (2.7%)")
plot_angular_gaps(SalvageLogged.B, 40, 0.95, mytext="Salvage logged stage 2 (N = 23)\nACC75: 79.9% (11.4%)")
plot_angular_gaps(OilPalm, 40, 0.95, mytext="Oil palm (N = 13)\nACC75: 60.3% (18.8%)")

dev.off()

# How many plots per forest type
gaps_spc_field_anc %>% group_by(Type.new) %>% summarize(count = n())

# Mean and sd clos75
gaps_spc_field_anc %>%  group_by(Type.new) %>% dplyr::select(ACC75_my) %>% 
  summarize(mean = round(mean(ACC75_my), 1), sd = round(sd(ACC75_my), 1))


# Plot angular profile of shape function ----------------------------------

plot_angular_shpfun <- function(data, title){
  data_cols <- as.data.frame(t(data[,c("shpfun_Gaps1", "shpfun_Gaps2", "shpfun_Gaps3",
                                       "shpfun_Gaps4", "shpfun_Gaps5")]))
  colnames(data_cols) <- data$SampleID
  
  data_meansd <- data.frame(shpfun_mean=rowMeans(data_cols),
                            shpfun_sd=apply(data_cols, 1, sd),
                            shpfun_min=apply(data_cols, 1, min),
                            shpfun_max=apply(data_cols, 1, max))
  data_meansd$name <- factor(x=c("Gaps1", "Gaps2", "Gaps3",
                                 "Gaps4", "Gaps5"), levels=c("Gaps1", "Gaps2", "Gaps3",
                                                             "Gaps4", "Gaps5"))
  
  with(data_meansd, plot(1:5, shpfun_mean, type='b', pch=19, xlab="Ring", ylab="Shape function", ylim=c(0,2), main=title))
  with(data_meansd, arrows(1:5, shpfun_mean-shpfun_sd, 1:5, shpfun_mean+shpfun_sd, length=0.05, angle=90, code=3))
  points(data_meansd$shpfun_min, pch=2)
  points(data_meansd$shpfun_max, pch=6)
}


pdf("graphs/remove_poor_verypoor/angular_shpfun_types.pdf")

par(oma = c(0,0,0,0), mai = c(0.3,0.3,0.3,0.2), 
    ps = 12, cex = 1, mgp = c(1.3, 0.5, 0), mfrow=c(2,3))
plot_angular_shpfun(Primary, title="Primary (N=5)\nClos75 90.5% (0.7%)")
plot_angular_shpfun(LightlyLogged, title="Lightly logged (N=8)\nClos75 90.9% (2.4%)")
plot_angular_shpfun(SalvageLogged, title="Salvage logged (N=26)\nClos75 88.0% (2.7%)")
plot_angular_shpfun(TwiceLogged, title="Twice logged (N=13)\nClos75 90.9% (1.9%)")
plot_angular_shpfun(OilPalm, title="Oil palm (N=13)\nClos75 60.3% (18.8%)")

dev.off()



# FOR MANUSCRIPT: Plot angular profile of shape function IN ONE FIGURE ----------------------------------

meansd_shpfun <- function(data){
  data_cols <- as.data.frame(t(data[,c("shpfun_Gaps1", "shpfun_Gaps2", "shpfun_Gaps3",
                                       "shpfun_Gaps4", "shpfun_Gaps5")]))
  colnames(data_cols) <- data$SampleID
  
  data_meansd <- data.frame(shpfun_mean=rowMeans(data_cols),
                            shpfun_sd=apply(data_cols, 1, sd),
                            shpfun_min=apply(data_cols, 1, min),
                            shpfun_max=apply(data_cols, 1, max))
  data_meansd$name <- factor(x=c("Gaps1", "Gaps2", "Gaps3",
                                 "Gaps4", "Gaps5"), levels=c("Gaps1", "Gaps2", "Gaps3",
                                                             "Gaps4", "Gaps5"))
  return(data_meansd)
}

shpfun.Primary <- meansd_shpfun(Primary)
shpfun.LightlyLogged <- meansd_shpfun(LightlyLogged)
shpfun.TwiceLogged <- meansd_shpfun(TwiceLogged)
shpfun.SalvageLogged.A <- meansd_shpfun(SalvageLogged.A)
shpfun.SalvageLogged.B <- meansd_shpfun(SalvageLogged.B)
shpfun.OilPalm <- meansd_shpfun(OilPalm)

ring.center <- c(7, 23, 38, 53, 68)
source("munge/point_pch_col_forestType.R")
# pts.col

pdf("graphs/FINAL_for_manuscript/angular_shapefun_types.pdf", width=3, height=3.5, pointsize=10)                    # [For manuscript] *************

par(oma = c(0,0,0,0), mai = c(0.45,0.35,0.1,0.1), 
    ps = 8, mgp = c(1.7, 0.5, 0), mar = c(2.7,2.5,4,1))                       # mgp = c(1.3, 0.5, 0), added mar for legend outside

with(shpfun.Primary, plot(ring.center, shpfun_mean, type='b', pch=pts.pchs[1], col=pts.cols[1], xlab="View zenith angle (degrees)", ylab="Shape function", ylim=c(0.6,1.4), yaxp=c(0.6,1.4,by=4), cex=0.8))
# with(shpfun.Primary, arrows(ring.center, shpfun_mean-shpfun_sd, ring.center, shpfun_mean+shpfun_sd, length=0.05, angle=90, code=3, xlim = c(0,80), col=pts.cols[1]))

with(shpfun.LightlyLogged, points(ring.center, shpfun_mean, type='b', pch=pts.pchs[2], col=pts.cols[2], xaxt='n', yaxt='n', cex=0.8))
# with(shpfun.LightlyLogged, arrows(ring.center, shpfun_mean-shpfun_sd, ring.center, shpfun_mean+shpfun_sd, length=0.05, angle=90, code=3, col=pts.cols[2]))

with(shpfun.TwiceLogged, points(ring.center, shpfun_mean, type='b', pch=pts.pchs[3], col=pts.cols[3], xaxt='n', yaxt='n', cex=0.8))
# with(shpfun.TwiceLogged, arrows(ring.center, shpfun_mean-shpfun_sd, ring.center, shpfun_mean+shpfun_sd, length=0.05, angle=90, code=3, col=pts.cols[3]))

with(shpfun.SalvageLogged.A, points(ring.center, shpfun_mean, type='b', pch=pts.pchs[4], col=pts.cols[4], xaxt='n', yaxt='n', cex=0.8))
# with(shpfun.SalvageLogged.A, arrows(ring.center, shpfun_mean-shpfun_sd, ring.center, shpfun_mean+shpfun_sd, length=0.05, angle=90, code=3, col=pts.cols[4]))

with(shpfun.SalvageLogged.B, points(ring.center, shpfun_mean, type='b', pch=pts.pchs[5], col=pts.cols[5], xaxt='n', yaxt='n', cex=0.8))
# with(shpfun.SalvageLogged.B, arrows(ring.center, shpfun_mean-shpfun_sd, ring.center, shpfun_mean+shpfun_sd, length=0.05, angle=90, code=3, col=pts.cols[5]))

with(shpfun.OilPalm, points(ring.center, shpfun_mean, type='b', pch=pts.pchs[6], col=pts.cols[6], xaxt='n', yaxt='n', cex=0.8))
# with(shpfun.OilPalm, arrows(ring.center, shpfun_mean-shpfun_sd, ring.center, shpfun_mean+shpfun_sd, length=0.05, angle=90, code=3, col=pts.cols[6]))

par(oma = c(0,0,0,0), mar = c(0,0,0,0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("top", xpd = TRUE, legend=c("Primary", "Lightly Logged", "Twice Logged", "Salvage Logged stage 1", "Salvage Logged stage 2", "Oil Palm"),
       col=pts.cols, pch=pts.pchs, lty=rep(1,6), ncol=2, cex=0.9, inset = c(0,-0.001), bty="n", x.intersp=0.5)

dev.off()




###############################################################################################
# Below is for sanity check of the gaps, not tested yet

# # PLOT ANGULAR PROFILE OF ALL PLOTS ---------------------------------------
# source("C:/Users/hadi1/Dropbox/PhD_Hadi_Aalto/Scripts/repcol_row_function.R")
# plot_angular_gaps_allplots <- function(data, title){
#   data_cols <- as.data.frame(t(data[,c("Gaps1_my", "Gaps2_my", "Gaps3_my",
#                                        "Gaps4_my", "Gaps5_my")]))
#   colnames(data_cols) <- data$SampleID
#   
#   with(data_cols, matplot(rep.col(1:5, ncol(data_cols)), data_cols, type='b', pch=19, xlab="Ring", ylab="Gaps", ylim=c(0,1), main=title))
#   
# }
# 
# Plot separately for each field plot
data <- gaps_spc_field_anc
data_cols <- as.data.frame(t(data[,c("Gaps1_my", "Gaps2_my", "Gaps3_my",
                                     "Gaps4_my", "Gaps5_my")]))
colnames(data_cols) <- data$SampleID


# tiff("angular_gaps_plots61to65.tiff", res=1000, height=12, width=17, units="cm", compression="lzw", pointsize=8)
pdf("graphs/angular_gaps_plots81to89.pdf")
par(oma = c(0,0,0,0), mai = c(0.3,0.3,0.2,0),
    ps = 7, cex = 1, mgp = c(1.3, 0.5, 0), mfrow=c(4,5))
for(i in 81:89){
  plot(1:5, data_cols[,i], type='b', pch=19, xlab="Ring", ylab="Gaps", ylim=c(0,1), main=colnames(data_cols)[i])
}

dev.off()


