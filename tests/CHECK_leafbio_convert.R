
require(plyr)
require(dplyr)
require(tidyverse)
detach(package:plyr)


leafbio <- read_csv2("data/leaf/Angers2003_leafbio.csv")
# C_a, C_b, C_ab, C_car in ug/cm2
# EWT in g/cm2
# LMA in g/cm2


# Convert Cab (ug/cm2) to (mg/g)
leafbio <- leafbio %>% mutate(C_ab.permass = C_ab * 0.001 * 1/LMA,
                              C_a.permass = C_a * 0.001 * 1/LMA,
                              C_b.permass = C_b * 0.001 * 1/LMA)

leafbio %>% dplyr::filter(Species == "Sycamore maple") %>% dplyr::select(C_ab, C_ab.permass) %>% 
  apply(2, function(x) sd(x) / mean(x))
# C_ab        C_ab.permass 
# 0.6655427    0.5922964      # so rel-to-mean is closer to actual std compared to row-wise

leafbio %>% dplyr::filter(Species == "Sycamore maple") %>% dplyr::select(C_a, C_a.permass) %>% 
  apply(2, function(x) sd(x) / mean(x))

leafbio %>% dplyr::filter(Species == "Sycamore maple") %>% dplyr::select(C_b, C_b.permass) %>% 
  apply(2, function(x) sd(x) / mean(x))

# Convert Cm (g/cm2) to (g/m2)
leafbio <- leafbio %>% mutate(LMA.perm2 = LMA * 10000)

# Convert Cw (g/cm2) to (%)
leafbio <- leafbio %>% mutate(EWT.perc = 100 * (EWT/(EWT+LMA)))
                             

# ======================================================================================

# Check if std(C_ab) = std(C_ab.permass) / (0.001 * 1/std(LMA))
leafbio %>% dplyr::filter(Species == "Sycamore maple") %>% dplyr::select(C_ab) %>% apply(2, sd)
# C_ab 
# 21.68452  # per area

temp <- leafbio %>% dplyr::filter(Species == "Sycamore maple") %>% dplyr::select(C_ab.permass, LMA.perm2) 
with(temp, sd(C_ab.permass) * sd(LMA.perm2) * 0.1)
# 8.193084 # so, row-wise is wrong!



# Check if std(EWT) = std(EWT.perc)
temp <- leafbio %>% dplyr::filter(Species == "Sycamore maple") %>% dplyr::select(EWT, EWT.perc) 
apply(temp, 2, sd)
# EWT           EWT.perc 
# 0.002193989 4.721083094     # So row-wise is waayyy wrong!

# What about relative to mean
temp <- leafbio %>% dplyr::filter(Species == "Sycamore maple") %>% dplyr::select(EWT, EWT.perc) 
apply(temp, 2, function(x) sd(x)/mean(x))
# EWT         EWT.perc 
# 0.21741693 0.06484849 


# Check if correlation on per-area basis differs from on per-mass basis
leafbio %>% dplyr::filter(Species == "Sycamore maple") %>% 
  dplyr::select(N, C_ab, EWT, LMA) %>% cor                                        # per-area
#           N      C_ab       EWT       LMA
# N    1.0000000 0.4599590 0.5702190 0.5088809
# C_ab 0.4599590 1.0000000 0.4099583 0.3587253
# EWT  0.5702190 0.4099583 1.0000000 0.7972668
# LMA  0.5088809 0.3587253 0.7972668 1.0000000

leafbio %>% dplyr::filter(Species == "Sycamore maple") %>% 
  dplyr::select(N, C_ab.permass, EWT.perc, LMA.perm2) %>% cor                     # per-mass
#                 N         C_ab.permass   EWT.perc  LMA.perm2
# N             1.00000000   0.02057608 -0.3817041  0.5088809
# C_ab.permass  0.02057608   1.00000000  0.3797397 -0.3167412
# EWT.perc     -0.38170413   0.37973966  1.0000000 -0.8624055
# LMA.perm2     0.50888091  -0.31674123 -0.8624055  1.0000000





# Check Reflectance vs Transmittance --------------------------------------
angers.folder <- "data/leaf/Angers2003"
angers.files <- list.files(angers.folder, full.names = TRUE)


# For each sample ID, cbind R and T (saves as list = c('1'=tibble(wav,refl,trans), '2'=...))
angers.RT <- list()                                              # yeah this is not comp-efficient not to pre-allocate the size
angers.id <- 1:276; angers.id <- as.character(angers.id)
for(i in 1:276) {   #  rough approach based on files order in the folder, string (names) matching would be necessary otherwise
  tempR <- read_table(angers.files[i], col_names = c("wav", "refl"))           # the "wav" column in .txt has white space!!!
  tempT <- read_table(angers.files[i + 276], col_names = c("wav", "trans"))
  tempRT <- mutate(tempR, wav = seq(400,2450,by=1), trans = tempT$trans)       # Re-populate wav 
  
  id <- angers.id[i]
  angers.RT[[id]] <- tempRT
}

str(angers.RT)

# Resample to Landsat ETM
source("munge/my_spectralResampling.R")
# my_spectralResampling <- function(wL.hypers, wav.hypers, response = Landsat_7_response, wav = Landsat_7_wav)
# out <- list(b1, b2, ...)

angers.RT.L7 <- list()
wav.hypers <- as.numeric(angers.RT[[1]]$wav)            # = seq(400,2450,by=1)
band <- c("b1", "b2", "b3", "b4", "b5", "b7")
for(i in seq_along(angers.RT)) {
  id <- as.character(i)
  R.hypers <- angers.RT[[i]]$refl; T.hypers <- angers.RT[[i]]$trans
  R.L7 <- my_spectralResampling(R.hypers, wav.hypers); T.L7 <- my_spectralResampling(T.hypers, wav.hypers)
  angers.RT.L7[[id]] <- tibble(id = id, band = band, Refl = unlist(R.L7), Trans = unlist(T.L7))
}

# Rbind all sample ID
require(plyr)
angers.RT.L7.tall <- ldply(angers.RT.L7); angers.RT.L7.tall <- as_tibble(angers.RT.L7.tall)
detach(package:plyr)


# Select only Sycamore maple
leafbio$id <- as.character(1:276)
angers.RT.L7.tall <- merge(angers.RT.L7.tall, leafbio, by = "id")
maple.RT.L7.tall <- angers.RT.L7.tall %>%  dplyr::filter(Species == "Sycamore maple")

# Range
maple.RT.L7.tall %>% dplyr::select(band, Alb) %>%  dplyr::group_by(band) %>% 
  summarise(min.Alb = round(min(Alb), 3), max.Alb = round(max(Alb), 3))

maple.RT.L7.tall %>% dplyr::select(band, Alb) %>%  dplyr::group_by(band) %>% 
  summarise(meanminsd.Alb = round(mean(Alb) - sd(Alb), 3), 
            meanplussd.Alb = round(mean(Alb) + sd(Alb), 3))



# Correlation
maple.RT.L7.tall <- maple.RT.L7.tall %>%  mutate(band = as.factor(band))
maple.RT.L7.tall %>% dplyr::select(band, Refl, Trans) %>%  dplyr::group_by(band) %>%  
  summarise(cor = cor(Refl, Trans))

angers.RT.L7.tall %>% dplyr::select(band, Refl, Trans) %>%  dplyr::group_by(band) %>%  
  summarise(cor = cor(Refl, Trans))

# Plotting
pdf("graphs/sensitivity/ReflvsTrans_ETM_Angers276_maple.pdf")
ggplot(maple.RT.L7.tall, aes(Refl, Trans)) + geom_point(shape = 1) + facet_wrap( ~ band, nrow = 2) +
  xlim(0,0.7) + ylim(0,0.7) + geom_abline(slope = 1, intercept = 0) + theme(aspect.ratio = 1) +
  ggtitle("Correlation b1 = 0.84, b2 = 0.85, b3 = 0.87, b4 = -0.89, b5 = 0.19, b7 = 0.68")
dev.off()



# NIR Refl vs Cm & NIR Trans vs Cm; NIR Refl vs Cw & NIR Trans vs Cw,; NIR Refl vs N & NIR Trans vs N
temp <- maple.RT.L7.tall %>% dplyr::filter(band == "b4")                           # NIR

temp %>% dplyr::select(Refl, Trans, LMA, EWT, N, C_ab) %>% cor()

pdf("graphs/sensitivity/AngersMaple_NIR_LMAnEWT.pdf")
par(mfrow = c(3,2), ps = 14)
plot(temp$LMA, temp$Refl, xlab = "LMA (g/cm2)", ylab = "ETM NIR Refl", main = "r = 0.49")
plot(temp$LMA, temp$Trans, xlab = "LMA (g/cm2)", ylab = "ETM NIR Trans", main = "r = -0.54")
plot(temp$EWT, temp$Refl, xlab = "EWT (g/cm2)", ylab = "ETM NIR Refl", main = "r = 0.51")
plot(temp$EWT, temp$Trans, xlab = "EWT (g/cm2)", ylab = "ETM NIR Trans", main = "r = -0.58")
plot(temp$N, temp$Refl, xlab = "N (retrieved from PROSPECT)", ylab = "ETM NIR Refl", main = "r = 0.90")
plot(temp$N, temp$Trans, xlab = "N (retrieved from PROSPECT)", ylab = "ETM NIR Trans", main = "r = -0.94")
dev.off()


temp <- maple.RT.L7.tall %>% dplyr::filter(band == "b5")                            # SWIR-1

temp %>% dplyr::select(Refl, Trans, LMA, EWT, N, C_ab) %>% cor()

pdf("graphs/sensitivity/AngersMaple_SWIR1_LMAnEWT.pdf")
par(mfrow = c(3,2), ps = 14)
plot(temp$LMA, temp$Refl, xlab = "LMA (g/cm2)", ylab = "ETM SWIR-1 Refl", main = "r = -0.46")
plot(temp$LMA, temp$Trans, xlab = "LMA (g/cm2)", ylab = "ETM SWIR-1 Trans", main = "r = -0.73")
plot(temp$EWT, temp$Refl, xlab = "EWT (g/cm2)", ylab = "ETM SWIR-1 Refl", main = "r = -0.51")
plot(temp$EWT, temp$Trans, xlab = "EWT (g/cm2)", ylab = "ETM SWIR-1 Trans", main = "r = -0.80")
plot(temp$N, temp$Refl, xlab = "N (retrieved from PROSPECT)", ylab = "ETM SWIR-1 Refl", main = "r = 0.27")
plot(temp$N, temp$Trans, xlab = "N (retrieved from PROSPECT)", ylab = "ETM SWIR-1 Trans", main = "r = -0.89")
dev.off()


temp <- maple.RT.L7.tall %>% dplyr::filter(band == "b7")                            # SWIR-2

temp %>% dplyr::select(Refl, Trans, LMA, EWT, N, C_ab) %>% cor()

pdf("graphs/sensitivity/AngersMaple_SWIR2_LMAnEWT.pdf")
par(mfrow = c(3,2), ps = 14)
plot(temp$LMA, temp$Refl, xlab = "LMA (g/cm2)", ylab = "ETM SWIR-2 Refl", main = "r = -0.79")
plot(temp$LMA, temp$Trans, xlab = "LMA (g/cm2)", ylab = "ETM SWIR-2 Trans", main = "r = -0.80")
plot(temp$EWT, temp$Refl, xlab = "EWT (g/cm2)", ylab = "ETM SWIR-2 Refl", main = "r = -0.82")
plot(temp$EWT, temp$Trans, xlab = "EWT (g/cm2)", ylab = "ETM SWIR-2 Trans", main = "r = -0.88")
plot(temp$N, temp$Refl, xlab = "N (retrieved from PROSPECT)", ylab = "ETM SWIR-2 Refl", main = "r = -0.26")
plot(temp$N, temp$Trans, xlab = "N (retrieved from PROSPECT)", ylab = "ETM SWIR-2 Trans", main = "r = -0.80")
dev.off()


pdf("graphs/sensitivity/AngersMaple_LMAvsEWT.pdf")
plot(temp$EWT, temp$LMA, xlab = "EWT (g/cm2)", ylab = "LMA (g/cm2)", main = "r = 0.80")
dev.off()



# N vs wL -----------------------------------------------------------------
maple.RT.L7.tall$Alb <- maple.RT.L7.tall$Refl + maple.RT.L7.tall$Trans
maple.RT.L7.tall %>% dplyr::select(band, N, Alb) %>%  dplyr::group_by(band) %>%  
  summarise(cor = cor(N, Alb))


pdf("graphs/sensitivity/NvsAlbedo_ETM_Angers276_maple.pdf")
ggplot(maple.RT.L7.tall, aes(N, Alb)) + geom_point(shape = 1) + facet_wrap( ~ band, nrow = 2) +
  ylim(0,1) + theme(aspect.ratio = 1) +
  ggtitle("Correlation b1 = -0.42, b2 = -0.44, b3 = -0.42, b4 = -0.37, b5 = -0.65, b7 = -0.67")
dev.off()


# NIR Albedo vs Cm 
temp <- maple.RT.L7.tall %>% dplyr::filter(band == "b4")                           # NIR
temp <- temp %>% mutate(Albedo=Refl+Trans)

temp %>% dplyr::select(Albedo, LMA, EWT, N, C_ab) %>% cor()

pdf("graphs/sensitivity/AngersMaple_NIRalbedo_LMAnEWTnCab.pdf", width=7.5, height=3, pointsize=10)
par(mfrow = c(1,3), ps = 14)
plot(temp$LMA, temp$Albedo, xlab = "LMA (g/cm2)", ylab = "ETM NIR Albedo", main = "r = -0.25")
plot(temp$EWT, temp$Albedo, xlab = "EWT (g/cm2)", ylab = "ETM NIR Albedo", main = "r = -0.28")
plot(temp$C_ab, temp$Albedo, xlab = "Cab (ug/cm2)", ylab = "ETM NIR Albedo", main = "r = -0.24")
dev.off()


