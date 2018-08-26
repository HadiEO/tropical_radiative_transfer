# Parallelize iteration ---------------------------------------------------
# 20170215: Hadi suggests to implement parallel processing                     # Implement parallel processing
# install.packages("doParallel")                                               # If not installed yet
require("doParallel")
getDoParWorkers()                                                              # How many workers (cores) available in system
registerDoParallel(cores=2)                                                    # Register parallel backend






# Test sensitivity of PROSPECT model --------------------------------------
require(sensitivity)
source("tests/sobolMultOut.R")
require(Rprospect)                                                                       # The required library
#  prospect4(N, Cab, Cw, Cm)
sobol.prospect <- function(X) {                                                          # Function to pass to sobol's model          
  n <- dim(X)[[1]]
  res <- matrix(NA, ncol = 2101, nrow = n)
  for (i in 1:n) {
    lrt <- Rprospect::prospect4(N = X[i, 1], Cab = X[i, 2], Cw = X[i, 3], Cm = X[i, 4])  # ?? Unsure here
    res[i, ] <- lrt$Reflectance + lrt$Transmittance
    
  }
  return(res)
}

p <- 4                                                                                   # No. of model inputs

set.seed(123)
prospect.input <- parameterSets(par.ranges =                   # Generate 2 * p columns, left matrix = X1, right matrix = X2 for Monte Carlo SI estimation
                                  list(N = c(1.5, 3.5), Cab = c(27.35, 122.83), Cw = c(0.0128, 0.0176), Cm = c(0.0047, 0.0231),
                                       N = c(1.5, 3.5), Cab = c(27.35, 122.83), Cw = c(0.0128, 0.0176), Cm = c(0.0047, 0.0231)), 
                                samples = 10000,                  
                                method = "sobol")
colnames(prospect.input) <- c("N", "Cab", "Cw", "Cm", "N", "Cab", "Cw", "Cm")

saveRDS(prospect.input, "results/sensitivity/prospect_2std_10000_samples.rds") # OK stores 20000 samples, but takes too long to calculate SI :(

prospect.SI <- sobolMultOut(model = sobol.prospect, q = 2101,    # q = 2101 wavelengths of sobol.prospect() output
                            X1 = prospect.input[, 1:p], X2 = prospect.input[, (p+1):(2*p)],
                            MCmethod="soboljansen", plotFct=TRUE)

saveRDS(prospect.SI, "results/sensitivity/prospect_2std_10000_SI.rds")

# Is plotFct=TRUE the SI for each output?
# x = x0 = sobol(model=NULL)
# xx  List concatenating all Sobol objects for all outputs, length q: no. of model output
# q : number of model output
# p : number of input
# S matrix q rows * p cols
# So YES!

print(prospect.SI)
x11(); plot(prospect.SI)



# Plot sobol indices (first-order S and total ST) -------------------------
S <- prospect.SI$Sfct; S <- as_tibble(S); colnames(S) <- c("N", "Cab", "Cw", "Cm")                            # Run from here
ST <- prospect.SI$Tfct; ST <- as_tibble(ST); colnames(ST) <- c("N", "Cab", "Cw", "Cm")
wav <- read_csv2("data/leaf/prospect_wav.csv"); wav <- wav$x

pdf("graphs/sensitivity/prospect_2std_10000_SI.pdf")

par(mfrow = c(2,1), ps = 12, las = 1, mar=c(5.1, 4.1, 4.1, 6.1), xpd=FALSE)
plot(wav, S$N, type = 'l', col = 'dark blue', ylim = c(-0.12, 1), xaxp = c(400,2400,10), lwd = 2,              # First-effect SI
     main = "First-order SI", xlab = "Wavelength (nm)", ylab = "Si")
abline(h = 0, lty = 2, col = "grey20")
lines(wav, S$Cab, col = 'chartreuse4', lwd = 2)
lines(wav, S$Cw, col = 'dark orange', lwd = 2)
lines(wav, S$Cm, col = 'magenta', lwd = 2)
legend("topright", c("N", "Cab", "Cw", "Cm"), col = c("dark blue", "chartreuse4", "dark orange", "magenta"), 
       lwd = c(2,2,2,2), ncol = 1, xpd = TRUE, inset = c(-0.21,0))
# Add Landsat-7 bands
rect.fill <- adjustcolor("grey", alpha.f = 0.4)
rect.fill.b1 <- adjustcolor("grey", alpha.f = 0.2)
rect(450, 0, 520, 140, border = "transparent", col = rect.fill.b1)
rect(520, 0, 600, 140, border = "transparent", col = rect.fill)
rect(630, 0, 690, 140, border = "transparent", col = rect.fill)
rect(770, 0, 900, 140, border = "transparent", col = rect.fill)
rect(1550, 0, 1750, 140, border = "transparent", col = rect.fill)
rect(2090, 0, 2350, 140, border = "transparent", col = rect.fill)
# Add 710-790nm DASF retrieval wavelength
rect(710, 0, 790, 140, border = "grey", density = 30, col = "grey")


plot(wav, ST$N, type = 'l', col = 'dark blue', ylim = c(-0.12, 1), xaxp = c(400,2400,10), lwd = 2,              # Total SI
     main = "Total SI", xlab = "Wavelength (nm)", ylab = "STi")
abline(h = 0, lty = 2, col = "grey20")
lines(wav, ST$Cab, col = 'chartreuse4', lwd = 2)
lines(wav, ST$Cw, col = 'dark orange', lwd = 2)
lines(wav, ST$Cm, col = 'magenta', lwd = 2)
legend("topright", c("N", "Cab", "Cw", "Cm"), col = c("dark blue", "chartreuse4", "dark orange", "magenta"), 
       lwd = c(2,2,2,2), ncol = 1, xpd = TRUE, inset = c(-0.21,0))
# Add Landsat-7 bands
rect.fill <- adjustcolor("grey", alpha.f = 0.4)
rect.fill.b1 <- adjustcolor("grey", alpha.f = 0.2)
rect(450, 0, 520, 140, border = "transparent", col = rect.fill.b1)
rect(520, 0, 600, 140, border = "transparent", col = rect.fill)
rect(630, 0, 690, 140, border = "transparent", col = rect.fill)
rect(770, 0, 900, 140, border = "transparent", col = rect.fill)
rect(1550, 0, 1750, 140, border = "transparent", col = rect.fill)
rect(2090, 0, 2350, 140, border = "transparent", col = rect.fill)
# Add 710-790nm DASF retrieval wavelength
rect(710, 0, 790, 140, border = "grey", density = 30, col = "grey")


dev.off()

