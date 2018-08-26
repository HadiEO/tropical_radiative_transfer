# install.packages('jpeg')
require(jpeg)

# img <- readJPEG("graphs/OP_mature_leaf_refl.JPG") 
# img <- readJPEG("graphs/Asner1998_litter_spectra.JPG")
# img <- readJPEG("graphs/Asner1998_stem_spectra.JPG")

# img <- readJPEG("graphs/global_humidtropics_leaf_reflectance.JPG")
# img <- readJPEG("graphs/global_humidtropics_leaf_transmittance.JPG")
# img <- readJPEG("graphs/prospect_bias_HAWAII_refl_vis.JPG")
# img <- readJPEG("graphs/prospect_bias_HAWAII_trans_vis.JPG")
# img <- readJPEG("graphs/prospect_bias_HAWAII_refl_irswir.JPG")
# img <- readJPEG("graphs/prospect_bias_HAWAII_trans_irswir.JPG")
img <- readJPEG("graphs/Borneo_canopyReflCV_Asner2012.JPG")


# dev.new()
x11()

plot(1:2, type='n')                                             # 1:2 is the axis (x & y) values of the plot
# rasterImage(img, 1.2, 1.27, 1.8, 1.73)
graphics::rasterImage(img, xleft = 1, ybottom = 1, xright = 2, ytop = 2)  # Need to call plot() first

calpoints <- locator(n=4,type='p',pch=4,col='blue',lwd=2)       # Record (click) 4 calibration points, first 2 in x axis, next 2 in y axis. Avoid 0.
as.data.frame(calpoints)

data <- locator(type='p',pch=1,col='red',lwd=1.2,cex=1.2)       # Digitize (click) along the curve in Figure. Right click to stop.
as.data.frame(data)

# Define the function
calibrate = function(calpoints, data, x1, x2, y1, y2)           # Function to convert values on graphic display into true data points values needed
{
  x  <- calpoints$x[c(1,2)]
  y  <- calpoints$y[c(3,4)]
  cx <- lm(formula = c(x1,x2) ~ c(x))$coeff
  cy <- lm(formula = c(y1,y2) ~ c(y))$coeff
  data$x <- data$x*cx[2]+cx[1]
  data$y <- data$y*cy[2]+cy[1]
  return(as.data.frame(data))
}

# Run the function
true.data <- calibrate(calpoints = calpoints, data = data, x1 = 700, x2 = 2200, y1 = 5, y2 = 20)
# write.csv2(true.data, "results/leaf/OP_mature_leaf_refl_digitize.csv")
# write.csv2(true.data, "results/leaf/Asner1998_litterRefl_darkest_digitize.csv")
# write.csv2(true.data, "results/leaf/Asner1998_stemRefl_mean_digitize.csv")
# write.csv2(true.data, "results/leaf/Asner1998_litterRefl_mean_digitize.csv")
# write.csv2(true.data, "results/leaf/globalhumidtropics_leafRefl_min_digitize.csv")
# write.csv2(true.data, "results/leaf/globalhumidtropics_leafRefl_max_digitize.csv")
# write.csv2(true.data, "results/leaf/globalhumidtropics_leafTrans_min_digitize.csv")
# write.csv2(true.data, "results/leaf/globalhumidtropics_leafTrans_max_digitize.csv")
# write.csv2(true.data, "results/leaf/prospect_bias_HAWAII_refl_vis_digitize.csv")
# write.csv2(true.data, "results/leaf/prospect_bias_HAWAII_trans_vis_digitize.csv")
# write.csv2(true.data, "results/leaf/prospect_bias_HAWAII_refl_irswir_digitize.csv")
# write.csv2(true.data, "results/leaf/prospect_bias_HAWAII_trans_irswir_digitize.csv")
write.csv2(true.data, "results/leaf/Diptero_canopyReflCV_Asner_digitize.csv")


# Interpolate at 1nm from 400 to 1000nm
# Linear does not extrapolate
true.data.lin <- approx(true.data$x, true.data$y, method = "linear", xout = seq(400,1000,by=1))
# Spline: default method 'fmm', 'natural' extrapolates better
true.data.spl <- spline(true.data$x, true.data$y, method = "natural", xout = seq(400,2500,by=1))     # Use this
# true.data.spl <- spline(true.data$x, true.data$y, method = "natural", xout = seq(800,2450,by=1)) 
# write.csv2(true.data.spl, "results/leaf/OP_mature_leaf_refl_digitize_full.csv")
# write.csv2(true.data.spl, "results/leaf/Asner1998_litterRefl_darkest_digitize_full.csv")
# write.csv2(true.data.spl, "results/leaf/Asner1998_stemRefl_mean_digitize_full.csv")
# write.csv2(true.data.spl, "results/leaf/Asner1998_litterRefl_mean_digitize_full.csv")
# write.csv2(true.data.spl, "results/leaf/globalhumidtropics_leafRefl_min_digitize_full.csv")
# write.csv2(true.data.spl, "results/leaf/globalhumidtropics_leafRefl_max_digitize_full.csv")
# write.csv2(true.data.spl, "results/leaf/globalhumidtropics_leafTrans_min_digitize_full.csv")
# write.csv2(true.data.spl, "results/leaf/globalhumidtropics_leafTrans_max_digitize_full.csv")
# write.csv2(true.data.spl, "results/leaf/prospect_bias_HAWAII_refl_vis_digitize_full.csv")
# write.csv2(true.data.spl, "results/leaf/prospect_bias_HAWAII_trans_vis_digitize_full.csv")
# write.csv2(true.data.spl, "results/leaf/prospect_bias_HAWAII_refl_irswir_digitize_full.csv")
# write.csv2(true.data.spl, "results/leaf/prospect_bias_HAWAII_trans_irswir_digitize_full.csv")
write.csv2(true.data.spl, "results/leaf/Diptero_canopyReflCV_Asner_digitize_full.csv")

# Plot to check
windows()
# plot(true.data, type = 'b', pch = 1, col = 'blue', lwd = 1.1, bty = 'l', ylim = c(-0.04, 0.04))
plot(true.data, type = 'b', pch = 1, col = 'blue', lwd = 1.1, bty = 'l', ylim = c(400, 2500), xlim = c(0,25))
# lines(true.data.lin$x, true.data.lin$y, col = 'red', lwd = 1.1)
lines(true.data.spl$x, true.data.spl$y, col = 'brown', lwd = 1.1)






