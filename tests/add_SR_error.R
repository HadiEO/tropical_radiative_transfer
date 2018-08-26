add_SR_error <- function(data) {
  data <- data %>% mutate(
    SR_B2_green.minerr = SR_B2_green - (0.05 * SR_B2_green + 0.005),
    SR_B2_green.pluserr = SR_B2_green + (0.05 * SR_B2_green + 0.005),
    
    SR_B3_red.minerr = SR_B3_red - (0.05 * SR_B3_red + 0.005),
    SR_B3_red.pluserr = SR_B3_red + (0.05 * SR_B3_red + 0.005),
    
    SR_B4_nir.minerr = SR_B4_nir - (0.05 * SR_B4_nir + 0.005),
    SR_B4_nir.pluserr = SR_B4_nir + (0.05 * SR_B4_nir + 0.005),
    
    SR_B5_swir1.minerr = SR_B5_swir1 - (0.05 * SR_B5_swir1 + 0.005),
    SR_B5_swir1.pluserr = SR_B5_swir1 + (0.05 * SR_B5_swir1 + 0.005),
    
    SR_B7_swir2.minerr = SR_B7_swir2 - (0.05 * SR_B7_swir2 + 0.005),
    SR_B7_swir2.pluserr = SR_B7_swir2 + (0.05 * SR_B7_swir2 + 0.005)
  )
  
  return(data)
}