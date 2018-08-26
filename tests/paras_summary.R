paras.summary <- function(data) {
  out <- data$input
    
  # Black soil  #################################################################################
  out$BRF_b1_bs.mean = apply(data$b1$bs, 1, base::mean)                        # bs b1
  out$BRF_b1_bs.sd = apply(data$b1$bs, 1, stats::sd)
  out$BRF_b1_bs.meanplussd = out$BRF_b1_bs.mean + out$BRF_b1_bs.sd
  out$BRF_b1_bs.meanminsd = out$BRF_b1_bs.mean - out$BRF_b1_bs.sd
  out$BRF_b1_bs.min = apply(data$b1$bs, 1, base::min)
  out$BRF_b1_bs.max = apply(data$b1$bs, 1, base::max)
  
  out$BRF_b2_bs.mean = apply(data$b2$bs, 1, base::mean)                         # bs b2
  out$BRF_b2_bs.sd = apply(data$b2$bs, 1, stats::sd)
  out$BRF_b2_bs.meanplussd = out$BRF_b2_bs.mean + out$BRF_b2_bs.sd
  out$BRF_b2_bs.meanminsd = out$BRF_b2_bs.mean - out$BRF_b2_bs.sd
  out$BRF_b2_bs.min = apply(data$b2$bs, 1, base::min)
  out$BRF_b2_bs.max = apply(data$b2$bs, 1, base::max)

  out$BRF_b3_bs.mean = apply(data$b3$bs, 1, base::mean)                        # bs b3
  out$BRF_b3_bs.sd = apply(data$b3$bs, 1, stats::sd)
  out$BRF_b3_bs.meanplussd = out$BRF_b3_bs.mean + out$BRF_b3_bs.sd
  out$BRF_b3_bs.meanminsd = out$BRF_b3_bs.mean - out$BRF_b3_bs.sd
  out$BRF_b3_bs.min = apply(data$b3$bs, 1, base::min)
  out$BRF_b3_bs.max = apply(data$b3$bs, 1, base::max)
  
  out$BRF_b4_bs.mean = apply(data$b4$bs, 1, base::mean)                        # bs b4
  out$BRF_b4_bs.sd = apply(data$b4$bs, 1, stats::sd)
  out$BRF_b4_bs.meanplussd = out$BRF_b4_bs.mean + out$BRF_b4_bs.sd
  out$BRF_b4_bs.meanminsd = out$BRF_b4_bs.mean - out$BRF_b4_bs.sd
  out$BRF_b4_bs.min = apply(data$b4$bs, 1, base::min)
  out$BRF_b4_bs.max = apply(data$b4$bs, 1, base::max)
  
  out$BRF_b5_bs.mean = apply(data$b5$bs, 1, base::mean)                         # bs b5
  out$BRF_b5_bs.sd = apply(data$b5$bs, 1, stats::sd)
  out$BRF_b5_bs.meanplussd = out$BRF_b5_bs.mean + out$BRF_b5_bs.sd
  out$BRF_b5_bs.meanminsd = out$BRF_b5_bs.mean - out$BRF_b5_bs.sd
  out$BRF_b5_bs.min = apply(data$b5$bs, 1, base::min)
  out$BRF_b5_bs.max = apply(data$b5$bs, 1, base::max)
  
  out$BRF_b7_bs.mean = apply(data$b7$bs, 1, base::mean)                       # bs b7
  out$BRF_b7_bs.sd = apply(data$b7$bs, 1, stats::sd)
  out$BRF_b7_bs.meanplussd = out$BRF_b7_bs.mean + out$BRF_b7_bs.sd
  out$BRF_b7_bs.meanminsd = out$BRF_b7_bs.mean - out$BRF_b7_bs.sd
  out$BRF_b7_bs.min = apply(data$b7$bs, 1, base::min)
  out$BRF_b7_bs.max = apply(data$b7$bs, 1, base::max)
  
  # With soil #################################################################################
  out$BRF_b1_s.mean = apply(data$b1$s, 1, base::mean)                       # s b1
  out$BRF_b1_s.sd = apply(data$b1$s, 1, stats::sd)
  out$BRF_b1_s.meanplussd = out$BRF_b1_s.mean + out$BRF_b1_s.sd
  out$BRF_b1_s.meanminsd = out$BRF_b1_s.mean - out$BRF_b1_s.sd
  out$BRF_b1_s.min = apply(data$b1$s, 1, base::min)
  out$BRF_b1_s.max = apply(data$b1$s, 1, base::max)

  out$BRF_b2_s.mean = apply(data$b2$s, 1, base::mean)                        # s b2
  out$BRF_b2_s.sd = apply(data$b2$s, 1, stats::sd)
  out$BRF_b2_s.meanplussd = out$BRF_b2_s.mean + out$BRF_b2_s.sd
  out$BRF_b2_s.meanminsd = out$BRF_b2_s.mean - out$BRF_b2_s.sd
  out$BRF_b2_s.min = apply(data$b2$s, 1, base::min)
  out$BRF_b2_s.max = apply(data$b2$s, 1, base::max)

  out$BRF_b3_s.mean = apply(data$b3$s, 1, base::mean)                         # s b3
  out$BRF_b3_s.sd = apply(data$b3$s, 1, stats::sd)
  out$BRF_b3_s.meanplussd = out$BRF_b3_s.mean + out$BRF_b3_s.sd
  out$BRF_b3_s.meanminsd = out$BRF_b3_s.mean - out$BRF_b3_s.sd
  out$BRF_b3_s.min = apply(data$b3$s, 1, base::min)
  out$BRF_b3_s.max = apply(data$b3$s, 1, base::max)
  
  out$BRF_b4_s.mean = apply(data$b4$s, 1, base::mean)                        # s b4
  out$BRF_b4_s.sd = apply(data$b4$s, 1, stats::sd)
  out$BRF_b4_s.meanplussd = out$BRF_b4_s.mean + out$BRF_b4_s.sd
  out$BRF_b4_s.meanminsd = out$BRF_b4_s.mean - out$BRF_b4_s.sd
  out$BRF_b4_s.min = apply(data$b4$s, 1, base::min)
  out$BRF_b4_s.max = apply(data$b4$s, 1, base::max)
  
  out$BRF_b5_s.mean = apply(data$b5$s, 1, base::mean)                      # s b5
  out$BRF_b5_s.sd = apply(data$b5$s, 1, stats::sd)
  out$BRF_b5_s.meanplussd = out$BRF_b5_s.mean + out$BRF_b5_s.sd
  out$BRF_b5_s.meanminsd = out$BRF_b5_s.mean - out$BRF_b5_s.sd
  out$BRF_b5_s.min = apply(data$b5$s, 1, base::min)
  out$BRF_b5_s.max = apply(data$b5$s, 1, base::max)
  
  out$BRF_b7_s.mean = apply(data$b7$s, 1, base::mean)                       # s b7
  out$BRF_b7_s.sd = apply(data$b7$s, 1, stats::sd)
  out$BRF_b7_s.meanplussd = out$BRF_b7_s.mean + out$BRF_b7_s.sd
  out$BRF_b7_s.meanminsd = out$BRF_b7_s.mean - out$BRF_b7_s.sd
  out$BRF_b7_s.min = apply(data$b7$s, 1, base::min)
  out$BRF_b7_s.max = apply(data$b7$s, 1, base::max)
    
  
  
  return(out)
  
}

paras.summary.addSRnoise <- function(data) {                      # Add surface reflectance noise to error bars
  out <- data$input
  
  # Black soil  #################################################################################
  out$BRF_b1_bs.mean = apply(data$b1$bs, 1, base::mean)                        # bs b1
  out$BRF_b1_bs.sd = apply(data$b1$bs, 1, stats::sd) 
  out$BRF_b1_bs.meanplussd = out$BRF_b1_bs.mean + out$BRF_b1_bs.sd + (0.05 * out$BRF_b1_bs.mean + 0.005)
  out$BRF_b1_bs.meanminsd = out$BRF_b1_bs.mean - out$BRF_b1_bs.sd - (0.05 * out$BRF_b1_bs.mean + 0.005)
  out$BRF_b1_bs.min = apply(data$b1$bs, 1, base::min) - (0.05 * out$BRF_b1_bs.mean + 0.005)
  out$BRF_b1_bs.max = apply(data$b1$bs, 1, base::max) + (0.05 * out$BRF_b1_bs.mean + 0.005)
  
  out$BRF_b2_bs.mean = apply(data$b2$bs, 1, base::mean)                         # bs b2
  out$BRF_b2_bs.sd = apply(data$b2$bs, 1, stats::sd)
  out$BRF_b2_bs.meanplussd = out$BRF_b2_bs.mean + out$BRF_b2_bs.sd + (0.05 * out$BRF_b2_bs.mean + 0.005)
  out$BRF_b2_bs.meanminsd = out$BRF_b2_bs.mean - out$BRF_b2_bs.sd - (0.05 * out$BRF_b2_bs.mean + 0.005)
  out$BRF_b2_bs.min = apply(data$b2$bs, 1, base::min) - (0.05 * out$BRF_b2_bs.mean + 0.005)
  out$BRF_b2_bs.max = apply(data$b2$bs, 1, base::max) + (0.05 * out$BRF_b2_bs.mean + 0.005)
  
  out$BRF_b3_bs.mean = apply(data$b3$bs, 1, base::mean)                        # bs b3
  out$BRF_b3_bs.sd = apply(data$b3$bs, 1, stats::sd)
  out$BRF_b3_bs.meanplussd = out$BRF_b3_bs.mean + out$BRF_b3_bs.sd + (0.05 * out$BRF_b3_bs.mean + 0.005)
  out$BRF_b3_bs.meanminsd = out$BRF_b3_bs.mean - out$BRF_b3_bs.sd - (0.05 * out$BRF_b3_bs.mean + 0.005)
  out$BRF_b3_bs.min = apply(data$b3$bs, 1, base::min) - (0.05 * out$BRF_b3_bs.mean + 0.005)
  out$BRF_b3_bs.max = apply(data$b3$bs, 1, base::max) + (0.05 * out$BRF_b3_bs.mean + 0.005)
  
  out$BRF_b4_bs.mean = apply(data$b4$bs, 1, base::mean)                        # bs b4
  out$BRF_b4_bs.sd = apply(data$b4$bs, 1, stats::sd)
  out$BRF_b4_bs.meanplussd = out$BRF_b4_bs.mean + out$BRF_b4_bs.sd + (0.05 * out$BRF_b4_bs.mean + 0.005)
  out$BRF_b4_bs.meanminsd = out$BRF_b4_bs.mean - out$BRF_b4_bs.sd - (0.05 * out$BRF_b4_bs.mean + 0.005)
  out$BRF_b4_bs.min = apply(data$b4$bs, 1, base::min) - (0.05 * out$BRF_b4_bs.mean + 0.005)
  out$BRF_b4_bs.max = apply(data$b4$bs, 1, base::max) + (0.05 * out$BRF_b4_bs.mean + 0.005)
  
  out$BRF_b5_bs.mean = apply(data$b5$bs, 1, base::mean)                         # bs b5
  out$BRF_b5_bs.sd = apply(data$b5$bs, 1, stats::sd)
  out$BRF_b5_bs.meanplussd = out$BRF_b5_bs.mean + out$BRF_b5_bs.sd + (0.05 * out$BRF_b5_bs.mean + 0.005)
  out$BRF_b5_bs.meanminsd = out$BRF_b5_bs.mean - out$BRF_b5_bs.sd - (0.05 * out$BRF_b5_bs.mean + 0.005)
  out$BRF_b5_bs.min = apply(data$b5$bs, 1, base::min) - (0.05 * out$BRF_b5_bs.mean + 0.005)
  out$BRF_b5_bs.max = apply(data$b5$bs, 1, base::max) + (0.05 * out$BRF_b5_bs.mean + 0.005)
  
  out$BRF_b7_bs.mean = apply(data$b7$bs, 1, base::mean)                       # bs b7
  out$BRF_b7_bs.sd = apply(data$b7$bs, 1, stats::sd)
  out$BRF_b7_bs.meanplussd = out$BRF_b7_bs.mean + out$BRF_b7_bs.sd + (0.05 * out$BRF_b7_bs.mean + 0.005)
  out$BRF_b7_bs.meanminsd = out$BRF_b7_bs.mean - out$BRF_b7_bs.sd - (0.05 * out$BRF_b7_bs.mean + 0.005)
  out$BRF_b7_bs.min = apply(data$b7$bs, 1, base::min) - (0.05 * out$BRF_b7_bs.mean + 0.005)
  out$BRF_b7_bs.max = apply(data$b7$bs, 1, base::max) + (0.05 * out$BRF_b7_bs.mean + 0.005)
  
  # With soil #################################################################################
  out$BRF_b1_s.mean = apply(data$b1$s, 1, base::mean)                       # s b1
  out$BRF_b1_s.sd = apply(data$b1$s, 1, stats::sd)
  out$BRF_b1_s.meanplussd = out$BRF_b1_s.mean + out$BRF_b1_s.sd + (0.05 * out$BRF_b1_s.mean + 0.005)
  out$BRF_b1_s.meanminsd = out$BRF_b1_s.mean - out$BRF_b1_s.sd - (0.05 * out$BRF_b1_s.mean + 0.005)
  out$BRF_b1_s.min = apply(data$b1$s, 1, base::min) - (0.05 * out$BRF_b1_s.mean + 0.005)
  out$BRF_b1_s.max = apply(data$b1$s, 1, base::max) + (0.05 * out$BRF_b1_s.mean + 0.005)
  
  out$BRF_b2_s.mean = apply(data$b2$s, 1, base::mean)                        # s b2
  out$BRF_b2_s.sd = apply(data$b2$s, 1, stats::sd)
  out$BRF_b2_s.meanplussd = out$BRF_b2_s.mean + out$BRF_b2_s.sd + (0.05 * out$BRF_b2_s.mean + 0.005)
  out$BRF_b2_s.meanminsd = out$BRF_b2_s.mean - out$BRF_b2_s.sd - (0.05 * out$BRF_b2_s.mean + 0.005)
  out$BRF_b2_s.min = apply(data$b2$s, 1, base::min) - (0.05 * out$BRF_b2_s.mean + 0.005)
  out$BRF_b2_s.max = apply(data$b2$s, 1, base::max) + (0.05 * out$BRF_b2_s.mean + 0.005)
  
  out$BRF_b3_s.mean = apply(data$b3$s, 1, base::mean)                         # s b3
  out$BRF_b3_s.sd = apply(data$b3$s, 1, stats::sd)
  out$BRF_b3_s.meanplussd = out$BRF_b3_s.mean + out$BRF_b3_s.sd + (0.05 * out$BRF_b3_s.mean + 0.005)
  out$BRF_b3_s.meanminsd = out$BRF_b3_s.mean - out$BRF_b3_s.sd - (0.05 * out$BRF_b3_s.mean + 0.005)
  out$BRF_b3_s.min = apply(data$b3$s, 1, base::min) - (0.05 * out$BRF_b3_s.mean + 0.005)
  out$BRF_b3_s.max = apply(data$b3$s, 1, base::max) + (0.05 * out$BRF_b3_s.mean + 0.005)
  
  out$BRF_b4_s.mean = apply(data$b4$s, 1, base::mean)                        # s b4
  out$BRF_b4_s.sd = apply(data$b4$s, 1, stats::sd)
  out$BRF_b4_s.meanplussd = out$BRF_b4_s.mean + out$BRF_b4_s.sd + (0.05 * out$BRF_b4_s.mean + 0.005)
  out$BRF_b4_s.meanminsd = out$BRF_b4_s.mean - out$BRF_b4_s.sd - (0.05 * out$BRF_b4_s.mean + 0.005)
  out$BRF_b4_s.min = apply(data$b4$s, 1, base::min) - (0.05 * out$BRF_b4_s.mean + 0.005)
  out$BRF_b4_s.max = apply(data$b4$s, 1, base::max) + (0.05 * out$BRF_b4_s.mean + 0.005)
  
  out$BRF_b5_s.mean = apply(data$b5$s, 1, base::mean)                      # s b5
  out$BRF_b5_s.sd = apply(data$b5$s, 1, stats::sd)
  out$BRF_b5_s.meanplussd = out$BRF_b5_s.mean + out$BRF_b5_s.sd + (0.05 * out$BRF_b5_s.mean + 0.005)
  out$BRF_b5_s.meanminsd = out$BRF_b5_s.mean - out$BRF_b5_s.sd - (0.05 * out$BRF_b5_s.mean + 0.005)
  out$BRF_b5_s.min = apply(data$b5$s, 1, base::min) - (0.05 * out$BRF_b5_s.mean + 0.005)
  out$BRF_b5_s.max = apply(data$b5$s, 1, base::max) + (0.05 * out$BRF_b5_s.mean + 0.005)
  
  out$BRF_b7_s.mean = apply(data$b7$s, 1, base::mean)                       # s b7
  out$BRF_b7_s.sd = apply(data$b7$s, 1, stats::sd)
  out$BRF_b7_s.meanplussd = out$BRF_b7_s.mean + out$BRF_b7_s.sd + (0.05 * out$BRF_b7_s.mean + 0.005)
  out$BRF_b7_s.meanminsd = out$BRF_b7_s.mean - out$BRF_b7_s.sd - (0.05 * out$BRF_b7_s.mean + 0.005)
  out$BRF_b7_s.min = apply(data$b7$s, 1, base::min) - (0.05 * out$BRF_b7_s.mean + 0.005)
  out$BRF_b7_s.max = apply(data$b7$s, 1, base::max) + (0.05 * out$BRF_b7_s.mean + 0.005)
  
  
  
  return(out)
  
}