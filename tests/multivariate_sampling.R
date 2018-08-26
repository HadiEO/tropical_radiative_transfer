# source("cache/libPaths.R")
require(MASS)
require(tidyverse)
require(mvtnorm)
library(psych)

# Todo: if Car needs to be used, need to adapt the mvrnorm function arguments

leaf.bio.sim <- function(n = 2500, leaf.data = prospect_in, 
                         mean = "Dipt_mean", sd = "Dipt_sd", cor = NULL) {
  
  leaf.bio.mean <- leaf.data %>%  dplyr::filter(Group == mean) %>%  
    dplyr::select(Cab, Car, Cw, Cm) %>% mutate(Car = as.numeric(Car))
  
  leaf.bio.sd <- leaf.data %>%  dplyr::filter(Group == sd) %>%  
    dplyr::select(Cab, Car, Cw, Cm) %>% mutate(Car = as.numeric(Car))
  
  # Mean and sd 1.Cab, 2.Cw, 3.Cm
  mu1 <- leaf.bio.mean$Cab; sigma1 <- leaf.bio.sd$Cab
  mu2 <- leaf.bio.mean$Cw; sigma2 <- leaf.bio.sd$Cw
  mu3 <- leaf.bio.mean$Cm; sigma3 <- leaf.bio.sd$Cm

  # Correlations and covariances
  cor12 <- cor[1]; cov12 <- cor12*sigma1*sigma2
  cor23 <- cor[2]; cov23 <- cor23*sigma2*sigma3
  cor13 <- cor[3]; cov13 <- cor13*sigma1*sigma3
  
  set.seed(123)
  prosp_mvr_emp <- mvrnorm(n, mu=c(mu1, mu2, mu3), 
                           Sigma=cbind(c(sigma1^2,cov12,cov13), c(cov12,sigma2^2,cov23), c(cov13,cov23,sigma3^2)),
                           empirical=T)
  
  prosp_mvr_emp <- as_tibble(prosp_mvr_emp)
  colnames(prosp_mvr_emp) <- c("Cab", "Cw", "Cm")
  
  # Need to remove negative values 
  posnev <- prosp_mvr_emp[,1]*prosp_mvr_emp[,2]*prosp_mvr_emp[,3]
  prosp_mvr_emp_pos <- prosp_mvr_emp[which(posnev > 0), ]
  
  return(prosp_mvr_emp_pos)
}


#########################################################################################################

leaf.bio.sim.cop <- function(n = 2500, leaf.data, 
                             mean, min, max, cor) {
  
  leaf.bio.mean <- leaf.data %>%  dplyr::filter(Group == mean) %>%  
    dplyr::select(Cab, Car, Cw, Cm) %>% mutate(Car = as.numeric(Car))
  
  leaf.bio.min <- leaf.data %>%  dplyr::filter(Group == min) %>%  
    dplyr::select(Cab, Car, Cw, Cm) %>% mutate(Car = as.numeric(Car))
  
  leaf.bio.max <- leaf.data %>%  dplyr::filter(Group == max) %>%  
    dplyr::select(Cab, Car, Cw, Cm) %>% mutate(Car = as.numeric(Car))
  
  # Mean, min, and max of  1.Cab, 2.Cw, 3.Cm
  mu1 <- leaf.bio.mean$Cab; min1 <- leaf.bio.min$Cab; max1 <- leaf.bio.max$Cab
  mu2 <- leaf.bio.mean$Cw; min2 <- leaf.bio.min$Cw; max2 <- leaf.bio.max$Cw
  mu3 <- leaf.bio.mean$Cm; min3 <- leaf.bio.min$Cm; max3 <- leaf.bio.max$Cm
  
  # Correlations and covariances
  cor12 <- cor[1] # ; cov12 <- cor12*sigma1*sigma2
  cor23 <- cor[2] # ; cov23 <- cor23*sigma2*sigma3
  cor13 <- cor[3] # ; cov13 <- cor13*sigma1*sigma3
  
  set.seed(123)
  prosp_mvr_emp <- mvrnorm(n,   # This is the start i.e., multivariate normal distributions with mean 0              
    # mu=c(mu1, mu2, mu3), 
    mu=c(0, 0, 0),              # Mean vector 0
  # Sigma=cbind(c(sigma1^2,cov12,cov13), c(cov12,sigma2^2,cov23), c(cov13,cov23,sigma3^2)),
    Sigma=cbind(c(1,cor12,cor13), c(cor12,1,cor23), c(cor13,cor23,1)),     # Correlation matrix, instead of covariance matrix      
    empirical=T)
  
  u <- pnorm(prosp_mvr_emp)                   # This is the trick to transform normal distr -> uniform           
  
  # Choose the marginals
  x1 <- qunif(u[,1], min1, max1)
  x2 <- qunif(u[,2], min2, max2)
  x3 <- qunif(u[,3], min3, max3)
  
  df <- cbind(x1, x2, x3)
  colnames(df) <- c("Cab", "Cw", "Cm")
  
  #  Need to remove negative values 
  posnev <- df[,1]*df[,2]*df[,3]
  df_pos <- df[which(posnev > 0), ]
  
  df_pos <- as_tibble(df_pos)
  return(df_pos)
}


#################################################################################### Add N distribution
leaf.bio.sim.cop.addN <- function(n = 2500, leaf.data, 
                             mean, min, max, cor) {
  
  leaf.bio.mean <- leaf.data %>%  dplyr::filter(Group == mean) %>%  
    dplyr::select(N, Cab, Car, Cw, Cm) %>% mutate(Car = as.numeric(Car))
  
  leaf.bio.min <- leaf.data %>%  dplyr::filter(Group == min) %>%  
    dplyr::select(N, Cab, Car, Cw, Cm) %>% mutate(Car = as.numeric(Car))
  
  leaf.bio.max <- leaf.data %>%  dplyr::filter(Group == max) %>%  
    dplyr::select(N, Cab, Car, Cw, Cm) %>% mutate(Car = as.numeric(Car))
  
  # Mean, min, and max of  1.Cab, 2.Cw, 3.Cm, 4.N
  mu1 <- leaf.bio.mean$Cab; min1 <- leaf.bio.min$Cab; max1 <- leaf.bio.max$Cab
  mu2 <- leaf.bio.mean$Cw; min2 <- leaf.bio.min$Cw; max2 <- leaf.bio.max$Cw
  mu3 <- leaf.bio.mean$Cm; min3 <- leaf.bio.min$Cm; max3 <- leaf.bio.max$Cm
  mu4 <- leaf.bio.mean$N; min4 <- leaf.bio.min$N; max4 <- leaf.bio.max$N
  
  # Correlations and covariances
  cor12 <- cor[1] # ; cov12 <- cor12*sigma1*sigma2
  cor23 <- cor[2] # ; cov23 <- cor23*sigma2*sigma3
  cor13 <- cor[3] # ; cov13 <- cor13*sigma1*sigma3
  
  cor14 <- cor[4]
  cor24 <- cor[5]
  cor34 <- cor[6]
  
  set.seed(123)
  prosp_mvr_emp <- mvrnorm(n,   # This is the start i.e., multivariate normal distributions with mean 0              
                           # mu=c(mu1, mu2, mu3), 
                           mu=c(0, 0, 0, 0),              # Mean vector 0
                           # Sigma=cbind(c(sigma1^2,cov12,cov13), c(cov12,sigma2^2,cov23), c(cov13,cov23,sigma3^2)),
                           Sigma=cbind(c(1,cor12,cor13,cor14), c(cor12,1,cor23,cor24), c(cor13,cor23,1,cor34), c(cor14,cor24,cor34,1)),     # Correlation matrix, instead of covariance matrix      
                           empirical=T)
  
  u <- pnorm(prosp_mvr_emp)                   # This is the trick to transform normal distr -> uniform           
  
  # Choose the marginals
  x1 <- qunif(u[,1], min1, max1)
  x2 <- qunif(u[,2], min2, max2)
  x3 <- qunif(u[,3], min3, max3)
  x4 <- qunif(u[,4], min4, max4)
  
  df <- cbind(x1, x2, x3, x4)
  colnames(df) <- c("Cab", "Cw", "Cm", "N")
  
  #  Need to remove negative values 
  posnev <- df[,1]*df[,2]*df[,3]*df[,4]
  df_pos <- df[which(posnev > 0), ]
  
  df_pos <- as_tibble(df_pos)
  return(df_pos)
}
