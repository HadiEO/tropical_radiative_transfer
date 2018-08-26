my_prospect <- function(leaf.data = prospect_in, species = "Dipt_mean", N = 1.7, v4 = TRUE) {
  leaf.bio <- leaf.data %>%  filter(Group == species) %>%  dplyr::select(Cab, Car, Cw, Cm) %>% mutate(Car = as.numeric(Car))
  if (v4 == FALSE) {
    if (is.na(leaf.bio$Car)) stop("Car measurement not available")
    leaf.spc <- with(leaf.bio, prospect5(N, Car, Cab, Cw, Cm)) # prospect5 uses Carotenoid as well
  }
  
  leaf.spc <- with(leaf.bio, prospect4(N, Cab, Cw, Cm)) 
  
  leaf.spc$Albedo <- leaf.spc$Reflectance + leaf.spc$Transmittance
  leaf.spc <- as_tibble(leaf.spc)
  return(leaf.spc)
}
