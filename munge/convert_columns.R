gaps_spc_field_anc <- read_csv2("results/gaps_spc_field_anc.csv")
gaps_spc_field_anc <- mutate(gaps_spc_field_anc, scene.Date = as.character(scene.Date))
gaps_spc_field_anc <- gaps_spc_field_anc %>% 
  mutate(ForestQuality = 
           dplyr::if_else(ForestQuality == "1", "Very poor", 
                          dplyr::if_else(ForestQuality == "2", "Poor", 
                                         dplyr::if_else(ForestQuality == "3", "OK",
                                                        dplyr::if_else(ForestQuality == "4", "Good", "Very good")))))
