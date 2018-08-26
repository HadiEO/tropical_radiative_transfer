rm.qual <- c("Poor", "Very poor")

gaps_spc_field_anc <- gaps_spc_field_anc %>%  dplyr::filter(!ForestQuality %in% rm.qual) # 65 plots