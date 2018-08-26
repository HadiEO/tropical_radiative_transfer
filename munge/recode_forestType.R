gaps_spc_field_anc <- gaps_spc_field_anc %>% mutate(Type.new = Type) 


gaps_spc_field_anc <- gaps_spc_field_anc %>% 
  mutate(Type.new = dplyr::if_else(Type == "Salvage Logged" & ForestQuality %in% c("Very poor", "Poor"),
                                   "Salvage Logged stage 2", Type))

gaps_spc_field_anc <- gaps_spc_field_anc %>% 
  mutate(Type.new = dplyr::if_else(Type == "Salvage Logged" & !ForestQuality %in% c("Very poor", "Poor"),
                                   "Salvage Logged stage 1", Type.new))


# gaps_spc_field_anc %>%  dplyr::select(ForestQuality, Type, Type.new) %>% View  

