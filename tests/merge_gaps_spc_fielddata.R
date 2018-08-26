require(readr)

# Field data --------------------------------------------------------------
field_data <- read_csv2("data/forest/LAI_Malaysia_NoDoublon.csv")


# Landsat reflectance ---------------------------------------------------------
spc <- read_csv2("results/SAFE_spc_3scenes_108plots.csv")


# Gaps --------------------------------------------------------------------
gaps_min <- read_csv2("results/SAFE_gaps_expMin.csv")
gaps_auto <- read_csv2("results/SAFE_gaps_expAuto.csv")
gaps_plus <- read_csv2("results/SAFE_gaps_expPlus.csv")


# Use expMin, except some plots with clouds problem
# plot 745, 744, 740 AUTO
# plot 759 PLUS
gaps <- gaps_min
gaps[gaps$SampleID %in% c(745, 744, 740), ] <- gaps_auto[gaps_auto$SampleID %in% c(745, 744, 740), ]
gaps[gaps$SampleID == 759, ] <- gaps_plus[gaps_plus$SampleID == 759, ]




# Merge them all ----------------------------------------------------------
gaps_spc_field <- merge(gaps, spc, by = "SampleID")
gaps_spc_field <- merge(gaps_spc_field, field_data, by = "SampleID")
gaps_spc_field <- as_tibble(gaps_spc_field) # merge 1



# Add ancillary data to gaps_spc_field------------------------------------------------------
# Forest quality etc.
plotTraits <- read_csv("data/forest/PlotTraits_deadwoodPaper_Hadi.csv")
plotTraits <- as_tibble(plotTraits) %>% mutate(SampleID = Plot)
gaps_spc_field_anc <- merge(gaps_spc_field, plotTraits, by = "SampleID") # merge 2

# Height max (m)
height_biomass <- read.xlsx("data/forest/LAI_Biomass_Quality_Status_February2015.xlsx", sheetName = "Height_Volume", header = TRUE)
height <- as_tibble(height_biomass) %>% select(Plot, Height_max_m) %>% mutate(SampleID = Plot)
gaps_spc_field_anc <- merge(gaps_spc_field_anc, height, by = "SampleID") # merge 3



# Drop unnecessary columns ------------------------------------------------
drop_cols <- c("X1.x", "X1.y", "SampleType", "SampleDim", "Exposure", "Notes", "Plot.x", "Block.y", "Lat", "Long", "Plot.y")
gaps_spc_field_anc <- gaps_spc_field_anc %>% select(-one_of(drop_cols))


# Save
write.csv2(gaps_spc_field_anc, "results/gaps_spc_field_anc.csv")
 

