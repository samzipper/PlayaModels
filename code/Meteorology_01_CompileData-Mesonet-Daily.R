## Meteorology_01_CompileData-Mesonet-Daily.R
# This script takes annual CSV files of daily meteorological data downloaded
# from the Lane County mesonet station and compiles them into a single CSV file.

source(file.path("code", "paths+packages.R"))

# find files
met_files <- list.files(path = file.path("data", "meteorology", "raw"), 
                        pattern = "Mesonet-LaneCo_Daily_",
                        full.names = T)

# loop through and combine
for (i in 1:length(met_files)){
  df_yr <- read_csv(met_files[i],
                    col_types = "Dcddddddddddddd")
  
  if (i == 1){
    df_met <- df_yr
  } else {
    df_met <- rbind(df_met, df_yr)
  }
}

## QAQC
# check NAs
summary(df_met)

# inspect and gap-fill columns manually
ggplot(df_met, aes(x = Timestamp, y = tAirMax_C)) +
  geom_line()

which(is.na(df_met$tAirMax_C)) # only isolated dates - use linear interpolation
df_met$tAirMax_C <- na.approx(df_met$tAirMax_C)
ggplot(df_met, aes(x = Timestamp, y = tAirMax_C)) +
  geom_line()

which(is.na(df_met$tAirMin_C)) # only isolated dates - use linear interpolation
df_met$tAirMin_C <- na.approx(df_met$tAirMin_C)
ggplot(df_met, aes(x = Timestamp, y = tAirMin_C)) +
  geom_line()

which(df_met$tAirMin_C > df_met$tAirMax_C)

which(is.na(df_met$precip_mm)) # no gap-filling needed
ggplot(df_met, aes(x = Timestamp, y = precip_mm)) +
  geom_col()

which(is.na(df_met$EToGrass_mm)) # only isolated dates - use linear interpolation
df_met$EToGrass_mm <- na.approx(df_met$EToGrass_mm)
ggplot(df_met, aes(x = Timestamp, y = EToGrass_mm)) +
  geom_col()

which(is.na(df_met$EToAlfalfa_mm)) # only isolated dates - use linear interpolation
df_met$EToAlfalfa_mm <- na.approx(df_met$EToAlfalfa_mm)
ggplot(df_met, aes(x = Timestamp, y = EToAlfalfa_mm)) +
  geom_col()

# check NAs
summary(df_met)

## save output
first_year <- min(year(df_met$Timestamp))
last_year <- max(year(df_met$Timestamp))
write_csv(df_met, file.path("data", "meteorology", paste0("Mesonet-LaneCo_Daily_", first_year, "-", last_year, "_Clean.csv")))
