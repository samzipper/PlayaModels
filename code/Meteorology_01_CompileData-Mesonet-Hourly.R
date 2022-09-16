## Meteorology_01_CompileData-Mesonet-Hourly.R
# This script takes annual CSV files of hourly meteorological data downloaded
# from the Lane County mesonet station and compiles them into a single CSV file.

source(file.path("code", "paths+packages.R"))

# find files
met_files <- list.files(path = file.path("data", "meteorology", "raw"), 
                        pattern = "Mesonet-LaneCo_Hourly_",
                        full.names = T)

# loop through and combine
for (i in 1:length(met_files)){
  df_yr <- read_csv(met_files[i],
                    col_types = "Tcdddddddddddddddddd")
  
  if (i == 1){
    df_met <- df_yr
  } else {
    df_met <- bind_rows(df_met, df_yr)
  }
}

# update names with units etc.
df_names <-
  data.frame(oldnames = c("TIMESTAMP", "STATION", "PRESSUREAVG", "TEMP2MAVG", "RELHUM2MAVG", 
                          "VPDEFAVG", "PRECIP", "SRAVG", "SR", "WSPD2MAVG", "VWC5CM", "VWC10CM", 
                          "TEMP2MMIN", "TEMP2MMAX", "VWC20CM", "TEMP10MAVG", "SOILTMP20AVG655", 
                          "SOILTMP5AVG655", "SOILTMP10AVG655", "SOILTMP50AVG655", "VWC50CM", 
                          "PRECIP2"),
             newnames = c("Timestamp", "Station", "pressure_kPa", "tAir2mMean_C", "rh2m_prc", 
                          "vpd_kPa", "precip1_mm", "rs_kW.m2", "rs_MJ.m2", "wspd2m_m.s", "vwc5cm", "vwc10cm", 
                          "tAir2mMin_C", "tAir2mMax_C", "vwc20cm", "tAir10mMean_C", "tSoil20cm_C", 
                          "tSoil5cm_C", "tSoil10cm_C", "tSoil50cm_C", "vwc50cm", 
                          "precip2_mm"))

name_match <- match(names(df_met), df_names$oldnames)
names(df_met)[na.omit(name_match)] <- df_names$newnames[!is.na(name_match)]

## QAQC - manual stuff based on summary
# check summary of data
summary(df_met)

## PRECIPITATION
# precip2 is all 0s or NA - drop this and just use precip1
df_met$precip2_mm <- NULL
names(df_met)[names(df_met) == "precip1_mm"] <- "precip_mm"
#which(is.na(df_met$precip_mm))
# manual inspection of missing timesteps - all are in the midst of long stretches of 0, so set to 0 (via linear interpolation)
df_met$precip_mm <- na.approx(df_met$precip_mm)
#ggplot(df_met, aes(x = Timestamp, y = precip_mm)) + geom_col()

## PRESSURE
#ggplot(df_met, aes(x = Timestamp, y = pressure_kPa)) + geom_line()
# prior to 2015-06-29 01:00:00 - calculate based on station elevation (872 m) using FAO 56 eq. 7
i_bad <- which(df_met$Timestamp == ymd_hms("2015-06-29 01:00:00"))
df_met$pressure_kPa[1:i_bad] <- 101.3*((293-0.0065*872)/293)^5.26
df_met$pressure_kPa <- na.approx(df_met$pressure_kPa) # linear interpolate remaining gaps

## SOLAR RADIATION
# for Penman-Monteith we need MJ/m2, not kW/m2, so drop one column
df_met$rs_kW.m2 <- NULL
#which(is.na(df_met$rs_MJ.m2)) # linear interpolate
df_met$rs_MJ.m2 <- na.approx(df_met$rs_MJ.m2)
#ggplot(df_met, aes(x = Timestamp, y = rs_MJ.m2)) + geom_line()

## TEMPERATURE
# 2m mean - mostly complete
#which(is.na(df_met$tAir2mMean_C)) # linear interpolate
df_met$tAir2mMean_C <- na.approx(df_met$tAir2mMean_C)
#ggplot(df_met, aes(x = Timestamp, y = tAir2mMean_C)) + geom_line()

# 10m mean - complete from mid-2017 onwards
#ggplot(df_met, aes(x = Timestamp, y = tAir10mMean_C)) + geom_line()
# good data starts 2017-06-16 11:00:00
i_bad <- which(df_met$Timestamp == ymd_hms("2017-06-16 10:00:00"))
df_met$tAir10mMean_C[1:i_bad] <- NA
df_met$tAir10mMean_C <- na.approx(df_met$tAir10mMean_C, na.rm = F)
#ggplot(df_met, aes(x = Timestamp, y = tAir10mMean_C)) + geom_line()

# 2m min/max only available for 2 years - delete
#ggplot(df_met, aes(x = Timestamp, y = tAir2mMin_C)) + geom_line()
#ggplot(df_met, aes(x = Timestamp, y = tAir2mMax_C)) + geom_line()
df_met$tAir2mMin_C <- NULL
df_met$tAir2mMax_C <- NULL

## REL HUMIDITY AND VPD
#which(is.na(df_met$rh2m_prc)) # linear interpolate
df_met$rh2m_prc <- na.approx(df_met$rh2m_prc)
#ggplot(df_met, aes(x = Timestamp, y = rh2m_prc)) + geom_line()

#which(is.na(df_met$vpd_kPa))  # linear interpolate
df_met$vpd_kPa <- na.approx(df_met$vpd_kPa)
#ggplot(df_met, aes(x = Timestamp, y = vpd_kPa)) + geom_line()

## WIND SPEED
#which(is.na(df_met$wspd2m_m.s)) # linear interpolate
df_met$wspd2m_m.s <- na.approx(df_met$wspd2m_m.s)
#ggplot(df_met, aes(x = Timestamp, y = wspd2m_m.s)) + geom_line()

## SOIL TEMPERATURE
#df_met %>% 
#  select(Timestamp, starts_with("tSoil")) %>% 
#  pivot_longer(-Timestamp) %>% 
#  ggplot(aes(x = Timestamp, y = value)) +
#  geom_line() +
#  facet_wrap(~ name)
# mostly start in 2017 - start of record look weird so delete before 2017-08-16 15:00:00
i_bad <- which(df_met$Timestamp == ymd_hms("2017-08-16 15:00:00"))
df_met$tSoil5cm_C[1:i_bad] <- NA
df_met$tSoil10cm_C[1:i_bad] <- NA
df_met$tSoil20cm_C[1:i_bad] <- NA
df_met$tSoil50cm_C[1:i_bad] <- NA
# linear interpolate remaining missing values
df_met$tSoil5cm_C <- na.approx(df_met$tSoil5cm_C, na.rm = F)
df_met$tSoil10cm_C <- na.approx(df_met$tSoil10cm_C, na.rm = F)
df_met$tSoil20cm_C <- na.approx(df_met$tSoil20cm_C, na.rm = F)
df_met$tSoil50cm_C <- na.approx(df_met$tSoil50cm_C, na.rm = F)
#df_met %>% 
#  select(Timestamp, starts_with("tSoil")) %>% 
#  pivot_longer(-Timestamp) %>% 
#  ggplot(aes(x = Timestamp, y = value)) +
#  geom_line() +
#  facet_wrap(~ name)

## VWC
#df_met %>% 
#  select(Timestamp, starts_with("vwc")) %>% 
#  pivot_longer(-Timestamp) %>% 
#  ggplot(aes(x = Timestamp, y = value)) +
#  geom_line() +
#  facet_wrap(~ name)
# mostly start in 2017 - start of record look weird so delete before 2017-08-16 15:00:00
i_bad <- which(df_met$Timestamp == ymd_hms("2017-08-16 15:00:00"))
# one other weird timestep in late 2019? just for a single hour. set to NA then linearly interpolate
i_weird <- which.min(df_met$vwc50cm)
df_met[i_weird, c("vwc5cm", "vwc10cm", "vwc20cm", "vwc50cm")] <- NA
df_met$vwc5cm <- na.approx(df_met$vwc5cm, na.rm = F)
df_met$vwc10cm <- na.approx(df_met$vwc10cm, na.rm = F)
df_met$vwc20cm <- na.approx(df_met$vwc20cm, na.rm = F)
df_met$vwc50cm <- na.approx(df_met$vwc50cm, na.rm = F)
#df_met %>% 
#  select(Timestamp, starts_with("vwc")) %>% 
#  pivot_longer(-Timestamp) %>% 
#  ggplot(aes(x = Timestamp, y = value)) +
#  geom_line() +
#  facet_wrap(~ name)

## check NAs
summary(df_met)

## CALCULATE ETo
source(file.path("code", "FAO_PenmanMonteith_Subdaily.R"))

df_met$ETo_mm <- 
  FAO_PenmanMonteith_SubDaily(met_data = data.frame(datetime = df_met$Timestamp, 
                                                    R_s = df_met$rs_MJ.m2,
                                                    P = df_met$pressure_kPa,
                                                    Tair = df_met$tAir2mMean_C,
                                                    VPD = df_met$vpd_kPa,
                                                    U2 = df_met$wspd2m_m.s),
                              elev = 872,
                              lat = 38.509,        # latitude in degrees
                              Lm = 100.615,         # longitude [deg W of Greenwich]
                              Lm.center = 90,  # longitude of center of time zone [deg W of Greenwich] 75, 90, 105, and 120 for Eastern, Central, Rocky Mountain, and Pacific time zones
                              DST = 1)

## INSPECT ETo
# sum to daily and compare to daily ETo extracted directly from mesonet
df_daily <- read_csv(file.path("data", "meteorology", "Mesonet-LaneCo_Daily_2014-2021_Clean.csv")) |>
  mutate(Year = year(Timestamp),
         DOY = yday(Timestamp))

df_hrToDay <-
  df_met |>
  mutate(Year = year(Timestamp),
         DOY = yday(Timestamp)) |> 
  group_by(Year, DOY) |> 
  summarize(ETo_mm_sum = sum(ETo_mm),
            precip_mm_sum = sum(precip_mm))

df_compare <- 
  left_join(df_daily[,c("Year", "DOY", "EToGrass_mm", "EToAlfalfa_mm", "precip_mm")], df_hrToDay[,c("Year", "DOY", "ETo_mm_sum", "precip_mm_sum")], by = c("Year", "DOY"))

ggplot(df_compare, aes(x = EToGrass_mm, y = ETo_mm_sum)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red")

ggplot(df_compare, aes(x = precip_mm, y = precip_mm_sum)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red")

df_compare |> 
  select(Year, DOY, EToGrass_mm, ETo_mm_sum) |> 
  pivot_longer(-c("Year", "DOY")) |> 
  ggplot(aes(x = DOY, y = value, color = name)) +
  geom_line() +
  facet_wrap(~Year)

## save output
first_year <- min(year(df_met$Timestamp))
last_year <- max(year(df_met$Timestamp))
write_csv(df_met, file.path("data", "meteorology", paste0("Mesonet-LaneCo_Hourly_", first_year, "-", last_year, "_Clean.csv")))
