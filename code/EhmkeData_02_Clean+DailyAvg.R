## EhmkeData_02_Clean+DailyAvg.R
# This script is intended to clean and average relevant data from Ehmke playa.

source(file.path("code", "paths+packages.R"))
source(file.path("code", "VanGenuchten.R"))

# load data
df_raw <- read_csv(file.path("C:/Users", "s947z036", "OneDrive - University of Kansas", 
                             "Research", "Playas", "EhmkeDataFromRandy", "Ehmke_Combined_Data_Trimmed.csv")) |> 
  mutate(Datetime = mdy_hm(Datetime))

# calculate soil moisture from swp using VG function
# parameters from Salley et al SI for silt (top layer)
thetaR <- 0.034
thetaS <- 0.46
alpha <- 1.6 # [m] = 0.016 [1/cm]
n <- 1.37
kPa_to_m <- 1/9.804139432 # conversion factor, swp in kPa to m; 9.8 kPa per m

df_raw$vwc_12cm <- VG_ThetaFromHead(df_raw$swp_kPa_12cm*kPa_to_m, # convert to meters
                                    ThetaR = thetaR, ThetaS = thetaS, alpha = alpha, n = n)
df_raw$vwc_47cm <- VG_ThetaFromHead(df_raw$swp_kPa_47cm*kPa_to_m, # convert to meters
                                    ThetaR = thetaR, ThetaS = thetaS, alpha = alpha, n = n)
df_raw$vwc_96cm <- VG_ThetaFromHead(df_raw$swp_kPa_96cm*kPa_to_m, # convert to meters
                                    ThetaR = thetaR, ThetaS = thetaS, alpha = alpha, n = n)
df_raw$vwc_152cm <- VG_ThetaFromHead(df_raw$swp_kPa_152cm*kPa_to_m, # convert to meters
                                     ThetaR = thetaR, ThetaS = thetaS, alpha = alpha, n = n)

# calculate depth-integrated soil moisture storage
bucket_depth <- 2 # [m] how deep to go
# vwc_12cm = 0 to 0.3 m = 0.3 m
# vwc_47cm = 0.3 to 0.72 m = 0.42 m
# vwc_96cm = 0.72 to 1.24 m = 0.52 m
# vwc_152cm = 1.24 to bucket_depth = bucket_depth - 1.24 m
df_raw$soilMoisture_m <- 
  df_raw$vwc_12cm*0.3 + 
  df_raw$vwc_47cm*0.42 +
  df_raw$vwc_96cm*0.52 +
  df_raw$vwc_152cm*(bucket_depth - 1.24)

# rename
names(df_raw)[names(df_raw) == "soilMoisture_m"] <- paste0("soilMoisture_m_top", bucket_depth, "m")

## inspect
df_raw |> 
  dplyr::select(Datetime, vwc_12cm, vwc_47cm, vwc_96cm, vwc_152cm) |> 
  pivot_longer(-Datetime, names_to = "Depth", values_to = "VWC") |> 
  ggplot(aes(x = Datetime, y = VWC, color = Depth)) + 
  geom_line()

ggplot(df_raw, aes(x = Datetime, y = soilMoisture_m_top2m)) +
  geom_line()

## average to daily
df_d <- 
  df_raw |> 
  mutate(Date = date(Datetime)) |> 
  group_by(Date) |> 
  summarize(across(c(Tair_C, vwc_12cm, vwc_47cm, vwc_96cm, vwc_152cm, starts_with("soilMoisture")), mean),
            rain_mm = sum(rain_mm))

## save output
# hourly
df_raw |> 
  dplyr::select(Datetime, rain_mm, Tair_C, ends_with("cm"), starts_with("soilMoisture")) |> 
  mutate(across(c(ends_with("cm"), starts_with("soilMoisture")), ~ round(.x, digits = 3))) |> 
  write_csv(file.path("data", "EhmkeData_Subdaily.csv"))

df_d |> 
  dplyr::select(Date, rain_mm, Tair_C, ends_with("cm"), starts_with("soilMoisture")) |> 
  mutate(across(c(ends_with("cm"), starts_with("soilMoisture")), ~ round(.x, digits = 3))) |> 
  write_csv(file.path("data", "EhmkeData_Daily.csv"))
