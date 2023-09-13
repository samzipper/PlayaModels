## Bucket_01_RunBucket.R
# This script runs the bucket model and generates some plots. 
# The user can choose the timestep (daily or hourly) using the 'ts' variable at the top of the script.


# Workspace prep ----------------------------------------------------------

## load various packages + bucket
source(file.path("code", "paths+packages.R"))
source(file.path("code", "BucketModel.R"))

## CHOOSE TIMESTEP
ts <- 1  # [days] options are 1 (daily model) or 1/24 (hourly model)

## prep data and parameters
# load meteorological data
if (ts == 1){
  df_met <- 
    read_csv(file.path("data", "meteorology", "Mesonet-LaneCo_Daily_2014-2021_Clean.csv")) |> 
    mutate(Year = year(Timestamp)) |> 
    rename(ETo_mm = EToGrass_mm) |> 
    select(Timestamp, Year, ETo_mm, precip_mm)
  
  # for daily, there are two ETo options: grass or alfalfa
  # need to rename one of them ETo_mm so it gets pulled in to model
  names(df_met)[names(df_met) == "EToGrass_mm"] <- "ETo_mm"
  
  # subset to relevant columns
  
} else if (ts == 1/24){
  df_met <- 
    read_csv(file.path("data", "meteorology", "Mesonet-LaneCo_Hourly_2014-2021_Clean.csv")) |> 
    mutate(Year = year(Timestamp)) |> 
    select(Timestamp, Year, ETo_mm, precip_mm)
}

# define parameters
int_depth <- 0.005      # [m] maximum quantity of interception
vwc_porosity <- 0.46    # [m3/m3] porosity of soil (Salley et al., 0.46 m3/m3)
vwc_residual <- 0.07    # [m3/m3] minimum volumetric water content of soil (0.07 based on inspection of Mesonet data)
Ksat <- 0.35            # [m/day] saturated hydraulic conductivity of bucket soils
## Salley et al. (2022): playa Ksat = 1.6e-3 to 7.8e-3 mm/s = 0.14 to 0.67 m/d
##                       interplaya Ksat = 2.3e-3 to 7.2e-3 mm/s = 0.20 to 0.62 m/d
##                       in HYDRUS model, used 6 cm/d at surface = 0.06 m/d
## convert mm/s to m/d: *86400/1000
## convert cm/d to m/d: /100

S_field  <- 0.16/vwc_porosity # based on inspection of Mesonet soil moisture data
S_stress <- 0.3         # refine
S_init <- S_field       # [-] relative soil moisture at initial conditions
S_resid  <- vwc_residual/vwc_porosity # [-] relative soil moisture at residual
z_bucket <- 1.5         # [m] bucket depth - results highly sensitive to this parameter

# preferential flow
S_open <- S_stress
S_closed <- S_stress
pref_frac <- 0.75  # Castaneda and Garcia-Vera (2007)

## set up spin-up
spinup_length_yrs <- 10
yr_options <- unique(df_met$Year)
set.seed(1)
spinup_yrs <- sample(yr_options, spinup_length_yrs, replace = T)

for (y in 1:length(spinup_yrs)){
  yr <- spinup_yrs[y]
  df_met_spinup_y <- subset(df_met, Year == yr)
  if (y == 1){
    df_met_spinup <- df_met_spinup_y
  } else {
    df_met_spinup <- bind_rows(df_met_spinup, df_met_spinup_y)
  }
}
spinup_ts <- dim(df_met_spinup)[1]
if (ts == 1){
  spinup_start_datetime <- df_met$Timestamp[1] - days(spinup_ts)  
  spinup_datetime <- seq(spinup_start_datetime, (df_met$Timestamp[1] - days(1)), by = "day")
} else if (ts == 1/24){
  spinup_start_datetime <- df_met$Timestamp[1] - hours(spinup_ts) 
  spinup_datetime <- seq(spinup_start_datetime, (df_met$Timestamp[1] - hours(1)), by = "hour")
}

precip_mm_with_spinup <- c(df_met_spinup$precip_mm, df_met$precip_mm)
ETo_mm_with_spinup <- c(df_met_spinup$ETo_mm, df_met$ETo_mm)

# Run models --------------------------------------------------------------

# run interplaya bucket
interplaya_bucket <- bucket_model(precip = precip_mm_with_spinup/1000, 
                                  runon = NULL,
                                  PET = ETo_mm_with_spinup/1000, 
                                  ts = ts,
                                  Ksat = Ksat, 
                                  porosity = vwc_porosity, 
                                  S_field = S_field, 
                                  S_stress = S_stress, 
                                  S_resid = S_resid, 
                                  S_init = S_init, 
                                  int_depth = int_depth, 
                                  z_bucket = z_bucket,
                                  use_Kunsat = F,
                                  pref_flow = F,
                                  ponding = F) |> 
  mutate(timestep = seq(1, length(precip_mm_with_spinup)),
         datetime = c(spinup_datetime, df_met$Timestamp))

# calculate runon from interplaya to playa
#  runon is equal to runoff from interplaya * (watershed area - playa area)/playa area
#  (any units OK as long as area_watershed and area_playa the same units)
area_watershed <- 438        # [ha] from Salley et al.
area_playa <- (38.9+70.5)/2  # [ha] average of range from Salley et al
runon_mm <- interplaya_bucket$runoff_mm*(area_watershed - area_playa)/area_playa

# run playa bucket
playa_bucket <- bucket_model(precip = precip_mm_with_spinup/1000, 
                             runon = runon_mm/1000,
                             PET = ETo_mm_with_spinup/1000, 
                             ts = ts,
                             Ksat = Ksat, 
                             porosity = vwc_porosity, 
                             S_field = S_field, 
                             S_stress = S_stress, 
                             S_resid = S_resid, 
                             S_init = S_init, 
                             int_depth = int_depth, 
                             z_bucket = z_bucket,
                             use_Kunsat = F,
                             pref_flow = T,
                             ponding = T,
                             S_open = S_open,
                             S_closed = S_closed,
                             pref_frac = pref_frac) |> 
  mutate(timestep = seq(1, length(precip_mm_with_spinup)),
         datetime = c(spinup_datetime, df_met$Timestamp))

# plot results ------------------------------------------------------------

## set up for plotting
# remove spinup data
playa_plot <- 
  playa_bucket |> 
  subset(timestep > spinup_ts) |> 
  mutate(soilMoisture_m = soilMoisture_prc*vwc_porosity*z_bucket,
         Year = year(datetime),
         DOY.dec = yday(datetime)+hour(datetime)/24,
         Source = "Model",
         Setting = "Interplaya") |> 
  select(-timestep, -soilMoisture_prc)

interplaya_plot <- 
  interplaya_bucket |> 
  subset(timestep > spinup_ts) |> 
  mutate(soilMoisture_m = soilMoisture_prc*vwc_porosity*z_bucket,
         Year = year(datetime),
         DOY.dec = yday(datetime)+hour(datetime)/24,
         Source = "Model",
         Setting = "Interplaya") |> 
  select(-timestep, -soilMoisture_prc)

# set up label for plot title
ts_label <- ifelse(ts == 1, "Daily", "Hourly")

## playa
playa_plot |> 
  pivot_longer(-c("datetime", "Year", "DOY.dec", "Source", "Setting")) |> 
  ggplot(aes(x = datetime, y = value)) + 
  geom_line() +
  facet_wrap(~name, scales = "free") +
  labs(title = paste0("Playa results, ", ts_label, " ts"))

playa_plot |> 
  pivot_longer(-c("datetime", "Year", "DOY.dec", "Source", "Setting")) |> 
  ggplot(aes(x = value)) + 
  geom_histogram() +
  facet_wrap(~name, scales = "free") +
  labs(title = paste0("Playa results, ", ts_label, " ts"))

## interplaya
interplaya_plot |> 
  pivot_longer(-c("datetime", "Year", "DOY.dec", "Source", "Setting")) |> 
  ggplot(aes(x = datetime, y = value)) + 
  geom_line() +
  facet_wrap(~name, scales = "free") +
  labs(title = paste0("Interplaya results, ", ts_label, " ts"))

interplaya_plot |> 
  pivot_longer(-c("datetime", "Year", "DOY.dec", "Source", "Setting")) |> 
  ggplot(aes(x = value)) + 
  geom_histogram() +
  facet_wrap(~name, scales = "free") +
  labs(title = paste0("Interplaya results, ", ts_label, " ts"))

## compare simulated and measured soil moisture
# load soil moisture data
df_vwc_mesonet <- 
  read_csv(file.path("data", "meteorology", "Mesonet-LaneCo_DailyFromHourly_SoilMoisture_Clean.csv")) |> 
  mutate(Source = "Mesonet",
         Setting = "Interplaya")
df_vwc_ehmke <- 
  read_csv(file.path("data", "EhmkeData_Daily.csv")) |> 
  mutate(Source = "Ehmke",
         Setting = "Playa")

# combine observed soil moisture data
df_vwc_obs <-
  bind_rows(
    dplyr::select(df_vwc_mesonet, Date, soilMoisture_m_top1.5m, Source, Setting),
    dplyr::select(df_vwc_ehmke, Date, soilMoisture_m_top1.5m, Source, Setting)
  ) |> 
  mutate(Type = "Obs")

# get simulated soil moisture data
df_vwc_sim <-
  bind_rows(
    dplyr::select(playa_plot, datetime, soilMoisture_m, Source, Setting),
    dplyr::select(interplaya_plot, datetime, soilMoisture_m, Source, Setting)
  ) |> 
  mutate(Type = "Model")

# combine for comparison




ggplot(df_vwc_obs, aes(x = Date, y = soilMoisture_m_top1.5m, color = Setting)) +
  geom_point()

p_vwc <- 
  ggplot() +
  geom_line(data = df_vwc, aes(x = DOY.dec, y = VWC, color = Variable)) +
  geom_line(data = subset(interplaya_plot, Year >= 2017), aes(x = DOY.dec, y = soilMoisture_vwc), linetype = "dashed") +
  geom_line(data = subset(playa_plot, Year >= 2017), aes(x = DOY.dec, y = soilMoisture_vwc)) +
  facet_wrap(~Year, ncol = 1) +
  scale_color_manual(values = c(col.cat.blu, col.cat.grn, col.cat.org, col.cat.yel)) +
  scale_x_continuous(name = "Day of Year", expand = c(0,0)) +
  scale_y_continuous(name = "Volumetric Water Content [m\u00b3/m\u00b3]") +
  labs(title = "Comparing buckets (black) to soil moisture sensors (colors)",
       subtitle = "Dashed = interplaya bucket, solid = playa bucket")
p_vwc

ggsave(file.path("plots", paste0("Bucket_CompareVWC_", ts_label, ".png")),
       p_vwc, width = 120, height = 210, units = "mm")

# inspect VWC data to get thresholds
ggplot() +
  geom_line(data = df_vwc, aes(x = DOY.dec, y = VWC, color = Variable)) +
  scale_color_manual(values = c(col.cat.blu, col.cat.grn, col.cat.org, col.cat.yel)) +
  facet_wrap(~Year, ncol = 1) +
  geom_hline(yintercept = 0.07, color = col.gray) + # approx minimum
  geom_hline(yintercept = 0.16, color = col.gray) + # approx field capacity
  geom_hline(yintercept = 0.46, color = col.gray)   # porosity (from Salley)
