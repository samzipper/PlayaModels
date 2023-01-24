## HourlyBucket_01_RunBucket.R

# load various packages + bucket
source(file.path("code", "paths+packages.R"))
source(file.path("code", "BucketModel.R"))

# load daily meteorological data
df_met <- 
  read_csv(file.path("data", "meteorology", "Mesonet-LaneCo_Hourly_2014-2021_Clean.csv")) |> 
  mutate(Year = year(Timestamp))

# define parameters
ts <- 1/24              # [days] = timestep of 1 hour
int_depth <- 0.005      # [m] maximum quantity of interception
porosity <- 0.46        # [-] porosity of soil (Salley et al., 0.46 m3/m3)
Ksat <- 0.35            # [m/day] saturated hydraulic conductivity of bucket soils
## Salley et al. (2022): playa Ksat = 1.6e-3 to 7.8e-3 mm/s = 0.14 to 0.67 m/d
##                       interplaya Ksat = 2.3e-3 to 7.2e-3 mm/s = 0.20 to 0.62 m/d
##                       in HYDRUS model, used 6 cm/d at surface = 0.06 m/d
## convert mm/s to m/d: *86400/1000
## convert cm/d to m/d: /100

S_field <- 0.3/porosity # refine
S_stress <- 0.3         # refine
S_init <- S_field       # [-] relative soil moisture at initial conditions
z_bucket <- 2.0         # at Mesonet site, 20 cm and 50 cm soil moisture sensors look like clay

# preferential flow
S_open <- S_stress
S_closed <- S_stress
pref_frac <- 0.75  # Castaneda and Garcia-Vera (2007)

# set up spin-up
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
spinup_start_datetime <- df_met$Timestamp[1] - hours(spinup_ts)
spinup_datetime <- seq(spinup_start_datetime, (df_met$Timestamp[1] - hours(1)), by = "hour")
precip_mm_with_spinup <- c(df_met_spinup$precip_mm, df_met$precip_mm)
ETo_mm_with_spinup <- c(df_met_spinup$ETo_mm, df_met$ETo_mm)

# run interplaya bucket model
interplaya_bucket <- bucket_model(precip = precip_mm_with_spinup/1000, 
                                  runon = NULL,
                                  PET = ETo_mm_with_spinup/1000, 
                                  ts = ts,
                                  Ksat = Ksat, 
                                  porosity = porosity, 
                                  S_field = S_field, 
                                  S_stress = S_stress, 
                                  S_init = S_init, 
                                  int_depth = int_depth, 
                                  z_bucket = z_bucket,
                                  use_Kunsat = F,
                                  pref_flow = F,
                                  ponding = F) %>% 
  mutate(timestep = seq(1, length(precip_mm_with_spinup)),
         datetime = c(spinup_datetime, df_met$Timestamp))

interplaya_bucket |> 
  select(timestep, infiltration_mm, soilMoisture_prc, runoff_mm) |> 
  pivot_longer(-timestep) |> 
  ggplot(aes(x = timestep, y = value)) +
  facet_wrap(~name, ncol = 1, scales = "free_y") +
  geom_line() +
  geom_vline(xintercept = spinup_ts, color = "red")

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
                             porosity = porosity, 
                             S_field = S_field, 
                             S_stress = S_stress, 
                             S_init = S_init, 
                             int_depth = int_depth, 
                             z_bucket = z_bucket,
                             use_Kunsat = F,
                             pref_flow = T,
                             ponding = T,
                             S_open = S_open,
                             S_closed = S_closed,
                             pref_frac = pref_frac) %>% 
  mutate(timestep = seq(1, length(precip_mm_with_spinup)),
         datetime = c(spinup_datetime, df_met$Timestamp))

### hourly plots
## compare simulated and measured VWC
df_vwc_playa <- 
  playa_bucket |> 
  subset(timestep > spinup_ts) |> 
  left_join(df_met, by = c("datetime"="Timestamp", "precip_mm")) |> 
  mutate(vwc_bucket = soilMoisture_prc*porosity) |> 
  select(datetime, precip_mm, starts_with("vwc")) |> 
  mutate(Year = year(datetime),
         DOY.dec = yday(datetime)+hour(datetime)/24) |> 
  pivot_longer(starts_with("vwc"), values_to = "VWC") |> 
  subset(Year >= 2017)

df_vwc_playa$Variable <- 
  factor(df_vwc_playa$name, 
         levels = c("vwc5cm", "vwc10cm", "vwc20cm", "vwc50cm", "vwc_bucket"),
         labels = c("Mesonet, 5 cm", "Mesonet, 10 cm", "Mesonet, 20 cm", "Mesonet, 50 cm", "Bucket Model"))

df_vwc_interplaya <- 
  left_join(interplaya_bucket, df_met, by = c("datetime"="Timestamp", "precip_mm")) |> 
  mutate(vwc_bucket = soilMoisture_prc*porosity) |> 
  select(datetime, precip_mm, starts_with("vwc")) |> 
  pivot_longer(starts_with("vwc"), values_to = "VWC") |> 
  mutate(Year = year(datetime),
         DOY.dec = yday(datetime)+hour(datetime)/24) |> 
  subset(Year >= 2017)

df_vwc_interplaya$Variable <- 
  factor(df_vwc_interplaya$name, 
         levels = c("vwc5cm", "vwc10cm", "vwc20cm", "vwc50cm", "vwc_bucket"),
         labels = c("Mesonet, 5 cm", "Mesonet, 10 cm", "Mesonet, 20 cm", "Mesonet, 50 cm", "Bucket Model"))

p_vwc_interplaya <- 
  ggplot(df_vwc_interplaya, aes(x = DOY.dec, y = VWC, color = Variable)) +
  geom_line() +
  facet_wrap(~Year) +
  scale_color_manual(values = c(col.cat.blu, col.cat.grn, col.cat.org, col.cat.yel, "black")) +
  scale_x_continuous(name = "Day of Year", expand = c(0,0)) +
  scale_y_continuous(name = "Volumetric Water Content [m\u00b3/m\u00b3]") +
  labs(title = "Interplaya soil moisture results, hourly")

p_vwc_playa <- 
  ggplot(df_vwc_playa, aes(x = DOY.dec, y = VWC, color = Variable)) +
  geom_line() +
  facet_wrap(~Year) +
  scale_color_manual(values = c(col.cat.blu, col.cat.grn, col.cat.org, col.cat.yel, "black")) +
  scale_x_continuous(name = "Day of Year", expand = c(0,0)) +
  scale_y_continuous(name = "Volumetric Water Content [m\u00b3/m\u00b3]") +
  labs(title = "Playa soil moisture results, hourly")

p_combo <-
  (p_vwc_interplaya / p_vwc_playa) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
p_combo
ggsave(file.path("plots", "HourlyBucket_CompareVWC.png"),
       p_combo, width = 190, height = 210, units = "mm")

## playa
playa_bucket %>% 
  pivot_longer(-datetime) %>% 
  ggplot(aes(x = datetime, y = value)) + 
  geom_line() +
  facet_wrap(~name, scales = "free") +
  labs(title = "Playa results, hourly")

playa_bucket %>% 
  pivot_longer(-datetime) %>% 
  ggplot(aes(x = value)) + 
  geom_histogram() +
  facet_wrap(~name, scales = "free") +
  labs(title = "Playa results, hourly")

## interplaya
interplaya_bucket %>% 
  pivot_longer(-datetime) %>% 
  ggplot(aes(x = datetime, y = value)) + 
  geom_line() +
  facet_wrap(~name, scales = "free") +
  labs(title = "Interplaya results, hourly")

interplaya_bucket %>% 
  pivot_longer(-datetime) %>% 
  ggplot(aes(x = value)) + 
  geom_histogram() +
  facet_wrap(~name, scales = "free") +
  labs(title = "Interplaya results, hourly")

### daily plots
## average to day
interplaya_bucket_day <- 
  interplaya_bucket |> 
  subset(timestep > spinup_ts) |> 
  mutate(Year = year(datetime),
         DOY = floor(yday(datetime)+hour(datetime)/24)) |> 
  group_by(Year, DOY) |> 
  # sum fluxes; average stores
  summarize(precip_mm.d = sum(precip_mm),
            PET_mm.d = sum(PET_mm),
            precipEff_mm.d = sum(precipEff_mm),
            interception_mm.d = sum(interception_mm),
            runoff_mm.d = sum(runoff_mm),
            infiltration_mm.d = sum(infiltration_mm),
            ET_mm.d = sum(ET_mm),
            leakage_mat_mm.d = sum(leakage_mat_mm),
            soilMoisture_prc.d = mean(soilMoisture_prc),
            date = mean(datetime))

playa_bucket_day <- 
  playa_bucket |> 
  subset(timestep > spinup_ts) |> 
  mutate(Year = year(datetime),
         DOY = floor(yday(datetime)+hour(datetime)/24)) |> 
  group_by(Year, DOY) |> 
  # sum fluxes; average stores
  summarize(precip_mm.d = sum(precip_mm),
            PET_mm.d = sum(PET_mm),
            precipEff_mm.d = sum(precipEff_mm),
            interception_mm.d = sum(interception_mm),
            pond_depth_mm.d = mean(pond_depth_mm),
            infiltration_mm.d = sum(infiltration_mm),
            ET_mm.d = sum(ET_mm),
            leakage_mat_mm.d = sum(leakage_mat_mm),
            leakage_pref_mm.d = sum(leakage_pref_mm),
            soilMoisture_prc.d = mean(soilMoisture_prc),
            date = mean(datetime))

## plot: playa
playa_bucket_day %>% 
  pivot_longer(-c("date", "Year", "DOY")) %>% 
  ggplot(aes(x = date, y = value)) + 
  geom_line() +
  facet_wrap(~name, scales = "free") +
  labs(title = "Playa results, averaged to daily")

playa_bucket_day %>% 
  pivot_longer(-c("date", "Year", "DOY")) %>% 
  ggplot(aes(x = value)) + 
  geom_histogram() +
  facet_wrap(~name, scales = "free") +
  labs(title = "Playa results, averaged to daily")

## plot: interplaya
interplaya_bucket_day %>% 
  pivot_longer(-c("date", "Year", "DOY")) %>% 
  ggplot(aes(x = date, y = value)) + 
  geom_line() +
  facet_wrap(~name, scales = "free") +
  labs(title = "Interplaya results, averaged to daily")

interplaya_bucket_day %>% 
  pivot_longer(-c("date", "Year", "DOY")) %>% 
  ggplot(aes(x = value)) + 
  geom_histogram() +
  facet_wrap(~name, scales = "free") +
  labs(title = "Interplaya results, averaged to daily")