## HourlyBucket_01_RunBucket.R

# load various packages + bucket
source(file.path("code", "paths+packages.R"))
source(file.path("code", "BucketModel.R"))

# load daily meteorological data
df_met <- read_csv(file.path("data", "meteorology", "Mesonet-LaneCo_Hourly_2014-2021_Clean.csv"))

# define parameters
ts <- 1/24              # [days] = timestep of 1 hour
int_depth <- 0.005      # [m] maximum quantity of interception
porosity <- 0.46        # [-] porosity of soil (Salley et al., 0.46 m3/m3)
Ksat <- 5*(86400/1000)  # [m/day] saturated hydraulic conductivity of bucket soils
## Salley et al. (2022): playa Ksat = 1.6e-3 to 7.8e-3 mm/s
##                       interplaya Ksat = 2.3e-3 to 7.2e-3 mm/s
##                       in HYDRUS model, used 6 cm/d at surface

S_field <- 0.3/porosity # refine
S_stress <- 0.3         # refine
S_init <- S_field       # [-] relative soil moisture at initial conditions
z_bucket <- 0.15        # at Mesonet site, 20 cm and 50 cm soil moisture sensors look like clay

# preferential flow
S_open <- S_stress
S_closed <- S_stress
pref_frac <- 0.75  # Castaneda and Garcia-Vera (2007)

# run interplaya bucket model
interplaya_bucket <- bucket_model(precip = df_met$precip_mm/1000, 
                                  runon = NULL,
                                  PET = df_met$ETo_mm/1000, 
                                  ts = ts,
                                  Ksat = Ksat, 
                                  porosity = porosity, 
                                  S_field = S_field, 
                                  S_stress = S_stress, 
                                  S_init = S_init, 
                                  int_depth = int_depth, 
                                  z_bucket = z_bucket,
                                  pref_flow = F,
                                  ponding = F) %>% 
  mutate(date = df_met$Timestamp)

# calculate runon from interplaya to playa
#  runon is equal to runoff from interplaya * (watershed area - playa area)/playa area
#  (any units OK as long as area_watershed and area_playa the same units)
area_watershed <- 438        # [ha] from Salley et al.
area_playa <- (38.9+70.5)/2  # [ha] average of range from Salley et al
runon_mm <- interplaya_bucket$runoff_mm*(area_watershed - area_playa)/area_playa

# run playa bucket
playa_bucket <- bucket_model(precip = df_met$precip_mm /1000, 
                             runon = runon_mm/1000,
                             PET = df_met$ETo_mm/1000, 
                             ts = ts,
                             Ksat = Ksat, 
                             porosity = porosity, 
                             S_field = S_field, 
                             S_stress = S_stress, 
                             S_init = S_init, 
                             int_depth = int_depth, 
                             z_bucket = z_bucket,
                             pref_flow = T,
                             ponding = T,
                             S_open = S_open,
                             S_closed = S_closed,
                             pref_frac = pref_frac) %>% 
  mutate(date = df_met$Timestamp)


### some plots
playa_bucket %>% 
  pivot_longer(-date) %>% 
  ggplot(aes(x = date, y = value)) + 
  geom_line() +
  facet_wrap(~name, scales = "free")

df_bucket %>% 
  pivot_longer(-date) %>% 
  ggplot(aes(x = value)) + 
  geom_histogram() +
  facet_wrap(~name, scales = "free")

## compare simulated and measured VWC
df_vwc_playa <- 
  left_join(playa_bucket, df_met, by = c("date"="Timestamp", "precip_mm")) |> 
  mutate(vwc_bucket = soilMoisture_prc*porosity) |> 
  select(date, precip_mm, starts_with("vwc")) |> 
  pivot_longer(starts_with("vwc"), values_to = "VWC") |> 
  mutate(Year = year(date),
         DOY = yday(date)+hour(date)/24) |> 
  subset(Year >= 2017)
df_vwc_playa$Variable <- 
  factor(df_vwc_playa$name, 
         levels = c("vwc5cm", "vwc10cm", "vwc20cm", "vwc50cm", "vwc_bucket"),
         labels = c("Mesonet, 5 cm", "Mesonet, 10 cm", "Mesonet, 20 cm", "Mesonet, 50 cm", "Bucket Model"))

df_vwc_interplaya <- 
  left_join(interplaya_bucket, df_met, by = c("date"="Timestamp", "precip_mm")) |> 
  mutate(vwc_bucket = soilMoisture_prc*porosity) |> 
  select(date, precip_mm, starts_with("vwc")) |> 
  pivot_longer(starts_with("vwc"), values_to = "VWC") |> 
  mutate(Year = year(date),
         DOY = yday(date)+hour(date)/24) |> 
  subset(Year >= 2017)
df_vwc_interplaya$Variable <- 
  factor(df_vwc_interplaya$name, 
         levels = c("vwc5cm", "vwc10cm", "vwc20cm", "vwc50cm", "vwc_bucket"),
         labels = c("Mesonet, 5 cm", "Mesonet, 10 cm", "Mesonet, 20 cm", "Mesonet, 50 cm", "Bucket Model"))

p_vwc_interplaya <- 
  ggplot(df_vwc_interplaya, aes(x = DOY, y = VWC, color = Variable)) +
  geom_line() +
  facet_wrap(~Year) +
  scale_color_manual(values = c(col.cat.blu, col.cat.grn, col.cat.org, col.cat.yel, "black")) +
  scale_x_continuous(name = "Day of Year", expand = c(0,0)) +
  scale_y_continuous(name = "Volumetric Water Content [m\u00b3/m\u00b3]") +
  labs(title = "Interplaya soil moisture results")

p_vwc_playa <- 
  ggplot(df_vwc_playa, aes(x = DOY, y = VWC, color = Variable)) +
  geom_line() +
  facet_wrap(~Year) +
  scale_color_manual(values = c(col.cat.blu, col.cat.grn, col.cat.org, col.cat.yel, "black")) +
  scale_x_continuous(name = "Day of Year", expand = c(0,0)) +
  scale_y_continuous(name = "Volumetric Water Content [m\u00b3/m\u00b3]") +
  labs(title = "Playa soil moisture results")

p_combo <-
  (p_vwc_interplaya / p_vwc_playa) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
p_combo
ggsave(file.path("plots", "HourlyBucket_CompareVWC.png"),
       p_combo, width = 190, height = 210, units = "mm")
