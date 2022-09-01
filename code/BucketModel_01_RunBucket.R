## BucketModel_01_RunBucket.R

# load various packages + bucket
source(file.path("code", "paths+packages.R"))
source(file.path("code", "BucketModel.R"))

# load daily meteorological data
df_met <- read_csv(file.path("results", "Meteorology_Mesonet-LaneCo_Daily-AllYears_Clean.csv"))

# define parameters
int_depth <- 0.005      # [m] maximum quantity of interception
porosity <- 0.4         # [-] porosity of soil
Ksat <- 1               # [m/day] saturated hydraulic conductivity of bucket soils
S_field <- 0.3/porosity # refine
S_stress <- 0.3         # refine
S_init <- 0.5           # [-] relative soil moisture at initial conditions
z_bucket <- 2           # refine

# run bucket model
df_bucket <- bucket_model(precip = df_met$precip_mm/1000, 
                          PET = df_met$EToGrass_mm/1000, 
                          Ksat = Ksat, 
                          porosity = porosity, 
                          S_field = S_field, 
                          S_stress = S_stress, 
                          S_init = S_init, 
                          int_depth = int_depth, 
                          z_bucket = z_bucket) %>% 
  mutate(date = df_met$Timestamp)

### some plots
df_bucket %>% 
  pivot_longer(-date) %>% 
  ggplot(aes(x = date, y = value)) + 
  geom_line() +
  facet_wrap(~name, scales = "free")

df_bucket %>% 
  pivot_longer(-date) %>% 
  ggplot(aes(x = value)) + 
  geom_histogram() +
  facet_wrap(~name, scales = "free")
