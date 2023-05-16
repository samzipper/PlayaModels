## EhmkeData_01_Load+Inspect.R

source(file.path("code", "paths+packages.R"))

# load Van Genuchten function
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

df_raw$vwc_12cm <- VG_ThetaFromHead(df_raw$swp_kPa_12cm*9.804139432, # convert to meters
                                    ThetaR = thetaR, ThetaS = thetaS, alpha = alpha, n = n)
df_raw$vwc_47cm <- VG_ThetaFromHead(df_raw$swp_kPa_47cm*9.804139432, # convert to meters
                                    ThetaR = thetaR, ThetaS = thetaS, alpha = alpha, n = n)
df_raw$vwc_96cm <- VG_ThetaFromHead(df_raw$swp_kPa_96cm*9.804139432, # convert to meters
                                    ThetaR = thetaR, ThetaS = thetaS, alpha = alpha, n = n)
df_raw$vwc_152cm <- VG_ThetaFromHead(df_raw$swp_kPa_152cm*9.804139432, # convert to meters
                                     ThetaR = thetaR, ThetaS = thetaS, alpha = alpha, n = n)

# inspect
ggplot(df_raw, aes(x = Datetime, y = swp_kPa_152cm)) +
  geom_point()
