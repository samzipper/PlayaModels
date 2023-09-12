## EhmkeData_03_CompareToMesonet.R

source(file.path("code", "paths+packages.R"))
source(file.path("code", "VanGenuchten.R"))

# load data
#df_playa <- read_csv(file.path("data", "EhmkeData_Subdaily.csv"))
#df_meso <- read_csv(file.path("data", "meteorology", "Mesonet-LaneCo_Hourly_2014-2021_Clean.csv"))

## test compare other VG parameters
# calculate soil moisture from swp using VG function
# parameters from Salley et al SI for silt (top layer)
thetaR <- 0.095 # Salley: 0.034
thetaS <- 0.46  # Salley: 0.46
alpha <- 1.9    # Salley: 1.6 [m] = 0.016 [1/cm]
n <- 1.31       # Salley: 1.37

kPa_to_m <- 1/9.804139432 # conversion factor, swp in kPa to m; 9.8 kPa per m

df_playa$vwc_12cm <- VG_ThetaFromHead(df_playa$swp_kPa_12cm*kPa_to_m, # convert to meters
                                      ThetaR = thetaR, ThetaS = thetaS, alpha = alpha, n = n)
df_playa$vwc_47cm <- VG_ThetaFromHead(df_playa$swp_kPa_47cm*kPa_to_m, # convert to meters
                                      ThetaR = thetaR, ThetaS = thetaS, alpha = alpha, n = n)

# create an NA for visualization
i_NAs <- c(max(which(year(df_playa$Datetime) == 2017)), max(which(year(df_playa$Datetime) == 2019)))
df_playa$vwc_12cm[i_NAs] <- NaN
df_playa$vwc_47cm[i_NAs] <- NaN

p_vwc_10cm <-
  ggplot() +
  geom_line(data = subset(df_meso, Timestamp >= min(df_playa$Datetime) & 
                            Timestamp <= max(df_playa$Datetime)),
            aes(x = Timestamp, y = vwc10cm)) +
  geom_line(data = df_playa,
            aes(x = Datetime, y = vwc_12cm), color = "blue") +
  scale_y_continuous(limits = c(0.05, 0.55)) +
  labs(title = "Blue = Playa @ 12 cm; Black = Mesonet @ 10 cm",
       subtitle = paste0("thetaR = ", thetaR, ", thetaS = ", thetaS, ", alpha = ", alpha, ", n = ", n),
       x = "Datetime",
       y = "VWC")

p_vwc_50cm <-
  ggplot() +
  geom_line(data = subset(df_meso, Timestamp >= min(df_playa$Datetime) & 
                            Timestamp <= max(df_playa$Datetime)),
            aes(x = Timestamp, y = vwc50cm)) +
  geom_line(data = df_playa,
            aes(x = Datetime, y = vwc_47cm), color = "blue") +
  scale_y_continuous(limits = c(0.05, 0.55)) +
  labs(title = "Blue = Playa @ 47 cm; Black = Mesonet @ 50 cm",
       subtitle = paste0("thetaR = ", thetaR, ", thetaS = ", thetaS, ", alpha = ", alpha, ", n = ", n),
       y = "VWC")

(p_vwc_10cm + p_vwc_50cm) +
  plot_layout(ncol = 1)