## EhmkeData_03_CompareToMesonet.R

source(file.path("code", "paths+packages.R"))
source(file.path("code", "VanGenuchten.R"))

# load data
df_playa <- read_csv(file.path("data", "EhmkeData_Daily.csv")) |> 
  mutate(Year = year(Date),
         DOY = yday(Date))
df_meso <- read_csv(file.path("data", "meteorology", "Mesonet-LaneCo_DailyFromHourly_SoilMoisture_Clean.csv")) |> 
  mutate(Year = year(Date),
         DOY = yday(Date))

# create an NA for visualization
i_NAs <- c(max(which(year(df_playa$Date) == 2017)), max(which(year(df_playa$Date) == 2019)))
df_playa$vwc_12cm[i_NAs] <- NaN
df_playa$vwc_47cm[i_NAs] <- NaN

p_vwc_10cm <-
  ggplot() +
  geom_line(data = subset(df_meso, Year %in% c(2017, 2019, 2021)),
            aes(x = DOY, y = vwc10cm)) +
  geom_line(data = subset(df_playa, Year %in% c(2017, 2019, 2021)),
            aes(x = DOY, y = vwc_12cm), color = "blue") +
  scale_y_continuous(limits = c(0.05, 0.55)) +
  facet_wrap(~ factor(Year)) +
  labs(title = "Blue = Playa @ 12 cm; Black = Mesonet @ 10 cm",
       x = "DOY",
       y = "VWC")

p_vwc_50cm <-
  ggplot() +
  geom_line(data = subset(df_meso, Year %in% c(2017, 2019, 2021)),
            aes(x = DOY, y = vwc50cm)) +
  geom_line(data = subset(df_playa, Year %in% c(2017, 2019, 2021)),
            aes(x = DOY, y = vwc_47cm), color = "blue") +
  scale_y_continuous(limits = c(0.05, 0.55)) +
  facet_wrap(~ factor(Year)) +
  labs(title = "Blue = Playa @ 47 cm; Black = Mesonet @ 50 cm",
       x = "DOY",
       y = "VWC")

(p_vwc_10cm + p_vwc_50cm) +
  plot_layout(ncol = 1)
ggsave(file.path("plots", "Ehmke-Mesonet_SoilMoistureComparison.png"),
       width = 190, height = 120, units = "mm")
