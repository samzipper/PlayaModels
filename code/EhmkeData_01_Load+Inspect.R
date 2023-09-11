## EhmkeData_01_Load+Inspect.R

source(file.path("code", "paths+packages.R"))

# load Van Genuchten function
source(file.path("code", "VanGenuchten.R"))

# load data
df_raw <- read_csv(file.path("C:/Users", "s947z036", "OneDrive - University of Kansas", 
                             "Research", "Playas", "EhmkeDataFromRandy", "Ehmke_Combined_Data_Trimmed.csv")) |> 
  mutate(Datetime = mdy_hm(Datetime))

# add a blank NaN row between the two time periods - for graphing
df_blank <- df_raw[1,]
df_blank$Datetime <- mdy_hm("6/10/2019 15:00")
df_blank[,2:18] <- NaN

df_raw <- bind_rows(df_raw, df_blank) |> 
  arrange(Datetime)

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

# inspect - ggplot
df_raw |> 
  dplyr::select(Datetime, vwc_12cm, vwc_47cm, vwc_96cm, vwc_152cm) |> 
  pivot_longer(-Datetime, names_to = "Depth", values_to = "VWC") |> 
  ggplot(aes(x = Datetime, y = VWC, color = Depth)) + 
  geom_line()

# inspect - ggplot
df_raw |> 
  dplyr::select(Datetime, swp_kPa_12cm, swp_kPa_47cm, swp_kPa_96cm, swp_kPa_152cm) |> 
  pivot_longer(-Datetime, names_to = "Depth", values_to = "SWP") |> 
  ggplot(aes(x = Datetime, y = SWP, color = Depth)) + 
  geom_line() +
  scale_y_log10()


# inspect - dygraph
library(dygraphs)
library(xts)

#format data
df_xts <- df_raw |> 
  dplyr::select(Datetime, vwc_12cm, vwc_47cm, vwc_96cm, vwc_152cm)
  
df_xts<-xts(df_xts, order.by=df_xts$Datetime)
df_xts<-df_xts[,-1]

#Plot
m <- 
  dygraph(df_xts) %>%
  dyRangeSelector() %>%
  dyLegend() %>%
  dyOptions(strokeWidth = 1.5) %>%
  dyOptions(labelsUTC = TRUE) %>%
  dyHighlight(highlightCircleSize = 5,
              highlightSeriesBackgroundAlpha = 0.2,
              hideOnMouseOut = FALSE) %>%
  dyAxis("y")
m

htmlwidgets::saveWidget(m, 'docs//Playa_SoilMoistureData.html')
# see at: https://samzipper.github.io/PlayaModels/Playa_SoilMoistureData.html

# add precip plot
df_precip <- 
  df_raw |> 
  dplyr::select(Datetime, rain_mm)

df_precip<-xts(df_precip, order.by=df_precip$Datetime)
df_precip<-df_precip[,-1]

m_precip <- 
  dygraph(df_precip) %>%
  dyRangeSelector() %>%
  dyLegend() %>%
  dyOptions(strokeWidth = 1.5) %>%
  dyOptions(labelsUTC = TRUE) %>%
  dyHighlight(highlightCircleSize = 5,
              highlightSeriesBackgroundAlpha = 0.2,
              hideOnMouseOut = FALSE) %>%
  dyAxis("y")
m_precip
htmlwidgets::saveWidget(m_precip, 'docs//Playa_PrecipData.html')
# see at: https://samzipper.github.io/PlayaModels/Playa_PrecipData.html