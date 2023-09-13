## EhmkeData_01_Load+Inspect.R

source(file.path("code", "paths+packages.R"))

# load Van Genuchten function
source(file.path("code", "VanGenuchten.R"))

# load data
df_raw <- read_csv(file.path("C:/Users", "s947z036", "OneDrive - University of Kansas", 
                             "Research", "Playas", "EhmkeDataFromRandy", "Ehmke_Combined_Data_Trimmed.csv")) |> 
  mutate(Datetime = mdy_hm(Datetime))

# add a blank NaN row between time periods - for graphing
i_chunk1end <- max(which(year(df_raw$Datetime) == 2017))
i_chunk2end <- max(which(year(df_raw$Datetime) == 2019))
df_raw[i_chunk1end, 2:14] <- NaN
df_raw[i_chunk2end, 2:14] <- NaN

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
  geom_line()

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

ggplot(df_raw, aes(x = Datetime, y = soilMoisture_m)) +
  geom_line()

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

# add swp plot
df_swp <- 
  df_raw |> 
  dplyr::select(Datetime, starts_with("swp_"))

# calculate absolute values
df_swp[,2:5] <- abs(df_swp[,2:5])

df_swp<-xts(df_swp, order.by=df_swp$Datetime)
df_swp<-df_swp[,-1]

m_swp <- 
  dygraph(df_swp) %>%
  dyRangeSelector() %>%
  dyLegend() %>%
  dyOptions(strokeWidth = 1.5) %>%
  dyOptions(labelsUTC = TRUE) %>%
  dyHighlight(highlightCircleSize = 5,
              highlightSeriesBackgroundAlpha = 0.2,
              hideOnMouseOut = FALSE) %>%
  dyAxis("y", label = "abs(SWP)", logscale = T)
m_swp
htmlwidgets::saveWidget(m_swp, 'docs//Playa_SWPdata.html')
# see at: https://samzipper.github.io/PlayaModels/Playa_SWPdata.html
