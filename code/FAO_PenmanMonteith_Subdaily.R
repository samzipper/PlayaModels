## FAO_PenmanMonteith_SubDaily.R
# Goal: calculate Penman-Monteith reference ET (ETo) as described in FAO Paper #56 at an hourly timestep.
# This is based on the SubDailyPET.m script by Eric Booth, adapted for R.
#
# Reference: Allen, R., Pereira, L.S., Raes, D. and Smith, M., 1998. Crop 
# evapotranspiration: Guidelines for computing crop requirements, FAO, 
# Rome, Italy. http://www.fao.org/docrep/X0490E/x0490e00.htm
#
# met_data should be a vector of: datetime, R_s [MJ m-2 d-1], P [kPa], Tair [C], VPD [kPa], U2 [m/s]

FAO_PenmanMonteith_SubDaily <- function(met_data, 
                                        elev,       # elevation [m]
                                        lat,        # latitude in degrees
                                        Lm,         # longitude [deg W of Greenwich]
                                        Lm.center,  # longitude of center of time zone [deg W of Greenwich] 75, 90, 105, and 120 for Eastern, Central, Rocky Mountain, and Pacific time zones
                                        DST         # daylight savings time? [1=yes]
){
  
  library(lubridate)
  
  # calculate some derived variables
  DOY <- yday(met_data$datetime)
  time_step <- as.numeric(difftime(met_data$datetime[2], met_data$datetime[1], units = "days"))
  lat <- (pi/180)*lat        # convert latitude to radians
  Lz <- Lm.center - DST*15   # center of timezone longitude [deg W of Greenwich]
  
  # calculate ea [kPa]
  ea <- 0.6108*exp((17.502*met_data$Tair)/(met_data$Tair+240.97)) - met_data$VPD
  
  # make sure no wind speed < 0
  met_data$U2[met_data$U2<0] <- 0
  
  # do some work with the timesteps
  data_1hr <- round((1/24)/time_step)  # number of timesteps in 1 hour
  data_3hr <- round((3/24)/time_step)  # number of timsteps in 3 hours
  
  # Determine psychrometric constant
  ca <- 1.005e-3 # heat capacity of air in [MJ/kg/K]
  lambda_v <- 2.50 - 2.36e-3*met_data$Tair
  psy<- ca*met_data$P/(0.622*lambda_v)
  
  ## Determine air humidity values
  # slope of saturation vapor pressure curve at air temperature
  delta <- 4098*0.6108*exp(17.27*met_data$Tair/(met_data$Tair+237.3))/(met_data$Tair+237.3)^2 # eqn 13
  
  ## Determination of extraterrestrial radiation, R_a
  # seasonal correction for solar time
  b <- 2*pi*(DOY-81)/364 # eqn 33
  Sc <- 0.1645*sin(2*b)-0.1255*cos(b)-0.025*sin(b) # eqn 32
  # solar time angle at midpoint of period
  t <- (DOY-floor(DOY))*24 + time_step*24/2
  w <- pi/12*((t+0.06667*(Lz-Lm)+Sc)-12) # eqn 31
  # solar time angles at beginning and end of the period
  t1 <- time_step*24 # length of period in hours
  w1 <- w-pi*t1/24 # eqn 29
  w2 <- w+pi*t1/24 # eqn 30
  # inverse relative distance Earth-Sun
  d_r <- 1 + 0.033*cos(2*pi*DOY/365) # eqn 23
  # solar decimation
  del <- 0.409*sin(2*pi*DOY/365-1.39) # eqn 24
  # sunset hour angle
  w_s <- acos(-tan(lat)*tan(del)) # eqn 25
  # extraterrestrial radiation
  R_a <- ((1/(time_step*24))*24*12*60/pi)*0.0820*d_r*((w2-w1)*sin(lat)*sin(del) + cos(lat)*cos(del)*(sin(w2)-sin(w1))) # eqn 28 
  R_a[abs(w)>w_s] <- 0
  
  ## Determination of clear-sky solar radiation, R_so
  R_so <- (0.75 + 2e-5*elev)*R_a    # eqn 37
  
  ## Determination of the Rs/Rso ratio (Rfrac1)
  # the ratio needs to be bookended by 0.33 and 1.0
  Rfrac1 <- met_data$R_s/R_so
  Rfrac1[Rfrac1>1] <- 1
  Rfrac1[Rfrac1<0.33] <- 0.33
  
  ## set nighttime Rfrac based on discussion in the paragraph
  # below eqn 53
  #
  # figure out when is night/day
  night <- matrix(data=TRUE, ncol=1, nrow=length(Rfrac1))  # set all points equal to night
  night[R_so > 0] <- FALSE  #identify daytime points based on extraterrestrial radiation
  
  # initialize a vector stating from what index Rfrac should be selected
  i.Rfrac <- seq(1,length(Rfrac1))
  
  # for all points, determine the index of the most recent day point
  i.last.day <- i.Rfrac
  last.day <- function(i, night.log, i.all){
    out <- max(i.all[which(!night.log & i.all<i)])
    out[out<0] <- NaN
    return(out)
  }
  i.last.day[night] <- lapply(X=i.last.day[night], FUN=last.day, night.log=night, i.all=i.Rfrac)
  i.last.day <- unlist(i.last.day)
  i.Rfrac[night] <- i.last.day[night]-data_3hr
  
  # update Rfrac for night points
  Rfrac1[night] <- Rfrac1[i.Rfrac[night]]
  
  # NaNs will occur for all the points in the night before the first day; just set these equal to 0.5
  Rfrac1[is.na(Rfrac1)] <- 0.5
  
  ## Determination of net incoming solar radiation, R_ns
  albedo <- 0.23 # value for hypothetical grass reference crop
  R_ns <- (1 - albedo)*met_data$R_s  # eqn 38
  
  ## Determination of net outgoing longwave radiation, R_nl
  s <- (4.903e-9)*time_step     # Stefan-Boltzmann constant
  TairK4 <- (met_data$Tair + 273.16)^4
  R_nl <- s*TairK4*(0.34-0.14*met_data$ea^0.5)*(1.35*Rfrac1-0.35) # eqn 39
  
  ## Determination of net radiation, R_n
  R_n <- R_ns - R_nl # eqn 40
  
  ## Determination of soil heat flux, G
  G <- 0.1*R_n     # eqn 45
  G[night] <- 0.5*R_n[night]   # eqn46
  
  ## Calculate grass reference potential evapotranspiration, ETo [mm/hr]
  ETo <- (0.408*delta*(R_n-G)/24+psy*37/(met_data$Tair+273)*met_data$U2*met_data$VPD)/(delta + psy*(1 + 0.34*met_data$U2))   # [mm hr-1] eqn 53
  return(data.frame(datetime=met_data$datetime, 
                    ETo=ETo))
}

#out <- data.frame(julian=met_data$julian, 
#                  ETo=ETo,
#                  R_n=R_n)
#out$DOY <- floor(out$julian)
#out.day <- dplyr::summarize(group_by(out, DOY),
#                            ETo = sum(ETo)/4,
#                            R_n = mean(R_n))
#
#qplot(out.day$DOY, out.day$ETo)
#qplot(out.day$DOY, out.day$R_n/0.0864)
