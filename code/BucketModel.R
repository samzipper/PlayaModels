## BucketModel.R
# This is a function that creates a daily ecohydrological 'bucket model' soil water balance accounting model.
#
# Daily input data (vectors of same length):
# - precip = precipitation [m/day]
# - PET = potential ET [m/day]
# Static input data:
# - Ksat = saturated hydraulic conductivity of soils [m/day]
# - porosity = porosity of soil [-]
# - S_field = relative soil moisture at field capacity, where relative soil moisture is defined as Vwater/(Vair + Vwater) [-]
#     If you have an estimate of Sy, you can estimate S_field as (Sy/porosity)
# - S_stress = relative soil moisture below which water stress occurs [-]
# - S_init = relative soil moisture at initial conditions [-]
# - int_depth = maximum quantity of interception [m]
# - z_bucket = depth of bucket [m] - assumed <= root depth in playas

bucket_model <- function(precip, PET, Ksat, porosity, S_field, S_stress, S_init, int_depth, z_bucket){
  
  # Input data checks --------------------------------
  if (length(precip) != length(PET)) stop("precip and PET vector lengths differ")
  if (sum(is.na(c(precip, PET))) > 0) stop("NA values in precip and/or PET input data")
  
  # Create empty vectors to hold your output --------------------------------
  # There should be one empty vector for each desired model output. For
  # now, they will only contain NaNs, and we will fill them in as we go.
  
  n_days <- length(precip) # [days] desired length of model simulation
  precip_eff   <- rep(NA, length = n_days)  # effective precipitation
  interception <- rep(NA, length = n_days)
  runoff       <- rep(NA, length = n_days)
  infiltration <- rep(NA, length = n_days)
  ET           <- rep(NA, length = n_days)
  leakage      <- rep(NA, length = n_days)
  soil.moisture<- rep(NA, length = n_days)  # remember, this is relative soil moisture = Vwater/(Vair + Vwater) = theta/porosity 
  
  # Run the model -----------------------------------------------------------
  
  # First, set up your initial conditions
  soil.moisture[1] <- S_init  # S_init is a user parameter defined above
  
  # set all other fluxes to 0 at the first timestep
  precip_eff[1] <- 0
  interception[1] <- 0
  runoff[1] <- 0
  infiltration[1] <- 0
  ET[1] <- 0
  leakage[1] <- 0
  
  # Partition rainfall between effective precipitation and interception 
  # for each day based on int_depth
  for (i in 2:n_days){  # start on day 2 because of your initial conditions
    if (precip[i] < int_depth){
      # if there is less precip than interception depth (int_depth), it should all go to interception. 
      interception[i] <- precip[i]
      
      # because it all went to interception, there is no effective precipitation, infiltration, runoff
      precip_eff[i] <- 0
      infiltration[i] <- 0
      runoff[i] <- 0
      
    } else {
      # if there is more precipitation than interception depth, the 
      # quantity int_depth will be intercepted, and the rest will reach the ground
      interception[i] <- int_depth
      
      # the rest of rainfall should be effective precipitation
      precip_eff[i] <- precip[i] - interception[i]
      
      # divide effective precipitation into infiltration and runoff
      
      # calculate the maximum possible infiltration based on previous soil moisture storage
      infiltration.max <- (porosity*z_bucket - soil.moisture[i-1]*porosity*z_bucket)
      
      # decide whether effective precipitation infiltrates or runs off
      if (precip_eff[i] <= infiltration.max){
        # infiltrate all
        infiltration[i] <- precip_eff[i]
        runoff[i] <- 0
      } else {
        # fill the bucket, rest is runoff
        infiltration[i] <- infiltration.max
        runoff[i] <- precip_eff[i] - infiltration[i]
      }
    }
    
    # calculate leakage out of bottom
    if (soil.moisture[i-1] < S_field) {
      # if relative soil moisture is less than field capacity, there is no leakage
      leakage[i] <- 0
    } else {
      # if we are above field capacity, linearly scale from 0-Ksat
      leakage[i] <- Ksat*(soil.moisture[i-1] - S_field)/(1-S_field)
      
      # make sure that you don't remove enough water to reduce soil
      # moisture in bucket below field capacity
      if (leakage[i] >= (soil.moisture[i-1] - S_field)*porosity*z_bucket){
        leakage[i] <- (soil.moisture[i-1] - S_field)*porosity*z_bucket
      }
    }
    
    # calculate ET as a function of soil moisture at previous timestep
    if (soil.moisture[i-1] > S_stress){
      # if relative soil moisture is larger than S_stress ET=PET
      ET[i] <- PET[i]
    } else{
      # if relative soil moisture is less than S_stress, reduce linearly to 0
      ET[i] <- PET[i]*(soil.moisture[i-1])/(S_stress)
      
      # make sure ET is not so much that it drops us below 0
      if (ET[i] >= soil.moisture[i-1]*porosity*z_bucket){
        ET[i] <- soil.moisture[i-1]*porosity*z_bucket
      }
    }
    
    # update your soil moisture calculation
    soil.moisture[i] <- soil.moisture[i-1] + 
      (infiltration[i] - leakage[i] - ET[i])/(porosity*z_bucket)   # inflows - outflows
    
  }
  
  # combine output into single data frame
  df_out <- 
    data.frame(
      precip_mm = precip*1000, 
      PET_mm = PET*1000,
      precipEff_mm = precip_eff*1000,
      interception_mm = interception*1000,
      runoff_mm = runoff*1000,
      infiltration_mm = infiltration*1000,
      ET_mm = ET*1000,
      leakage_mm = leakage*1000,
      soilMoisture_prc = soil.moisture
    )
  return(df_out)
}
