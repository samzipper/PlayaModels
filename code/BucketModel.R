## BucketModel.R
# This is a function that creates a daily ecohydrological 'bucket model' soil water balance accounting model.
#
# Time-varying input data (vectors of same length, set NULL to disable):
# - precip = precipitation [m/timestep]
# - runon = runon [m/timestep]
# - PET = potential ET [m/timestep]
# Static input data:
# - ts = timestep of input data timeseries [days]
# - Ksat = saturated hydraulic conductivity of soils [m/day]
# - porosity = porosity of soil [-]
# - S_field = relative soil moisture at field capacity, where relative soil moisture is defined as Vwater/(Vair + Vwater) [-]
#     If you have an estimate of Sy, you can estimate S_field as (Sy/porosity)
# - S_stress = relative soil moisture below which water stress occurs [-]
# - S_init = relative soil moisture at initial conditions [-]
# - int_depth = maximum quantity of interception [m]
# - z_bucket = depth of bucket [m] - assumed <= root depth in playas
# - output_vars = "all" or a vector specifying names of variables that should be output
# - pref_flow = should preferential flow be simulated? defaults to FALSE.
# - ponding = should ponding be simulated? defaults to FALSE.
# Additional parameters needed if pref_flow = T
# - S_open = relative soil moisture at which cracks are fully open
# - S_closed = relative soil moisture at which cracks are closed
# - pref_frac = maximum fraction of precipitation going to preferential flow

bucket_model <- function(precip, runon, PET, ts, 
                         Ksat, porosity, S_field, S_stress, S_init, int_depth, z_bucket,
                         output_vars = "all", pref_flow = F, ponding = F, ...){
  
  # Input data checks --------------------------------
  if (length(precip) != length(PET)) stop("precip and PET vector lengths differ")
  if (sum(is.na(c(precip, PET))) > 0) stop("NA values in precip and/or PET input data")
  
  # Unit conversions based on timestep --------------------------------------
  Ksat_ts <- Ksat*ts  # convert to meters per timestep
  
  # Create empty vectors to hold your output --------------------------------
  # There should be one empty vector for each desired model output. For
  # now, they will only contain NaNs, and we will fill them in as we go.
  
  n_ts <- length(precip) # number of timesteps in model simulation
  if (is.null(precip)) precip <- rep(0, length = n_ts)
  if (is.null(runon))  runon  <- rep(0, length = n_ts)
  if (is.null(PET))    PET    <- rep(0, length = n_ts)
  surface_water<- rep(NA, length = n_ts)  # water available at the surface (precip + ponded + runon)
  precip_eff   <- rep(NA, length = n_ts)  # effective precipitation [m/ts]
  interception <- rep(NA, length = n_ts)  # interception flux [m/ts]
  int_storage  <- rep(NA, length = n_ts)  # interception storage [m]
  runoff       <- rep(NA, length = n_ts)  # runoff [m/ts]
  infiltration <- rep(NA, length = n_ts)  # infiltration [m/ts]
  ET           <- rep(NA, length = n_ts)  # ET [m/ts]
  leakage_mat  <- rep(NA, length = n_ts)  # leakage through matrix [m/ts]
  leakage_pref <- rep(NA, length = n_ts)  # leakage through preferential flow [m/ts]
  pond_depth   <- rep(NA, length = n_ts)  # ponded water depth [m]
  soil.moisture<- rep(NA, length = n_ts)  # relative soil moisture = theta/porosity 
  
  # Run the model -----------------------------------------------------------
  
  # First, set up your initial conditions
  soil.moisture[1] <- S_init  # S_init is a user parameter defined above
  
  # set all other fluxes to 0 at the first timestep
  surface_water[1]<- 0
  precip_eff[1]   <- 0
  int_storage[1]  <- 0
  interception[1] <- 0
  runoff[1]       <- 0
  infiltration[1] <- 0
  ET[1]           <- 0
  leakage_mat[1]  <- 0
  leakage_pref[1] <- 0
  pond_depth[1]   <- 0
  
  # Partition rainfall between effective precipitation and interception 
  for (i in 2:n_ts){
    
    # scale K as function of soil moisture
    Kunsat_ts <- Ksat_ts*soil.moisture[i-1]
    
    # available water is sum of precip + runon + pond depth from last timestep
    surface_water[i] <- precip[i] + runon[i] + pond_depth[i-1]
    
    # identify interception storage capacity
    int_capacity <- int_depth - int_storage[i-1]
    
    if (surface_water[i] < int_capacity){
      # if there is less precip than interception depth, it should all go to interception. 
      interception[i]  <- surface_water[i]
      int_storage[i]   <- int_storage[i-1] + interception[i]
      precip_eff[i]    <- 0
      infiltration[i]  <- 0
      runoff[i]        <- 0
      pond_depth[i]    <- 0
      leakage_pref[i]  <- 0
      
    } else {
      # if there is more precipitation than interception depth, partition among fluxes
      interception[i] <- int_capacity
      int_storage[i]  <- int_storage[i-1] + interception[i]
      
      # calculate preferential flow recharge if enabled
      if (pref_flow){
        # preferential flow through cracks should occur if S > S_closed
        if (soil.moisture[i-1] >= S_closed){
          pref_scale <- 0
        } else if (soil.moisture[i-1] >= S_open){
          pref_scale <- (soil.moisture[i-1] - S_closed)/(S_open - S_closed)
        } else {
          pref_scale <- 1
        }
        leakage_pref[i] <- pref_frac*pref_scale*(surface_water[i]-interception[i])
        
      } else {
        # if no pref_flow, should be 0 all the time
        leakage_pref[i] <- 0
      }
      
      # effective precipitation = surface water after subtracting interception and preferential flow
      precip_eff[i] <- surface_water[i] - interception[i] - leakage_pref[i]
      
      # calculate the maximum possible infiltration based on Kunsat (hortonian) and/or previous soil moisture storage (dunne)
      infiltration.max <- min(c(
        Kunsat_ts,
        (porosity*z_bucket - soil.moisture[i-1]*porosity*z_bucket)
      ))
      
      # decide whether excess water infiltrates or runs off/ponds
      if (precip_eff[i] <= infiltration.max){
        # infiltrate all
        infiltration[i] <- precip_eff[i]
        excess_water <- 0
      } else {
        # fill the bucket, rest does not infiltrate
        infiltration[i] <- infiltration.max
        excess_water <- precip_eff[i] - infiltration[i]
      }
      
      # put into either ponding or runoff
      if (ponding){
        pond_depth[i] <- excess_water
        runoff[i] <- 0
      } else {
        pond_depth[i] <- 0
        runoff[i] <- excess_water
      }
    }
    
    # calculate matrix leakage out of bottom
    if (soil.moisture[i-1] < S_field) {
      # if relative soil moisture is less than field capacity, there is no leakage
      leakage_mat[i] <- 0
    } else {
      # if we are above field capacity, linearly scale from 0-Kunsat_ts
      leakage_mat[i] <- Kunsat_ts*(soil.moisture[i-1] - S_field)/(1-S_field)
      
      # make sure that you don't remove enough water to reduce soil
      # moisture in bucket below field capacity
      if (leakage_mat[i] >= (soil.moisture[i-1] - S_field)*porosity*z_bucket){
        leakage_mat[i] <- (soil.moisture[i-1] - S_field)*porosity*z_bucket
      }
    }
    
    # first, evaporate intercepted water
    if (PET[i] > int_storage[i]){
      # PET > int_storage: all interception is evaporated
      ET_int <- int_storage[i]
      int_storage[i] <- 0
    } else {
      # PET < int_storage: use all PET for interception
      ET_int <- PET[i]
      int_storage[i] <- int_storage[i] - ET_int
    }
    
    # second, evaporate ponded water
    if ((PET[i] - ET_int) > pond_depth[i]){
      # PET is greater than pond_depth, evaporate all ponded water
      ET_pond <- pond_depth[i]
      pond_depth[i] <- 0
    } else {
      # PET is less than pond depth, use up all remaining PET
      ET_pond <- PET[i] - ET_int
      pond_depth[i] <- pond_depth[i] - ET_pond
    }
    
    # calculate ET as a function of soil moisture at previous timestep
    if (soil.moisture[i-1] > S_stress){
      # if relative soil moisture is larger than S_stress ET=PET
      ET_soil <- (PET[i] - ET_int - ET_pond)
    } else{
      # if relative soil moisture is less than S_stress, reduce linearly to 0
      ET_soil <- (PET[i] - ET_int - ET_pond)*(soil.moisture[i-1])/(S_stress)
      
      # make sure ET is not so much that it drops us below 0
      if (ET_soil >= soil.moisture[i-1]*porosity*z_bucket){
        ET_soil <- soil.moisture[i-1]*porosity*z_bucket
      }
    }
    
    # ET is sum of interception, pond, and soil ET
    ET[i] <- ET_int + ET_soil + ET_pond
    
    # update your soil moisture calculation
    soil.moisture[i] <- soil.moisture[i-1] + 
      (infiltration[i] - leakage_mat[i] - ET_soil)/(porosity*z_bucket)   # inflows - outflows
    
  }
  
  # combine output into single data frame
  df_all <- 
    data.frame(
      precip_mm = precip*1000, 
      PET_mm = PET*1000,
      precipEff_mm = precip_eff*1000,
      interception_mm = interception*1000,
      runoff_mm = runoff*1000,
      pond_depth_mm = pond_depth*1000,
      infiltration_mm = infiltration*1000,
      ET_mm = ET*1000,
      leakage_mat_mm = leakage_mat*1000,
      leakage_pref_mm = leakage_pref*1000,
      surface_water_mm = surface_water*1000,
      soilMoisture_prc = soil.moisture
    )
  
  # get rid of some unneeded variables depending on flags
  if (!pref_flow) df_all$leakage_pref_mm <- NULL
  if (ponding)    df_all$runoff_mm <- NULL
  if (!ponding)   df_all$pond_depth_mm <- NULL
  
  # pull desired variables
  if (output_vars == "all"){
    df_out <- df_all
  } else {
    df_out <- df_all[,output_vars]
  }
  
  return(df_out)
}
