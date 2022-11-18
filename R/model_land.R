.landSim<-function(landModel = "spwb_land", 
                   y, SpParams, meteo, dates = NULL,
                   summaryFreq = "years",
                   localControl = medfate::defaultControl(),
                   correctionFactors = defaultWatershedCorrectionFactors(),
                   progress = TRUE) {

  #check input
  if(!inherits(y, "sf")) stop("'y' has to be of class 'sf'.")
  if(!is.null(dates)) if(!inherits(dates, "Date")) stop("'dates' has to be of class 'Date'.")

  #duplicate input (to avoid modifying input objects)
  y = rlang::duplicate(y)
  
  if(landModel == "spwb_land") localModel = "spwb"
  else if(landModel=="growth_land") localModel = "growth"
  else if(landModel=="fordyn_land") localModel = "growth"
  
  latitude = sf::st_coordinates(sf::st_transform(sf::st_geometry(y),4326))[,2]


  if(inherits(meteo,"data.frame")) {
    datesMeteo = as.Date(row.names(meteo))
  } else if(inherits(meteo, "MeteorologyInterpolationData")) {
    datesMeteo = meteo@dates
  } else if(inherits(meteo, "SpatialGridMeteorology")) {
    datesMeteo = meteo@dates
  } else if(inherits(meteo, "SpatialPixelsMeteorology")) {
    datesMeteo = meteo@dates
  } else if(inherits(meteo, "stars")) {
    datesMeteo = as.Date(stars::st_get_dimension_values(meteo, "date"))
  }
  if(is.null(dates)) {
    dates = datesMeteo
  } else {
    if(sum(dates %in% datesMeteo)<length(dates))
      stop("Dates in 'dates' is not a subset of dates in 'meteo'.")
  }
  date.factor = cut(dates, breaks=summaryFreq)
  df.int = as.numeric(date.factor)
  nDays = length(dates)
  nCells = nrow(y)
  isSoilCell = y$landcovertype %in% c("wildland", "agriculture")
  nSoil = sum(isSoilCell)
  nWild = sum(y$landcovertype %in% c("wildland"))
  nAgri = sum(y$landcovertype %in% c("agriculture"))
  nRock = sum(y$landcovertype %in% c("rock"))
  nArti = sum(y$landcovertype %in% c("artificial"))
  nWater = sum(y$landcovertype %in% c("water"))
  nSummary = sum(table(date.factor)>0)
  t.df = as.numeric(table(df.int))

  #Determine outlet cells (those without downhill neighbors)
  outlets = which(unlist(lapply(y$waterQ, sum))==0)

  patchsize = sqrt(y$representedarea[1])

  #Print information area
  if(progress) {
    cat(paste0("\n------------ ", landModel," ------------\n"))
    cat(paste0("Grid cells: ", nCells,", patchsize: ", patchsize," m2, area: ", nCells*patchsize/10000," ha\n"))
    cat(paste0("Cell land use wildland: ", nWild, " agriculture: ", nAgri, " artificial: ", nArti, " rock: ", nRock, " water: ", nWater,"\n"))
    cat(paste0("Cells with soil: ", nSoil,"\n"))
    cat(paste0("Meteorological input class: ", class(meteo),"\n"))
    cat(paste0("Number of days to simulate: ",nDays,"\n"))
    cat(paste0("Number of summaries: ", nSummary,"\n"))
    cat(paste0("Number of outlet cells: ", length(outlets),"\n\n"))
    
    cat(paste0("Preparing ", localModel, " input...\n"))
  }
  for(i in 1:nCells) {
    if(y$landcovertype[i] %in% c("wildland", "agriculture")) {
      f = y$forest[[i]]
      s = y$soil[[i]]
      if(localModel=="spwb") y$state[[i]] = forest2spwbInput(f, s, SpParams, localControl)
      else if(localModel=="growth") y$state[[i]] = forest2growthInput(f, s, SpParams, localControl)
    } 
  }
  if(progress) cat("done.\n\n")

  #Output matrices
  DailyRunoff = matrix(0,nrow = nDays, ncol = length(outlets))
  colnames(DailyRunoff) = outlets
  rownames(DailyRunoff) = as.character(dates)
  
  summarylist = vector("list", nCells)
  vars = c("Runon", "Runoff", "Infiltration", "Rain", "NetRain", "Snow",
           "Snowmelt", "Interception", "DeepDrainage", "AquiferDischarge", "SaturationExcess",
           "SoilEvaporation", "Transpiration", "SWE", "SoilVol","Psi1", "WTD", "DTA",
           "InterflowInput", "InterflowOutput", "BaseflowInput", "BaseflowOutput")
  for(i in 1:nCells) {
    m = matrix(0, nrow = nSummary, ncol = length(vars))
    colnames(m) = vars
    rownames(m) = levels(date.factor)[1:nSummary]
    summarylist[[i]] = m
  }

  LandscapeBalance = data.frame(Precipitation = rep(0, nSummary),
                                Snow = rep(0, nSummary),
                                Snowmelt = rep(0, nSummary),
                                Rain = rep(0, nSummary),
                                NetRain = rep(0, nSummary),
                                Interception = rep(0, nSummary),
                                SoilEvaporation = rep(0,nSummary),
                                Transpiration = rep(0, nSummary),
                                Runoff = rep(0, nSummary),
                                Runon = rep(0, nSummary),
                                DeepDrainage = rep(0, nSummary),
                                SaturationExcess = rep(0, nSummary),
                                AquiferDischarge = rep(0, nSummary),
                                Interflow = rep(0, nSummary),
                                Baseflow = rep(0, nSummary),
                                Export = rep(0, nSummary))
  SoilLandscapeBalance = data.frame(Precipitation = rep(0, nSummary),
                                    Snow = rep(0, nSummary),
                                    Snowmelt = rep(0, nSummary),
                                    Rain = rep(0, nSummary),
                                    NetRain = rep(0, nSummary),
                                    Interception = rep(0, nSummary),
                                    SoilEvaporation = rep(0,nSummary),
                                    Transpiration = rep(0, nSummary),
                                    InterflowInput = rep(0, nSummary),
                                    InterflowOutput = rep(0, nSummary),
                                    Runon = rep(0, nSummary),
                                    Runoff = rep(0, nSummary),
                                    DeepDrainage = rep(0, nSummary),
                                    SaturationExcess = rep(0, nSummary),
                                    AquiferDischarge = rep(0, nSummary))
  

  soil_summary_function = function(object, model="SX") {
    if(!inherits(object,"soil")) return(list(SWE=NA, Psi1=NA, SoilVol=NA, WTD=NA))
    list(SWE = object$SWE,
         Psi1 = soil_psi(object)[1],
         SoilVol = sum(soil_water(object, model)),
         WTD = soil_waterTableDepth(object, model))
  }
  
  initialSoilContent = mean(extract_variables(y, "soilvolcurr")$soilvolcurr, na.rm=TRUE)
  initialSnowContent = mean(extract_variables(y, "snowpack")$snowpack, na.rm=TRUE)
  initialAquiferContent = mean(extract_variables(y, "aquifervolume")$aquifervolume, na.rm=T)
  initialLandscapeContent = initialSoilContent*(nSoil/nCells)+initialAquiferContent+initialSnowContent
  
  if(progress) {
    cat(paste0("Initial average soil water content (mm): ", round(initialSoilContent,2),"\n"))
    cat(paste0("Initial average snowpack water content (mm): ", round(initialSnowContent,2),"\n"))
    cat(paste0("Initial average aquifer water content (mm): ", round(initialAquiferContent,2),"\n"))
    cat(paste0("Initial watershed water content (mm): ", round(initialLandscapeContent,2),"\n"))
    
    cat(paste0("\nPerforming daily simulations:\n"))
  }
  
  if(inherits(meteo, "SpatialPixelsMeteorology") || inherits(meteo, "SpatialGridMeteorology")) {
    meteo = meteoland::extractgridpoints(meteo, as(sf::st_geometry(y), "Spatial"))
    # now meteo is a SpatialPointsMeteorology
  }  
  for(day in 1:nDays) {
    # cat(paste("Day #", day))
    if(progress) cat(".")
    doy = as.numeric(format(dates[day],"%j"))
    datechar = as.character(dates[day])
    if(inherits(meteo,"MeteorologyInterpolationData")) {
      spt = SpatialPointsTopography(as(sf::st_geometry(y), "Spatial"), 
                                    elevation = y$elevation, 
                                    slope = y$slope, 
                                    aspect = y$aspect)
      ml = interpolationpoints(meteo, spt, dates[day], verbose = FALSE)
      gridMinTemperature = rep(NA, nCells)
      gridMaxTemperature = rep(NA, nCells)
      gridMinRelativeHumidity = rep(NA, nCells)
      gridMaxRelativeHumidity = rep(NA, nCells)
      gridPrecipitation = rep(NA, nCells)
      gridRadiation = rep(NA, nCells)
      gridWindSpeed = rep(NA, nCells)
      for(iml in 1:nCells) {
        meti = ml@data[[iml]]
        gridMinTemperature[iml] = meti$MinTemperature[1]
        gridMaxTemperature[iml] = meti$MaxTemperature[1]
        gridMinRelativeHumidity[iml] = meti$MinRelativeHumidity[1]
        gridMaxRelativeHumidity[iml] = meti$MaxRelativeHumidity[1]
        gridPrecipitation[iml] = meti$Precipitation[1]
        gridRadiation[iml] = meti$Radiation[1]
        gridWindSpeed[iml] = meti$WindSpeed[1]
      }
    } else if(inherits(meteo,"stars")) {
      pt_sf = sf::st_sf(geometry = sf::st_geometry(y), 
                        elevation = y$elevation, slope = y$slope, aspect = y$aspect)
      met = meteoland::interpolate_data(pt_sf, meteo, dates = dates[day], verbose = FALSE)
      ml = tidyr::unnest(met, cols = "interpolated_data")
      gridMinTemperature = ml$MinTemperature
      gridMaxTemperature = ml$MaxTemperature
      gridMinRelativeHumidity = ml$MinRelativeHumidity
      gridMaxRelativeHumidity = ml$MaxRelativeHumidity
      gridPrecipitation = ml$Precipitation
      gridRadiation = ml$Radiation
      gridWindSpeed = ml$WindSpeed      
    } else if(inherits(meteo,"SpatialPointsMeteorology")) {
      imeteo = which(datesMeteo == dates[day]) #date index in meteo data
      for(iml in 1:nCells) {
        meti = meteo@data[[iml]]
        gridMinTemperature[iml] = meti$MinTemperature[imeteo]
        gridMaxTemperature[iml] = meti$MaxTemperature[imeteo]
        gridMinRelativeHumidity[iml] = meti$MinRelativeHumidity[imeteo]
        gridMaxRelativeHumidity[iml] = meti$MaxRelativeHumidity[imeteo]
        gridPrecipitation[iml] = meti$Precipitation[imeteo]
        gridRadiation[iml] = meti$Radiation[imeteo]
        gridWindSpeed[iml] = meti$WindSpeed[imeteo]
      }
    } else { # data frame
      imeteo = which(datesMeteo == dates[day]) #date index in meteo data
      # repeat values for all cells
      gridMinTemperature = rep(meteo[imeteo,"MinTemperature"], nCells)
      gridMaxTemperature = rep(meteo[imeteo,"MaxTemperature"], nCells)
      gridMinRelativeHumidity = rep(meteo[imeteo,"MinRelativeHumidity"], nCells)
      gridMaxRelativeHumidity = rep(meteo[imeteo,"MaxRelativeHumidity"], nCells)
      gridPrecipitation = rep(meteo[imeteo,"Precipitation"], nCells)
      gridRadiation = rep(meteo[imeteo, "Radiation"], nCells)
      gridWindSpeed = rep(meteo[imeteo, "WindSpeed"], nCells)
    }
    gridRadiation[is.na(gridRadiation)] = mean(gridRadiation, na.rm=T)
    gridMeteo = data.frame(MinTemperature = gridMinTemperature, 
                           MaxTemperature = gridMaxTemperature,
                           MinRelativeHumidity = gridMinRelativeHumidity,
                           MaxRelativeHumidity = gridMaxRelativeHumidity,
                           Precipitation = gridPrecipitation,
                           Radiation = gridRadiation,
                           WindSpeed = gridWindSpeed)
    ws_day = .watershedDay(localModel,
                           y$landcovertype, y$state, y$soil,
                           y$waterOrder, y$queenNeigh, y$waterQ,
                           y$depthtobedrock, y$bedrockconductivity, y$bedrockporosity, 
                           y$aquifer, y$snowpack,
                           correctionFactors,
                           datechar,
                           gridMeteo,
                           latitude, y$elevation, y$slope, y$aspect,
                           patchsize, progress)
    
    res_day = ws_day[["WatershedWaterBalance"]]
    
    if(progress) cat("+")
    summary_df = landscape_summary(y, "soil", soil_summary_function, localControl$soilFunctions, 
                                   unlist = TRUE)
    ifactor = df.int[day]
    varsSum = c("Runon", "Runoff", "Rain", "NetRain", "Snow", "Snowmelt",
                "Infiltration", "DeepDrainage", "SaturationExcess",
                "AquiferDischarge", "SoilEvaporation", "Transpiration",
                "InterflowInput", "InterflowOutput", "BaseflowInput", "BaseflowOutput")
    varsState = c("SWE", "Psi1", "SoilVol", "WTD")
    DTAday = (y$depthtobedrock/1000.0) - (y$aquifer/y$bedrockporosity)/1000.0
    for(i in 1:nCells) {
      for(v in varsSum) {
        summarylist[[i]][ifactor,v] = summarylist[[i]][ifactor,v] + res_day[[v]][i]
      }
      summarylist[[i]][ifactor,"Interception"] = summarylist[[i]][ifactor,"Interception"] + (res_day[["Rain"]][i] - res_day[["NetRain"]][i])
      for(v in varsState) {
        summarylist[[i]][ifactor,v] = summarylist[[i]][ifactor,v] + summary_df[[v]][i]/t.df[ifactor]
      }  
      summarylist[[i]][ifactor,"DTA"] = summarylist[[i]][ifactor,"DTA"] + DTAday[i]/t.df[ifactor]
    }

    DailyRunoff[day,] = res_day$Runoff[outlets]*patchsize/1e6 ## Runoff in m3/day

    #Landscape balance
    LandscapeBalance$Rain[ifactor]= LandscapeBalance$Rain[ifactor] + sum(res_day$Rain, na.rm=T)/nCells
    LandscapeBalance$Snow[ifactor]= LandscapeBalance$Snow[ifactor] + sum(res_day$Snow, na.rm=T)/nCells
    LandscapeBalance$DeepDrainage[ifactor] = LandscapeBalance$DeepDrainage[ifactor] + sum(res_day$DeepDrainage, na.rm=T)/nCells
    LandscapeBalance$SaturationExcess[ifactor] = LandscapeBalance$SaturationExcess[ifactor] + sum(res_day$SaturationExcess, na.rm=T)/nCells
    LandscapeBalance$AquiferDischarge[ifactor] = LandscapeBalance$AquiferDischarge[ifactor] + sum(res_day$AquiferDischarge, na.rm=T)/nCells
    LandscapeBalance$Runoff[ifactor] = LandscapeBalance$Runoff[ifactor] + sum(res_day$Runoff, na.rm=T)/nCells
    LandscapeBalance$Runon[ifactor] = LandscapeBalance$Runon[ifactor] + sum(res_day$Runon, na.rm=T)/nCells
    LandscapeBalance$Snowmelt[ifactor]= LandscapeBalance$Snowmelt[ifactor] + sum(res_day$Snowmelt, na.rm=T)/nCells
    LandscapeBalance$NetRain[ifactor]= LandscapeBalance$NetRain[ifactor] + sum(res_day$NetRain, na.rm=T)/nCells
    LandscapeBalance$Interception[ifactor]= LandscapeBalance$Interception[ifactor] + (sum(res_day$Rain, na.rm=T) - sum(res_day$NetRain, na.rm=T))/nCells
    LandscapeBalance$Infiltration[ifactor]= LandscapeBalance$Infiltration[ifactor] + sum(res_day$Infiltration, na.rm=T)/nCells
    LandscapeBalance$SoilEvaporation[ifactor] = LandscapeBalance$SoilEvaporation[ifactor] + sum(res_day$SoilEvaporation, na.rm=T)/nCells
    LandscapeBalance$Transpiration[ifactor] = LandscapeBalance$Transpiration[ifactor] + sum(res_day$Transpiration, na.rm=T)/nCells
    LandscapeBalance$Export[ifactor] = LandscapeBalance$Export[ifactor] + (sum(res_day$Runoff[outlets], na.rm=T)/nCells)
    LandscapeBalance$Interflow[ifactor]= LandscapeBalance$Interflow[ifactor] + sum(res_day$InterflowInput, na.rm=T)/nCells
    LandscapeBalance$Baseflow[ifactor]= LandscapeBalance$Baseflow[ifactor] + sum(res_day$BaseflowInput, na.rm=T)/nCells
    
    SoilLandscapeBalance$Rain[ifactor]= SoilLandscapeBalance$Rain[ifactor] + sum(res_day$Rain[isSoilCell], na.rm=T)/nSoil
    SoilLandscapeBalance$Snow[ifactor]= SoilLandscapeBalance$Snow[ifactor] + sum(res_day$Snow[isSoilCell], na.rm=T)/nSoil
    SoilLandscapeBalance$DeepDrainage[ifactor] = SoilLandscapeBalance$DeepDrainage[ifactor] + sum(res_day$DeepDrainage[isSoilCell], na.rm=T)/nSoil
    SoilLandscapeBalance$SaturationExcess[ifactor] = SoilLandscapeBalance$SaturationExcess[ifactor] + sum(res_day$SaturationExcess[isSoilCell], na.rm=T)/nSoil
    SoilLandscapeBalance$AquiferDischarge[ifactor] = SoilLandscapeBalance$AquiferDischarge[ifactor] + sum(res_day$AquiferDischarge[isSoilCell], na.rm=T)/nSoil
    SoilLandscapeBalance$InterflowInput[ifactor]= SoilLandscapeBalance$InterflowInput[ifactor] + sum(res_day$InterflowInput[isSoilCell], na.rm=T)/nSoil
    SoilLandscapeBalance$InterflowOutput[ifactor]= SoilLandscapeBalance$InterflowOutput[ifactor] + sum(res_day$InterflowOutput[isSoilCell], na.rm=T)/nSoil
    SoilLandscapeBalance$Runoff[ifactor] = SoilLandscapeBalance$Runoff[ifactor] + sum(res_day$Runoff[isSoilCell], na.rm=T)/nSoil
    SoilLandscapeBalance$Runon[ifactor] = SoilLandscapeBalance$Runon[ifactor] + sum(res_day$Runon[isSoilCell], na.rm=T)/nSoil
    SoilLandscapeBalance$Snowmelt[ifactor]= SoilLandscapeBalance$Snowmelt[ifactor] + sum(res_day$Snowmelt[isSoilCell], na.rm=T)/nSoil
    SoilLandscapeBalance$NetRain[ifactor]= SoilLandscapeBalance$NetRain[ifactor] + sum(res_day$NetRain[isSoilCell], na.rm=T)/nSoil
    SoilLandscapeBalance$Interception[ifactor]= SoilLandscapeBalance$Interception[ifactor] + (sum(res_day$Rain[isSoilCell], na.rm=T) - sum(res_day$NetRain[isSoilCell], na.rm=T))/nSoil
    SoilLandscapeBalance$Infiltration[ifactor]= SoilLandscapeBalance$Infiltration[ifactor] + sum(res_day$Infiltration[isSoilCell], na.rm=T)/nSoil
    SoilLandscapeBalance$SoilEvaporation[ifactor] = SoilLandscapeBalance$SoilEvaporation[ifactor] + sum(res_day$SoilEvaporation[isSoilCell], na.rm=T)/nSoil
    SoilLandscapeBalance$Transpiration[ifactor] = SoilLandscapeBalance$Transpiration[ifactor] + sum(res_day$Transpiration[isSoilCell], na.rm=T)/nSoil
  }
  if(progress) cat("done\n\n")
  #Average summaries
  LandscapeBalance$Precipitation = LandscapeBalance$Rain + LandscapeBalance$Snow
  SoilLandscapeBalance$Precipitation = SoilLandscapeBalance$Rain + SoilLandscapeBalance$Snow
  
  finalSoilContent = mean(extract_variables(y, "soilvolcurr")$soilvolcurr, na.rm=TRUE)
  finalSnowContent = mean(extract_variables(y, "snowpack")$snowpack, na.rm=TRUE)
  finalAquiferContent = mean(extract_variables(y, "aquifervolume")$aquifervolume, na.rm=T)
  finalLandscapeContent = initialSoilContent*(nSoil/nCells)+initialAquiferContent+initialSnowContent

  Precipitationsum = sum(LandscapeBalance$Precipitation, na.rm=T)
  Rainfallsum = sum(LandscapeBalance$Rain, na.rm=T)
  NetRainsum = sum(LandscapeBalance$NetRain, na.rm=T)
  Interceptionsum = sum(LandscapeBalance$Interception, na.rm=T)
  Infiltrationsum = sum(LandscapeBalance$Infiltration, na.rm=T)
  Snowsum = sum(LandscapeBalance$Snow, na.rm=T)
  Snowmeltsum = sum(LandscapeBalance$Snowmelt, na.rm=T)
  Runonsum = sum(LandscapeBalance$Runon, na.rm=T)
  Runoffsum = sum(LandscapeBalance$Runoff, na.rm=T)
  DeepDrainagesum= sum(LandscapeBalance$DeepDrainage, na.rm=T)
  SaturationExcesssum= sum(LandscapeBalance$SaturationExcess, na.rm=T)
  SoilEvaporationsum= sum(LandscapeBalance$SoilEvaporation , na.rm=T)
  Transpirationsum = sum(LandscapeBalance$Transpiration , na.rm=T)
  AquiferDischargesum = sum(LandscapeBalance$AquiferDischarge , na.rm=T)
  Exportsum = sum(LandscapeBalance$Export, na.rm=T)
  Interflowsum = sum(LandscapeBalance$Interflow , na.rm=T)
  Baseflowsum = sum(LandscapeBalance$Baseflow , na.rm=T)
  
  SoilPrecipitationsum = sum(SoilLandscapeBalance$Precipitation, na.rm=T)
  SoilRainfallsum = sum(SoilLandscapeBalance$Rain, na.rm=T)
  SoilNetRainsum = sum(SoilLandscapeBalance$NetRain, na.rm=T)
  SoilInterceptionsum = sum(SoilLandscapeBalance$Interception, na.rm=T)
  SoilInfiltrationsum = sum(SoilLandscapeBalance$Infiltration, na.rm=T)
  SoilSnowsum = sum(SoilLandscapeBalance$Snow, na.rm=T)
  SoilSnowmeltsum = sum(SoilLandscapeBalance$Snowmelt, na.rm=T)
  SoilRunonsum = sum(SoilLandscapeBalance$Runon, na.rm=T)
  SoilRunoffsum = sum(SoilLandscapeBalance$Runoff, na.rm=T)
  SoilSaturationExcesssum= sum(SoilLandscapeBalance$SaturationExcess, na.rm=T)
  SoilDeepDrainagesum= sum(SoilLandscapeBalance$DeepDrainage, na.rm=T)
  SoilSoilEvaporationsum= sum(SoilLandscapeBalance$SoilEvaporation , na.rm=T)
  SoilTranspirationsum = sum(SoilLandscapeBalance$Transpiration , na.rm=T)
  SoilAquiferDischargesum = sum(SoilLandscapeBalance$AquiferDischarge , na.rm=T)
  SoilInterflowInputsum = sum(SoilLandscapeBalance$InterflowInput , na.rm=T)
  SoilInterflowOutputsum = sum(SoilLandscapeBalance$InterflowOutput , na.rm=T)
  
  
  if(progress){
    cat(paste0("Final average soil water content (mm): ", round(finalSoilContent,2),"\n"))
    cat(paste0("Final average snowpack water content (mm): ", round(finalSnowContent,2),"\n"))
    cat(paste0("Final average aquifer water content (mm): ", round(finalAquiferContent,2),"\n"))
    cat(paste0("Final watershed water content (mm): ", round(finalLandscapeContent,2),"\n"))
  }
  
  snowpack_wb = Snowsum - Snowmeltsum
  if(progress) {
    cat(paste0("\nChange in snowpack water content (mm): ", round(finalSnowContent - initialSnowContent,2),"\n"))
    cat(paste0("Snowpack water balance result (mm): ",round(snowpack_wb,2),"\n"))
    cat(paste0("Snowpack water balance components:\n"))
    cat(paste0("  Snow fall (mm) ", round(Snowsum,2), " Snow melt (mm) ",round(Snowmeltsum,2),"\n"))
  }
  soil_input = (SoilNetRainsum + SoilSnowmeltsum + SoilRunonsum + SoilAquiferDischargesum+SoilInterflowInputsum)
  soil_output = (SoilRunoffsum + SoilDeepDrainagesum + SoilSoilEvaporationsum + SoilTranspirationsum +SoilInterflowOutputsum)
  soil_wb =  soil_input - soil_output
  if(progress) {
    cat(paste0("\nChange in soil water content (mm): ", round(finalSoilContent - initialSoilContent,2),"\n"))
    cat(paste0("Soil water balance result (mm): ",round(soil_wb,2),"\n"))
    cat(paste0("Soil water balance components:\n"))
    cat(paste0("  Net rainfall (mm) ", round(SoilNetRainsum,2)," Snow melt (mm) ", round(SoilSnowmeltsum,2),"\n"))
    cat(paste0("  Runon (mm) ", round(SoilRunonsum,2)," Runoff (mm) ",round(SoilRunoffsum,2),"\n"))
    cat(paste0("  Subsurface input (mm) ",round(SoilInterflowInputsum,2),"  Subsurface output (mm) ",round(SoilInterflowOutputsum,2),"\n"))
    cat(paste0("  Deep drainage (mm) ",round(SoilDeepDrainagesum,2)," Aquifer discharge (mm) ", round(SoilAquiferDischargesum,2),"\n"))
    cat(paste0("  Soil evaporation (mm) ",round(SoilSoilEvaporationsum,2), " Plant transpiration (mm) ", round(SoilTranspirationsum,2),"\n"))
  }
  
  aquifer_wb = DeepDrainagesum - AquiferDischargesum
  if(progress){
    cat(paste0("\nChange in aquifer water content (mm): ", round(finalAquiferContent - initialAquiferContent,2),"\n"))
    cat(paste0("Aquifer water balance result (mm): ",round(aquifer_wb,2),"\n"))
    cat(paste0("Aquifer water balance components:\n"))
    cat(paste0("  Deep drainage (mm) ", round(DeepDrainagesum,2), " Aquifer discharge (mm) ",round(AquiferDischargesum,2),"\n"))
  }
  
  landscape_wb = Precipitationsum - Exportsum - SoilEvaporationsum - Transpirationsum - Interceptionsum
  if(progress) {
    cat(paste0("\nChange in watershed water content (mm): ", round(finalLandscapeContent - initialLandscapeContent,2),"\n"))
    cat(paste0("Watershed water balance result (mm): ",round(landscape_wb,2),"\n"))
    cat(paste0("Watershed water balance components:\n"))
    cat(paste0("  Precipitation (mm) ", round(Precipitationsum,2),"\n"))
    cat(paste0("  Interception (mm) ", round(Interceptionsum,2), " Soil evaporation (mm) ",round(SoilEvaporationsum,2), " Plant Transpiration (mm) ",round(Transpirationsum,2),"\n"))
    cat(paste0("  Export (mm) ", round(Exportsum,2),"\n"))
    cat(paste0("Watershed lateral flows:\n"))
    cat(paste0("  Subsurface flow (mm) ",round(Interflowsum,2),"\n"))
    cat(paste0("  Groundwater flow (mm) ", round(Baseflowsum,2),"\n"))
    
    cat(paste0("\n------------ ",landModel," ------------\n"))
  }

  sf = sf::st_sf(geometry=sf::st_geometry(y))
  sf$state = y$state
  sf$aquifer = y$aquifer
  sf$snowpack = y$snowpack
  sf$summary = summarylist

  l <- list(sf = sf::st_as_sf(tibble::as_tibble(sf)),
            WatershedBalance = LandscapeBalance,
            WatershedSoilBalance = SoilLandscapeBalance,
            DailyRunoff = DailyRunoff)
  class(l)<-c(landModel, "list")
  return(l)
}

#' Watershed simulations
#' 
#' Function \code{spwb_land} implements a distributed hydrological model that simulates daily local water balance, from \code{\link{spwb_day}}, 
#' on grid cells of a watershed while accounting for overland runoff, subsurface flow and groundwater flow between cells. 
#' Function \code{growth_land} is similar, but includes daily local carbon balance and growth processes in grid cells, 
#' provided by \code{\link{growth_day}}.
#' 
#' @param y An object of class \code{\link{sf}} containing watershed information.
#' @param SpParams A data frame with species parameters (see \code{\link{SpParamsMED}}).
#' @param meteo Input meteorological data (see section details).
#' @param dates A \code{\link{Date}} object describing the days of the period to be modeled.
#' @param summaryFreq Frequency in which summary layers will be produced (e.g. "years", "months", ...) (see \code{\link{cut.Date}}).
#' @param localControl A list of control parameters (see \code{\link{defaultControl}}) for function \code{\link{spwb_day}} or \code{\link{growth_day}}.
#' @param correctionFactors A list of watershed correction factors for hydraulic parameters.
#' @param progress Boolean flag to display progress information for simulations.
#'
#' @details Functions \code{spwb_land} and \code{growth_land} require daily meteorological data over grid cells. 
#' The user may supply four different inputs:
#'
#' \enumerate{
#'   \item{A data frame with meteorological data common for all spatial location (spatial variation of weather not considered).}
#'   \item{An object of class \code{\link{stars}} with interpolation data, created by package meteoland.}
#'   \item{DEPRECATED: An object of \code{\link{SpatialPixelsMeteorology-class}} or \code{\link{SpatialGridMeteorology-class}}. 
#'   All the spatio-temporal variation of weather is already supplied by the user.}
#'   \item{DEPRECATED: An object of \code{\link{MeteorologyInterpolationData-class}}. Interpolation of weather is performed over each spatial unit every simulated day.}
#'   }
#'  
#' @return Function \code{spwb_land} list of class 'spwb_land' with the following elements:
#' \itemize{
#'   \item{\code{sf}: An object of class \code{\link{sf}}, similar to the output of \code{\link{spwb_spatial}}, 
#'   with the following columns:
#'     \itemize{
#'        \item{\code{state}: A list of model input objects for each simulated stand.}
#'        \item{\code{aquifer}: A numeric vector with the water volume in the aquifer of each cell.}
#'        \item{\code{snowpack}: A numeric vector with the snowpack water equivalent volume of each cell.}
#'        \item{\code{summary}: A list of cell summaries, containing the following variables:
#'         \itemize{
#'           \item{\code{Rain}: Rainfall (in mm).}
#'           \item{\code{Snow}: Snowfall (in mm).}
#'           \item{\code{Snowmelt}: Snow melt (in mm).}
#'           \item{\code{Interception}: Rainfall interception (in mm).}
#'           \item{\code{NetRain}: Net rain, i.e. throughfall, (in mm).}
#'           \item{\code{Runon}: The amount of water imported from other cells via surface runoff (in mm).}
#'           \item{\code{Runoff}: The amount of water exported via surface runoff (in mm).}
#'           \item{\code{Infiltration}: The amount of water infiltrating into the soil (in mm).}
#'           \item{\code{DeepDrainage}: The amount of water draining from soil to the aquifer via deep drainage (in mm).}
#'           \item{\code{SaturationExcess}: The amount of water that reaches the soil surface because of soil saturation (in mm).}
#'           \item{\code{AquiferDischarge}: The amount of water that reaches deepest soil layer from a saturated aquifer (in mm).}
#'           \item{\code{SubsurfaceInput}: The amount of water that reaches the soil from adjacent cells via subsurface flow (in mm).}
#'           \item{\code{SubsurfaceOutput}: The amount of water that leaves the soil towards adjacent cells via subsurface flow (in mm).}
#'           \item{\code{GroundwaterInput}: The amount of water that reaches the aquifer from adjacent cells via groundwater flow (in mm).}
#'           \item{\code{GroundwaterOutput}: The amount of water that leaves the aquifer towards adjacent cells via groundwater flow (in mm).}
#'           \item{\code{SoilEvaporation}: Bare soil evaporation (in mm).}
#'           \item{\code{Transpiration}: Plant transpiration (in mm).}
#'           \item{\code{SWE}: Snow water equivalent (in mm) of the snowpack.}
#'           \item{\code{Psi1}: Soil water potential of the topmost layer (in MPa).}
#'           \item{\code{SoilVol}: Soil water volume integrated across vertical layers (in mm).}
#'           \item{\code{WTD}: Water table depth (in mm from surface).}
#'         }
#'       }
#'     }
#'   }
#'   \item{\code{WatershedBalance}: A data frame with as many rows as summary points and where columns are components of the water balance at the watershed level (i.e., rain, snow, interception, infiltration, soil evaporation, plant transpiration, ...).}
#'   \item{\code{WatershedSoilBalance}: A data frame with as many rows as summary points and where columns are components of the water balance at the watershed level restricted to those cells with a soil definition.}
#'   \item{\code{DailyRunoff}: A matrix with daily runoff (in m3/day) at each of the outlet cells of the landscape.}
#' }
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF.
#' 
#' @seealso \code{\link{spwb_day}},  \code{\link{growth_day}},
#' \code{\link{spwb_spatial}}
#' 
#' @examples 
#' \dontrun{
#' # Load example watershed data
#' data("examplewatershed")
#' 
#' # Transform example to 'sf' 
#' y = sp_to_sf(examplewatershed)
#'   
#' # Load example meteo data frame from package meteoland
#' data("examplemeteo")
#'   
#' # Load default medfate parameters
#' data("SpParamsMED")
#'   
#' # Set simulation period
#' dates = seq(as.Date("2001-01-01"), as.Date("2001-03-31"), by="day")
#' 
#' # Launch simulations
#' res = spwb_land(y, SpParamsMED, examplemeteo, dates = dates, summaryFreq = "month")
#' }
#' 
#' @name spwb_land
spwb_land<-function(y, SpParams, meteo, dates = NULL,
                   summaryFreq = "years",
                   localControl = medfate::defaultControl(),
                   correctionFactors = defaultWatershedCorrectionFactors(),
                   progress = TRUE) {
  return(.landSim("spwb_land",
                  y = y, SpParams = SpParams, meteo = meteo, dates = dates,
                  summaryFreq = summaryFreq, 
                  localControl = localControl,
                  correctionFactors = correctionFactors, progress = progress))
}
#' @rdname spwb_land
growth_land<-function(y, SpParams, meteo, dates = NULL,
                   summaryFreq = "years",
                   localControl = medfate::defaultControl(),
                   correctionFactors = defaultWatershedCorrectionFactors(),
                   progress = TRUE) {
  return(.landSim("growth_land",
                  y = y, SpParams = SpParams, meteo = meteo, dates = dates,
                  summaryFreq = summaryFreq, 
                  localControl = localControl,
                  correctionFactors = correctionFactors, progress = progress))
}