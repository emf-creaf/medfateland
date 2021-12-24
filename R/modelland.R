.landSim<-function(landModel = "spwbland", 
                   y, SpParams, meteo, dates = NULL,
                   summaryFreq = "years",
                   localControl = medfate::defaultControl(),
                   correctionFactors = defaultWatershedCorrectionFactors()) {

  #check input
  if(!inherits(y, "DistributedWatershed")) stop("'y' has to be of class 'DistributedWatershed'.")
  if(!inherits(meteo,"SpatialPixelsMeteorology") &&
     !inherits(meteo,"data.frame") &&
     !inherits(meteo, "MeteorologyInterpolationData"))
    stop("'meteo' has to be of class 'SpatialPixelsMeteorology', 'MeteorologyInterpolationData' or 'data.frame'.")
  if(!is.null(dates)) if(!inherits(dates, "Date")) stop("'dates' has to be of class 'Date'.")

  if(landModel == "spwbland") localModel = "spwb"
  else if(landModel=="growthland") localModel = "growth"
  else if(landModel=="fordynland") localModel = "growth"
  
  sp = spTransform(as(y, "SpatialPoints"), CRS(SRS_string = "EPSG:4326"))
  latitude = sp@coords[,2]
  elevation = y@data$elevation
  slope = y@data$slope
  aspect = y@data$aspect

  if(inherits(meteo,"data.frame")) {
    oneMeteoCell = ("MeanTemperature" %in% names(meteo))
    datesMeteo = as.Date(row.names(meteo))
  } else if(inherits(meteo, "MeteorologyInterpolationData")) {
    datesMeteo = meteo@dates
  } else if(inherits(meteo, "SpatialPixelsMeteorology")) {
    datesMeteo = meteo@dates
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
  nCells = length(y@forestlist)
  isSoilCell = y@lct %in% c("wildland", "agriculture")
  nSoil = sum(isSoilCell)
  nWild = sum(y@lct %in% c("wildland"))
  nAgri = sum(y@lct %in% c("agriculture"))
  nRock = sum(y@lct %in% c("rock"))
  nArti = sum(y@lct %in% c("artificial"))
  nWater = sum(y@lct %in% c("water"))
  nSummary = sum(table(date.factor)>0)
  t.df = as.numeric(table(df.int))

  #Determine outlet cells (those without downhill neighbors)
  outlets = which(unlist(lapply(y@waterQ, sum))==0)

  patchsize = prod(y@grid@cellsize)

  #Print information area
  cat(paste0("\n------------ ", landModel," ------------\n"))
  cat(paste0("Grid cells: ", nCells,", patchsize: ", patchsize," m2, area: ", nCells*patchsize/10000," ha\n"))
  cat(paste0("Cell land use wildland: ", nWild, " agriculture: ", nAgri, " artificial: ", nArti, " rock: ", nRock, " water: ", nWater,"\n"))
  cat(paste0("Cells with soil: ", nSoil,"\n"))
  cat(paste0("Meteorological input class: ", class(meteo),"\n"))
  cat(paste0("Number of days to simulate: ",nDays,"\n"))
  cat(paste0("Number of summaries: ", nSummary,"\n"))
  cat(paste0("Number of outlet cells: ", length(outlets),"\n\n"))

  cat(paste0("Preparing ", localModel, " input:\n"))

  pb = txtProgressBar(0, nCells, style=3)
  for(i in 1:nCells) {
    setTxtProgressBar(pb,i)
    if(y@lct[i] %in% c("wildland", "agriculture")) {
      f = y@forestlist[[i]]
      s = y@soillist[[i]]
      if(localModel=="spwb") y@xlist[[i]] = forest2spwbInput(f, s, SpParams, localControl)
      else if(localModel=="growth") y@xlist[[i]] = forest2growthInput(f, s, SpParams, localControl)
    } else {
      y@xlist[[i]] = NA
    }
  }
  cat("\n\n")

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
  

  summary_function = function(object, model="SX") {
    if(!inherits(object,"soil")) return(list(SWE=NA, Psi1=NA, SoilVol=NA, WTD=NA))
    list(SWE = object$SWE,
         Psi1 = soil_psi(object)[1],
         SoilVol = sum(soil_water(object, model)),
         WTD = soil_waterTableDepth(object, model))
  }
  
  initialSoilContent = mean(getLandscapeVariable(y, "SoilVol"), na.rm=TRUE)
  initialSnowContent = mean(getLandscapeVariable(y, "SWE"), na.rm=TRUE)
  initialAquiferContent = mean(getLandscapeVariable(y, "AquiferVolume"), na.rm=T)
  initialLandscapeContent = initialSoilContent*(nSoil/nCells)+initialAquiferContent+initialSnowContent
  
  cat(paste0("Initial average soil water content (mm): ", round(initialSoilContent,2),"\n"))
  cat(paste0("Initial average snowpack water content (mm): ", round(initialSnowContent,2),"\n"))
  cat(paste0("Initial average aquifer water content (mm): ", round(initialAquiferContent,2),"\n"))
  cat(paste0("Initial watershed water content (mm): ", round(initialLandscapeContent,2),"\n"))
  
  cat(paste0("\nPerforming daily simulations:\n"))
  for(day in 1:nDays) {
    # cat(paste("Day #", day))
    cat(".")
    i = which(datesMeteo == dates[day]) #date index in meteo data
    doy = as.numeric(format(dates[day],"%j"))
    datechar = as.character(dates[day])
    if(inherits(meteo,"MeteorologyInterpolationData")) {
      ml = interpolationpixels(meteo, y, dates[day], verbose = TRUE)
      ml = ml@data
      # print(head(ml))
      gridMinTemperature = ml$MinTemperature
      gridMaxTemperature = ml$MaxTemperature
      gridMinRelativeHumidity = ml$MinRelativeHumidity
      gridMaxRelativeHumidity = ml$MaxRelativeHumidity
      gridPrecipitation = ml$Precipitation
      gridRadiation = ml$Radiation
      gridWindSpeed = ml$WindSpeed
    } else if(inherits(meteo,"SpatialPixelsMeteorology")) {
      ml = meteo@data[[i]]
      gridMinTemperature = ml$MinTemperature
      gridMaxTemperature = ml$MaxTemperature
      gridMinRelativeHumidity = ml$MinRelativeHumidity
      gridMaxRelativeHumidity = ml$MaxRelativeHumidity
      gridPrecipitation = ml$Precipitation
      gridRadiation = ml$Radiation
      gridWindSpeed = ml$WindSpeed
    } else {
      if(!oneMeteoCell) { # Read meteo grid from file
        f = paste(meteo$dir[i], meteo$filename[i],sep="/")
        if(!file.exists(f)) stop(paste("Meteorology file '", f,"' does not exist!", sep=""))
        ml = readmeteorologygrid(f)
        gridMinTemperature = as.numeric(ml["MinTemperature"])
        gridMaxTemperature = as.numeric(ml["MaxTemperature"])
        gridMinRelativeHumidity = as.numeric(ml["MinRelativeHumidity"])
        gridMaxRelativeHumidity = as.numeric(ml["MaxRelativeHumidity"])
        gridPrecipitation = as.numeric(ml["Precipitation"])
        gridRadiation = as.numeric(ml["Radiation"])
        gridWindSpeed = as.numeric(ml["WindSpeed"])
      } else { # repeat values for all cells
        gridMinTemperature = rep(meteo[i,"MeanTemperature"], nCells)
        gridMaxTemperature = rep(meteo[i,"MaxTemperature"], nCells)
        gridMinRelativeHumidity = rep(meteo[i,"MinRelativeHumidity"], nCells)
        gridMaxRelativeHumidity = rep(meteo[i,"MaxRelativeHumidity"], nCells)
        gridPrecipitation = rep(meteo[i,"Precipitation"], nCells)
        gridRadiation = rep(meteo[i, "Radiation"], nCells)
        gridWindSpeed = rep(meteo[i, "WindSpeed"], nCells)
      }
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
                           y@lct, y@xlist, y@soillist,
                           y@waterOrder, y@queenNeigh, y@waterQ,
                           y@bedrock, y@aquifer, y@snowpack,
                           correctionFactors,
                           datechar,
                           gridMeteo,
                           latitude, elevation, slope, aspect,
                           patchsize)
    
    res_day = ws_day[["WatershedWaterBalance"]]
    
    cat("+")
    summary_day = spatialSoilSummary(y, summary_function, localControl$soilFunctions)
    summary_df = summary_day@data
    ifactor = df.int[day]
    varsSum = c("Runon", "Runoff", "Rain", "NetRain", "Snow", "Snowmelt",
                "Infiltration", "DeepDrainage", "SaturationExcess",
                "AquiferDischarge", "SoilEvaporation", "Transpiration",
                "InterflowInput", "InterflowOutput", "BaseflowInput", "BaseflowOutput")
    varsState = c("SWE", "Psi1", "SoilVol", "WTD")
    DTAday = (y@bedrock$DepthToBedrock/1000.0) - (y@aquifer/y@bedrock$Porosity)/1000.0
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
  cat("done\n\n")
  #Average summaries
  LandscapeBalance$Precipitation = LandscapeBalance$Rain + LandscapeBalance$Snow
  SoilLandscapeBalance$Precipitation = SoilLandscapeBalance$Rain + SoilLandscapeBalance$Snow
  
  finalSoilContent = mean(getLandscapeVariable(y, "SoilVol"), na.rm=TRUE)
  finalSnowContent = mean(getLandscapeVariable(y, "SWE"), na.rm=TRUE)
  finalAquiferContent = mean(getLandscapeVariable(y, "AquiferVolume"), na.rm=T)
  finalLandscapeContent = finalSoilContent*(nSoil/nCells)+finalAquiferContent+finalSnowContent
  
  

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
  
  
  cat(paste0("Final average soil water content (mm): ", round(finalSoilContent,2),"\n"))
  cat(paste0("Final average snowpack water content (mm): ", round(finalSnowContent,2),"\n"))
  cat(paste0("Final average aquifer water content (mm): ", round(finalAquiferContent,2),"\n"))
  cat(paste0("Final watershed water content (mm): ", round(finalLandscapeContent,2),"\n"))
  
  
  snowpack_wb = Snowsum - Snowmeltsum
  cat(paste0("\nChange in snowpack water content (mm): ", round(finalSnowContent - initialSnowContent,2),"\n"))
  cat(paste0("Snowpack water balance result (mm): ",round(snowpack_wb,2),"\n"))
  cat(paste0("Snowpack water balance components:\n"))
  cat(paste0("  Snow fall (mm) ", round(Snowsum,2), " Snow melt (mm) ",round(Snowmeltsum,2),"\n"))
  
  soil_input = (SoilNetRainsum + SoilSnowmeltsum + SoilRunonsum + SoilAquiferDischargesum+SoilInterflowInputsum)
  soil_output = (SoilRunoffsum + SoilDeepDrainagesum + SoilSoilEvaporationsum + SoilTranspirationsum +SoilInterflowOutputsum)
  soil_wb =  soil_input - soil_output
  cat(paste0("\nChange in soil water content (mm): ", round(finalSoilContent - initialSoilContent,2),"\n"))
  cat(paste0("Soil water balance result (mm): ",round(soil_wb,2),"\n"))
  cat(paste0("Soil water balance components:\n"))
  cat(paste0("  Net rainfall (mm) ", round(SoilNetRainsum,2)," Snow melt (mm) ", round(SoilSnowmeltsum,2),"\n"))
  cat(paste0("  Runon (mm) ", round(SoilRunonsum,2)," Runoff (mm) ",round(SoilRunoffsum,2),"\n"))
  cat(paste0("  Subsurface input (mm) ",round(SoilInterflowInputsum,2),"  Subsurface output (mm) ",round(SoilInterflowOutputsum,2),"\n"))
  cat(paste0("  Deep drainage (mm) ",round(SoilDeepDrainagesum,2)," Aquifer discharge (mm) ", round(SoilAquiferDischargesum,2),"\n"))
  cat(paste0("  Soil evaporation (mm) ",round(SoilSoilEvaporationsum,2), " Plant transpiration (mm) ", round(SoilTranspirationsum,2),"\n"))
  
  aquifer_wb = DeepDrainagesum - AquiferDischargesum
  cat(paste0("\nChange in aquifer water content (mm): ", round(finalAquiferContent - initialAquiferContent,2),"\n"))
  cat(paste0("Aquifer water balance result (mm): ",round(aquifer_wb,2),"\n"))
  cat(paste0("Aquifer water balance components:\n"))
  cat(paste0("  Deep drainage (mm) ", round(DeepDrainagesum,2), " Aquifer discharge (mm) ",round(AquiferDischargesum,2),"\n"))

  
  landscape_wb = Precipitationsum - Exportsum - SoilEvaporationsum - Transpirationsum - Interceptionsum
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

  l <- list(sp = as(y, "SpatialPixels"), 
            summarylist = summarylist,
            WatershedBalance = LandscapeBalance,
            WatershedSoilBalance = SoilLandscapeBalance,
            DailyRunoff = DailyRunoff)
  class(l)<-c(landModel,"summarypixels","list")
  return(l)
}

spwbland<-function(y, SpParams, meteo, dates = NULL,
                   summaryFreq = "years",
                   localControl = medfate::defaultControl(),
                   correctionFactors = defaultWatershedCorrectionFactors()) {
  return(.landSim("spwbland",
                  y = y, SpParams = SpParams, meteo = meteo, dates = dates,
                  summaryFreq = summaryFreq, 
                  localControl = localControl,
                  correctionFactors = correctionFactors))
}
growthland<-function(y, SpParams, meteo, dates = NULL,
                   summaryFreq = "years",
                   localControl = medfate::defaultControl(),
                   correctionFactors = defaultWatershedCorrectionFactors()) {
  return(.landSim("growthland",
                  y = y, SpParams = SpParams, meteo = meteo, dates = dates,
                  summaryFreq = summaryFreq, 
                  localControl = localControl,
                  correctionFactors = correctionFactors))
}