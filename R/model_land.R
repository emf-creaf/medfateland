.landSim<-function(landModel = "spwb_land", 
                   y, SpParams, meteo, dates = NULL,
                   CO2ByYear = numeric(0), 
                   summary_frequency = "years",
                   local_control = medfate::defaultControl(),
                   correction_factors = default_watershed_correction_factors(),
                   progress = TRUE, header_footer = progress) {

  #check input
  if(!inherits(y, "sf")) stop("'y' has to be of class 'sf'.")
  if(!is.null(dates)) if(!inherits(dates, "Date")) stop("'dates' has to be of class 'Date'.")

  #duplicate input (to avoid modifying input objects)
  y = rlang::duplicate(y)
  
  if(landModel == "spwb_land") localModel = "spwb"
  else if(landModel=="growth_land") localModel = "growth"
  else if(landModel=="fordyn_land") localModel = "growth"
  
  latitude = sf::st_coordinates(sf::st_transform(sf::st_geometry(y),4326))[,2]


  if(!is.null(meteo)) {
    if(inherits(meteo,"data.frame")) {
      datesMeteo = as.Date(row.names(meteo))
    } else if(inherits(meteo, "stars")) {
      datesMeteo = as.Date(stars::st_get_dimension_values(meteo, "date"))
    }
  } else {
    if(!("meteo" %in% names(y))) stop("Column 'meteo' must be defined in 'y' if not supplied separately")
    datesMeteo = as.Date(row.names(y$meteo[[1]]))
    # check that all items have same dates
    for(i in 1:nrow(y)) {
      if(!all(as.Date(row.names(y$meteo[[i]]))==datesMeteo)) stop("All spatial elements need to have the same weather dates.")
    }
  }
  if(is.null(dates)) {
    dates = datesMeteo
  } else {
    if(sum(dates %in% datesMeteo)<length(dates))
      stop("Dates in 'dates' is not a subset of dates in 'meteo'.")
  }
  date.factor = cut(dates, breaks=summary_frequency)
  df.int = as.numeric(date.factor)
  nDays = length(dates)
  nCells = nrow(y)
  isSoilCell = y$land_cover_type %in% c("wildland", "agriculture")
  nSoil = sum(isSoilCell)
  nWild = sum(y$land_cover_type %in% c("wildland"))
  nAgri = sum(y$land_cover_type %in% c("agriculture"))
  nRock = sum(y$land_cover_type %in% c("rock"))
  nArti = sum(y$land_cover_type %in% c("artificial"))
  nWater = sum(y$land_cover_type %in% c("water"))
  nSummary = sum(table(date.factor)>0)
  t.df = as.numeric(table(df.int))

  #Determine outlet cells (those without downhill neighbors)
  isOutlet = (unlist(lapply(y$waterQ, sum))==0)
  outlets = which(isOutlet)

  patchsize = mean(y$represented_area, na.rm=TRUE)

  #Print information area
  if(header_footer) {
    cli::cli_li(paste0("Grid cells: ", nCells,", patchsize: ", patchsize," m2, area: ", nCells*patchsize/10000," ha"))
    cli::cli_li(paste0("Cell land use wildland: ", nWild, " agriculture: ", nAgri, " artificial: ", nArti, " rock: ", nRock, " water: ", nWater))
    cli::cli_li(paste0("Cells with soil: ", nSoil))
    cli::cli_li(paste0("Number of days to simulate: ",nDays))
    cli::cli_li(paste0("Number of summaries: ", nSummary))
    cli::cli_li(paste0("Number of outlet cells: ", length(outlets)))
  }
  #Check neighbours
  if(header_footer) cli::cli_progress_step(paste0("Checking topology"))
  if(max(y$waterOrder)>nCells) cli::cli_abort(paste0("Water order values outside valid range [1-",nCells,"]"))
  for(i in 1:nCells) { 
    ni <- y$queenNeigh[[i]]
    qi <- y$waterQ[[i]]
    if(max(ni)>nCells || min(ni) < 1) {
      cli::cli_abort(paste0("Cell ", i, " pointed to non-existing neighbors"))
    }
    if(length(qi) != length(ni)) {
      cli::cli_abort(paste0("Cell ", i, " has different number of neighbors in 'waterQ' than 'queenNeigh'"))
    }
    if(!isOutlet[i]) {
      if(abs(sum(qi) - 1) > 0.0001) {
        cli::cli_abort(paste0("'waterQ' values for cell ", i, " do not add up to 1"))
      }
    }
  }
  
  if(header_footer) cli::cli_progress_step(paste0("Checking ", localModel, " input"))
  initialized_cells <- 0
  for(i in 1:nCells) { #Initialize if not previously initialized
    if((y$land_cover_type[i] == "wildland") && (is.null(y$state[[i]]))) {
      f = y$forest[[i]]
      s = y$soil[[i]]
      if(localModel=="spwb") y$state[[i]] = forest2spwbInput(f, s, SpParams, local_control)
      else if(localModel=="growth") y$state[[i]] = forest2growthInput(f, s, SpParams, local_control)
      initialized_cells <- initialized_cells + 1
    } 
    else if((y$land_cover_type[i] == "agriculture") && (is.null(y$state[[i]]))) {
      s = y$soil[[i]]
      cf = y$crop_factor[i]
      if(inherits(s, "data.frame")) s = medfate::soil(s)
      y$state[[i]] = .aspwbInput(cf, local_control, s)
      initialized_cells <- initialized_cells + 1
    } 
  }
  if(header_footer) {
    cli::cli_progress_step(paste0( initialized_cells, " cells needed initialization"))
  }

  #Output matrices
  DailyRunoff = matrix(0,nrow = nDays, ncol = length(outlets))
  colnames(DailyRunoff) = outlets
  rownames(DailyRunoff) = as.character(dates)
  
  summarylist = vector("list", nCells)
  vars = c("MinTemperature","MaxTemperature","PET", "Runon", "Runoff", 
           "Infiltration", "Rain", "NetRain", "Snow",
           "Snowmelt", "Interception", "DeepDrainage", "AquiferDischarge", "SaturationExcess",
           "SoilEvaporation", "Transpiration", "SWE", "SoilVol","Psi1", "WTD", "DTA",
           "InterflowInput", "InterflowOutput", "BaseflowInput", "BaseflowOutput")
  varsSum = c("PET","Runon", "Runoff", "Rain", "NetRain", "Snow", "Snowmelt",
               "Infiltration", "DeepDrainage", "SaturationExcess",
               "AquiferDischarge", "SoilEvaporation", "Transpiration",
               "InterflowInput", "InterflowOutput", "BaseflowInput", "BaseflowOutput")
  varsMean = c( "MinTemperature", "MaxTemperature")
  varsState = c("SWE", "Psi1", "SoilVol", "WTD")
  for(i in 1:nCells) {
    m = matrix(0, nrow = nSummary, ncol = length(vars))
    colnames(m) = vars
    rownames(m) = levels(date.factor)[1:nSummary]
    summarylist[[i]] = m
  }

  LandscapeBalance = data.frame(dates = levels(date.factor)[1:nSummary],
                                Precipitation = rep(0, nSummary),
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
  SoilLandscapeBalance = data.frame(dates = levels(date.factor)[1:nSummary],
                                    Precipitation = rep(0, nSummary),
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
  initialAquiferContent = mean(extract_variables(y, "aquifer_volume")$aquifer_volume, na.rm=T)
  initialLandscapeContent = initialSoilContent*(nSoil/nCells)+initialAquiferContent+initialSnowContent
  
  if(progress) {
    # cli::cli_li(paste0("Initial average soil water content (mm): ", round(initialSoilContent,2)))
    # cli::cli_li(paste0("Initial average snowpack water content (mm): ", round(initialSnowContent,2)))
    # cli::cli_li(paste0("Initial average aquifer water content (mm): ", round(initialAquiferContent,2)))
    # cli::cli_li(paste0("Initial watershed water content (mm): ", round(initialLandscapeContent,2)))
    # 
    cli::cli_progress_bar("Daily simulations", total = nDays)
  }
  
  if(inherits(meteo, "SpatialPixelsMeteorology") || inherits(meteo, "SpatialGridMeteorology")) {
    meteo = meteoland::extractgridpoints(meteo, as(sf::st_geometry(y), "Spatial"))
    # now meteo is a SpatialPointsMeteorology
  }  
  for(day in 1:nDays) {
    # cat(paste("Day #", day))
    if(progress) cli::cli_progress_update()
    doy = as.numeric(format(dates[day],"%j"))
    datechar = as.character(dates[day])
    yearString = substr(datechar, 1, 4)
    gridMinTemperature = rep(NA, nCells)
    gridMaxTemperature = rep(NA, nCells)
    gridMinRelativeHumidity = rep(NA, nCells)
    gridMaxRelativeHumidity = rep(NA, nCells)
    gridPrecipitation = rep(NA, nCells)
    gridRadiation = rep(NA, nCells)
    gridWindSpeed = rep(NA, nCells)
    Catm = NA
    if(yearString %in% names(CO2ByYear)) Catm = CO2ByYear[yearString]
    gridCO2 = rep(Catm, nCells)
    
    if(!is.null(meteo)) {
      if(inherits(meteo,"stars")) {
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
      } 
      else { # data frame
        imeteo = which(datesMeteo == dates[day]) #date index in meteo data
        # repeat values for all cells
        gridMinTemperature = rep(meteo[imeteo,"MinTemperature"], nCells)
        gridMaxTemperature = rep(meteo[imeteo,"MaxTemperature"], nCells)
        gridMinRelativeHumidity = rep(meteo[imeteo,"MinRelativeHumidity"], nCells)
        gridMaxRelativeHumidity = rep(meteo[imeteo,"MaxRelativeHumidity"], nCells)
        gridPrecipitation = rep(meteo[imeteo,"Precipitation"], nCells)
        gridRadiation = rep(meteo[imeteo, "Radiation"], nCells)
        gridWindSpeed = rep(meteo[imeteo, "WindSpeed"], nCells)
        if("CO2" %in% names(meteo)) gridCO2 = rep(meteo[imeteo, "CO2"], nCells)
      }
    } 
    else {
      imeteo = which(datesMeteo == dates[day]) #date index in meteo data
      for(iml in 1:nCells) {
        meti = y$meteo[[iml]]
        gridMinTemperature[iml] = meti$MinTemperature[imeteo]
        gridMaxTemperature[iml] = meti$MaxTemperature[imeteo]
        gridMinRelativeHumidity[iml] = meti$MinRelativeHumidity[imeteo]
        gridMaxRelativeHumidity[iml] = meti$MaxRelativeHumidity[imeteo]
        gridPrecipitation[iml] = meti$Precipitation[imeteo]
        gridRadiation[iml] = meti$Radiation[imeteo]
        gridWindSpeed[iml] = meti$WindSpeed[imeteo]
        if("CO2" %in% names(meti)) gridCO2[iml] = meti$CO2[imeteo]
      }
    }
    
    gridRadiation[is.na(gridRadiation)] = mean(gridRadiation, na.rm=T)
    gridMeteo = data.frame(MinTemperature = gridMinTemperature, 
                           MaxTemperature = gridMaxTemperature,
                           MinRelativeHumidity = gridMinRelativeHumidity,
                           MaxRelativeHumidity = gridMaxRelativeHumidity,
                           Precipitation = gridPrecipitation,
                           Radiation = gridRadiation,
                           WindSpeed = gridWindSpeed,
                           CO2 = gridCO2)
    ws_day = .watershedDay(localModel,
                           y$land_cover_type, y$state, y$soil,
                           y$waterOrder, y$queenNeigh, y$waterQ,
                           y$depth_to_bedrock, y$bedrock_conductivity, y$bedrock_porosity, 
                           y$aquifer, y$snowpack,
                           correction_factors,
                           datechar,
                           gridMeteo,
                           latitude, y$elevation, y$slope, y$aspect,
                           patchsize, FALSE)
    
    res_day = ws_day[["WatershedWaterBalance"]]
    
    summary_df = landscape_summary(y, "soil", soil_summary_function, local_control$soilFunctions, 
                                   unlist = TRUE)
    ifactor = df.int[day]

    DTAday = (y$depth_to_bedrock/1000.0) - (y$aquifer/y$bedrock_porosity)/1000.0
    for(i in 1:nCells) {
      for(v in varsSum) {
        summarylist[[i]][ifactor,v] = summarylist[[i]][ifactor,v] + res_day[[v]][i]
      }
      for(v in varsMean) {
        summarylist[[i]][ifactor,v] = summarylist[[i]][ifactor,v] + res_day[[v]][i]/t.df[ifactor]
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
  if(progress) cli::cli_progress_done()
  #Average summaries
  LandscapeBalance$Precipitation = LandscapeBalance$Rain + LandscapeBalance$Snow
  SoilLandscapeBalance$Precipitation = SoilLandscapeBalance$Rain + SoilLandscapeBalance$Snow
  
  finalSoilContent = mean(extract_variables(y, "soilvolcurr")$soilvolcurr, na.rm=TRUE)
  finalSnowContent = mean(extract_variables(y, "snowpack")$snowpack, na.rm=TRUE)
  finalAquiferContent = mean(extract_variables(y, "aquifer_volume")$aquifer_volume, na.rm=T)
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
  
  
  # if(progress){
  #   cli::cli_li(paste0("Final average soil water content (mm): ", round(finalSoilContent,2)))
  #   cli::cli_li(paste0("Final average snowpack water content (mm): ", round(finalSnowContent,2)))
  #   cli::cli_li(paste0("Final average aquifer water content (mm): ", round(finalAquiferContent,2)))
  #   cli::cli_li(paste0("Final watershed water content (mm): ", round(finalLandscapeContent,2)))
  # }
  
  snowpack_wb = Snowsum - Snowmeltsum
  if(header_footer) {
    cli::cli_li("Water balance check")
    cat(paste0("  Change in snowpack water content (mm): ", round(finalSnowContent - initialSnowContent,2),"\n"))
    cat(paste0("  Snowpack water balance result (mm): ",round(snowpack_wb,2),"\n"))
    cat(paste0("  Snowpack water balance components:\n"))
    cat(paste0("    Snow fall (mm) ", round(Snowsum,2), " Snow melt (mm) ",round(Snowmeltsum,2),"\n"))
  }
  soil_input = (SoilNetRainsum + SoilSnowmeltsum + SoilRunonsum + SoilAquiferDischargesum+SoilInterflowInputsum)
  soil_output = (SoilRunoffsum + SoilDeepDrainagesum + SoilSoilEvaporationsum + SoilTranspirationsum +SoilInterflowOutputsum)
  soil_wb =  soil_input - soil_output
  if(header_footer) {
    cat(paste0("\n  Change in soil water content (mm): ", round(finalSoilContent - initialSoilContent,2),"\n"))
    cat(paste0("  Soil water balance result (mm): ",round(soil_wb,2),"\n"))
    cat(paste0("  Soil water balance components:\n"))
    cat(paste0("    Net rainfall (mm) ", round(SoilNetRainsum,2)," Snow melt (mm) ", round(SoilSnowmeltsum,2),"\n"))
    cat(paste0("    Runon (mm) ", round(SoilRunonsum,2)," Runoff (mm) ",round(SoilRunoffsum,2),"\n"))
    cat(paste0("    Subsurface input (mm) ",round(SoilInterflowInputsum,2),"  Subsurface output (mm) ",round(SoilInterflowOutputsum,2),"\n"))
    cat(paste0("    Deep drainage (mm) ",round(SoilDeepDrainagesum,2)," Aquifer discharge (mm) ", round(SoilAquiferDischargesum,2),"\n"))
    cat(paste0("    Soil evaporation (mm) ",round(SoilSoilEvaporationsum,2), " Plant transpiration (mm) ", round(SoilTranspirationsum,2),"\n"))
  }
  
  aquifer_wb = DeepDrainagesum - AquiferDischargesum
  if(header_footer){
    cat(paste0("\n  Change in aquifer water content (mm): ", round(finalAquiferContent - initialAquiferContent,2),"\n"))
    cat(paste0("  Aquifer water balance result (mm): ",round(aquifer_wb,2),"\n"))
    cat(paste0("  Aquifer water balance components:\n"))
    cat(paste0("    Deep drainage (mm) ", round(DeepDrainagesum,2), " Aquifer discharge (mm) ",round(AquiferDischargesum,2),"\n"))
  }
  
  landscape_wb = Precipitationsum - Exportsum - SoilEvaporationsum - Transpirationsum - Interceptionsum
  if(header_footer) {
    cat(paste0("\n  Change in watershed water content (mm): ", round(finalLandscapeContent - initialLandscapeContent,2),"\n"))
    cat(paste0("  Watershed water balance result (mm): ",round(landscape_wb,2),"\n"))
    cat(paste0("  Watershed water balance components:\n"))
    cat(paste0("    Precipitation (mm) ", round(Precipitationsum,2),"\n"))
    cat(paste0("    Interception (mm) ", round(Interceptionsum,2), " Soil evaporation (mm) ",round(SoilEvaporationsum,2), " Plant Transpiration (mm) ",round(Transpirationsum,2),"\n"))
    cat(paste0("    Export (mm) ", round(Exportsum,2),"\n"))
    cat(paste0("  Watershed lateral flows:\n"))
    cat(paste0("    Subsurface flow (mm) ",round(Interflowsum,2),"\n"))
    cat(paste0("    Groundwater flow (mm) ", round(Baseflowsum,2),"\n"))
  }
  if(header_footer)  cli::cli_li("Done")
  
  sf = sf::st_sf(geometry=sf::st_geometry(y))
  sf$state = y$state
  sf$aquifer = y$aquifer
  sf$snowpack = y$snowpack
  sf$summary = summarylist

  l <- list(sf = sf::st_as_sf(tibble::as_tibble(sf)),
            watershed_balance = LandscapeBalance,
            watershed_soil_balance = SoilLandscapeBalance,
            daily_runoff = DailyRunoff)
  class(l)<-c(landModel, "list")
  return(l)
}

#' Watershed simulations
#' 
#' Functions to perform simulations on a watershed described by a set of connected grid cells
#' 
#' @details
#' \itemize{
#'   \item{Function \code{spwb_land} implements a distributed hydrological model that simulates daily local water balance, from \code{\link{spwb_day}}, 
#'         on grid cells of a watershed while accounting for overland runoff, subsurface flow and groundwater flow between cells.}
#'   \item{Function \code{growth_land} is similar to \code{spwb_land}, but includes daily local carbon balance, growth and mortality processes in grid cells, 
#'         provided by \code{\link{growth_day}}.} 
#'   \item{Function \code{fordyn_land} extends the previous two functions with the simulation of management, recruitment
#'         and resprouting.}
#' }
#' @param sf An object of class \code{\link{sf}} with the following columns:
#'   \itemize{
#'     \item{\code{geometry}: Spatial geometry.}
#'     \item{\code{id}: Cell ids (normally row number).}
#'     \item{\code{elevation}: Elevation above sea level (in m).}
#'     \item{\code{slope}: Slope (in degrees).}
#'     \item{\code{aspect}: Aspect (in degrees).}
#'     \item{\code{land_cover_type}: Land cover type of each grid cell (values should be 'wildland', 'agriculture', 'rock', 'artificial' or 'water').}
#'     \item{\code{forest}: Objects of class \code{\link{forest}}.}
#'     \item{\code{soil}: Objects of class \code{\link{soil}} or data frames of physical properties.}
#'     \item{\code{state}: Objects of class \code{\link{spwbInput}} or \code{\link{growthInput}} (optional).}
#'     \item{\code{meteo}: Data frames with weather data (required if parameter \code{meteo = NULL}).}
#'     \item{\code{crop_factor}: Crop evapo-transpiration factor. Only required for 'agriculture' land cover type.}
#'     \item{\code{waterOrder}: Integer vector indicating cell processing order.}
#'     \item{\code{waterQ}: A list of water discharge values to neighbors.}
#'     \item{\code{queenNeigh}: A list of integers identifying the (up to 8) queen neighbors, for each cell.}
#'     \item{\code{channel}: A logical vector indicating whether each cell belongs to the channel network.}
#'     \item{\code{depth_to_bedrock}: Depth to bedrock (mm).}
#'     \item{\code{bedrock_conductivity}: Bedrock (saturated) conductivity (in m·day-1).}
#'     \item{\code{bedrock_porosity}: Bedrock porosity.}
#'     \item{\code{snowpack}: A numeric vector with the snow water equivalent content of the snowpack in each cell.}
#'     \item{\code{aquifer}: A numeric vector with the water content of the aquifer in each cell.}
#'     \item{\code{represented_area}: Area represented by each cell (in m2).}
#'     \item{\code{management_arguments}: Lists with management arguments (optional, relevant for \code{fordyn_land} only).}
#'   }
#' @param SpParams A data frame with species parameters (see \code{\link{SpParamsMED}}).
#' @param meteo Input meteorological data (see \code{\link{spwb_spatial}}).
#' @param dates A \code{\link{Date}} object describing the days of the period to be modeled.
#' @param CO2ByYear A named numeric vector with years as names and atmospheric CO2 concentration (in ppm) as values. Used to specify annual changes in CO2 concentration along the simulation (as an alternative to specifying daily values in \code{meteo}).
#' @param summary_frequency Frequency in which summary layers will be produced (e.g. "years", "months", ...) (see \code{\link{cut.Date}}).
#'                          In \code{fordyn_land} summaries are always produced at monthly resolution. 
#' @param local_control A list of control parameters (see \code{\link{defaultControl}}) for function \code{\link{spwb_day}} or \code{\link{growth_day}}.
#' @param correction_factors A list of watershed correction factors for hydraulic parameters.
#' @param progress Boolean flag to display progress information for simulations.
#' @param management_function A function that implements forest management actions (see \code{\link{fordyn}}).
#' of such lists, one per spatial unit.
#'  
#' @return Functions \code{spwb_land}, \code{growth_land} and \code{growth_land} return a list of class of the same name as the function with the following elements:
#' \itemize{
#'   \item{\code{sf}: An object of class \code{\link{sf}}, similar to the output of \code{\link{spwb_spatial}}, 
#'   with the following columns:
#'     \itemize{
#'        \item{\code{geometry}: Spatial geometry.}
#'        \item{\code{id}: Cell id, taken from the input.}
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
#'     In function \code{fordyn_land} the \code{\link{sf}} object contains additional columns:
#'     \itemize{
#'        \item{\code{forest}: A list of \code{\link{forest}} objects for each simulated stand (only in \code{fordynspatial}), to be used in subsequent simulations (see \code{\link{update_landscape}}).}
#'        \item{\code{management_arguments}: A list of management arguments for each simulated stand (only in \code{fordynspatial}), to be used in subsequent simulations (see \code{\link{update_landscape}}).}
#'        \item{\code{tree_table}: A list of data frames for each simulated stand, containing the living trees at each time step.}
#'        \item{\code{shrub_table}: A list of data frames for each simulated stand, containing the living shrub at each time step.}
#'        \item{\code{dead_tree_table}: A list of data frames for each simulated stand, containing the dead trees at each time step.}
#'        \item{\code{dead_shrub_table}: A list of data frames for each simulated stand, containing the dead shrub at each time step.}
#'        \item{\code{cut_tree_table}: A list of data frames for each simulated stand, containing the cut trees at each time step.}
#'        \item{\code{cut_shrub_table}: A list of data frames for each simulated stand, containing the cut shrub at each time step.}
#'     }
#'   }
#'   \item{\code{watershed_balance}: A data frame with as many rows as summary points and where columns are components of the water balance at the watershed level (i.e., rain, snow, interception, infiltration, soil evaporation, plant transpiration, ...).}
#'   \item{\code{watershed_soil_balance}: A data frame with as many rows as summary points and where columns are components of the water balance at the watershed level restricted to those cells with a soil definition.}
#'   \item{\code{daily_runoff}: A matrix with daily runoff (in m3/day) at each of the outlet cells of the landscape.}
#' }
#' 
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF.
#' 
#' @seealso \code{\link{spwb_day}},  \code{\link{growth_day}},
#' \code{\link{spwb_spatial}}
#' 
#' @examples 
#' \dontrun{
#' # Load example watershed data
#' data("example_watershed")
#' 
#' # Set crop factor 
#' example_watershed$crop_factor <- NA
#' example_watershed$crop_factor[example_watershed$land_cover_type=="agriculture"] <- 0.75
#' 
#' # Load example meteo data frame from package meteoland
#' data("examplemeteo")
#'   
#' # Load default medfate parameters
#' data("SpParamsMED")
#'   
#' # Set simulation period
#' dates <- seq(as.Date("2001-01-01"), as.Date("2001-03-31"), by="day")
#' 
#' # Launch simulations
#' res <- spwb_land(example_watershed, SpParamsMED, examplemeteo, 
#'                  dates = dates, summary_frequency = "month")
#' }
#' 
#' @name spwb_land
spwb_land<-function(sf, SpParams, meteo= NULL, dates = NULL,
                    CO2ByYear = numeric(0), 
                    summary_frequency = "years",
                    local_control = medfate::defaultControl(),
                    correction_factors = default_watershed_correction_factors(),
                    progress = TRUE) {
  if(progress) cli::cli_h1(paste0("Simulation of model 'spwb' over a watershed"))
  return(.landSim("spwb_land",
                  y = sf, SpParams = SpParams, meteo = meteo, dates = dates,
                  CO2ByYear = CO2ByYear,
                  summary_frequency = summary_frequency, 
                  local_control = local_control,
                  correction_factors = correction_factors, progress = progress, header_footer = progress))
}
#' @rdname spwb_land
growth_land<-function(sf, SpParams, meteo = NULL, dates = NULL,
                      CO2ByYear = numeric(0), 
                      summary_frequency = "years",
                      local_control = medfate::defaultControl(),
                      correction_factors = default_watershed_correction_factors(),
                      progress = TRUE) {
  if(progress) cli::cli_h1(paste0("Simulation of model 'growth' over a watershed"))
  return(.landSim("growth_land",
                  y = sf, SpParams = SpParams, meteo = meteo, dates = dates,
                  CO2ByYear = CO2ByYear,
                  summary_frequency = summary_frequency, 
                  local_control = local_control,
                  correction_factors = correction_factors, progress = progress, header_footer = progress))
}

#' @rdname spwb_land
fordyn_land <- function(sf, SpParams, meteo = NULL, dates = NULL,
                        CO2ByYear = numeric(0), 
                        local_control = medfate::defaultControl(),
                        correction_factors = default_watershed_correction_factors(),
                        management_function = NULL,
                        progress = TRUE) {
  
  if(progress) cli::cli_h1(paste0("Simulation of model 'fordyn' over a watershed"))
  
  nCells = nrow(sf)
  isSoilCell = sf$land_cover_type %in% c("wildland", "agriculture")
  nSoil = sum(isSoilCell)
  nWild = sum(sf$land_cover_type %in% c("wildland"))
  nAgri = sum(sf$land_cover_type %in% c("agriculture"))
  nRock = sum(sf$land_cover_type %in% c("rock"))
  nArti = sum(sf$land_cover_type %in% c("artificial"))
  nWater = sum(sf$land_cover_type %in% c("water"))
  
  patchsize = mean(sf$represented_area, na.rm=TRUE)
  
  if(is.null(dates)) {
    # Try to get dates from input
    if(!is.null(meteo)) {
      if(inherits(meteo, "data.frame")) dates <- as.Date(row.names(meteo))
    } else {
      if("meteo" %in% names(sf)) {
        dates = as.Date(row.names(sf$meteo[[1]]))
      }
    }
  }
  years <- as.numeric(format(dates, "%Y"))
  months <- as.numeric(format(dates, "%m"))
  yearsUnique <- unique(years)
  nYears <- length(yearsUnique)
  
  if(progress) {
    cli::cli_li(paste0("Grid cells: ", nCells,", patchsize: ", patchsize," m2, area: ", nCells*patchsize/10000," ha"))
    cli::cli_li(paste0("Cell land use wildland: ", nWild, " agriculture: ", nAgri, " artificial: ", nArti, " rock: ", nRock, " water: ", nWater))
    cli::cli_li(paste0("Cells with soil: ", nSoil))
    cli::cli_li(paste0("Number of years to simulate: ",nYears))
  }
  # Init growth 
  if(progress) cli::cli_h3(paste0("Initialisation"))
  if(local_control$allowRecruitment) {
    for(i in 1:nCells) { 
      if(sf$land_cover_type[i] == "wildland")  {
        forest <- sf$forest[[i]]
        forest$treeData <- forest$treeData[,c("Species","DBH", "Height","N","Z50","Z95")]
        forest$shrubData <- forest$shrubData[,c("Species","Height","Cover", "Z50","Z95")]
        sf$forest[[i]] <- forest
      }
    }
  }
  sf <- initialize_landscape(sf, SpParams, local_control = local_control, model = "growth")
  
  LandscapeBalance <- NULL 
  SoilLandscapeBalance <- NULL
  DailyRunoff <- NULL
  cell_summary <- NULL
  
  #initial tree/shrub tables
  if(progress) cli::cli_progress_step("Initializing 'fordyn' output tables")
  treeTableVec <- vector("list", nCells)
  shrubTableVec <- vector("list", nCells)
  deadTreeTableVec <- vector("list", nCells)
  deadShrubTableVec <- vector("list", nCells)
  cutTreeTableVec <- vector("list", nCells)
  cutShrubTableVec <- vector("list", nCells)
  for(i in 1:nCells) {
    if(sf$land_cover_type[i] == "wildland")  {
      xi <- sf$state[[i]]
      treeTable <- medfate:::.createTreeTable(0, NA, xi)
      shrubTable <- medfate:::.createShrubTable(0, NA, xi)
      treeTableVec[[i]] <- treeTable
      shrubTableVec[[i]] <- shrubTable
      deadTreeTableVec[[i]] <- medfate:::.createDeadTreeTable(0, NA, xi)
      deadShrubTableVec[[i]] <- medfate:::.createDeadShrubTable(0, NA, xi)
      cutTreeTableVec[[i]] <- treeTable[numeric(),,drop = FALSE]
      cutShrubTableVec[[i]] <- shrubTable[numeric(),,drop = FALSE]
    }
  }
  if(progress) cli::cli_progress_done()
  
  #Simulations
  for(iYear in 1:nYears) {
    year <- yearsUnique[iYear]
    if(progress) cli::cli_h3(paste0("Simulating year ", year, " [", iYear,"/", nYears,"]"))
    meteoYear <- meteo[years==year,]
    monthsYear <- months[years==year]
    datesYear <- dates[years==year]
    # 1.1 Calls growth_land model
    if(progress) cli::cli_li(paste0("Growth/mortality"))
    GL <- .landSim("growth_land",
                   y = sf, SpParams = SpParams, meteo = meteo, dates = datesYear,
                   CO2ByYear = CO2ByYear,
                   summary_frequency = "month", # Summary frequency to use statistics
                   local_control = local_control,
                   correction_factors = correction_factors, progress = progress, header_footer = FALSE)
    
    # Store snowpack and aquifer state
    sf$aquifer <- GL$sf$aquifer
    sf$snowpack <- GL$sf$snowpack
    
    #Store landscape and cell summaries
    if(iYear==1) {
      DailyRunoff <- GL$daily_runoff
      LandscapeBalance <- GL$watershed_balance
      SoilLandscapeBalance <- GL$watershed_soil_balance
      cell_summary <- GL$sf$summary
    } else {
      DailyRunoff <- rbind(DailyRunoff, GL$daily_runoff)
      LandscapeBalance <- rbind(LandscapeBalance, GL$watershed_balance)
      SoilLandscapeBalance <- rbind(SoilLandscapeBalance, GL$watershed_soil_balance)
      for(i in 1:nCells) { 
        cell_summary[[i]] <- rbind(cell_summary[[i]], GL$sf$summary[[i]])
      }
    }
    if(progress) cli::cli_li(paste0("Management/recruitment/resprouting"))
    for(i in 1:nCells) { 
      if(sf$land_cover_type[i] == "wildland")  {
        forest <- sf$forest[[i]]
        # 1 Retrieve modified growth output
        xo <- GL$sf$state[[i]]
        # 2.1 Update dead tree/shrub tables
        deadTreeTableYear <- medfate:::.createDeadTreeTable(iYear, year, xo)
        deadShrubTableYear <- medfate:::.createDeadShrubTable(iYear, year, xo)
        
        # 2.2 Update forest structural variables
        isTree <- is.na(xo$above$Cover)
        forest$treeData$N  <- xo$above$N[isTree]
        forest$treeData$DBH  <- xo$above$DBH[isTree]
        forest$treeData$Height  <- xo$above$H[isTree]
        if(local_control$shrubDynamics) {
          forest$shrubData$Cover  <- xo$above$Cover[!isTree]
          forest$shrubData$Height  <- xo$above$H[!isTree]
        }
        # 2.3 Call management function if required
        cutTreeTableYear <- NULL
        cutShrubTableYear <- NULL
        management_result <- NULL
        planted_forest <- emptyforest()
        if(!is.null(management_function) && ("management_args" %in% names(sf))) {
          management_args <- sf$management_args[[i]]
          if(!is.null(management_args)) {
            management_result <- do.call(management_function, list(x = forest, args= management_args, verbose = FALSE))
            # Update forest and xo objects
            forest$treeData$N <- pmax(0,forest$treeData$N - management_result$N_tree_cut)
            xo$above$N[isTree] <- forest$treeData$N
            forest$shrubData$Cover <- pmax(0,forest$shrubData$Cover - management_result$Cover_shrub_cut)
            xo$above$Cover[!isTree] <- forest$shrubData$Cover
            # Update cut tables
            cutTreeTableYear <- medfate:::.createCutTreeTable(iYear, year, xo, management_result$N_tree_cut)
            cutShrubTableYear <- medfate:::.createCutShrubTable(iYear, year, xo, management_result$Cover_shrub_cut)
            # Retrieve plantation information
            planted_forest <- management_result$planted_forest
            if(nrow(planted_forest$treeData)>0) {
              for(i in 1:nrow(planted_forest$treeData)) {
                planted_forest$treeData$Z50[i] <- species_parameter(planted_forest$treeData$Species[i], SpParams,"RecrZ50")
                planted_forest$treeData$Z95[i] <- species_parameter(planted_forest$treeData$Species[i], SpParams,"RecrZ95")
                if(is.na(planted_forest$treeData$Z50[i])) planted_forest$treeData$Z50[i] <- 250
                if(is.na(planted_forest$treeData$Z95[i])) planted_forest$treeData$Z95[i] <- 500
              }
            }
            if(nrow(planted_forest$shrubData)>0) {
              for(i in 1:nrow(planted_forest$shrubData)) {
                planted_forest$shrubData$Z50[i] <- species_parameter(planted_forest$shrubData$Species[i], SpParams,"RecrZ50")
                planted_forest$shrubData$Z95[i] <- species_parameter(planted_forest$shrubData$Species[i], SpParams,"RecrZ95")
                if(is.na(planted_forest$shrubData$Z50[i])) planted_forest$shrubData$Z50[i] <- 100
                if(is.na(planted_forest$shrubData$Z95[i])) planted_forest$shrubData$Z95[i] <- 300
              }
            }
          
            # Store new management arguments (may have changed)
            sf$management_args[[i]] <- management_result$management_args
          }
        }
        # 3.1 Simulate species local recruitment
        if(local_control$allowRecruitment) {
          summary_i <- GL$sf$summary[[i]]
          monthlyMinTemp <- summary_i[, "MinTemperature"]
          monthlyMaxTemp <- summary_i[, "MaxTemperature"]
          monthlyPrecipitation <- summary_i[, "Snow"] + summary_i[, "Rain"]
          monthlyPET <- summary_i[,"PET"]
          monthlyTemp <- 0.606*monthlyMaxTemp + 0.394*monthlyMinTemp
          minMonthTemp <- min(monthlyTemp, na.rm=TRUE)
          moistureIndex <- sum(monthlyPrecipitation, na.rm=TRUE)/sum(monthlyPET, na.rm=TRUE)
          recr_forest <- medfate::recruitment(forest, SpParams, local_control, minMonthTemp, moistureIndex, verbose = FALSE)
        } else {
          recr_forest <- emptyforest()
        }
        # 3.2 Simulate species resprouting
        if(local_control$allowResprouting) {
          resp_forest <- medfate::resprouting(forest, xo$internalMortality, SpParams, local_control,
                                              management_result)
        } else {
          resp_forest <- emptyforest()
        }
        # 4. Update inputs for next year
        nyf <- medfate:::.nextYearForest(forest, xo, SpParams, local_control,
                                         planted_forest, recr_forest, resp_forest)

        # 5 Store current forest state
        sf$forest[[i]] <- nyf$forest
        sf$state[[i]] <- nyf$xi
        
        
        # 6.1 Create tree/shrub table of final state (after management/recruitment/resprouting)
        treeTableYear <- medfate:::.createTreeTable(iYear, year, nyf$xi)
        shrubTableYear <- medfate:::.createShrubTable(iYear, year, nyf$xi)
        
        # 6.2 Store tables
        treeTableVec[[i]] <- rbind(treeTableVec[[i]], treeTableYear)
        shrubTableVec[[i]] <- rbind(shrubTableVec[[i]], shrubTableYear)
        deadTreeTableVec[[i]] <- rbind(deadTreeTableVec[[i]], deadTreeTableYear)
        deadShrubTableVec[[i]] <- rbind(deadShrubTableVec[[i]], deadShrubTableYear)
        if(!is.null(cutTreeTableYear)) cutTreeTableVec[[i]] <- rbind(cutTreeTableVec[[i]], cutTreeTableYear)
        if(!is.null(cutShrubTableYear)) cutShrubTableVec[[i]] <- rbind(cutShrubTableVec[[i]], cutShrubTableYear)
        
      }
    }
  }
  out_sf <- sf::st_sf(geometry=sf::st_geometry(sf))
  out_sf$id <- sf$id
  out_sf$forest <- sf$forest
  out_sf$state <- sf$state
  out_sf$aquifer <- sf$aquifer
  out_sf$snowpack <- sf$snowpack
  out_sf$summary <- cell_summary
  if("management_args" %in% names(sf)) out_sf$management_arguments <- sf$management_arguments
  out_sf$tree_table <- treeTableVec
  out_sf$shrub_table <- shrubTableVec
  out_sf$dead_tree_table <- deadTreeTableVec
  out_sf$dead_shrub_table <- deadShrubTableVec
  out_sf$cut_tree_table <- cutTreeTableVec
  out_sf$cut_shrub_table <- cutShrubTableVec
  l <- list(sf = sf::st_as_sf(tibble::as_tibble(out_sf)),
            watershed_balance = LandscapeBalance,
            watershed_soil_balance = SoilLandscapeBalance,
            daily_runoff = DailyRunoff)
  class(l)<-c("fordyn_land", "list")
  return(l)
}