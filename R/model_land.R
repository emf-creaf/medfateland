
# Checks that soils have equal number of layers and width
.check_equal_soil_discretization<-function(soil_column, force_equal_layer_widths) {
  nlayers <- NA
  widths <- NA
  for(i in 1:length(soil_column)) {
    s <- soil_column[[i]]
    if(!is.null(s) && inherits(s, "soil")) {
      widths_i <- s[["widths"]]
      if(!is.na(nlayers)) {
        if(length(widths_i)!=nlayers) stop("All soil elements need to have the same number of layers.")
        if(!all(widths_i==widths)) {
          if(!force_equal_layer_widths) stop("Soil layer width needs to be the same for all cells.")
          for(l in 1:nlayers) {
            widths_i[l] <- widths[l]
          }
          s[["widths"]] <- widths_i
          soil_column[[i]] <- s
        }
      } else {
        widths <- widths_i
        nlayers <- length(widths_i)
      }
    }
  }
  return(soil_column)
}



.f_landunit_day<-function(xi, model, date, internalCommunication){
  out <- NA
  if(model=="spwb") {
    if(inherits(xi$x, "spwbInput")){
      medfate::spwb_day_inner(internalCommunication, xi$x, date, xi$meteovec,
                                latitude = xi$latitude, elevation = xi$elevation, slope = xi$slope, aspect = xi$aspect, 
                                runon = xi$runon, lateralFlows = xi$lateralFlows, waterTableDepth = xi$waterTableDepth, 
                                modifyInput = TRUE)
      res <- medfate::copy_model_output(internalCommunication, xi$x, "spwb")
      out <- list("final_state" = xi$x, "simulation_results" = res)
    } else if(inherits(xi$x, "aspwbInput")) {
      res <- medfate::aspwb_day_inner(internalCommunication, xi$x, date, xi$meteovec,
                        latitude = xi$latitude, elevation = xi$elevation, slope = xi$slope, aspect = xi$aspect, 
                        runon = xi$runon, lateralFlows = xi$lateralFlows, waterTableDepth = xi$waterTableDepth, 
                        modifyInput = TRUE)
      out <- list("final_state" = xi$x, "simulation_results" = res)
    }
  } else if(model=="growth") {
    if(inherits(xi$x, "growthInput")) {
      medfate::growth_day_inner(internalCommunication, xi$x, date, xi$meteovec,
                               latitude = xi$latitude, elevation = xi$elevation, slope = xi$slope, aspect = xi$aspect, 
                               runon = xi$runon, lateralFlows = xi$lateralFlows, waterTableDepth = xi$waterTableDepth, 
                               modifyInput = TRUE)
      res <- medfate::copy_model_output(internalCommunication, xi$x, "growth")
      out <- list("final_state" = xi$x, "simulation_results" = res)
    } else if(inherits(xi$x, "aspwbInput")) {
      res <- medfate::aspwb_day_inner(internalCommunication, xi$x, date, xi$meteovec,
                                latitude = xi$latitude, elevation = xi$elevation, slope = xi$slope, aspect = xi$aspect, 
                                runon = xi$runon, lateralFlows = xi$lateralFlows, waterTableDepth = xi$waterTableDepth, 
                                modifyInput = TRUE)
      out <- list("final_state" = xi$x, "simulation_results" = res)
    }
  } 
  return(out)
}

.get_dates_stars_list <- function(meteo) {
  datesStarsList <- NULL
  if(!is.null(meteo)) {
    if(inherits(meteo, "list")) {
      datesStarsList <- vector("list", length(meteo))
      for(i in 1:length(meteo)) {
        datesStarsList[[i]] <- as.Date(stars::st_get_dimension_values(meteo[[i]], "date"))
      }
    }
  }
  return(datesStarsList)
}
.get_dates_meteo <- function(y, meteo) {
  datesMeteo <- NULL
  if(!is.null(meteo)) {
    if(inherits(meteo,"data.frame")) {
      if(!("dates" %in% names(meteo))) {
        datesMeteo <- as.Date(row.names(meteo))
      } else {
        datesMeteo <- as.Date(meteo$dates)
      }
    } else if(inherits(meteo, "stars")) {
      datesMeteo <- as.Date(stars::st_get_dimension_values(meteo, "date"))
    } else if(inherits(meteo, "list")) {
      for(i in 1:length(meteo)) {
        datesMeteo_i <- as.Date(stars::st_get_dimension_values(meteo[[i]], "date"))
        if(is.null(datesMeteo)) datesMeteo <- datesMeteo_i
        else datesMeteo <- c(datesMeteo, datesMeteo_i)
      }
    }
  } else {
    if(!("meteo" %in% names(y))) cli::cli_abort("Column 'meteo' must be defined in 'y' if not supplied separately")
    if(!("dates" %in% names(y$meteo[[1]]))) {
      datesMeteo <- as.Date(row.names(y$meteo[[1]]))
    } else {
      datesMeteo <- as.Date(y$meteo[[1]]$dates)
    }
    # check that all items have same dates
    for(i in 1:nrow(y)) {
      if(!("dates" %in% names(y$meteo[[i]]))) {
        datesMeteo_i <- as.Date(row.names(y$meteo[[i]]))
      } else {
        datesMeteo_i <- as.Date(y$meteo[[i]]$dates)
      }
      if(!all(datesMeteo_i==datesMeteo)) cli::cli_abort("All spatial elements need to have the same weather dates.")
    }
  }
  return(datesMeteo)
}

.get_meteo_mapping <- function(r, y, meteo, sf_coords, sf2cell, 
                               agg_fact){
  pts_sf_meteo <- NULL
  pts_sf_meteo_2_sf <- NULL
  if(!is.null(meteo)) {
    if(inherits(meteo, "stars") || inherits(meteo, "list")) {
      nCells <- nrow(y)
      r$elevation <- NA
      r$slope <- NA
      r$aspect <- NA
      r$elevation[sf2cell] <- y$elevation
      r$slope[sf2cell] <- y$slope
      r$aspect[sf2cell] <- y$aspect
      agg_fact <- as.integer(agg_fact)
      r_meteo <- r
      if(agg_fact > 1) {
        r_meteo <- terra::aggregate(r_meteo, fact = agg_fact, fun = "median", na.rm = TRUE)
      }
      pts_sf_meteo <- sf::st_as_sf(terra::as.points(r_meteo))
      pts_sf2cell_meteo <- terra::cellFromXY(r_meteo, sf::st_coordinates(pts_sf_meteo))
      sf2cell_meteo <- terra::cellFromXY(r_meteo, sf_coords)
      pts_sf_meteo_2_sf <- rep(NA, nCells)
      for(i in 1:length(pts_sf_meteo_2_sf)) pts_sf_meteo_2_sf[i] <- which(pts_sf2cell_meteo==sf2cell_meteo[i])
    }
  }
  return(list("pts_sf_meteo" = pts_sf_meteo, 
              "pts_sf_meteo_2_sf" = pts_sf_meteo_2_sf))
}

.build_grid_meteo_day <- function(y, meteo, datesMeteo, date, 
                                  meteo_mapping,
                                  datesStarsList = NULL, 
                                  CO2ByYear = numeric(0)) {
  pts_sf_meteo <- meteo_mapping[["pts_sf_meteo"]] 
  pts_sf_meteo_2_sf <- meteo_mapping[["pts_sf_meteo_2_sf"]]
  nCells <- nrow(y)
  doy <- as.numeric(format(date,"%j"))
  datechar <- as.character(date)
  yearString <- substr(datechar, 1, 4)
  gridMinTemperature <- rep(NA, nCells)
  gridMaxTemperature <- rep(NA, nCells)
  gridMinRelativeHumidity <- rep(NA, nCells)
  gridMaxRelativeHumidity <- rep(NA, nCells)
  gridPrecipitation <- rep(NA, nCells)
  gridRadiation <- rep(NA, nCells)
  gridWindSpeed <- rep(NA, nCells)
  Catm <- NA
  if(yearString %in% names(CO2ByYear)) Catm <- CO2ByYear[yearString]
  gridCO2 = rep(Catm, nCells)
  
  if(!is.null(meteo)) {
    if(inherits(meteo,"stars") || inherits(meteo,"list")) {
      if(inherits(meteo,"stars")) {
        i_meteo <- meteo
      } else {
        i_stars <- NA
        for(i in 1:length(datesStarsList)) {
          if(date %in% datesStarsList[[i]]) i_stars <- i
        }
        if(is.na(i_stars)) stop("Date to be processed not found in interpolator list")
        i_meteo <- meteo[[i_stars]]
      }
      met <- meteoland::interpolate_data(pts_sf_meteo, i_meteo, dates = date, 
                                         verbose = FALSE, ignore_convex_hull_check = TRUE)
      ml <- tidyr::unnest(met, cols = "interpolated_data")
      gridMinTemperature <- ml$MinTemperature[pts_sf_meteo_2_sf]
      gridMaxTemperature <- ml$MaxTemperature[pts_sf_meteo_2_sf]
      gridMinRelativeHumidity <- ml$MinRelativeHumidity[pts_sf_meteo_2_sf]
      gridMaxRelativeHumidity <- ml$MaxRelativeHumidity[pts_sf_meteo_2_sf]
      gridPrecipitation <- ml$Precipitation[pts_sf_meteo_2_sf]
      gridRadiation <- ml$Radiation[pts_sf_meteo_2_sf]
      gridWindSpeed <- ml$WindSpeed[pts_sf_meteo_2_sf]    
    } else { # data frame
      imeteo <- which(datesMeteo == date) #date index in meteo data
      # repeat values for all cells
      gridMinTemperature <- rep(meteo[imeteo,"MinTemperature"], nCells)
      gridMaxTemperature <- rep(meteo[imeteo,"MaxTemperature"], nCells)
      gridMinRelativeHumidity <- rep(meteo[imeteo,"MinRelativeHumidity"], nCells)
      gridMaxRelativeHumidity <- rep(meteo[imeteo,"MaxRelativeHumidity"], nCells)
      gridPrecipitation <- rep(meteo[imeteo,"Precipitation"], nCells)
      gridRadiation <- rep(meteo[imeteo, "Radiation"], nCells)
      gridWindSpeed <- rep(meteo[imeteo, "WindSpeed"], nCells)
      if("CO2" %in% names(meteo)) gridCO2 <- rep(meteo[imeteo, "CO2"], nCells)
    }
  } 
  else {
    imeteo = which(datesMeteo == date) #date index in meteo data
    for(iml in 1:nCells) {
      meti <- y$meteo[[iml]]
      gridMinTemperature[iml] <- meti$MinTemperature[imeteo]
      gridMaxTemperature[iml] <- meti$MaxTemperature[imeteo]
      gridMinRelativeHumidity[iml] <- meti$MinRelativeHumidity[imeteo]
      gridMaxRelativeHumidity[iml] <- meti$MaxRelativeHumidity[imeteo]
      gridPrecipitation[iml] <- meti$Precipitation[imeteo]
      gridRadiation[iml] <- meti$Radiation[imeteo]
      gridWindSpeed[iml] <- meti$WindSpeed[imeteo]
      if("CO2" %in% names(meti)) gridCO2[iml] <- meti$CO2[imeteo]
    }
  }
  
  gridRadiation[is.na(gridRadiation)] <- mean(gridRadiation, na.rm=T)
  gridMeteo <- data.frame(MinTemperature = gridMinTemperature, 
                          MaxTemperature = gridMaxTemperature,
                          MinRelativeHumidity = gridMinRelativeHumidity,
                          MaxRelativeHumidity = gridMaxRelativeHumidity,
                          Precipitation = gridPrecipitation,
                          Radiation = gridRadiation,
                          WindSpeed = gridWindSpeed,
                          CO2 = gridCO2)
  
  return(gridMeteo)
}

# Define communication structures
.defineInternalCommunication <- function(y, local_model) {
  max_num_cohorts <- 1
  max_num_soil_layers <- 1
  max_num_canopy_layers <-1
  max_num_timesteps <- 24
  for(i in 1:nrow(y)) {
    if((y$land_cover_type[i]=="wildland") && (!is.null(y$state[[i]]))) {
      xi <- y$state[[i]]
      max_num_cohorts <- max(max_num_cohorts, nrow(xi$cohorts))
      max_num_soil_layers <- max(max_num_soil_layers, nrow(xi$soil))
      max_num_canopy_layers <- max(max_num_canopy_layers, nrow(xi$canopy))
      max_num_cohorts <- max(max_num_cohorts, xi$control$ndailysteps)
    }
  }
  internalCommunication <- medfate::general_communication_structures(max_num_cohorts, max_num_soil_layers, max_num_canopy_layers, max_num_timesteps,
                                                                     local_model);
  return(internalCommunication)
}

.watershedDayTetis<- function(output,
                              internalCommunication,
                              local_model,
                              y,
                              sf_routing,
                              watershed_control,
                              date,
                              gridMeteo,
                              latitude, 
                              standSummary = FALSE, carbonBalanceSummary = FALSE, biomassBalanceSummary = FALSE,
                              patchsize = NA, progress = TRUE) {

  nX <- nrow(y)
  
  waterOrder <- sf_routing$waterOrder
  queenNeigh <- sf_routing$queenNeigh
  waterQ <- sf_routing$waterQ
  isChannel <- sf_routing$channel
  isOutlet <- sf_routing$outlet
  target_outlet <- sf_routing$target_outlet
  distance_to_outlet <- sf_routing$distance_to_outlet
  outlet_backlog <- sf_routing$outlet_backlog
  
  # Reset from previous days
  .resetWaterBalanceDayOutput(output[["WatershedWaterBalance"]])
  
  # A. Landscape interflow and baseflow
  .tetisInterFlow(output[["WatershedWaterBalance"]], 
                  y,
                  waterOrder, queenNeigh, waterQ,
                  watershed_control,
                  patchsize)
  .tetisBaseFlow(output[["WatershedWaterBalance"]],
                 y,
                 waterOrder, queenNeigh, waterQ,
                 watershed_control,
                 patchsize)


  
  # A3b. Apply changes in aquifer to each cell
  .tetisApplyBaseflowChangesToAquifer(output[["WatershedWaterBalance"]],
                                      y,
                                      patchsize)
  
  
  # B. Simulation of soil cells, non-soil cells and overland flows
  .copySnowpackToSoil(y)
  .tetisModifyKsat(y, watershed_control, reverse = FALSE)
  .tetisSimulationWithOverlandFlows(local_model, date, internalCommunication,
                                    standSummary, carbonBalanceSummary, biomassBalanceSummary,
                                    output,
                                    y, 
                                    latitude,
                                    gridMeteo,
                                    waterOrder, queenNeigh, waterQ, isChannel,
                                    watershed_control)
  .copySnowpackFromSoil(y)
  .tetisModifyKsat(y, watershed_control, reverse = TRUE)
  
  #C. Applies capillarity rise, deep drainage to aquifer
  .tetisApplyLocalFlowsToAquifer(y,
                                 output[["WatershedWaterBalance"]])

  #D. Applies drainage from aquifer to a deeper aquifer
  .tetisApplyDeepAquiferLossToAquifer(output[["WatershedWaterBalance"]], 
                                      y, watershed_control)

}


## This function is in R to use parallelization
.watershedDaySerghei<- function(local_model,
                                lct, xList,
                                snowpack,
                                sf2cell,
                                serghei_interface,
                                watershed_control,
                                date,
                                gridMeteo,
                                latitude, elevation, slope, aspect,
                                progress = TRUE) {
  nX <- length(xList)
  
  MinTemperature <- rep(NA, nX)
  MaxTemperature <- rep(NA, nX)
  PET <- rep(0, nX)
  Precipitation <- rep(0, nX)
  Rain <- rep(0, nX)
  Snow <- rep(0, nX)
  Snowmelt <- rep(0, nX)
  NetRain <- rep(0, nX)
  SoilEvaporation <- rep(0, nX)
  Transpiration <- rep(0, nX)
  HerbTranspiration <- rep(0, nX)
  
  tminVec <- gridMeteo[["MinTemperature"]]
  tmaxVec <- gridMeteo[["MaxTemperature"]]
  rhminVec <- gridMeteo[["MinRelativeHumidity"]]
  rhmaxVec <- gridMeteo[["MaxRelativeHumidity"]]
  precVec <- gridMeteo[["Precipitation"]]
  radVec <- gridMeteo[["Radiation"]]
  wsVec <- gridMeteo[["WindSpeed"]]
  C02Vec <- gridMeteo[["CO2"]]
  
  XI <- vector("list", nX)
  for(i in 1:nX) {
    meteovec <- c(
      "MinTemperature" = tminVec[i],
      "MaxTemperature" = tmaxVec[i],
      "MinRelativeHumidity" = rhminVec[i],
      "MaxRelativeHumidity" = rhmaxVec[i],
      "Precipitation"  =precVec[i],
      "Radiation" = radVec[i],
      "WindSpeed" = wsVec[i],
      "CO2" = C02Vec[i]
    )
    XI[[i]] <- list(i = i, 
                    x = xList[[i]],
                    meteovec = meteovec,
                    latitude = latitude[i], 
                    elevation = elevation[i], 
                    slope= slope[i], 
                    aspect = aspect[i],
                    runon = 0,
                    lateralFlows = NULL,
                    waterTableDepth = NA)
  }
  
  #A. Vertical and surface fluxes
  localResults <- vector("list", nX)
  for(i in 1:nX) {
    localResults[[i]] = .f_landunit_day(XI[[i]], date = date, model = local_model)
  }
  for(i in 1:nX) {
    if((lct[i]=="wildland") || (lct[i]=="agriculture")) {
      res <- localResults[[i]]$simulation_results
      DB <- res[["WaterBalance"]]
      MinTemperature[i] <- tminVec[i]
      MaxTemperature[i] <- tmaxVec[i]
      Snow[i] <- DB["Snow"]
      Snowmelt[i] <- DB["Snowmelt"]
      PET[i] <- DB["PET"]
      Rain[i] <- DB["Rain"]
      SoilEvaporation[i] <- DB["SoilEvaporation"]
      NetRain[i] <- DB["NetRain"]
      
      if(lct[i]=="wildland") {
        PL <- res[["Plants"]]
        Transpiration[i] <- sum(PL["Transpiration"])
        HerbTranspiration[i] <- DB["HerbTranspiration"]
      } else {
        Transpiration[i] <- DB["Transpiration"]
      }
    } else { # Fill output vectors for non-wildland cells
      Rain[i] = 0.0
      Snow[i] = 0.0
      Snowmelt[i] = 0.0
      SoilEvaporation[i] <- 0.0
      tday <- meteoland::utils_averageDaylightTemperature(tminVec[i], tmaxVec[i])
      if(tday<0.0) {
        Snow[i] <- precVec[i]
        snowpack[i] <- snowpack[i] + Snow[i]
      } else {
        Rain[i] <- precVec[i]
      }
      NetRain[i] <- Rain[i]
      if(snowpack[i]>0.0) {
        melt <- medfate::hydrology_snowMelt(tday, radVec[i], 1.0, elevation[i])
        Snowmelt[i] <- min(melt, snowpack[i])
        snowpack[i] <- snowpack[i] - Snowmelt[i]
      }
    }
  }
  # B. Calls SERGHEI
  # .callSergheiDay(lct, xList,
  #                 gridMeteo, localResults,
  #                 sf2cell, serghei_interface)

  waterBalance <- data.frame("MinTemperature" = MinTemperature,
                             "MaxTemperature" = MaxTemperature, 
                             "PET" = PET, "Rain" = Rain, "Snow" = Snow,
                             "Snowmelt" = Snowmelt, "NetRain" = NetRain, 
                             "SoilEvaporation" = SoilEvaporation, "Transpiration" = Transpiration,
                             "HerbTranspiration" = HerbTranspiration)
  return(list("WatershedWaterBalance" = waterBalance,
              "LocalResults" = localResults))
}

.vars_stand <- function(type = "all") {
  varsStand <- c("LAI", "LAIherb", "LAIlive", "LAIexpanded", "LAIdead", "Cm", "LgroundPAR", "LgroundSWR")
  if(type %in% c("mean", "all")) return(varsStand)
  return(c())
}
.vars_waterbalance <- function(type = "all"){
  varsWaterBalance <- c("Snowmelt", "Interception", "NetRain",  
                        "Infiltration", "InfiltrationExcess",  "SaturationExcess", "Runon", "Runoff", 
                        "DeepDrainage", "CapillarityRise", "DeepAquiferLoss",
                        "SoilEvaporation", "Transpiration", "HerbTranspiration",
                        "InterflowInput", "InterflowOutput", "InterflowBalance", "BaseflowInput", "BaseflowOutput", "BaseflowBalance", "AquiferExfiltration")
  if(type %in% c("sum", "all")) return(varsWaterBalance)
  return(c())
}
.vars_carbonbalance <- function(type = "all") {
  varsCarbonBalance <- c("GrossPrimaryProduction","MaintenanceRespiration","SynthesisRespiration","NetPrimaryProduction")
  if(type %in% c("sum", "all")) return(varsCarbonBalance)
  return(c())
}
.vars_biomassbalance <- function(type = "all") {
  varsBiomassBalance <- c("StructuralBalance", "LabileBalance", "PlantBalance", "MortalityLoss", "CohortBalance")
  if(type %in% c("sum", "all")) return(varsBiomassBalance)
  return(c())
}
.vars_summary <- function(type = "all",
                          standSummary, waterBalanceSummary, carbonBalanceSummary, biomassBalanceSummary) {
  if(type=="state") {
    return(c("SWE", "RWC", "SoilVol","WTD"))
  } else if(type=="all") {
    vars <- c("MinTemperature","MaxTemperature","PET", "Rain", "Snow", "SWE", "RWC", "SoilVol","WTD","DTA")
  } else if(type=="sum") {
    vars <- c("PET","Rain", "Snow")
  } else if(type=="mean") {
    vars <- c("MinTemperature", "MaxTemperature")
  }
  if(waterBalanceSummary) vars <- c(vars, .vars_waterbalance(type))
  if(standSummary) vars <- c(vars, .vars_stand(type))
  if(carbonBalanceSummary) vars <- c(vars, .vars_carbonbalance(type))
  if(biomassBalanceSummary) vars <- c(vars, .vars_biomassbalance(type))
  return(vars)
}

.aggregate_summary_to_annual<-function(m, varsSum, varsMean, varsState) {
  month_weights <- c(31,28,31,30,31,30,31,31,30,31,30,31)[1:nrow(m)]
  month_weights <- month_weights/sum(month_weights)
  coln <- colnames(m)
  rown <- rownames(m)
  year_string <- paste0(substr(rown[1], 1,4),"-01-01")
  m_year <- matrix(NA, nrow=1, ncol = ncol(m), dimnames = list(year_string, coln))
  for(j in 1:ncol(m)) {
    if(coln[j] %in% varsSum) {
      m_year[1,j] <- sum(m[,j], na.rm=TRUE)
    } else if(coln[j] %in% varsMean) {
      m_year[1,j] <- sum(m[,j]*month_weights, na.rm=TRUE)
    } else if((coln[j] %in% varsState) || (coln[j] == "DTA")) {
      m_year[1,j] <- sum(m[,j]*month_weights, na.rm=TRUE)
    } else {
      stop(paste0("variable name ", coln[j]," not found in summary variables for sums or means"))
    }
  }
  return(m_year)
}

.simulate_land_inner <- function(local_model = "spwb", 
                                 r, y, sf_routing, 
                                 meteo, dates,
                                 CO2ByYear = numeric(0), 
                                 summary_frequency = "years",
                                 summary_blocks = character(0),
                                 watershed_control = default_watershed_control(),
                                 progress = TRUE, header_footer = progress) {
  
  nCells <- nrow(y)
  isSoilCell <- y$land_cover_type %in% c("wildland", "agriculture")
  isAgricultureCell <- y$land_cover_type %in% c("agriculture")
  isWildlandCell <- y$land_cover_type %in% c("wildland")
  nSoil <- sum(isSoilCell)
  
  represented_area_m2 <- as.vector(terra::values(terra::cellSize(r)))
  patchsize <- mean(represented_area_m2, na.rm=TRUE)
  
  watershed_model <- watershed_control$watershed_model
  
  date.factor <- cut(dates, breaks=summary_frequency)
  nSummary <- sum(table(date.factor)>0)
  df.int <- as.numeric(date.factor)
  t.df <- as.numeric(table(df.int))
  nDays <- length(dates)
  
  raster_matching <- .raster_sf_matching(r, y)
  sf_coords <- raster_matching$sf_coords
  sf2cell <- raster_matching$sf2cell
  cell2sf <- raster_matching$cell2sf
  
  #get latitude (for medfate)  
  latitude <- sf::st_coordinates(sf::st_transform(sf::st_geometry(y),4326))[,2]
  
  # Summary flags
  waterBalanceSummary <- "WaterBalance" %in% summary_blocks
  standSummary <- "Stand" %in% summary_blocks
  carbonBalanceSummary <- "CarbonBalance" %in% summary_blocks
  biomassBalanceSummary <- "BiomassBalance" %in% summary_blocks
  
  # Define communication structures
  internalCommunication <- .defineInternalCommunication(y, local_model)
  ws_day  <- .createDayOutput(nCells, standSummary, carbonBalanceSummary, biomassBalanceSummary)

  meteo_mapping <- .get_meteo_mapping(r, y, meteo, sf_coords, sf2cell, 
                                      watershed_control[["weather_aggregation_factor"]])
  datesStarsList <- .get_dates_stars_list(meteo)
  datesMeteo <- .get_dates_meteo(y, meteo)
  
  #Output matrices
  if(watershed_model =="tetis") {
    channel_cells <- which(sf_routing$channel)
    outlet_nonchannel_cells <- which(sf_routing$outlet & !sf_routing$channel)

    WatershedExport <- matrix(0,nrow = nDays, ncol = length(outlet_nonchannel_cells))
    colnames(WatershedExport) <- outlet_nonchannel_cells
    rownames(WatershedExport) <- as.character(dates)

    ChannelExport <- matrix(0,nrow = nDays, ncol = length(channel_cells))
    colnames(ChannelExport) <- channel_cells
    rownames(ChannelExport) <- as.character(dates)
    
    vars <- .vars_summary("all", standSummary = standSummary, waterBalanceSummary = waterBalanceSummary, carbonBalanceSummary = carbonBalanceSummary, biomassBalanceSummary = biomassBalanceSummary)
    varsSum <- .vars_summary("sum", standSummary = standSummary, waterBalanceSummary = waterBalanceSummary, carbonBalanceSummary = carbonBalanceSummary, biomassBalanceSummary = biomassBalanceSummary)
    varsMean <- .vars_summary("mean", standSummary = standSummary, waterBalanceSummary = waterBalanceSummary, carbonBalanceSummary = carbonBalanceSummary, biomassBalanceSummary = biomassBalanceSummary)
    varsState <- .vars_summary("state", standSummary = standSummary, waterBalanceSummary = waterBalanceSummary, carbonBalanceSummary = carbonBalanceSummary, biomassBalanceSummary = biomassBalanceSummary)
    varsWaterBalance <- .vars_waterbalance("all")
    varsCarbonBalance <- .vars_carbonbalance("all")
    varsStand <- .vars_stand("all")
    varsBiomassBalance <- .vars_biomassbalance("all")
    
    LandscapeBalance <- data.frame(dates = dates,
                                   PET = rep(0, nDays),
                                   Precipitation = rep(0, nDays),
                                   Rain = rep(0, nDays),
                                   Snow = rep(0, nDays),
                                   Snowmelt = rep(0, nDays),
                                   Interception = rep(0, nDays),
                                   NetRain = rep(0, nDays),
                                   Infiltration = rep(0, nDays),
                                   InfiltrationExcess = rep(0, nDays),
                                   SaturationExcess = rep(0, nDays),
                                   CellRunon = rep(0, nDays),
                                   CellRunoff = rep(0, nDays),
                                   DeepDrainage = rep(0, nDays),
                                   CapillarityRise = rep(0, nDays),
                                   DeepAquiferLoss = rep(0, nDays),
                                   SoilEvaporation = rep(0,nDays),
                                   Transpiration = rep(0, nDays),
                                   HerbTranspiration = rep(0, nDays),
                                   InterflowBalance = rep(0, nDays),
                                   BaseflowBalance = rep(0, nDays),
                                   AquiferExfiltration = rep(0, nDays),
                                   ChannelExport = rep(0, nDays),
                                   WatershedExport = rep(0, nDays))
    SoilLandscapeBalance <- data.frame(dates = dates,
                                       PET = rep(0, nDays),
                                       Precipitation = rep(0, nDays),
                                       Rain = rep(0, nDays),
                                       Snow = rep(0, nDays),
                                       Snowmelt = rep(0, nDays),
                                       Interception = rep(0, nDays),
                                       NetRain = rep(0, nDays),
                                       Infiltration = rep(0, nDays),
                                       InfiltrationExcess = rep(0, nDays),
                                       SaturationExcess = rep(0, nDays),
                                       CellRunon = rep(0, nDays),
                                       CellRunoff = rep(0, nDays),
                                       DeepDrainage = rep(0, nDays),
                                       DeepAquiferLoss = rep(0, nDays),
                                       CapillarityRise = rep(0, nDays),
                                       SoilEvaporation = rep(0,nDays),
                                       Transpiration = rep(0, nDays),
                                       HerbTranspiration = rep(0, nDays),
                                       InterflowBalance = rep(0, nDays),
                                       AquiferExfiltration = rep(0, nDays))
  }
  if(watershed_model =="serghei") {
    vars <- c("MinTemperature","MaxTemperature","PET", 
              "Rain", "NetRain", "Snow",
              "Snowmelt","Interception",
              "SoilEvaporation", "Transpiration", "SWE", "SoilVol","RWC")
    varsSum <- c("PET","Rain", "NetRain", "Snow", "Snowmelt",
                 "SoilEvaporation", "Transpiration")
    varsMean <- c( "MinTemperature", "MaxTemperature")
    varsState <- c("SWE", "RWC", "SoilVol")
    LandscapeBalance <- data.frame(dates = dates,
                                   Precipitation = rep(0, nDays),
                                   Snow = rep(0, nDays),
                                   Snowmelt = rep(0, nDays),
                                   Rain = rep(0, nDays),
                                   NetRain = rep(0, nDays),
                                   Interception = rep(0, nDays),
                                   SoilEvaporation = rep(0,nDays),
                                   Transpiration = rep(0, nDays),
                                   HerbTranspiration = rep(0, nDays))
  }
  resultlist <- vector("list", nCells)
  summarylist <- vector("list", nCells)
  for(i in 1:nCells) {
    # summaries
    m <- matrix(NA, nrow = nSummary, ncol = length(vars))
    colnames(m) <- vars
    rownames(m) <- levels(date.factor)[1:nSummary]
    summarylist[[i]] <- m
    # result cells
    if(y$result_cell[i]) {
      if(local_model=="spwb") {
        if(isWildlandCell[i]) {
          resultlist[[i]] <- medfate:::.defineSPWBDailyOutput(latitude[i], y$elevation[i], y$slope[i], y$aspect[i],
                                                              dates, y$state[[i]])
        } else if(isAgricultureCell[i]) {
          resultlist[[i]] <- medfate:::.defineASPWBDailyOutput(latitude[i], y$elevation[i], y$slope[i], y$aspect[i],
                                                               dates, y$state[[i]])
        }
      } else if(local_model=="growth") {
        if(isWildlandCell[i]) {
          resultlist[[i]] <- medfate:::.defineGrowthDailyOutput(latitude[i], y$elevation[i], y$slope[i], y$aspect[i],
                                                                dates, y$state[[i]])
        } else if(isAgricultureCell[i]) {
          resultlist[[i]] <- medfate:::.defineASPWBDailyOutput(latitude[i], y$elevation[i], y$slope[i], y$aspect[i],
                                                               dates, y$state[[i]])
        }
      }
    }
  }
  
  state_soil_summary_function <- function(object) {
    l = list(SWE=NA, RWC=NA, SoilVol=NA, WTD=NA)
    if(!is.null(object)) {
      if(inherits(object, "list")) {
        if(("soil" %in% names(object)) && ("control" %in% names(object))) {
          s <- object$soil
          control <- object$control
          model <- control$soilFunctions
          water_mm <- sum(soil_water(s, model))
          water_fc_mm <- sum(soil_waterFC(s, model))
          l <- list(SWE = object$snowpack,
                    RWC = 100*water_mm/water_fc_mm,
                    SoilVol = water_mm,
                    WTD = soil_saturatedWaterDepth(s, model))
        }
      }
    }
    return(l)
  }
  
  # INIT SERGHEI INTERFACE
  serghei_interface <-NULL
  if(watershed_model=="serghei") {
    serghei_parameters <- watershed_control[["serghei_parameters"]]
    serghei_interface <- .initSerghei(limits = as.vector(terra::ext(r)),
                                      nrow = terra::nrow(r),
                                      ncol = terra::ncol(r),
                                      sf2cell = sf2cell,
                                      y$state,
                                      input_dir = serghei_parameters[["input_dir"]],
                                      output_dir = serghei_parameters[["output_dir"]])
  } else if(watershed_model == "tetis") {
    # INITIAL STATE
    initialSoilContent <- 0 
    for(i in 1:nCells) {
      if((y$land_cover_type[i] %in% c("wildland", "agriculture")) && (!is.null(y$state[[i]]))) {
        x <- y$state[[i]]
        initialSoilContent <- initialSoilContent + (sum(soil_water(x$soil, model = "VG"), na.rm=TRUE)/nSoil)
      }
    }
    initialSnowContent <- sum(y$snowpack, na.rm=TRUE)/nCells
    initialAquiferContent <- sum(y$aquifer, na.rm=TRUE)/nCells
    initialLandscapeContent <- initialSoilContent*(nSoil/nCells)+initialAquiferContent+initialSnowContent
  }
  
  if(progress) {
    cli::cli_progress_bar("Daily simulations", total = nDays)
  }
  
  for(day in 1:nDays) {
    datechar <- as.character(dates[day])
    gridMeteo <- .build_grid_meteo_day(y, meteo, datesMeteo, dates[day], 
                                       meteo_mapping,
                                       datesStarsList,
                                       CO2ByYear)
    
    if(watershed_model=="tetis") {
      .watershedDayTetis(output = ws_day,
                         internalCommunication = internalCommunication,
                         local_model = local_model,
                         y = y,
                         sf_routing = sf_routing,
                         watershed_control = watershed_control,
                         date = datechar,
                         gridMeteo = gridMeteo,
                         latitude = latitude,
                         standSummary = standSummary, carbonBalanceSummary = carbonBalanceSummary, biomassBalanceSummary = biomassBalanceSummary,
                         patchsize = patchsize, progress = progress)
    } else if(watershed_model=="serghei") {
      ws_day <- .watershedDaySerghei(local_model = local_model,
                                     lct = y$land_cover_type, xList = y$state,
                                     snowpack = y$snowpack,
                                     sf2cell = sf2cell,
                                     serghei_interface = serghei_interface,
                                     watershed_control = watershed_control,
                                     date = datechar,
                                     gridMeteo = gridMeteo,
                                     latitude = latitude, elevation = y$elevation, slope = y$slope, aspect = y$aspect,
                                     progress = FALSE)
    }
    
    res_wb_day <- ws_day[["WatershedWaterBalance"]]
    if(standSummary) res_stand_day <- ws_day[["WatershedStand"]]
    if(carbonBalanceSummary) res_cb_day <- ws_day[["WatershedCarbonBalance"]]
    if(biomassBalanceSummary) res_bb_day <- ws_day[["WatershedBiomassBalance"]]
    local_res_day <- ws_day[["LocalResults"]]
    
    # Fill local daily results for result cells
    for(i in 1:nCells) {
      if(y$result_cell[i]) {
        x <- y$state[[i]]
        if(local_model=="spwb") {
          if(isWildlandCell[i]) {
            medfate:::.fillSPWBDailyOutput(resultlist[[i]], x = x, sDay = local_res_day[[i]]$simulation_results, iday = day-1)
          } else if(isAgricultureCell[i]) {
            medfate:::.fillASPWBDailyOutput(resultlist[[i]], x = x, sDay = local_res_day[[i]]$simulation_results, iday = day-1)
          }
        } else if(local_model =="growth") {
          if(isWildlandCell[i]) {
            medfate:::.fillGrowthDailyOutput(resultlist[[i]], x = x, sDay = local_res_day[[i]]$simulation_results, iday = day-1)
          } else if(isAgricultureCell[i]) {
            medfate:::.fillASPWBDailyOutput(resultlist[[i]], x = x, sDay = local_res_day[[i]]$simulation_results, iday = day-1)
          }
        }
      }
    }
    
    # Fill summaries for all cells
    ifactor <- df.int[day]
    if(watershed_model=="tetis") DTAday <- (y$depth_to_bedrock/1000.0) - (y$aquifer/y$bedrock_porosity)/1000.0
    for(i in 1:nCells) {
      for(v in varsSum) {
        if(v %in% varsStand) {
          if(!is.na(summarylist[[i]][ifactor,v])) summarylist[[i]][ifactor,v] <- summarylist[[i]][ifactor,v] + res_stand_day[[v]][i]
          else summarylist[[i]][ifactor,v] <- res_stand_day[[v]][i]
        } else if(v %in% varsCarbonBalance) {
          if(!is.na(summarylist[[i]][ifactor,v])) summarylist[[i]][ifactor,v] <- summarylist[[i]][ifactor,v] + res_cb_day[[v]][i]
          else summarylist[[i]][ifactor,v] <- res_cb_day[[v]][i]
        } else if(v %in% varsBiomassBalance) {
          if(!is.na(summarylist[[i]][ifactor,v])) summarylist[[i]][ifactor,v] <- summarylist[[i]][ifactor,v] + res_bb_day[[v]][i]
          else summarylist[[i]][ifactor,v] <- res_bb_day[[v]][i]
        } else {
          if(!is.na(summarylist[[i]][ifactor,v])) summarylist[[i]][ifactor,v] <- summarylist[[i]][ifactor,v] + res_wb_day[[v]][i]
          else summarylist[[i]][ifactor,v] <- res_wb_day[[v]][i]
        }
      }
      for(v in varsMean) {
        if(v %in% varsStand) {
          if(!is.na(summarylist[[i]][ifactor,v])) summarylist[[i]][ifactor,v] <- summarylist[[i]][ifactor,v] + res_stand_day[[v]][i]/t.df[ifactor]
          else summarylist[[i]][ifactor,v] <- res_stand_day[[v]][i]/t.df[ifactor]
        } else if(v %in% varsCarbonBalance) {
          if(!is.na(summarylist[[i]][ifactor,v])) summarylist[[i]][ifactor,v] <- summarylist[[i]][ifactor,v] + res_cb_day[[v]][i]/t.df[ifactor]
          else summarylist[[i]][ifactor,v] <- res_cb_day[[v]][i]/t.df[ifactor]
        } else if(v %in% varsBiomassBalance) {
          if(!is.na(summarylist[[i]][ifactor,v])) summarylist[[i]][ifactor,v] <- summarylist[[i]][ifactor,v] + res_bb_day[[v]][i]/t.df[ifactor]
          else summarylist[[i]][ifactor,v] <- res_bb_day[[v]][i]/t.df[ifactor]
        } else {
          if(!is.na(summarylist[[i]][ifactor,v])) summarylist[[i]][ifactor,v] <- summarylist[[i]][ifactor,v] + res_wb_day[[v]][i]/t.df[ifactor]
          else summarylist[[i]][ifactor,v] <- res_wb_day[[v]][i]/t.df[ifactor]
        }
      }
      if(!is.null(y$state[[i]])) {
        summary_i <- state_soil_summary_function(y$state[[i]])
        for(v in varsState) {
          if(!is.na(summarylist[[i]][ifactor,v])) summarylist[[i]][ifactor,v] <- summarylist[[i]][ifactor,v] + summary_i[[v]]/t.df[ifactor]
          else  summarylist[[i]][ifactor,v] <- summary_i[[v]]/t.df[ifactor]
        }  
      }
      if(watershed_model=="tetis") {
        if(!is.na(summarylist[[i]][ifactor, "DTA"])) summarylist[[i]][ifactor,"DTA"] <- summarylist[[i]][ifactor,"DTA"] + DTAday[i]/t.df[ifactor]
        else summarylist[[i]][ifactor,"DTA"] <- DTAday[i]/t.df[ifactor]
      }
    }
    
    ## Store watershed runoff reaching each outlet and channel
    if(watershed_model=="tetis") {
      WatershedExport[day,] <- res_wb_day$WatershedExport[outlet_nonchannel_cells]
      ChannelExport[day,] <- res_wb_day$ChannelExport[channel_cells]
    }
    
    #Landscape balance
    LandscapeBalance$PET[day] <- sum(res_wb_day$PET, na.rm=T)/nCells
    LandscapeBalance$Rain[day] <- sum(res_wb_day$Rain, na.rm=T)/nCells
    LandscapeBalance$Snow[day] <- sum(res_wb_day$Snow, na.rm=T)/nCells
    LandscapeBalance$Snowmelt[day] <- sum(res_wb_day$Snowmelt, na.rm=T)/nCells
    LandscapeBalance$NetRain[day] <- sum(res_wb_day$NetRain, na.rm=T)/nCells
    LandscapeBalance$SoilEvaporation[day] <- sum(res_wb_day$SoilEvaporation, na.rm=T)/nCells
    LandscapeBalance$Transpiration[day] <- sum(res_wb_day$Transpiration, na.rm=T)/nCells
    LandscapeBalance$HerbTranspiration[day] <- sum(res_wb_day$HerbTranspiration, na.rm=T)/nCells
    LandscapeBalance$Interception[day] <- (sum(res_wb_day$Rain, na.rm=T) - sum(res_wb_day$NetRain, na.rm=T))/nCells
    
    if(watershed_model=="tetis") {
      LandscapeBalance$DeepDrainage[day] <- sum(res_wb_day$DeepDrainage, na.rm=T)/nCells
      LandscapeBalance$SaturationExcess[day] <- sum(res_wb_day$SaturationExcess, na.rm=T)/nCells
      LandscapeBalance$InfiltrationExcess[day] <- sum(res_wb_day$InfiltrationExcess, na.rm=T)/nCells
      LandscapeBalance$CapillarityRise[day] <- sum(res_wb_day$CapillarityRise, na.rm=T)/nCells
      LandscapeBalance$DeepAquiferLoss[day] <- sum(res_wb_day$DeepAquiferLoss, na.rm=T)/nCells
      LandscapeBalance$AquiferExfiltration[day] <- sum(res_wb_day$AquiferExfiltration, na.rm=T)/nCells
      LandscapeBalance$CellRunoff[day] <- sum(res_wb_day$Runoff, na.rm=T)/nCells
      LandscapeBalance$CellRunon[day] <- sum(res_wb_day$Runon, na.rm=T)/nCells
      LandscapeBalance$Infiltration[day] <- sum(res_wb_day$Infiltration, na.rm=T)/nCells
      LandscapeBalance$InterflowBalance[day] <- sum(res_wb_day$InterflowBalance, na.rm=T)/nCells
      LandscapeBalance$BaseflowBalance[day] <- sum(res_wb_day$BaseflowBalance, na.rm=T)/nCells
      LandscapeBalance$ChannelExport[day] <- sum(res_wb_day$ChannelExport, na.rm=T)/nCells
      LandscapeBalance$WatershedExport[day] <- sum(res_wb_day$WatershedExport, na.rm=T)/nCells
      
      if(nSoil>0) {
        SoilLandscapeBalance$PET[day] <- sum(res_wb_day$PET[isSoilCell], na.rm=T)/nCells
        SoilLandscapeBalance$Rain[day] <- sum(res_wb_day$Rain[isSoilCell], na.rm=T)/nSoil
        SoilLandscapeBalance$Snow[day] <- sum(res_wb_day$Snow[isSoilCell], na.rm=T)/nSoil
        SoilLandscapeBalance$DeepDrainage[day] <- sum(res_wb_day$DeepDrainage[isSoilCell], na.rm=T)/nSoil
        SoilLandscapeBalance$InfiltrationExcess[day] <- sum(res_wb_day$InfiltrationExcess[isSoilCell], na.rm=T)/nSoil
        SoilLandscapeBalance$SaturationExcess[day] <- sum(res_wb_day$SaturationExcess[isSoilCell], na.rm=T)/nSoil
        SoilLandscapeBalance$CapillarityRise[day] <- sum(res_wb_day$CapillarityRise[isSoilCell], na.rm=T)/nSoil
        SoilLandscapeBalance$DeepAquiferLoss[day] <- sum(res_wb_day$DeepAquiferLoss, na.rm=T)/nSoil
        SoilLandscapeBalance$AquiferExfiltration[day] <- sum(res_wb_day$AquiferExfiltration[isSoilCell], na.rm=T)/nSoil
        SoilLandscapeBalance$InterflowBalance[day] <- sum(res_wb_day$InterflowBalance[isSoilCell], na.rm=T)/nSoil
        SoilLandscapeBalance$CellRunoff[day] <- sum(res_wb_day$Runoff[isSoilCell], na.rm=T)/nSoil
        SoilLandscapeBalance$CellRunon[day] <- sum(res_wb_day$Runon[isSoilCell], na.rm=T)/nSoil
        SoilLandscapeBalance$Snowmelt[day] <- sum(res_wb_day$Snowmelt[isSoilCell], na.rm=T)/nSoil
        SoilLandscapeBalance$NetRain[day] <- sum(res_wb_day$NetRain[isSoilCell], na.rm=T)/nSoil
        SoilLandscapeBalance$Interception[day] <- (sum(res_wb_day$Rain[isSoilCell], na.rm=T) - sum(res_wb_day$NetRain[isSoilCell], na.rm=T))/nSoil
        SoilLandscapeBalance$Infiltration[day] <- sum(res_wb_day$Infiltration[isSoilCell], na.rm=T)/nSoil
        SoilLandscapeBalance$SoilEvaporation[day] <- sum(res_wb_day$SoilEvaporation[isSoilCell], na.rm=T)/nSoil
        SoilLandscapeBalance$Transpiration[day] <- sum(res_wb_day$Transpiration[isSoilCell], na.rm=T)/nSoil
        SoilLandscapeBalance$HerbTranspiration[day] <- sum(res_wb_day$HerbTranspiration[isSoilCell], na.rm=T)/nSoil
      }
    }
    if(progress) cli::cli_progress_update()
  }
  if(progress) cli::cli_progress_done()
  
  if(watershed_model=="serghei") {
    .finishSerghei()
  }
  
  LandscapeBalance$Precipitation <- LandscapeBalance$Rain + LandscapeBalance$Snow
  if(watershed_model == "tetis"){
    SoilLandscapeBalance$Precipitation <- SoilLandscapeBalance$Rain + SoilLandscapeBalance$Snow
    # FINAL CONTENT
    finalSoilContent <- 0 
    for(i in 1:nCells) {
      if((y$land_cover_type[i] %in% c("wildland", "agriculture")) && (!is.null(y$state[[i]]))) {
        x <- y$state[[i]]
        finalSoilContent <- finalSoilContent + (sum(soil_water(x$soil, "VG"), na.rm=TRUE)/nSoil)
      }
    }
    finalSnowContent <- sum(y$snowpack, na.rm=TRUE)/nCells
    finalAquiferContent <- sum(y$aquifer, na.rm=TRUE)/nCells
    finalLandscapeContent <- finalSoilContent*(nSoil/nCells)+finalAquiferContent+finalSnowContent
    
    Precipitationsum <- sum(LandscapeBalance$Precipitation, na.rm=T)
    Rainfallsum <- sum(LandscapeBalance$Rain, na.rm=T)
    NetRainsum <- sum(LandscapeBalance$NetRain, na.rm=T)
    Interceptionsum <- sum(LandscapeBalance$Interception, na.rm=T)
    Infiltrationsum <- sum(LandscapeBalance$Infiltration, na.rm=T)
    Snowsum <- sum(LandscapeBalance$Snow, na.rm=T)
    Snowmeltsum <- sum(LandscapeBalance$Snowmelt, na.rm=T)
    CellRunoffsum <- sum(LandscapeBalance$CellRunoff, na.rm=T)
    CellRunonsum <- sum(LandscapeBalance$CellRunon, na.rm=T)
    DeepDrainagesum <- sum(LandscapeBalance$DeepDrainage, na.rm=T)
    CapillarityRisesum <- sum(LandscapeBalance$CapillarityRise, na.rm=T)
    DeepAquiferLosssum <- sum(LandscapeBalance$DeepAquiferLoss, na.rm=T)
    SaturationExcesssum <- sum(LandscapeBalance$SaturationExcess, na.rm=T)
    SoilEvaporationsum <- sum(LandscapeBalance$SoilEvaporation , na.rm=T)
    Transpirationsum <- sum(LandscapeBalance$Transpiration , na.rm=T)
    HerbTranspirationsum <- sum(LandscapeBalance$HerbTranspiration , na.rm=T)
    AquiferExfiltrationsum <- sum(LandscapeBalance$AquiferExfiltration , na.rm=T)
    InterflowBalancesum <- sum(LandscapeBalance$InterflowBalance , na.rm=T)
    BaseflowBalancesum <- sum(LandscapeBalance$BaseflowBalance , na.rm=T)
    snowpack_wb <- Snowsum - Snowmeltsum
    if(header_footer) {
      cli::cli_li(paste0("Snowpack balance",
                         " content (mm): ", round(finalSnowContent - initialSnowContent,2),
                         " fluxes (mm): ", round(snowpack_wb,2)))
    }
    SoilInterceptionsum <- sum(SoilLandscapeBalance$Interception, na.rm=T)
    SoilInfiltrationsum <- sum(SoilLandscapeBalance$Infiltration, na.rm=T)
    SoilSnowsum <- sum(SoilLandscapeBalance$Snow, na.rm=T)
    SoilSnowmeltsum <- sum(SoilLandscapeBalance$Snowmelt, na.rm=T)
    SoilSaturationExcesssum <- sum(SoilLandscapeBalance$SaturationExcess, na.rm=T)
    SoilDeepDrainagesum <- sum(SoilLandscapeBalance$DeepDrainage, na.rm=T)
    SoilCapillarityRisesum <- sum(SoilLandscapeBalance$CapillarityRise, na.rm=T)
    SoilSoilEvaporationsum <- sum(SoilLandscapeBalance$SoilEvaporation , na.rm=T)
    SoilHerbTranspirationsum <- sum(SoilLandscapeBalance$HerbTranspiration , na.rm=T)
    SoilTranspirationsum <- sum(SoilLandscapeBalance$Transpiration , na.rm=T)
    SoilInterflowBalancesum <- sum(SoilLandscapeBalance$InterflowBalance , na.rm=T)
    ChannelExportsum <- sum(ChannelExport, na.rm=T)/nCells
    WatershedExportsum <- sum(WatershedExport, na.rm=T)/nCells

    soil_input <- (SoilInfiltrationsum + SoilCapillarityRisesum + SoilInterflowBalancesum)
    soil_output <- (SoilDeepDrainagesum + SoilSoilEvaporationsum + SoilHerbTranspirationsum + SoilTranspirationsum + SoilSaturationExcesssum)
    soil_wb <-  soil_input - soil_output
    if(header_footer) {
      cli::cli_li(paste0("Soil balance",
                         " content (mm): ", round(finalSoilContent - initialSoilContent,2),
                         " fluxes (mm): ",round(soil_wb,2)))
    }
    
    aquifer_wb <- DeepDrainagesum - AquiferExfiltrationsum - CapillarityRisesum - DeepAquiferLosssum
    if(header_footer){
      cli::cli_li(paste0("Aquifer balance",
                         " content (mm): ", round(finalAquiferContent - initialAquiferContent,2),
                         " fluxes (mm): ",round(aquifer_wb,2)))
      cli::cli_li(paste0("Aquifer fluxes (mm)",
                         " Drainage input: ", round(DeepDrainagesum,2),
                         " Exfiltration: ",round(AquiferExfiltrationsum,2),
                         " Capillary rise: ",round(CapillarityRisesum,2),
                         " Deep loss: ",round(DeepAquiferLosssum,2)))
    }
    
    landscape_etp <- SoilEvaporationsum + Transpirationsum + HerbTranspirationsum + Interceptionsum
    landscape_wb <- Precipitationsum - ChannelExportsum - WatershedExportsum - landscape_etp - DeepAquiferLosssum
    if(header_footer) {
      cli::cli_li(paste0("Watershed balance",
                         " content (mm): ", round(finalLandscapeContent - initialLandscapeContent,2),
                         " fluxes (mm): ",round(landscape_wb,2)))
      cli::cli_li(paste0("Watershed fluxes (mm)",
                         " Precipitation: ", round(Precipitationsum,2),
                         " Surface export: ",round(ChannelExportsum + WatershedExportsum,2),
                         " Evapotransp.: ",round(landscape_etp,2),
                         " Deep loss: ",round(DeepAquiferLosssum,2)))
    }
  }
  
  sf <- sf::st_sf(geometry=sf::st_geometry(y))
  sf$state <- y$state
  if(watershed_model=="tetis") sf$aquifer <- y$aquifer
  sf$snowpack <- y$snowpack
  sf$summary <- summarylist
  sf$result <- resultlist
  if(watershed_model=="tetis") {
    l <- list(sf = sf::st_as_sf(tibble::as_tibble(sf)),
              watershed_balance = LandscapeBalance,
              watershed_soil_balance = SoilLandscapeBalance,
              channel_export = ChannelExport,
              watershed_export = WatershedExport)
  } else {
    l <- list(sf = sf::st_as_sf(tibble::as_tibble(sf)),
              watershed_balance = LandscapeBalance)
  }
  return(l)
}

.f_subwatershed_inner<-function(xi, 
                                raster_wrap,
                                local_model, 
                                meteo, dates,
                                CO2ByYear, 
                                summary_frequency,
                                summary_blocks,
                                watershed_control,
                                progress = FALSE, header_footer = progress) {
  return(.simulate_land_inner(local_model = local_model,
                              r = terra::unwrap(raster_wrap), 
                              y = xi[["y"]], 
                              sf_routing = xi[["sf_routing"]], 
                              meteo = meteo, dates = dates,
                              CO2ByYear = CO2ByYear, 
                              summary_frequency = summary_frequency,
                              summary_blocks = summary_blocks,
                              watershed_control = watershed_control,
                              progress = progress, header_footer = header_footer))
}

.simulate_land<-function(land_model = "spwb_land", 
                         r, y, SpParams, meteo, dates = NULL,
                         CO2ByYear = numeric(0), 
                         summary_blocks = NULL,
                         summary_frequency = "years",
                         local_control = medfate::defaultControl(),
                         watershed_control = default_watershed_control(),
                         parallelize = FALSE, num_cores = detectCores()-1,
                         progress = TRUE, header_footer = progress) {


  if(header_footer) cli::cli_h2("INPUT CHECKING")
  
  #land (local) model
  land_model <- match.arg(land_model, c("spwb_land", "growth_land", "fordyn_land"))
  if(land_model == "spwb_land") {
    local_model <- "spwb"
    summary_blocks <- match.arg(summary_blocks, c(NA, "WaterBalance", "Stand"),several.ok = TRUE)
  } else if(land_model=="growth_land") {
    local_model <- "growth"
    summary_blocks <- match.arg(summary_blocks, c(NA,"WaterBalance", "Stand","CarbonBalance", "BiomassBalance"),several.ok = TRUE)
  }
  else if(land_model=="fordyn_land") local_model <- "growth"
  
  summary_frequency <- match.arg(summary_frequency, c("days", "weeks","months", "quarters","years"))
  
  #watershed model
  watershed_model <- watershed_control$watershed_model
  watershed_model <- match.arg(watershed_model, c("tetis", "serghei"))
  
  #check input
  
  if(!inherits(y, "sf")) cli::cli_abort("'sf' has to be of class 'sf'.")
  
  if(header_footer) cli::cli_progress_step(paste0("Checking raster topology"))
  raster_matching <- .raster_sf_matching(r, y)
  sf_coords <- raster_matching$sf_coords
  sf2cell <- raster_matching$sf2cell
  cell2sf <- raster_matching$cell2sf
  nrastercells <- prod(dim(r)[1:2])
  
  if(header_footer) cli::cli_progress_step(paste0("Checking 'sf' data columns"))
  .check_sf_input(y)
  if(!is.null(dates)) if(!inherits(dates, "Date")) cli::cli_abort("'dates' has to be of class 'Date'.")

  represented_area_m2 <- as.vector(terra::values(terra::cellSize(r)))
  patchsize <- mean(represented_area_m2, na.rm=TRUE)

  if(!("snowpack" %in% names(y))) {
    cli::cli_alert_info("Column 'snowpack' was missing in 'sf'. Initializing empty snowpack.")
    y$snowpack <- rep(0, nrow(y))
  }
  ## TETIS: Check additional elements
  if(watershed_model == "tetis") {
    if(!("depth_to_bedrock" %in% names(y))) cli::cli_abort("'depth_to_bedrock' has to be defined in 'sf'.")
    if(!("bedrock_conductivity" %in% names(y))) cli::cli_abort("'bedrock_conductivity' has to be defined in 'sf'.")
    if(!("bedrock_porosity" %in% names(y))) cli::cli_abort("'bedrock_porosity' has to be defined in 'sf'.")
    if(sum(y$bedrock_porosity<=0)>0) {
      y$bedrock_porosity[y$bedrock_porosity<=0] <- 0.001
      cli::cli_alert_info("Minimum bedrock porosity set to 0.1%.")
    }
    if(!("aquifer" %in% names(y))) {
      cli::cli_alert_info("Column 'aquifer' was missing in 'sf'. Initializing empty aquifer.")
      y$aquifer <- rep(0, nrow(y))
    }
  }
  ## SERGHEI: Enforce same soil layer definition
  if(watershed_model=="serghei") {
    serghei_parameters <- watershed_control[["serghei_parameters"]]
    y$soil <- .check_equal_soil_discretization(y$soil, serghei_parameters[["force_equal_layer_widths"]])
  }
  
  #duplicate input (to avoid modifying input objects)
  y <- rlang::duplicate(y)
  
  # Define result cells (if not already specified)
  if(!("result_cell" %in% names(y))) {
    y$result_cell <- rep(FALSE, nrow(y))
  }
  
  # Set local control if not existing
  if(!("local_control" %in% names(y))) {
    y$local_control <- vector("list", nrow(y))
  }
  default_non_result_control <- local_control
  default_non_result_control$standResults <- FALSE
  default_non_result_control$plantResults <- FALSE
  default_non_result_control$soilResults <- FALSE
  default_non_result_control$fireHazardResults <- FALSE
  default_non_result_control$temperatureResults <- FALSE
  default_non_result_control$leafResults <- FALSE
  default_non_result_control$plantLabileCarbonBalanceResults  <- FALSE
  default_non_result_control$plantStructureResults  <- FALSE
  default_non_result_control$growthMortalityResults  <- FALSE
  for(i in 1:nrow(y)) {
    if(is.null(y$local_control[[i]]) && !y$result_cell[i]) {
      y$local_control[[i]] <- default_non_result_control
    } 
  }
  
  datesMeteo <- .get_dates_meteo(y, meteo)
  if(is.null(dates)) {
    dates <- datesMeteo
  } else {
    if(sum(dates %in% datesMeteo)<length(dates))
      cli::cli_abort("Dates in 'dates' is not a subset of dates in 'meteo'.")
  }
  date.factor <- cut(dates, breaks=summary_frequency)
  df.int <- as.numeric(date.factor)
  nDays <- length(dates)
  
  nCells <- nrow(y)
  isAgricultureCell <- y$land_cover_type %in% c("agriculture")
  isWildlandCell <- y$land_cover_type %in% c("wildland")
  isSoilCell <- y$land_cover_type %in% c("wildland", "agriculture")
  nSoil <- sum(isSoilCell)
  nWild <- sum(y$land_cover_type %in% c("wildland"))
  nAgri <- sum(y$land_cover_type %in% c("agriculture"))
  nRock <- sum(y$land_cover_type %in% c("rock"))
  nArti <- sum(y$land_cover_type %in% c("artificial"))
  nWater <- sum(y$land_cover_type %in% c("water"))
  nSummary <- sum(table(date.factor)>0)

  # Do not allow results on cells that are rock/artificial/water
  y$result_cell[!isSoilCell] <- FALSE
  
  # TETIS: Build/check neighbours
  if(header_footer) cli::cli_progress_step(paste0("Determining neighbors and overland routing for TETIS"))
  sf_routing <- .overland_routing_inner(r, y, 
                                        raster_matching = raster_matching, 
                                        channel_flow_speed = watershed_control$tetis_parameters$channel_flow_speed, 
                                        patchsize = patchsize,
                                        subwatersheds = watershed_control$tetis_parameters$subwatersheds,
                                        max_overlap = watershed_control$tetis_parameters$max_overlap)
  if(header_footer) cli::cli_progress_done()
  
  #Print information area
  if(header_footer) {
    cli::cli_li(paste0("Hydrological model: ", toupper(watershed_model)))
    cli::cli_li(paste0("Number of grid cells: ", nrastercells, " Number of target cells: ", nCells))
    cli::cli_li(paste0("Average cell area: ", round(patchsize),
                       " m2, Total area: ", round(sum(represented_area_m2, na.rm=TRUE)/10000),
                       " ha, Target area: ", round(sum(represented_area_m2[!is.na(cell2sf)], na.rm=TRUE)/10000)," ha"))
    cli::cli_li(paste0("Cell land use [wildland: ", nWild, " agriculture: ", nAgri, " artificial: ", nArti, " rock: ", nRock, " water: ", nWater, "]"))
    cli::cli_li(paste0("Cells with soil: ", nSoil))
    cli::cli_li(paste0("Number of days to simulate: ",nDays))
    cli::cli_li(paste0("Number of temporal cell summaries: ", nSummary))
    cli::cli_li(paste0("Number of cells with daily model results requested: ", sum(y$result_cell)))
    cli::cli_li(paste0("Number of channel cells: ", sum(sf_routing$channel)))
    cli::cli_li(paste0("Number of outlet cells: ", sum(sf_routing$outlet)))
    if(watershed_model =="tetis" && watershed_control$tetis_parameters$subwatersheds) cli::cli_li(paste0("Number of subwatersheds: ", length(unique(sf_routing$subwatershed))))
    if(!is.null(meteo)) if(inherits(meteo, "stars") || inherits(meteo, "list")) cli::cli_li(paste0("Weather interpolation factor: ", watershed_control[["weather_aggregation_factor"]]))
  }

  if(header_footer) cli::cli_h2("INITIALISATION")
  
  y <- initialize_landscape(y, SpParams = SpParams, local_control = local_control, 
                            model = local_model, replace = FALSE, progress = progress)


  if(watershed_model =="serghei") {
    if(header_footer) cli::cli_h2("WHOLE-WATERSHED SIMULATION")
    # SIMULATE SERGHEI
    res <- .simulate_land_inner(local_model = local_model,
                                r = r, y = y, sf_routing = sf_routing, 
                                meteo = meteo, dates = dates,
                                CO2ByYear = CO2ByYear, 
                                summary_frequency = summary_frequency,
                                summary_blocks = summary_blocks,
                                watershed_control = watershed_control,
                                progress = progress, header_footer = header_footer) 
    if(header_footer) cli::cli_li("Water balance check not possible with SERGHEI")
  } else {

    # SIMULATION
    if(watershed_control$tetis_parameters$subwatersheds) {
      # print(names(sf_routing))
      subwatersheds <- sort(unique(sf_routing$subwatershed))
      sf <- sf::st_sf(geometry=sf::st_geometry(y))
      sf$state <- vector("list", nCells)
      sf$aquifer <- rep(NA, nCells)
      sf$snowpack <- rep(NA, nCells)
      sf$summary <- vector("list", nCells)
      sf$result <- vector("list", nCells)

      channel_cells <- which(sf_routing$channel)
      outlet_nonchannel_cells <- which(sf_routing$outlet & !sf_routing$channel)
      
      WatershedExport <- matrix(0,nrow = nDays, ncol = length(outlet_nonchannel_cells))
      colnames(WatershedExport) <- outlet_nonchannel_cells
      rownames(WatershedExport) <- as.character(dates)
      
      ChannelExport <- matrix(0,nrow = nDays, ncol = length(channel_cells))
      colnames(ChannelExport) <- channel_cells
      rownames(ChannelExport) <- as.character(dates)
      
      XI <- vector("list", length(subwatersheds))
      for(i in subwatersheds) {
        sel_subwatershed <- sf_routing$subwatershed==i
        XI[[i]] <- list("y" = y[sel_subwatershed, , drop = FALSE],
                        "sf_routing" = sf_routing[sel_subwatershed, , drop = FALSE])
      }
      
      if(parallelize) {
        nc <- min(num_cores, length(subwatersheds))
        if(header_footer) cli::cli_h2(paste0("PARALLEL SIMULATION of ", length(subwatersheds), " SUB-WATERSHEDS in ",nc, " NODES"))
        
        cl<-parallel::makeCluster(nc)
        tryCatch({
          res_inner_list <- parallel::clusterApply(cl = cl, 
                                                   x = XI, 
                                                   fun = .f_subwatershed_inner, 
                                                   raster_wrap = terra::wrap(r),
                                                   local_model = local_model,
                                                   meteo = meteo, dates = dates,
                                                   CO2ByYear = CO2ByYear, 
                                                   summary_frequency = summary_frequency,
                                                   summary_blocks = summary_blocks,
                                                   watershed_control = watershed_control,
                                                   progress = FALSE, header_footer = FALSE)
        },
        finally = {
          parallel::stopCluster(cl)
        })
      } else {
        res_inner_list <- vector("list", length(subwatersheds))
        for(i in subwatersheds) {
          sel_subwatershed <- sf_routing$subwatershed==i
          nCellsSub <- sum(sel_subwatershed)
          if(header_footer) cli::cli_h2(paste0("SIMULATION of SUB-WATERSHED #", i, " (", nCellsSub, " cells)"))
          res_inner_list[[i]] <- .f_subwatershed_inner(XI[[i]],
                                                       raster_wrap = terra::wrap(r),
                                                       local_model = local_model,
                                                       meteo = meteo, dates = dates,
                                                       CO2ByYear = CO2ByYear, 
                                                       summary_frequency = summary_frequency,
                                                       summary_blocks = summary_blocks,
                                                       watershed_control = watershed_control,
                                                       progress = progress, header_footer = header_footer)
        }
      }
      if(header_footer) cli::cli_h2("MERGING SUB-WATERSHED RESULTS")
      for(i in subwatersheds) {
        res_inner_sub <- res_inner_list[[i]]
        sel_subwatershed <- sf_routing$subwatershed==i
        nCellsSub <- sum(sel_subwatershed)
        sf_sub <- res_inner_sub$sf
        sf$state[sel_subwatershed] <- sf_sub$state
        sf$aquifer[sel_subwatershed] <- sf_sub$aquifer
        sf$snowpack[sel_subwatershed] <- sf_sub$snowpack
        sf$summary[sel_subwatershed] <- sf_sub$summary
        sf$result[sel_subwatershed] <- sf_sub$result
        watershed_balance_sub <- res_inner_sub$watershed_balance
        watershed_soil_balance_sub <- res_inner_sub$watershed_soil_balance
        watershed_balance_sub[,-1] <- watershed_balance_sub[,-1, drop = FALSE]*(nCellsSub/nCells)
        watershed_soil_balance_sub[,-1] <- watershed_soil_balance_sub[,-1, drop = FALSE]*(nCellsSub/nCells)
        if(i == subwatersheds[1]) {
          LandscapeBalance <- watershed_balance_sub
          SoilLandscapeBalance <- watershed_soil_balance_sub
        } else {
          LandscapeBalance[,-1] <- LandscapeBalance[,-1, drop = FALSE] + watershed_balance_sub[,-1, drop = FALSE]
          SoilLandscapeBalance[,-1] <- SoilLandscapeBalance[,-1, drop = FALSE] + watershed_soil_balance_sub[,-1, drop = FALSE]
        }
        channel_cells_sub <- channel_cells %in% which(sel_subwatershed)
        outlet_nonchannel_cells_sub <- outlet_nonchannel_cells %in% which(sel_subwatershed)
        ChannelExport[,channel_cells_sub] <- res_inner_sub$channel_export
        WatershedExport[,outlet_nonchannel_cells_sub] <- res_inner_sub$watershed_export
      }
      res_inner <- list(sf = sf::st_as_sf(tibble::as_tibble(sf)),
                        watershed_balance = LandscapeBalance,
                        watershed_soil_balance = SoilLandscapeBalance,
                        channel_export = ChannelExport,
                        watershed_export = WatershedExport)
      
    } else {
      if(header_footer) cli::cli_h2("WHOLE-WATERSHED SIMULATION")
      res_inner <- .simulate_land_inner(local_model = local_model,
                                        r = r, y = y, sf_routing = sf_routing, 
                                        meteo = meteo, dates = dates,
                                        CO2ByYear = CO2ByYear, 
                                        summary_frequency = summary_frequency,
                                        summary_blocks = summary_blocks,
                                        watershed_control = watershed_control,
                                        progress = progress, header_footer = header_footer) 
    }
    
    outlet_cells <- which(sf_routing$outlet)
    channel_cells <- which(sf_routing$channel)
    
    outlet_non_channel <- !(outlet_cells %in% which(sf_routing$channel))
    # print(sum(outlet_non_channel))
    # print(length(outlet_cells))
    
    OutletExport_m3s <- matrix(0,nrow = nDays, ncol = length(outlet_cells))
    colnames(OutletExport_m3s) <- outlet_cells
    rownames(OutletExport_m3s) <- as.character(dates)
    # From mm/day = L/m2/day to m3/s
    # 1 L = 1 dm3 = 1e-3 m3
    # 1 d = 24h = 24*3600 s
    # Multiply by patch size to go from m3/m2 to m3 in the whole patch
    OutletExport_m3s[, outlet_non_channel] <- (res_inner$watershed_export/1e3)*patchsize/(3600*24)
    ChannelExport_m3s <- (res_inner$channel_export/1e3)*patchsize/(3600*24)
    initial_backlog_sum <- (sum(unlist(lapply(sf_routing$outlet_backlog, sum, na.rm= TRUE)))/1e3)*patchsize
    initial_outlet_amount <- sum(OutletExport_m3s*(3600*24))
    transport_target <- sum(ChannelExport_m3s*(3600*24))
    if(sum(sf_routing$channel)>0) {
      if(header_footer) {
        cli::cli_h2("CHANNEL ROUTING")
        cli::cli_li(paste0("Initial outlet backlog sum (m3): ", round(initial_backlog_sum)))
      }
      for(day in 1:nDays) {
        # cat(paste0("day ", day,"\n"))
        ChannelExport_vector <- rep(0, nCells)
        ChannelExport_vector[channel_cells] <- res_inner$channel_export[day,]
        WatershedExport_vector <- rep(0, nCells)
        .tetisChannelRouting(ChannelExport_vector, WatershedExport_vector,
                             sf_routing$channel, sf_routing$outlet, 
                             sf_routing$target_outlet, sf_routing$distance_to_outlet, sf_routing$outlet_backlog,
                             watershed_control, patchsize)
        OutletExport_m3s[day, ] <- OutletExport_m3s[day,] + (WatershedExport_vector[outlet_cells]/1e3)*patchsize/(3600*24)
      }
      final_backlog_sum <- (sum(unlist(lapply(sf_routing$outlet_backlog, sum, na.rm= TRUE)))/1e3)*patchsize
      final_outlet_amount <- sum(OutletExport_m3s*(3600*24))
      if(header_footer) {
        cli::cli_li(paste0("Channel balance target (m3): ", round(transport_target), " outlet change (m3): ", round(final_outlet_amount - initial_outlet_amount), " backlog change (m3): ", round(final_backlog_sum - initial_backlog_sum)))
        cli::cli_li(paste0("Final outlet backlog sum (m3): ", round(final_backlog_sum)))
      }
    }
    
    sf_out <- res_inner$sf
    sf_out$outlet <- sf_routing$outlet
    sf_out$outlet_backlog <- sf_routing$outlet_backlog

    res <- list(watershed_control = watershed_control,
                sf = sf_out,
                watershed_balance = res_inner$watershed_balance,
                watershed_soil_balance = res_inner$watershed_soil_balance,
                channel_export_m3s = ChannelExport_m3s,
                outlet_export_m3s = OutletExport_m3s)
  }
  
  class(res)<-c(land_model, "list")
  return(res)
}


#' Watershed simulations
#' 
#' Functions to perform simulations on a watershed described by a set of connected grid cells. 
#' \itemize{
#'   \item{Function \code{spwb_land} implements a distributed hydrological model that simulates daily local water balance, from \code{\link[medfate]{spwb_day}}, 
#'         on grid cells of a watershed while accounting for overland runoff, subsurface flow and groundwater flow between cells.}
#'   \item{Function \code{growth_land} is similar to \code{spwb_land}, but includes daily local carbon balance, growth and mortality processes in grid cells, 
#'         provided by \code{\link[medfate]{growth_day}}.} 
#'   \item{Function \code{fordyn_land} extends the previous two functions with the simulation of management, seed dispersal, recruitment
#'         and resprouting.}
#' }
#' 
#' @param r An object of class \code{\link[terra]{SpatRaster}}, defining the raster topology.
#' @param sf An object of class \code{\link[sf]{sf}} with the following columns:
#'   \itemize{
#'     \item{\code{geometry}: Spatial point geometry corresponding to cell centers.}
#'     \item{\code{elevation}: Elevation above sea level (in m).}
#'     \item{\code{slope}: Slope (in degrees).}
#'     \item{\code{aspect}: Aspect (in degrees).}
#'     \item{\code{land_cover_type}: Land cover type of each grid cell (values should be 'wildland', 'agriculture', 'rock', 'artificial' or 'water').}
#'     \item{\code{forest}: Objects of class \code{\link[medfate]{forest}}.}
#'     \item{\code{soil}: Objects of class \code{\link[medfate]{soil}} or data frames of physical properties.}
#'     \item{\code{state}: Objects of class \code{\link[medfate]{spwbInput}} or \code{\link[medfate]{growthInput}} (optional).}
#'     \item{\code{meteo}: Data frames with weather data (required if parameter \code{meteo = NULL}).}
#'     \item{\code{crop_factor}: Crop evapo-transpiration factor. Only required for 'agriculture' land cover type.}
#'     \item{\code{local_control}: A list of control parameters (optional). Used to override function parameter \code{local_control} for specific cells (values can be \code{NULL} for the remaining ones).}
#'     \item{\code{snowpack}: An optional numeric vector with the snow water equivalent content of the snowpack in each cell (in mm). If missing it will be initialized to zero.}
#'     \item{\code{management_arguments}: Lists with management arguments (optional, relevant for \code{fordyn_land} only).}
#'     \item{\code{result_cell}: A logical indicating that local model results are desired (optional, relevant for \code{spwb_land} and  \code{growth_land} only). Model results are only produced for wildland and agriculture cells. }
#'   }
#'   When using TETIS watershed model, the following columns are also REQUIRED:
#'   \itemize{
#'     \item{\code{depth_to_bedrock}: Depth to bedrock (mm).}
#'     \item{\code{bedrock_conductivity}: Bedrock (saturated) conductivity (in m·day-1).}
#'     \item{\code{bedrock_porosity}: Bedrock porosity (the proportion of pore space in the rock).}
#'   }
#'   When using TETIS watershed model, the following columns are OPTIONAL:
#'   \itemize{
#'     \item{\code{aquifer}: A numeric vector with the water content of the aquifer in each cell (in mm). If missing, it will be initialized to zero.}
#'     \item{\code{deep_aquifer_loss}: A numeric vector with the maximum daily loss to a deeper aquifer (in mm·day-1). If missing all cells take their value from \code{deep_aquifer_loss} in \code{\link{default_watershed_control}}}
#'     \item{\code{channel}: A logical (or binary) vector indicating overland channel routing.}
#'     \item{\code{outlet_backlog}: A list vector indicating channel backlog of outlet cells from a previous simulation.}
#'   }
#' @param SpParams A data frame with species parameters (see \code{\link[medfate]{SpParamsMED}}). IMPORTANT: If \code{sf} has been already initialized, this parameter has no effect.
#' @param meteo Input meteorological data (see \code{\link{spwb_spatial}} and details).
#' @param dates A \code{\link{Date}} object describing the days of the period to be modeled.
#' @param CO2ByYear A named numeric vector with years as names and atmospheric CO2 concentration (in ppm) as values. Used to specify annual changes in CO2 concentration along the simulation (as an alternative to specifying daily values in \code{meteo}).
#' @param summary_frequency Frequency in which cell summary will be produced (e.g. "years", "months", "weeks", ...) (see \code{\link{cut.Date}}).
#'                          In \code{fordyn_land} summary frequency can only be "months" or "years". 
#' @param summary_blocks A character vector with variable blocks for cell summaries (or \code{NULL} to retain only basic summaries). Accepted summary blocks for \code{spwb_land} are "WaterBalance" and "Stand". For \code{growth_land} and \code{fordyn_land}, "CarbonBalance" and "BiomassBalance" are also accepted.  
#' @param local_control A list of control parameters (see \code{\link[medfate]{defaultControl}}) for function \code{\link[medfate]{spwb_day}} or \code{\link[medfate]{growth_day}}. By default,
#'                      parameter \code{soilDomains} is set to \code{"single"}, meaning a single-domain Richards model. IMPORTANT: If \code{sf} has been already initialized, this parameter has no effect.
#' @param watershed_control A list of watershed control parameters (see \code{\link{default_watershed_control}}). Importantly, the sub-model used
#'                          for lateral water flows - either \enc{Francés}{Frances} et al. (2007) or \enc{Caviedes-Voullième}{Caviedes-Voullieme} et al. (2023) - is specified there.
#' @param management_function A function that implements forest management actions (see \code{\link[medfate]{fordyn}}).
#' of such lists, one per spatial unit.
#' @param parallelize Boolean flag to try parallelization (only works with subwatersheds, see details).
#' @param num_cores Integer with the number of cores to be used for parallel computation (by default it will use all clusters minus one).
#' @param progress Boolean flag to display progress information for simulations.
#'  
#' @return Functions \code{spwb_land}, \code{growth_land} and \code{fordyn_land} return a list of class of the same name as the function with the following elements:
#' \itemize{
#'   \item{\code{watershed_control}: A list with input control parameters.}
#'   \item{\code{sf}: An object of class \code{\link[sf]{sf}}, similar to the output of \code{\link{spwb_spatial}}, 
#'   with the following columns:
#'     \itemize{
#'        \item{\code{geometry}: Spatial geometry.}
#'        \item{\code{state}: A list of model input objects for each simulated stand.}
#'        \item{\code{aquifer}: A numeric vector with the water volume in the aquifer of each cell.}
#'        \item{\code{snowpack}: A numeric vector with the snowpack water equivalent volume of each cell.}
#'        \item{\code{summary}: A list of cell summaries containing at least the following variables (additional variables are summarized using \code{summary_blocks}):
#'         \itemize{
#'           \item{\code{MinTemperature}: Minimum temperature (degrees Celsius).}
#'           \item{\code{MaxTemperature}: Maximum temperature (degrees Celsius).}
#'           \item{\code{PET}: Potential evapotranspiration (in mm).}
#'           \item{\code{Rain}: Rainfall (in mm).}
#'           \item{\code{Snow}: Snowfall (in mm).}
#'           \item{\code{SWE}: Snow water equivalent (in mm) of the snowpack.}
#'           \item{\code{RWC}: Soil relative water content with respect to field capacity (in percent).}
#'           \item{\code{SoilVol}: Soil water volume integrated across vertical layers (in mm).}
#'           \item{\code{WTD}: Saturated soil water table depth (in mm from surface).}
#'           \item{\code{DTA}: Depth to aquifer (in m from surface).}
#'         }
#'       }
#'       \item{\code{result}: A list of cell detailed results (only for those indicated in the input), with contents depending on the local model.}
#'       \item{\code{outlet}: A logical vector indicating outlet cells.}
#'       \item{\code{outlet_backlog}: A list vector indicating channel backlog of outlet cells (for subsequent simulations).}
#'     }
#'     In function \code{fordyn_land} the \code{\link[sf]{sf}} object contains additional columns:
#'     \itemize{
#'        \item{\code{forest}: A list of \code{\link[medfate]{forest}} objects for each simulated stand, to be used in subsequent simulations (see \code{\link{update_landscape}}).}
#'        \item{\code{management_arguments}: A list of management arguments for each simulated stand, to be used in subsequent simulations (see \code{\link{update_landscape}}).}
#'        \item{\code{tree_table}: A list of data frames for each simulated stand, containing the living trees at each time step.}
#'        \item{\code{shrub_table}: A list of data frames for each simulated stand, containing the living shrub at each time step.}
#'        \item{\code{dead_tree_table}: A list of data frames for each simulated stand, containing the dead trees at each time step.}
#'        \item{\code{dead_shrub_table}: A list of data frames for each simulated stand, containing the dead shrub at each time step.}
#'        \item{\code{cut_tree_table}: A list of data frames for each simulated stand, containing the cut trees at each time step.}
#'        \item{\code{cut_shrub_table}: A list of data frames for each simulated stand, containing the cut shrub at each time step.}
#'     }
#'   }
#'   \item{\code{watershed_balance}: A data frame with as many rows as days and where columns are (spatially-averaged) components of the water balance at the watershed level (i.e., rain, snow, interception, infiltration, soil evaporation, plant transpiration, ...).}
#'   \item{\code{watershed_soil_balance}: A data frame with as many rows as days and where columns are (spatially-averaged) components of the water balance at the watershed level restricted to those cells with a soil definition.}
#'   \item{\code{channel_export_m3s}: A matrix with daily values of runoff (in m3/s) reaching each of the channel cells of the landscape (useful for channel processing with an external model).}
#'   \item{\code{outlet_export_m3s}: A matrix with daily values of runoff (in m3/s) reaching each of the outlet cells of the landscape. Each outlet drains its own subset of cells (sometimes including channel routing), so the 
#'                                   daily overall watershed export corresponds to the sum of row values.}
#' }
#' 
#' @details
#' IMPORTANT: Simulation function will normally call the initialization of state variables via an internal call to \code{\link{initialize_landscape}}, using parameters \code{local_control} and \code{SpParams} in this call. The default \code{soilDomains = "single"} means that vertical bulk soil flows are simulated using a single permeability domain with Richards equation.
#' However, if object \code{sf} has been previously initialized, then the control parameters of this previous initialization will remain the same. In other words, parameters \code{local_control} and \code{SpParams} will have no effect in the call to the simulation routines if the \code{sf} has been previously initialized.
#' 
#' Two sub-models are available for lateral water transfer processes (overland flow, sub-surface flow, etc.), either "TETIS" 
#' (similar to \enc{Francés}{Frances} et al. 2007) or "SERGHEI" (\enc{Caviedes-Voullième}{Caviedes-Voullieme} et al. 2023).
#' 
#' IMPORTANT: medfateland needs to be compiled along with SERGHEI model in order to launch simulations with using this
#' distributed hydrological model.
#'
#' When running \code{fordyn_land}, the input 'sf' object has to be in a Universal Transverse Mercator (UTM) coordinate system (or any other projection using meters as length unit)
#' for appropriate behavior of dispersal sub-model.
#'
#' Due to the large communication overload, parallel computation is only allowed for TETIS in combination with definition of subwatersheds (see flag of TETIS parameters in \code{\link{default_watershed_control}}).
#' 
#' When dealing with large data sets, weather data included in the 'sf' object will likely be very data hungry. In those cases, it is 
#' recommended to resort on weather interpolation (see \code{\link{spwb_spatial}}). Weather interpolation can be done using a coarser resolution
#' than that of raster 'r', by changing the watershed control parameter called 'weather_aggregation_factor' (see \code{\link{default_watershed_control}}).
#' 
#' @author 
#' Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF.
#' 
#' Maria \enc{González-Sanchís}{Gonzalez-Sanchis}, Universitat Politecnica de Valencia. 
#' 
#' Daniel \enc{Caviedes-Voullième}{Caviedes-Voullieme}, Forschungszentrum Julich.
#' 
#' Mario \enc{Morales-Hernández}{Morales-Hernandez}, Universidad de Zaragoza.
#' 
#' @seealso \code{\link{default_watershed_control}}, \code{\link{initialize_landscape}}, \code{\link{overland_routing}},
#' \code{\link{spwb_land_day}}, \code{\link[medfate]{spwb_day}},  \code{\link[medfate]{growth_day}},
#' \code{\link{spwb_spatial}}, \code{\link{fordyn_spatial}}, \code{\link{dispersal}}
#' 
#' @references 
#' \enc{Francés}{Frances}, F., \enc{Vélez}{Velez}, J.I. & \enc{Vélez}{Velez}, J.J. (2007). Split-parameter structure for the automatic calibration of distributed hydrological models. Journal of Hydrology, 332, 226–240. 
#' 
#' \enc{Caviedes-Voullième}{Caviedes-Voullieme}, D., \enc{Morales-Hernández}{Morales-Hernandez}, M., Norman, M.R. & Ogzen-Xian, I. (2023). SERGHEI (SERGHEI-SWE) v1.0: a performance-portable high-performance parallel-computing shallow-water solver for hydrology and environmental hydraulics. Geoscientific Model Development, 16, 977-1008.
#' 
#' @examples 
#' \donttest{
#' # Load example watershed data
#' data("example_watershed")
#' 
#' # Set crop factor 
#' example_watershed$crop_factor <- NA
#' example_watershed$crop_factor[example_watershed$land_cover_type=="agriculture"] <- 0.75
#' 
#' # Set request for daily model results in cells number 3, 6 (outlet) and 9
#' example_watershed$result_cell <- FALSE
#' example_watershed$result_cell[c(3,6,9)] <- TRUE
#' 
#' # Get bounding box to determine limits
#' b <- sf::st_bbox(example_watershed)
#' b
#' 
#' # Define a raster topology, using terra package, 
#' # with the same CRS as the watershed. In this example cells have 100 m side.
#' # Coordinates in the 'sf' object are assumed to be cell centers
#' r <-terra::rast(xmin = 401380, ymin = 4671820, xmax = 402880, ymax = 4672620, 
#'                 nrow = 8, ncol = 15, crs = "epsg:32631")
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
#' # Watershed control parameters (TETIS model; Frances et al. 2007)
#' ws_control <- default_watershed_control("tetis")
#' 
#' # Launch simulations 
#' res <- spwb_land(r, example_watershed, SpParamsMED, examplemeteo, 
#'                  dates = dates, summary_frequency = "month",
#'                  watershed_control = ws_control)
#'                  
#' # Print a summary of water balance components
#' summary(res)
#' 
#' # Option 'reduce_to_dominant = TRUE' in initialization, may be useful to speed up calculations
#' example_simplified <- initialize_landscape(example_watershed, SpParams = SpParamsMED,
#'                                            local_control = defaultControl(soilDomains = "single"), 
#'                                            reduce_to_dominant = TRUE)
#' 
#' # Launch simulations over simplified landscape (should be considerably faster)
#' res_simplified <- spwb_land(r, example_simplified, SpParamsMED, examplemeteo, 
#'                             dates = dates, summary_frequency = "month",
#'                             watershed_control = ws_control)
#' }
#' 
#' @name spwb_land
#' @export
spwb_land<-function(r, sf, SpParams, meteo= NULL, dates = NULL,
                    CO2ByYear = numeric(0), 
                    summary_blocks = NULL,
                    summary_frequency = "years",
                    local_control = defaultControl(soilDomains = "single"),
                    watershed_control = default_watershed_control(),
                    parallelize = FALSE, num_cores = parallel::detectCores()-1, 
                    progress = TRUE) {
  if(progress) cli::cli_h1(paste0("Simulation of model 'spwb' over a watershed"))
  return(.simulate_land("spwb_land",
                        r = r, y = sf, SpParams = SpParams, meteo = meteo, dates = dates,
                        CO2ByYear = CO2ByYear,
                        summary_frequency = summary_frequency, 
                        summary_blocks = summary_blocks,
                        local_control = local_control,
                        watershed_control = watershed_control, 
                        parallelize = parallelize, num_cores = num_cores,
                        progress = progress, header_footer = progress))
}
#' @rdname spwb_land
#' @export
growth_land<-function(r, sf, SpParams, meteo = NULL, dates = NULL,
                      CO2ByYear = numeric(0), 
                      summary_blocks = NULL,
                      summary_frequency = "years",
                      local_control = medfate::defaultControl(soilDomains = "single"),
                      watershed_control = default_watershed_control(),
                      parallelize = FALSE, num_cores = parallel::detectCores()-1,
                      progress = TRUE) {
  if(progress) cli::cli_h1(paste0("Simulation of model 'growth' over a watershed"))
  return(.simulate_land("growth_land",
                        r = r, y = sf, SpParams = SpParams, meteo = meteo, dates = dates,
                        CO2ByYear = CO2ByYear,
                        summary_blocks = summary_blocks, 
                        summary_frequency = summary_frequency, 
                        local_control = local_control,
                        watershed_control = watershed_control, 
                        parallelize = parallelize, num_cores = num_cores,
                        progress = progress, header_footer = progress))
}

#' @rdname spwb_land
#' @param dispersal_control A list of dispersal control parameters (see \code{\link{default_dispersal_control}}). If NULL, then dispersal is not simulated. 
#' @export
fordyn_land <- function(r, sf, SpParams, meteo = NULL, dates = NULL,
                        CO2ByYear = numeric(0), 
                        summary_blocks = NULL,
                        summary_frequency = "years",
                        local_control = medfate::defaultControl(soilDomains = "single"),
                        watershed_control = default_watershed_control(),
                        dispersal_control = default_dispersal_control(),
                        management_function = NULL,
                        parallelize = FALSE, num_cores = parallel::detectCores()-1, 
                        progress = TRUE) {
  
  #watershed model
  watershed_model <- watershed_control$watershed_model
  summary_frequency <- match.arg(summary_frequency, c("months", "years"))
  
  # Summary flags
  waterBalanceSummary <- "WaterBalance" %in% summary_blocks
  standSummary <- "Stand" %in% summary_blocks
  carbonBalanceSummary <- "CarbonBalance" %in% summary_blocks
  biomassBalanceSummary <- "BiomassBalance" %in% summary_blocks
  
  varsSum <- .vars_summary("sum", standSummary = standSummary, waterBalanceSummary = waterBalanceSummary, carbonBalanceSummary = carbonBalanceSummary, biomassBalanceSummary = biomassBalanceSummary)
  varsMean <- .vars_summary("mean", standSummary = standSummary, waterBalanceSummary = waterBalanceSummary, carbonBalanceSummary = carbonBalanceSummary, biomassBalanceSummary = biomassBalanceSummary)
  varsState <- .vars_summary("state", standSummary = standSummary, waterBalanceSummary = waterBalanceSummary, carbonBalanceSummary = carbonBalanceSummary, biomassBalanceSummary = biomassBalanceSummary)
  
  watershed_model <- match.arg(watershed_model, c("tetis", "serghei"))
  
  if(progress) cli::cli_h1(paste0("Simulation of model 'fordyn' over a watershed"))
  
  #check input
  
  if(progress) cli::cli_progress_step(paste0("Checking topology"))
  raster_matching <- .raster_sf_matching(r, sf)
  sf_coords <- raster_matching$sf_coords
  sf2cell <- raster_matching$sf2cell
  cell2sf <- raster_matching$cell2sf
  nrastercells <- prod(dim(r)[1:2])

  if(progress) cli::cli_progress_step(paste0("Checking 'sf' data"))
  .check_sf_input(sf)
  
  
  if(!is.null(dates)) if(!inherits(dates, "Date")) cli::cli_abort("'dates' has to be of class 'Date'.")
  if(!("snowpack" %in% names(sf))) cli::cli_abort("'snowpack' has to be defined in 'sf'.")
  represented_area_m2 <- as.vector(terra::values(terra::cellSize(r)))
  patchsize <- mean(represented_area_m2, na.rm=TRUE)
  
  nCells <- nrow(sf)
  isSoilCell <- sf$land_cover_type %in% c("wildland", "agriculture")
  nSoil <- sum(isSoilCell)
  nWild <- sum(sf$land_cover_type %in% c("wildland"))
  nAgri <- sum(sf$land_cover_type %in% c("agriculture"))
  nRock <- sum(sf$land_cover_type %in% c("rock"))
  nArti <- sum(sf$land_cover_type %in% c("artificial"))
  nWater <- sum(sf$land_cover_type %in% c("water"))
  

  if(is.null(dates)) {
    # Try to get dates from input
    if(!is.null(meteo)) {
      if(inherits(meteo, "data.frame")) {
        if(!("dates" %in% names(meteo))) {
          dates <- as.Date(row.names(meteo))
        } else {
          dates <- as.Date(meteo$dates)
        }
      } else if(inherits(meteo, "stars")) {
        dates <- as.Date(stars::st_get_dimension_values(meteo, "date"))
      } else if(inherits(meteo, "list")) {
        dates <- NULL
        for(i in 1:length(meteo)) {
          dates_i <- as.Date(stars::st_get_dimension_values(meteo[[i]], "date"))
          if(is.null(dates)) dates <- dates_i
          else dates <- c(dates, dates_i)
        }
      }
    } else {
      if("meteo" %in% names(sf)) {
        if(!("dates" %in% names(sf$meteo[[1]]))) {
          dates <- as.Date(row.names(sf$meteo[[1]]))
        } else {
          dates <- as.Date(sf$meteo[[1]]$dates)
        }
      } else {
        stop("Column 'meteo' must be defined in 'sf' if not supplied separately")
      }
    }
  }
  years <- as.numeric(format(dates, "%Y"))
  months <- as.numeric(format(dates, "%m"))
  yearsUnique <- unique(years)
  nYears <- length(yearsUnique)
  
  if(progress) {
    cli::cli_li(paste0("Hydrological model: ", toupper(watershed_model)))
    cli::cli_li(paste0("Number of grid cells: ", nrastercells, " Number of target cells: ", nCells))
    cli::cli_li(paste0("Average cell area: ", round(patchsize),
                       " m2, Total area: ", round(sum(represented_area_m2, na.rm=TRUE)/10000),
                       " ha, Target area: ", round(sum(represented_area_m2[!is.na(cell2sf)], na.rm=TRUE)/10000)," ha"))
    cli::cli_li(paste0("Cell land use wildland: ", nWild, " agriculture: ", nAgri, " artificial: ", nArti, " rock: ", nRock, " water: ", nWater))
    cli::cli_li(paste0("Cells with soil: ", nSoil))
    cli::cli_li(paste0("Number of years to simulate: ",nYears))
    if(is.null(management_function)) {
      cli::cli_li("Forest management process not considered.")
    } else {
      cli::cli_li("Forest management process included.")
    }
    if(is.null(dispersal_control)) {
      cli::cli_li("Seed dispersal process not considered.")
    } else {
      cli::cli_li("Seed dispersal process included.")
    }
  }
  # Init growth 
  if(progress) cli::cli_h3(paste0("Initialisation"))
  for(i in 1:nCells) { 
    if(sf$land_cover_type[i] == "wildland")  {
      forest <- sf$forest[[i]]
      if(is.numeric(forest$treeData$Species)) {
        forest$treeData$Species <- medfate:::.speciesCharacterParameterFromSpIndex(forest$treeData$Species, SpParams, "Name")
      }
      if(is.numeric(forest$shrubData$Species)) {
        forest$shrubData$Species <- medfate:::.speciesCharacterParameterFromSpIndex(forest$shrubData$Species, SpParams, "Name")
      }
      if(local_control$allowRecruitment) {
        forest$treeData <- forest$treeData[,c("Species","DBH", "Height","N","Z50","Z95")]
        forest$shrubData <- forest$shrubData[,c("Species","Height","Cover", "Z50","Z95")]
      }
      sf$forest[[i]] <- forest
    }
  }
  sf <- initialize_landscape(sf, SpParams, local_control = local_control, model = "growth", progress = progress)
  
  LandscapeBalance <- NULL 
  SoilLandscapeBalance <- NULL
  ChannelExport_m3s <- NULL
  OutletExport_m3s <- NULL
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
    monthsYear <- months[years==year]
    datesYear <- dates[years==year]
    # 1.1 Calls growth_land model
    if(progress) cli::cli_li(paste0("Growth/mortality"))
    GL <- .simulate_land("growth_land",
                         r = r, y = sf, SpParams = SpParams, meteo = meteo, dates = datesYear,
                         CO2ByYear = CO2ByYear,
                         summary_frequency = "month", # Summary frequency to use statistics
                         summary_blocks = summary_blocks,
                         local_control = local_control,
                         watershed_control = watershed_control, 
                         parallelize = parallelize, num_cores = num_cores, 
                         progress = progress, header_footer = FALSE)
    
    # Store snowpack and aquifer state
    sf$aquifer <- GL$sf$aquifer
    sf$snowpack <- GL$sf$snowpack
    
    #Store landscape and cell summaries
    cell_summary_i <- GL$sf$summary
    
    if(summary_frequency=="years") {
      for(i in 1:nCells) {
        cell_summary_i[[i]] <- .aggregate_summary_to_annual(cell_summary_i[[i]], 
                                                            varsSum, varsMean, varsState)
      }
    }
    if(iYear==1) {
      ChannelExport_m3s <- GL$channel_export_m3s
      OutletExport_m3s <- GL$outlet_export_m3s
      LandscapeBalance <- GL$watershed_balance
      SoilLandscapeBalance <- GL$watershed_soil_balance
      cell_summary <- cell_summary_i
    } else {
      ChannelExport_m3s <- rbind(ChannelExport_m3s, GL$channel_export_m3s)
      OutletExport_m3s <- rbind(OutletExport_m3s, GL$outlet_export_m3s)
      LandscapeBalance <- rbind(LandscapeBalance, GL$watershed_balance)
      SoilLandscapeBalance <- rbind(SoilLandscapeBalance, GL$watershed_soil_balance)
      for(i in 1:nCells) { 
        cell_summary[[i]] <- rbind(cell_summary[[i]],cell_summary_i[[i]])
      }
    }
    
    # Seedbank dynamics, seed production and dispersal
    if(!is.null(dispersal_control)) {
      if(progress) cli::cli_li(paste0("Seed production/dispersal"))
      seedbank_list <- dispersal(sf, SpParams, 
                                 local_control, 
                                 distance_step = dispersal_control[["distance_step"]],
                                 maximum_dispersal_distance = dispersal_control[["maximum_dispersal_distance"]],
                                 min_percent = dispersal_control[["min_percent"]],
                                 stochastic_resampling = dispersal_control[["stochastic_resampling"]],
                                 progress = FALSE)
      for(i in 1:nCells) { 
        if(sf$land_cover_type[i] == "wildland")  {
          sf$forest[[i]]$seedBank <- seedbank_list[[i]]
        }
      }
    } else {
      if(progress) cli::cli_li(paste0("Local seed dynamics"))
      for(i in 1:nCells) { 
        if(sf$land_cover_type[i] == "wildland")  {
          # Reduce seed bank according to longevity
          sf$forest[[i]]$seedBank <- regeneration_seedmortality(sf$forest[[i]]$seedBank, SpParams)
          # Seed local production
          seed_local <- regeneration_seedproduction(sf$forest[[i]], SpParams, local_control)
          # Seed rain from control
          seed_rain <- local_control$seedRain
          if(!is.null(seed_rain)) seed <- unique(c(seed_local, seed_rain)) 
          else seed <- seed_local
          # Refill seed bank with new seeds
          sf$forest[[i]]$seedBank <- regeneration_seedrefill(sf$forest[[i]]$seedBank, seed)
        }
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
        if(!is.null(management_function) && ("management_arguments" %in% names(sf))) {
          management_args <- sf$management_arguments[[i]]
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
            sf$management_arguments[[i]] <- management_result$management_args
          }
        }
        # 3.1 Simulate species local recruitment
        if(local_control$allowRecruitment) {
          # Seed recruitment
          summary_i <- GL$sf$summary[[i]]
          monthlyMinTemp <- summary_i[, "MinTemperature"]
          monthlyMaxTemp <- summary_i[, "MaxTemperature"]
          monthlyPrecipitation <- summary_i[, "Snow"] + summary_i[, "Rain"]
          monthlyPET <- summary_i[,"PET"]
          monthlyTemp <- 0.606*monthlyMaxTemp + 0.394*monthlyMinTemp
          minMonthTemp <- min(monthlyTemp, na.rm=TRUE)
          moistureIndex <- sum(monthlyPrecipitation, na.rm=TRUE)/sum(monthlyPET, na.rm=TRUE)
          recr_forest <- medfate::regeneration_recruitment(forest, SpParams, local_control, minMonthTemp, moistureIndex, verbose = FALSE)
          
        } else {
          recr_forest <- emptyforest()
        }
        # 3.2 Simulate species resprouting
        if(local_control$allowResprouting) {
          resp_forest <- medfate::regeneration_resprouting(forest, xo$internalMortality, SpParams, local_control,
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
  out_sf$state <- sf$state
  out_sf$aquifer <- sf$aquifer
  out_sf$snowpack <- sf$snowpack
  out_sf$summary <- cell_summary
  out_sf$forest <- sf$forest
  if("management_arguments" %in% names(sf)) out_sf$management_arguments <- sf$management_arguments
  out_sf$tree_table <- treeTableVec
  out_sf$shrub_table <- shrubTableVec
  out_sf$dead_tree_table <- deadTreeTableVec
  out_sf$dead_shrub_table <- deadShrubTableVec
  out_sf$cut_tree_table <- cutTreeTableVec
  out_sf$cut_shrub_table <- cutShrubTableVec
  l <- list(watershed_control = watershed_control,
            sf = sf::st_as_sf(tibble::as_tibble(out_sf)),
            watershed_balance = LandscapeBalance,
            watershed_soil_balance = SoilLandscapeBalance,
            channel_export_m3s = ChannelExport_m3s,
            outlet_export_m3s = OutletExport_m3s)
  class(l)<-c("fordyn_land", "list")
  return(l)
}


.simulate_land_day<-function(land_model = "spwb_land_day", 
                             r, y, SpParams, meteo, date,
                             local_control = medfate::defaultControl(),
                             watershed_control = default_watershed_control(),
                             parallelize = FALSE, num_cores = detectCores()-1, 
                             progress = TRUE, header_footer = progress) {
  
  #land (local) model
  land_model <- match.arg(land_model, c("spwb_land_day", "growth_land_day"))
  if(land_model == "spwb_land_day") local_model <- "spwb"
  else if(land_model=="growth_land_day") local_model <- "growth"

  date <- as.Date(date)
  datechar <- as.character(date)
  
  if(progress) cli::cli_h1(paste0("Simulation of model '", local_model, "' over a watershed for day '", date, "'"))
  
  #watershed model
  watershed_model <- watershed_control$watershed_model
  watershed_model <- match.arg(watershed_model, c("tetis", "serghei"))
  
  #check input
  
  if(header_footer) cli::cli_progress_step(paste0("Checking topology"))
  raster_matching <- .raster_sf_matching(r, y)
  sf_coords <- raster_matching$sf_coords
  sf2cell <- raster_matching$sf2cell
  cell2sf <- raster_matching$cell2sf
  nrastercells <- prod(dim(r)[1:2])

  if(header_footer) cli::cli_progress_step(paste0("Checking 'sf' data"))
  .check_sf_input(y)
  if(!("snowpack" %in% names(y))) cli::cli_abort("'snowpack' has to be defined in 'y'.")
  represented_area_m2 <- as.vector(terra::values(terra::cellSize(r)))
  patchsize <- mean(represented_area_m2, na.rm=TRUE)
  
  ## TETIS: Check additional elements
  if(watershed_model == "tetis") {
    if(!("depth_to_bedrock" %in% names(y))) cli::cli_abort("'depth_to_bedrock' has to be defined in 'y'.")
    if(!("bedrock_conductivity" %in% names(y))) cli::cli_abort("'bedrock_conductivity' has to be defined in 'y'.")
    if(!("bedrock_porosity" %in% names(y))) cli::cli_abort("'bedrock_porosity' has to be defined in 'y'.")
    if(!("aquifer" %in% names(y))) cli::cli_abort("'aquifer' has to be defined in 'y'.")
  }
  ## SERGHEI: Enforce same soil layer definition
  if(watershed_model=="serghei") {
    serghei_parameters <- watershed_control[["serghei_parameters"]]
    y$soil <- .check_equal_soil_discretization(y$soil, serghei_parameters[["force_equal_layer_widths"]])
  }
  
  #duplicate input (to avoid modifying input objects)
  y <- rlang::duplicate(y)
  
  #get latitude (for medfate)  
  latitude <- sf::st_coordinates(sf::st_transform(sf::st_geometry(y),4326))[,2]
  
  if(!("result_cell" %in% names(y))) {
    y$result_cell <- rep(FALSE, nrow(y))
  }

  datesMeteo <- .get_dates_meteo(y, meteo)
  datesStarsList <- .get_dates_stars_list(meteo)
  
  nCells <- nrow(y)
  isSoilCell <- y$land_cover_type %in% c("wildland", "agriculture")
  isAgricultureCell <- y$land_cover_type %in% c("agriculture")
  isWildlandCell <- y$land_cover_type %in% c("wildland")
  nSoil <- sum(isSoilCell)
  nWild <- sum(y$land_cover_type %in% c("wildland"))
  nAgri <- sum(y$land_cover_type %in% c("agriculture"))
  nRock <- sum(y$land_cover_type %in% c("rock"))
  nArti <- sum(y$land_cover_type %in% c("artificial"))
  nWater <- sum(y$land_cover_type %in% c("water"))
  # Do not allow results on cells that are rock/artificial/water
  y$result_cell[!isSoilCell] <- FALSE
  
  
  # TETIS: Build/check neighbours
  if(watershed_model=="tetis") {
    if(header_footer) cli::cli_progress_step(paste0("Determining neighbors and discharge for TETIS"))
    sf_routing <- .overland_routing_inner(r, y, 
                                          raster_matching = raster_matching, 
                                          channel_flow_speed = watershed_control$tetis_parameters$channel_flow_speed, 
                                          patchsize = patchsize)
    outlets <- which(sf_routing$outlet)
  }
  if(header_footer) cli::cli_progress_done()
  

  #Print information area
  if(header_footer) {
    cli::cli_li(paste0("Hydrological model: ", toupper(watershed_model)))
    cli::cli_li(paste0("Number of grid cells: ", nrastercells, " Number of target cells: ", nCells))
    cli::cli_li(paste0("Average cell area: ", round(patchsize),
                       " m2, Total area: ", round(sum(represented_area_m2, na.rm=TRUE)/10000),
                       " ha, Target area: ", round(sum(represented_area_m2[!is.na(cell2sf)], na.rm=TRUE)/10000)," ha"))
    cli::cli_li(paste0("Cell land use wildland: ", nWild, " agriculture: ", nAgri, " artificial: ", nArti, " rock: ", nRock, " water: ", nWater))
    cli::cli_li(paste0("Cells with soil: ", nSoil))
    cli::cli_li(paste0("Number of cells with daily model results requested: ", sum(y$result_cell)))
    if(watershed_model=="tetis") {
      cli::cli_li(paste0("Number of channel cells: ", sum(sf_routing$channel)))
      cli::cli_li(paste0("Number of outlet cells: ", sum(sf_routing$outlet)))
    }
    if(!is.null(meteo)) if(inherits(meteo, "stars") || inherits(meteo, "list")) cli::cli_li(paste0("Weather interpolation factor: ", watershed_control[["weather_aggregation_factor"]]))
  }
  
  if(header_footer) cli::cli_progress_step(paste0("Building ", local_model, " input"))
  initialized_cells <- 0
  for(i in 1:nCells) { #Initialize if not previously initialized
    local_control_i <- NULL
    if("local_control" %in% names(y)) {
      if(!is.null(y$local_control[[i]])) {
        if(inherits(y$local_control[[i]], "list")) local_control_i <- y$local_control[[i]]
      }
    }
    if(is.null(local_control_i)) local_control_i <- local_control
    
    if((y$land_cover_type[i] == "wildland") && (is.null(y$state[[i]]))) {
      f <- y$forest[[i]]
      s <- y$soil[[i]]
      if(inherits(s, "data.frame")) s <- medfate::soil(s)
      if(local_model=="spwb") y$state[[i]] <- medfate::spwbInput(f, s, SpParams, local_control_i)
      else if(local_model=="growth") y$state[[i]] <- medfate::growthInput(f, s, SpParams, local_control_i)
      initialized_cells <- initialized_cells + 1
    } 
    else if((y$land_cover_type[i] == "wildland") && (!is.null(y$state[[i]]))) {
      x_i <- y$state[[i]]
    }
    else if((y$land_cover_type[i] == "agriculture") && (is.null(y$state[[i]]))) {
      s <- y$soil[[i]]
      cf <- y$crop_factor[i]
      if(inherits(s, "data.frame")) s <- medfate::soil(s)
      y$state[[i]] <- medfate::aspwbInput(cf, local_control_i, s)
      initialized_cells <- initialized_cells + 1
    } 
  }
  if(header_footer) {
    cli::cli_progress_step(paste0( initialized_cells, " cells needed initialization"))
  }

  # Define communication structures
  internalCommunication <- .defineInternalCommunication(y, local_model)
  
  serghei_interface <-NULL
  if(watershed_model=="serghei") {
    serghei_parameters <- watershed_control[["serghei_parameters"]]
    serghei_interface <- .initSerghei(limits = as.vector(terra::ext(r)),
                                      nrow = terra::nrow(r),
                                      ncol = terra::ncol(r),
                                      sf2cell = sf2cell,
                                      y$state,
                                      input_dir = serghei_parameters[["input_dir"]],
                                      output_dir = serghei_parameters[["output_dir"]])
  }

  meteo_mapping <- .get_meteo_mapping(r, y, meteo, sf_coords, sf2cell, 
                                      watershed_control[["weather_aggregation_factor"]])

  gridMeteo <- .build_grid_meteo_day(y, meteo, datesMeteo, date, 
                                     meteo_mapping,
                                     datesStarsList)

  ws_day  <- .createDayOutput(nCells, FALSE, FALSE, FALSE)
  
  if(watershed_model=="tetis") {
    .watershedDayTetis(output = ws_day,
                       internalCommunication = internalCommunication,
                       local_model = local_model,
                       y = y,
                       sf_routing = sf_routing,
                       watershed_control = watershed_control,
                       date = datechar,
                       gridMeteo = gridMeteo,
                       latitude = latitude,
                       patchsize = patchsize, 
                       progress = progress)
  } else if(watershed_model=="serghei") {
    ws_day <- .watershedDaySerghei(local_model = local_model,
                                   lct = y$land_cover_type, xList = y$state,
                                   snowpack = y$snowpack,
                                   sf2cell = sf2cell,
                                   serghei_interface = serghei_interface,
                                   watershed_control = watershed_control,
                                   date = datechar,
                                   gridMeteo = gridMeteo,
                                   latitude = latitude, elevation = y$elevation, slope = y$slope, aspect = y$aspect,
                                   progress = FALSE)
    .finishSerghei()
  }
  
  res <- sf::st_sf(geometry=sf::st_geometry(y))
  res$state = y$state

  if(watershed_model=="tetis") res$aquifer <- y$aquifer
  res$snowpack <- y$snowpack
  res$result <- list(NULL)
  for(i in 1:nCells) {
    if(y$result_cell[i]) res$result[[i]] <- ws_day$LocalResults[[i]]$simulation_results
  }
  if(watershed_model=="tetis")  {
    res$outlet <- sf_routing$outlet
    res$outlet_backlog <- sf_routing$outlet_backlog
  }
  wb <- ws_day$WatershedWaterBalance
  for(n in names(wb)) res[[n]] <- wb[[n]]
  return(sf::st_sf(tibble::as_tibble(res)))
}



#' One-day watershed simulations
#' 
#' Functions to perform one-day simulations on a watershed described by a set of connected grid cells. 
#' \itemize{
#'   \item{Function \code{spwb_land_day} implements a distributed hydrological model that simulates daily local water balance, from \code{\link[medfate]{spwb_day}}, 
#'         on grid cells of a watershed while accounting for overland runoff, subsurface flow and groundwater flow between cells.}
#'   \item{Function \code{growth_land_day} is similar to \code{spwb_land_day}, but includes daily local carbon balance, growth and mortality processes in grid cells, 
#'         provided by \code{\link[medfate]{growth_day}}.} 
#' }
#' 
#' @param r An object of class \code{\link[terra]{SpatRaster}}, defining the raster topology.
#' @param sf An object of class \code{\link[sf]{sf}} as described in \code{\link{spwb_land}}.
#' @param SpParams A data frame with species parameters (see \code{\link[medfate]{SpParamsMED}}).
#' @param meteo Input meteorological data (see \code{\link{spwb_spatial}} and details).
#' @param date A string with the date to be simulated.
#' @param local_control A list of control parameters (see \code{\link[medfate]{defaultControl}}) for function \code{\link[medfate]{spwb_day}} or \code{\link[medfate]{growth_day}}.
#' @param watershed_control A list of watershed control parameters (see \code{\link{default_watershed_control}}). Importantly, the sub-model used
#'                          for lateral water flows - either \enc{Francés}{Frances} et al. (2007) or \enc{Caviedes-Voullième}{Caviedes-Voullieme} et al. (2023) - is specified there.
#' @param progress Boolean flag to display progress information for simulations.
#'  
#' @return Functions \code{spwb_land_day} and \code{spwb_land_day} return a sf object:
#' \itemize{
#'    \item{\code{geometry}: Spatial geometry.}
#'    \item{\code{state}: A list of model input objects for each simulated stand.}
#'    \item{\code{aquifer}: A numeric vector with the water volume in the aquifer of each cell.}
#'    \item{\code{snowpack}: A numeric vector with the snowpack water equivalent volume of each cell.}
#'    \item{\code{result}: A list of cell detailed results (only for those indicated in the input), with contents depending on the local model.}
#'    \item{\code{outlet}: A logical vector indicating outlet cells (for subsequent simulations).}
#'    \item{\code{outlet_backlog}: A list vector indicating channel backlog of outlet cells.}
#'    \item{\code{MinTemperature}: Minimum temperature (degrees Celsius).}
#'    \item{\code{MaxTemperature}: Maximum temperature (degrees Celsius).}
#'    \item{\code{PET}: Potential evapotranspiration (in mm).}
#'    \item{\code{Rain}: Rainfall (in mm).}
#'    \item{\code{Snow}: Snowfall (in mm).}
#'    \item{\code{Snowmelt}: Snow melt (in mm).}
#'    \item{\code{Interception}: Rainfall interception (in mm).}
#'    \item{\code{NetRain}: Net rainfall, i.e. throughfall, (in mm).}
#'    \item{\code{Infiltration}: The amount of water infiltrating into the soil (in mm).}
#'    \item{\code{InfiltrationExcess}: The amount of water exceeding the soil infiltration capacity (in mm).}
#'    \item{\code{SaturationExcess}: The amount of water that reaches the soil surface because of soil saturation (in mm).}
#'    \item{\code{Runoff}: The amount of water exported via surface runoff (in mm).}
#'    \item{\code{DeepDrainage}: The amount of water draining from soil to the aquifer via deep drainage (in mm).}
#'    \item{\code{CapillarityRise}: Water entering the soil via capillarity rise (mm) from the water table.}
#'    \item{\code{SoilEvaporation}: Bare soil evaporation (in mm).}
#'    \item{\code{Transpiration}: Woody plant transpiration (in mm).}
#'    \item{\code{HerbTranspiration}: Herbaceous transpiration (in mm).}
#'    \item{\code{InterflowInput}: The amount of water that reaches the soil of the cell from adjacent cells via subsurface flow (in mm).}
#'    \item{\code{InterflowOutput}: The amount of water that leaves the soil of the cell towards adjacent cells via subsurface flow (in mm).}
#'    \item{\code{InterflowBalance}: The balance of water circulating via subsurface flow (in mm).}
#'    \item{\code{BaseflowInput}: The amount of water that reaches the aquifer of the cell from adjacent cells via groundwater flow (in mm).}
#'    \item{\code{BaseflowOutput}: The amount of water that leaves the aquifer of the cell towards adjacent cells via groundwater flow (in mm).}
#'    \item{\code{BaseflowBalance}: The balance of water circulating via groundwater flow (in mm).}
#'    \item{\code{AquiferExfiltration}: The amount of water of the cell that generates surface runoff due to the aquifer reaching the soil surface (in mm).}
#'  }
#' 
#' @details
#' See details in \code{\link{spwb_land}}. Subwatershed units and parallelization are not possible, at present, for single-day watershed simulations.
#' 
#' @author 
#' Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF.
#' 
#' Maria \enc{González-Sanchís}{Gonzalez-Sanchis}, Universitat Politecnica de Valencia. 
#' 
#' Daniel \enc{Caviedes-Voullième}{Caviedes-Voullieme}, Forschungszentrum Julich.
#' 
#' Mario \enc{Morales-Hernández}{Morales-Hernandez}, Universidad de Zaragoza.
#' 
#' @seealso \code{\link{default_watershed_control}},  \code{\link[medfate]{spwb_day}},  \code{\link[medfate]{growth_day}},
#' \code{\link{spwb_land}}, 
#' 
#' @references 
#' \enc{Francés}{Frances}, F., \enc{Vélez}{Velez}, J.I. & \enc{Vélez}{Velez}, J.J. (2007). Split-parameter structure for the automatic calibration of distributed hydrological models. Journal of Hydrology, 332, 226–240. 
#' 
#' \enc{Caviedes-Voullième}{Caviedes-Voullieme}, D., \enc{Morales-Hernández}{Morales-Hernandez}, M., Norman, M.R. & Ogzen-Xian, I. (2023). SERGHEI (SERGHEI-SWE) v1.0: a performance-portable high-performance parallel-computing shallow-water solver for hydrology and environmental hydraulics. Geoscientific Model Development, 16, 977-1008.
#' 
#' @examples 
#' # Load example watershed data after burnin period
#' data("example_watershed_burnin")
#' 
#' # Set request for daily model results in cells number 3, 6 (outlet) and 9
#' example_watershed_burnin$result_cell <- FALSE
#' example_watershed_burnin$result_cell[c(3,6,9)] <- TRUE
#' 
#' # Get bounding box to determine limits
#' b <- sf::st_bbox(example_watershed_burnin)
#' b
#' 
#' # Define a raster topology, using terra package, 
#' # with the same CRS as the watershed. In this example cells have 100 m side.
#' # Coordinates in the 'sf' object are assumed to be cell centers
#' r <-terra::rast(xmin = 401380, ymin = 4671820, xmax = 402880, ymax = 4672620, 
#'                 nrow = 8, ncol = 15, crs = "epsg:32631")
#' 
#' # Load example meteo data frame from package meteoland
#' data("examplemeteo")
#'   
#' # Load default medfate parameters
#' data("SpParamsMED")
#'   
#' # Watershed control parameters (TETIS model; Frances et al. 2007)
#' ws_control <- default_watershed_control("tetis")
#' 
#' # Launch simulation 
#' date <- "2001-03-01"
#' sf_out <- spwb_land_day(r, example_watershed_burnin, SpParamsMED, examplemeteo, 
#'                         date = date, 
#'                         watershed_control = ws_control)
#' 
#' @name spwb_land_day
#' @export
spwb_land_day<-function(r, sf, SpParams, meteo= NULL, date = NULL,
                        local_control = medfate::defaultControl(soilDomains = "single"),
                        watershed_control = default_watershed_control(),
                        progress = TRUE) {
  return(.simulate_land_day("spwb_land_day",
                            r = r, y = sf, SpParams = SpParams, meteo = meteo, date = date,
                            local_control = local_control,
                            watershed_control = watershed_control, 
                            progress = progress, header_footer = progress))
}
#' @rdname spwb_land_day
#' @export
growth_land_day<-function(r, sf, SpParams, meteo= NULL, date = NULL,
                          local_control = medfate::defaultControl(soilDomains = "single"),
                          watershed_control = default_watershed_control(),
                          progress = TRUE) {
  return(.simulate_land_day("growth_land_day",
                            r = r, y = sf, SpParams = SpParams, meteo = meteo, date = date,
                            local_control = local_control,
                            watershed_control = watershed_control, 
                            progress = progress, header_footer = progress))
}


#' Summary of landscape simulations
#'
#' @param object An object of class \code{spwb_land} or \code{groth_land} 
#' @param ... Additional parameters for summary functions
#'
#' @export
#' 
#' @rdname spwb_land
summary.spwb_land<-function(object, ...){
  wb <- object$watershed_balance
  sb <- object$watershed_soil_balance
  Precipitationsum <- sum(wb$Precipitation, na.rm=T)
  Rainfallsum <- sum(wb$Rain, na.rm=T)
  NetRainsum <- sum(wb$NetRain, na.rm=T)
  Interceptionsum <- sum(wb$Interception, na.rm=T)
  Infiltrationsum <- sum(wb$Infiltration, na.rm=T)
  Snowsum <- sum(wb$Snow, na.rm=T)
  Snowmeltsum <- sum(wb$Snowmelt, na.rm=T)
  CellRunoffsum <- sum(wb$CellRunoff, na.rm=T)
  CellRunonsum <- sum(wb$CellRunon, na.rm=T)
  DeepDrainagesum <- sum(wb$DeepDrainage, na.rm=T)
  CapillarityRisesum <- sum(wb$CapillarityRise, na.rm=T)
  DeepAquiferLosssum <- sum(wb$DeepAquiferLoss, na.rm=T)
  SaturationExcesssum <- sum(wb$SaturationExcess, na.rm=T)
  SoilEvaporationsum <- sum(wb$SoilEvaporation , na.rm=T)
  Transpirationsum <- sum(wb$Transpiration , na.rm=T)
  HerbTranspirationsum <- sum(wb$HerbTranspiration , na.rm=T)
  AquiferExfiltrationsum <- sum(wb$AquiferExfiltration , na.rm=T)
  WatershedExportsum <- sum(wb$WatershedExport, na.rm=T)
  InterflowBalancesum <- sum(wb$InterflowBalance , na.rm=T)
  BaseflowBalancesum <- sum(wb$BaseflowBalance , na.rm=T)
  
  SoilPrecipitationsum <- sum(sb$Precipitation, na.rm=T)
  SoilRainfallsum <- sum(sb$Rain, na.rm=T)
  SoilNetRainsum <- sum(sb$NetRain, na.rm=T)
  SoilInterceptionsum <- sum(sb$Interception, na.rm=T)
  SoilInfiltrationsum <- sum(sb$Infiltration, na.rm=T)
  SoilSnowsum <- sum(sb$Snow, na.rm=T)
  SoilSnowmeltsum <- sum(sb$Snowmelt, na.rm=T)
  SoilCellRunoffsum <- sum(sb$CellRunoff, na.rm=T)
  SoilCellRunonsum <- sum(sb$CellRunon, na.rm=T)
  SoilSaturationExcesssum <- sum(sb$SaturationExcess, na.rm=T)
  SoilDeepDrainagesum <- sum(sb$DeepDrainage, na.rm=T)
  SoilCapillarityRisesum <- sum(sb$CapillarityRise, na.rm=T)
  SoilDeepAquiferLosssum <- sum(sb$DeepAquiferLoss, na.rm=T)
  SoilSoilEvaporationsum <- sum(sb$SoilEvaporation , na.rm=T)
  SoilHerbTranspirationsum <- sum(sb$HerbTranspiration , na.rm=T)
  SoilTranspirationsum <- sum(sb$Transpiration , na.rm=T)
  SoilAquiferExfiltrationsum <- sum(sb$AquiferExfiltration , na.rm=T)
  SoilInterflowBalancesum <- sum(sb$InterflowBalance , na.rm=T)
  
  cat(paste0("  Snowpack water balance components:\n"))
  cat(paste0("    Snow fall (mm) ", round(Snowsum,2), "  Snow melt (mm) ",round(Snowmeltsum,2),"\n"))
  cat(paste0("  Soil water balance components:\n"))
  cat(paste0("    Infiltration (mm) ", round(SoilInfiltrationsum,2),"  Saturation excess (mm) ",round(SoilSaturationExcesssum,2),"\n"))
  cat(paste0("    Deep drainage (mm) ",round(SoilDeepDrainagesum,2),"  Capillarity rise (mm) ", round(SoilCapillarityRisesum,2),"\n"))
  cat(paste0("    Soil evaporation (mm) ",round(SoilSoilEvaporationsum,2),  "  Plant transpiration (mm) ", round(SoilTranspirationsum + SoilHerbTranspirationsum,2),"\n"))
  cat(paste0("    Interflow balance (mm) ", round(SoilInterflowBalancesum,2),"\n"))
  cat(paste0("  Aquifer water balance components:\n"))
  cat(paste0("    Deep drainage (mm) ", round(DeepDrainagesum,2), "  Capillarity rise (mm) ",round(CapillarityRisesum,2),"\n"))
  cat(paste0("    Exfiltration (mm) ",round(AquiferExfiltrationsum,2),"  Deep aquifer loss (mm) ", round(DeepAquiferLosssum,2), "\n"))
  cat(paste0("  Watershed water balance components:\n"))
  cat(paste0("    Precipitation (mm) ", round(Precipitationsum,2),"\n"))
  cat(paste0("    Interception (mm) ", round(Interceptionsum,2), "  Soil evaporation (mm) ",round(SoilEvaporationsum,2),"\n"))
  cat(paste0("    Plant transpiration (mm) ",round(Transpirationsum + HerbTranspirationsum ,2),"\n"))
  cat(paste0("    Subsurface flow balance (mm) ",round(InterflowBalancesum,2),"\n"))
  cat(paste0("    Groundwater flow balance (mm) ", round(BaseflowBalancesum,2),"\n"))
  cat(paste0("    Export runoff (mm) ", round(WatershedExportsum,2),"\n"))
  
}

#' @export
#' 
#' @rdname spwb_land
summary.growth_land <- function(object, ...) {
  summary.spwb_land(object, ...)
}