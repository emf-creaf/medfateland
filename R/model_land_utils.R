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
    } else if((y$land_cover_type[i]=="agriculture") && (!is.null(y$state[[i]]))) {
      xi <- y$state[[i]]
      max_num_soil_layers <- max(max_num_soil_layers, nrow(xi$soil))
    }
  }
  internalCommunication <- medfate::general_communication_structures(max_num_cohorts, max_num_soil_layers, max_num_canopy_layers, max_num_timesteps,
                                                                     local_model);
  return(internalCommunication)
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
                        "InterflowBalance", "BaseflowBalance", "AquiferExfiltration")
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
.vars_firehazard <- function(type = "all"){
  varsFireHazard <- c("DFMC","CFMC_understory","CFMC_overstory","ROS_surface",
                      "I_b_surface","t_r_surface", "FL_surface","Ic_ratio",
                      "ROS_crown", "I_b_crown", "t_r_crown","FL_crown",       
                      "SFP","CFP")
  if(type %in% c("mean", "all")) return(varsFireHazard)
  return(c())
}

.vars_summary <- function(type = "all",
                          standSummary, waterBalanceSummary, fireHazardSummary,
                          carbonBalanceSummary, biomassBalanceSummary) {
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
  if(fireHazardSummary) vars <- c(vars, .vars_firehazard(type))
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
