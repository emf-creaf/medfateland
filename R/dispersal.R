kernel_fun<- function(r, alpha, c) {
  (c*exp(-(r/alpha)^c))/(2*pi*(alpha^2)*gamma(2/c))
}
kernel_sum <- function(target_index, neighbours, distances, areas,
                       alpha, c) {
  k_sum <- kernel_fun(0, alpha = alpha, c = c)*areas[target_index] # Local value
  for(j in 1:length(neighbours)) {
    n_ij <- neighbours[j]
    d_ij <- distances[j]
    k_ij <- kernel_fun(d_ij, alpha = alpha, c = c)*areas[n_ij] #Kernel parameters missing
    if(!is.na(k_ij)){
      k_sum <- k_sum + k_ij 
    }
  }
  return(k_sum)
}

#' Seed production and dispersal
#' 
#' Simulates seed bank mortality, seed production and dispersal among stands
#'
#' @param sf_utm An object of class \code{\link{sf}} using a UTM projection (to measure distances in m) and with the following columns:
#'   \itemize{
#'     \item{\code{geometry}: Spatial geometry.}
#'     \item{\code{forest}: Objects of class \code{\link{forest}}.}
#'     \item{\code{represented_area_m2}: Area represented by each stand in m2 (alternatively \code{represented_area_ha} for hectare units is also allowed).}
#'   }
#' @param SpParams A data frame with species parameters (see \code{\link{SpParamsMED}}).
#' @param local_control A list of control parameters (see \code{\link{defaultControl}})
#' @param maximumDispersalDistance Maximum dispersal distance in meters.
#' @param minPercent A minimum percent of seed bank to retain entry in \code{seedBank} element of \code{forest}.
#' 
#' @details
#' Dispersal kernel follows Clark et al. (1999) and potential seed donors (neighbors) are defined up to a given grid distance order. 
#' The sum of dispersal kernel values over the neighbors plus the kernel of the local 
#' grid cell is used to define the normalizing constant for the kernel. A maximum value of 100\% seed bank refill is attained for
#' species with seed production in all seed donors and the local cell.
#' 
#' @return A list with forest objects (for wildland cover type) containing a modified seed bank
#' 
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF.
#' 
#' @references 
#' Clark et al. (1999). Seed dispersal near and far: Patterns across temperate and tropical forests. Ecology 80(5):1475-1494
#' 
#' @seealso \code{\link{fordyn_land}}
#' 
#' @export
#'
#' @examples
#' data(example_watershed)
#' data(SpParamsMED)
#' 
#' # Transform to UTM31
#' example_watershed_utm31 <- sf::st_transform(example_watershed, crs = 32631)
#'
#' # Estimate seed production and dispersal over the watershed
#' forest_list <- dispersal(example_watershed_utm31, SpParamsMED)
#' 
#' forest_list[[1]]$seedBank
dispersal <- function(sf_utm, SpParams, 
                      local_control = medfate::defaultControl(), 
                      maximumDispersalDistance = 1000, minPercent = 1) {

  if(!inherits(sf_utm, "sf")) stop("'sf_utm' has to be of class 'sf'.")
  if(!("forest" %in% names(sf_utm))) stop("Column 'forest' must be defined.")
  if(!("represented_area_m2" %in% names(sf_utm))) {
    if("represented_area_ha" %in% names(sf_utm)) {
      sf_utm$represented_area_m2 <- sf_utm$represented_area_ha*10000
    } else {
      stop("Please, supply column 'represented_area_m2' or 'represented_area_ha'")
    }
  }
  
  n <- nrow(sf_utm)
  
  forest_list_out <- vector("list", n)
  seed_production <- vector("list", n)
  
  # Seed local production
  for(i in 1:n) { 
    forest <- sf_utm$forest[[i]]
    if(!is.null(forest))  {
      seed_production[[i]] <- medfate::regeneration_seedproduction(forest, SpParams, local_control)
    }
  }
  
  # Neighbours and distances
  neighbours <- vector("list", n)
  distances <- vector("list", n)
  coords <- sf::st_coordinates(sf_utm)
  for(i in 1:n) {
    d_vec <- sqrt((coords[i,1] - coords[,1])^2 + (coords[i,2] - coords[,2])^2)
    sel_vec <- (d_vec < maximumDispersalDistance)
    sel_vec[i] <- FALSE
    distances[[i]] <- d_vec[sel_vec]
    neighbours[[i]] <- which(sel_vec)
  }
  
  # Dispersal and seed bank dynamics
  for(i in 1:n) { 
    forest <- sf_utm$forest[[i]]
    
    if(!is.null(forest)) {
      n_i <- neighbours[[i]]
      d_i <- distances[[i]]

      # Reduce seed bank according to longevity
      forest <- medfate::regeneration_seedmortality(forest, SpParams, minPercent)
      
      # Refill seed bank with new local seeds
      seeds_local <- seed_production[[i]]
      seeds_disp_dist <- species_parameter(seeds_local, SpParams, "DispersalDistance")
      seeds_disp_shape <- species_parameter(seeds_local, SpParams, "DispersalShape")
      n_seeds <- length(seeds_local)
      seeds_perc <- rep(NA,n_seeds)
      for(k in 1:n_seeds) {
        k_sum <- kernel_sum(i, n_i, d_i, sf_utm$represented_area_m2,
                            alpha = seeds_disp_dist[k], c = seeds_disp_shape[k])
        seeds_perc[k] <- 100*(kernel_fun(0, alpha = seeds_disp_dist[k], c = seeds_disp_shape[k])*sf_utm$represented_area_m2[i])/k_sum 
      }
      forest <- medfate::regeneration_seedrefill(forest, seeds_local, seeds_perc)

      # Refill seed bank with neighbor seeds
      for(j in 1:length(n_i)) {
        n_ij <- n_i[j]
        d_ij <- d_i[j]
        seeds_neigh <- seed_production[[n_ij]]
        if(!is.null(seeds_neigh)) {
          seeds_disp_dist <- species_parameter(seeds_neigh, SpParams, "DispersalDistance")
          seeds_disp_shape <- species_parameter(seeds_neigh, SpParams, "DispersalShape")
          n_seeds <- length(seeds_neigh)
          if(n_seeds > 0) {
            seeds_perc <- rep(NA,n_seeds)
            for(k in 1:n_seeds) {
              k_sum <- kernel_sum(i, n_i, d_i, sf_utm$represented_area_m2,
                                  alpha = seeds_disp_dist[k], c = seeds_disp_shape[k])
              seeds_perc[k] <- 100*(kernel_fun(d_ij, alpha = seeds_disp_dist[k], c = seeds_disp_shape[k])*sf_utm$represented_area_m2[n_ij])/k_sum 
            }
          }
          forest <- medfate::regeneration_seedrefill(forest, seeds_neigh, percent = seeds_perc)
        }
      }
      # Add seed rain from control
      if(!is.null(local_control$seedRain)) {
        forest <- regeneration_seedrefill(forest, local_control$seedRain)
      }
      # Filter species not reaching minimum percent
      forest$seedBank <- forest$seedBank[forest$seedBank$Percent >= minPercent, , drop = FALSE]
      
      # Sort by alphabetical species order
      if(nrow(forest$seedBank) > 0) forest$seedBank <- forest$seedBank[order(forest$seedBank$Species),]
      
      # Store forest with updated seed bank
      forest_list_out[[i]] <- forest
    }
  }
  return(forest_list_out)
}
