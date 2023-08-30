extended_neighbours<-function(sf, order = 4) {
  neighbours<- NULL
  if(order==1) {
    neighbours <- sf$queenNeigh
  } else if(order > 1) {
    neighbours <- extended_neighbours(sf, order = order - 1)
    for(i in 1:length(neighbours)) {
      n_i <- neighbours[[i]]
      extension <- integer(0)
      for(j in n_i) {
        extension <- unique(c(extension, sf$queenNeigh[[j]]))
      }
      neighbours[[i]] <- sort(unique(c(n_i, extension)))
    }
    # Exclude himself
    for(i in 1:length(neighbours)) {
      n_i <- neighbours[[i]]
      if(i %in% n_i) {
        neighbours[[i]] <- n_i[n_i!=i]
      }
    }
  }
  return(neighbours)
}
neighbour_distances<- function(sf, neighbours, patchsize) {
  distances <- vector("list", length(neighbours))
  coords <- sf::st_coordinates(sf)
  min_distance <- 999999.9
  for(i in 1:length(neighbours)) {
    n_i<-neighbours[[i]]
    d_i<-rep(NA, length(n_i))
    for(j in 1:length(n_i)) {
      d_i[j] <- sqrt((coords[i,1] - coords[n_i[j],1])^2 + (coords[i,2] - coords[n_i[j],2])^2) 
    }
    distances[[i]] <- d_i
    min_d <- min(d_i, na.rm=TRUE)
    if(min_d < min_distance) min_distance <- min_d
  }
  # Translate distances to patchsize (in m)
  for(i in 1:length(distances)) {
    distances[[i]] <- (sqrt(patchsize)/min_distance)*distances[[i]]
  }
  return(distances)
}
kernel_fun<- function(r, alpha = 100, c = 2) exp(-(r/alpha)^c)
kernel_sums <- function(neighbours, distances, areas) {
  k_sums <- numeric(length(neighbours))
  for(i in 1:length(neighbours)) {
    n_i <- neighbours[[i]]
    d_i <- distances[[i]]
    k_sums[i] <- kernel_fun(0)*areas[i] # Local value
    for(j in 1:length(n_i)) {
      n_ij <- n_i[j]
      d_ij <- d_i[j]
      k_ij <- kernel_fun(d_ij)*areas[n_ij] #Kernel parameters missing
      if(!is.na(k_ij)){
        k_sums[i] <- k_sums[i] + k_ij 
      }
    }
  }
  return(k_sums)
}

#' Seed production and dispersal
#' 
#' Simulates seed bank mortality, seed production and dispersal among grid cells in a watershed
#'
#' @param sf An object of class \code{\link{sf}} with the following columns:
#'   \itemize{
#'     \item{\code{geometry}: Spatial geometry.}
#'     \item{\code{land_cover_type}: Land cover type of each grid cell (values should be 'wildland', 'agriculture', 'rock', 'artificial' or 'water').}
#'     \item{\code{forest}: Objects of class \code{\link{forest}}.}
#'     \item{\code{queenNeigh}: A list of integers identifying the (up to 8) queen neighbors, for each cell.}
#'     \item{\code{represented_area}: Area represented by each cell (in m2).}
#'   }
#' @param SpParams A data frame with species parameters (see \code{\link{SpParamsMED}}).
#' @param local_control A list of control parameters (see \code{\link{defaultControl}})
#' @param order Order of neighbors in a grid
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
#' forest_list <- dispersal(example_watershed, SpParamsMED)
#' 
#' forest_list[[1]]$seedBank
dispersal <- function(sf, SpParams, 
                     local_control = medfate::defaultControl(), 
                     order = 4, minPercent = 1) {

  n <- nrow(sf)
  patchsize <- mean(sf$represented_area, na.rm=TRUE)
  
  forest_list_out <- vector("list", n)
  seed_production <- vector("list", n)
  
  # Seed local production
  for(i in 1:n) { 
    if(sf$land_cover_type[i] == "wildland")  {
      forest <- sf$forest[[i]]
      seed_production[[i]] <- medfate::regeneration_seedproduction(forest, SpParams, local_control)
    }
  }
  
  # Neighbours and distances
  neighbours <- extended_neighbours(sf, order = order)
  distances <- neighbour_distances(sf, neighbours, patchsize)
  # Kernel sums
  k_sums <- kernel_sums(neighbours, distances, sf$represented_area)
  
  # Dispersal and seed bank dynamics
  for(i in 1:n) { 
    if(sf$land_cover_type[i] == "wildland")  {
      forest <- sf$forest[[i]]
      
      # Reduce seed bank according to longevity
      forest <- medfate::regeneration_seedmortality(forest, SpParams, minPercent)
      
      # Refill seed bank with new local seeds
      seeds_local <- seed_production[[i]]
      n_seeds <- length(seeds_local)
      seeds_perc <- rep(NA,n_seeds)
      for(k in 1:n_seeds) {
        #Kernel parameters missing
        seeds_perc[k] <- 100*(kernel_fun(0)*sf$represented_area[i])/k_sums[i] 
      }
      forest <- medfate::regeneration_seedrefill(forest, seeds_local, seeds_perc)

      # Refill seed bank with neighbour seeds
      n_i <- neighbours[[i]]
      d_i <- distances[[i]]
      for(j in 1:length(n_i)) {
        n_ij <- n_i[j]
        d_ij <- d_i[j]
        if(sf$land_cover_type[n_ij] == "wildland") {
          seeds_neigh <- seed_production[[n_ij]]
          n_seeds <- length(seeds_neigh)
          if(n_seeds > 0) {
            seeds_perc <- rep(NA,n_seeds)
            for(k in 1:n_seeds) {
              seeds_perc[k] <- 100*(kernel_fun(d_ij)*sf$represented_area[n_ij])/k_sums[i] #Kernel parameters missing
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
