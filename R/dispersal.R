kernel_fun<- function(r, alpha, c) {
  (c*exp(-(r/alpha)^c))/(2*pi*(alpha^2)*gamma(2/c))
}
kernel_int <- function(r, alpha, c) {
  #Define suitable steps for integration
  inc <- min(alpha/10, 0.1)
  step <- inc
  dist_vec <- step
  while(dist_vec[length(dist_vec)] < r) {
    step <- min(step*2,r/100)
    dist_vec <- c(dist_vec, dist_vec[length(dist_vec)]+ step)
  }
  dist_vec[length(dist_vec)] <- r
  #Integration
  k_sum <- kernel_fun(0, alpha = alpha, c = c)*pi*(dist_vec[1]^2)
  for(j in 1:length(dist_vec)) {
    d_ij <- dist_vec[j]
    if(j==length(dist_vec)) d_next <- 2*dist_vec[j] - dist_vec[j-1]
    else d_next <- dist_vec[j+1]
    k_ij <- kernel_fun((dist_vec[j]+d_next)/2, alpha = alpha, c = c)*pi*(d_next^2 - d_ij^2)
    if(!is.na(k_ij)){
      k_sum <- k_sum + k_ij 
    }
  }
  return(min(k_sum, 1.0))
}

# kernel_percentile <- function(alpha, c, prob = 0.5) {
#   k_sum <- kernel_fun(0, alpha = alpha, c = c)*pi*(dist_vec[1]^2)
#   for(j in 1:length(dist_vec)) {
#     d_ij <- dist_vec[j]
#     if(j==length(dist_vec)) d_next <- 2*dist_vec[j] - dist_vec[j-1]
#     else d_next <- dist_vec[j+1]
#     k_ij <- kernel_fun((dist_vec[j]+d_next)/2, alpha = alpha, c = c)*pi*(d_next^2 - d_ij^2)
#     if(!is.na(k_ij)){
#       k_sum <- k_sum + k_ij 
#     }
#     if(k_sum>= prob) return(d_ij)
#   }
#   return(NA)
# }
# kernel_plot <- function(alpha, c, log = "xy", xmax = 100) {
#   inc <- alpha/10
#   step <- inc
#   dist_vec <- step
#   while(dist_vec[length(dist_vec)] < xmax) {
#     step <- min(step*2,xmax/100)
#     dist_vec <- c(dist_vec, dist_vec[length(dist_vec)]+ step)
#   }
#   plot(dist_vec, kernel_fun(dist_vec, alpha,c), type = "l", log = log)
# }

#' Seed production, dispersal and seed bank dynamics
#' 
#' Simulates seed bank mortality, seed production and dispersal among stands
#'
#' @param sf An object of class \code{\link[sf]{sf}} using a UTM projection (to measure distances in m) and with the following columns:
#'   \itemize{
#'     \item{\code{geometry}: Spatial geometry.}
#'     \item{\code{forest}: Objects of class \code{\link[medfate]{forest}}.}
#'   }
#' @param SpParams A data frame with species parameters (see \code{\link[medfate]{SpParamsMED}}).
#' @param local_control A list of control parameters (see \code{\link[medfate]{defaultControl}})
#' @param distance_step Distance step in meters.
#' @param maximum_dispersal_distance Maximum dispersal distance in meters.
#' @param min_percent A minimum percent of seed bank to retain entry in \code{seedBank} element of \code{forest}.
#' @param stochastic_resampling A flag to indicate that stochastic resampling of stands is performed.
#' @param progress Boolean flag to display progress information.
#' 
#' @details
#' The input 'sf' object has to be in a Universal Transverse Mercator (UTM) coordinate system (or any other projection using meters as length unit)
#' for appropriate function behavior.
#'
#' Dispersal kernel follows Clark et al. (1999) and potential seed donors (neighbors) are defined up to a given grid distance order. 
#' A maximum value of 100% seed bank refill is attained for species with seed production in all seed donors and the local cell.
#' 
#' @return A list with forest objects (for wildland cover type) containing a modified seed bank
#' 
#' @author 
#' Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF.
#' 
#' Roberto Molowny-Horas, CREAF.
#' 
#' @references 
#' Clark et al. (1999). Seed dispersal near and far: Patterns across temperate and tropical forests. Ecology 80(5):1475-1494
#' 
#' @seealso \code{\link{fordyn_land}}
#' 
#' @export
#'
#' @examples
#' \donttest{
#' data(example_watershed)
#' data(SpParamsMED)
#' 
#' # Transform to UTM31
#' example_watershed_utm31 <- sf::st_transform(example_watershed, crs = 32631)
#'
#' # Estimate seed production and dispersal over the watershed
#' seedbank_list <- dispersal(example_watershed_utm31, SpParamsMED)
#' 
#' seedbank_list[[1]]
#' 
#' # Transform to UTM31
#' example_ifn_utm31 <- sf::st_transform(example_ifn, crs = 32631)
#' 
#' # Estimate seed production and dispersal over the set of forest inventory plots
#' seedbank_list <- dispersal(example_ifn_utm31, SpParamsMED)
#' 
#' seedbank_list[[1]]
#' }
dispersal <- function(sf, SpParams, 
                      local_control = medfate::defaultControl(), 
                      distance_step = 25, maximum_dispersal_distance = 3000, min_percent = 1, 
                      stochastic_resampling = FALSE, progress = TRUE) {

  if(!inherits(sf, "sf")) stop("'sf' has to be of class 'sf'.")
  if(!("forest" %in% names(sf))) stop("Column 'forest' must be defined.")

  n <- nrow(sf)
  
  seedbank_list <- vector("list", n)
  seed_production <- vector("list", n)
  
  # Reduce seed bank according to longevity
  if(progress) cli::cli_progress_step(paste0("Seed bank mortality"))
  for(i in 1:n) { 
    forest <- sf$forest[[i]]
    if(!is.null(forest) && inherits(forest, "forest"))  {
      seedBank <- forest$seedBank
      seedBank <- medfate::regeneration_seedmortality(seedBank, SpParams, min_percent)
      seedbank_list[[i]] <- seedBank
    }
  }
  # Seed local production
  if(progress) cli::cli_progress_step(paste0("Seed production"))
  for(i in 1:n) { 
    forest <- sf$forest[[i]]
    if(!is.null(forest) && inherits(forest, "forest"))  {
      seed_production[[i]] <- medfate::regeneration_seedproduction(forest, SpParams, local_control)
    } else {
      seed_production[[i]] <- character(0)
    }
  }
  
  spp <- sort(unique(unlist(seed_production)))
  
  # Neighbours and distances
  coords <- sf::st_coordinates(sf)
  dist_vec <- seq(distance_step, maximum_dispersal_distance, by = distance_step)
  if(stochastic_resampling) {
    if(progress) cli::cli_progress_step(paste0("Neighbor stochastic resampling"))
    neigh_matrix <- matrix(nrow = n, ncol = length(dist_vec))
    for(i in 1:n) {
      di_vec <- sqrt((coords[i,1] - coords[,1])^2 + (coords[i,2] - coords[,2])^2)
      for(j in 1:length(dist_vec)) {
        d_ij <- abs(dist_vec[j] - di_vec)
        if(any(d_ij==0)) {
          probs_ij <- rep(0, length(d_ij))
          probs_ij[d_ij==0] <- 1/sum(d_ij==0)
        } else {
          probs_ij <- 1/d_ij
        }
        neigh_matrix[i,j] <- sample(length(d_ij), 1, prob = probs_ij)
      }
    }
  } 
  
  # Kernel parameters
  if(progress) cli::cli_progress_step(paste0("Kernel estimation"))
  kernel_params <- data.frame(Species = spp)
  kernel_params$DispersalDistance  <- species_parameter(spp, SpParams, "DispersalDistance")
  kernel_params$DispersalShape  <- species_parameter(spp, SpParams, "DispersalShape")
  
  # Kernel F values
  F_kernel <- matrix(nrow = length(spp), ncol = length(dist_vec))
  F_kernel_inc <- matrix(nrow = length(spp), ncol = length(dist_vec))
  for(i in 1:length(spp)) {
    for(j in 1:length(dist_vec)) {
      F_kernel[i,j] <- kernel_int(dist_vec[j], kernel_params$DispersalDistance[i], kernel_params$DispersalShape[i])
    }
  }
  F_kernel_inc[,1] <- F_kernel[,1]
  F_kernel_inc[,-1] <-  F_kernel[,2:ncol(F_kernel)] - F_kernel[,1:(ncol(F_kernel)-1)]
    
  # Dispersal and seed bank dynamics
  if(progress) cli::cli_progress_step(paste0("Seed dispersal"))
  for(i in 1:n) { 
    seedBank <- seedbank_list[[i]]
    if(!is.null(seedBank)) {
      seeds_perc <- rep(0, length(spp))
      if(stochastic_resampling) {
        for(j in 1:length(dist_vec)) {
          nij <- neigh_matrix[i,j]
          seeds_neigh<- seed_production[[nij]]
          n_seeds <- length(seeds_neigh)
          if(n_seeds > 0) {
            for(k in 1:n_seeds) {
              i_spp <- which(spp==seeds_neigh[[k]])
              seeds_perc[i_spp] <- seeds_perc[i_spp] + 100*F_kernel_inc[i_spp, j]
            }
          }
        }
      } else {
        di_vec <- sqrt((coords[i,1] - coords[,1])^2 + (coords[i,2] - coords[,2])^2)
        for(j in 1:length(dist_vec)) {
          d_ij <- abs(dist_vec[j] - di_vec)
          if(any(d_ij==0)) {
            probs_ij <- rep(0, length(d_ij))
            probs_ij[d_ij==0] <- 1/sum(d_ij==0)
          } else {
            probs_ij <- 1/d_ij
          }
          probs_ij <- probs_ij/sum(probs_ij) # Normalize to sum 1
          for(k in 1:n) {
            seeds_neigh<- seed_production[[k]]
            n_seeds <- length(seeds_neigh)
            if(n_seeds > 0) {
              for(l in 1:n_seeds) {
                i_spp <- which(spp==seeds_neigh[[l]])
                seeds_perc[i_spp] <- seeds_perc[i_spp] + 100*probs_ij[k]*F_kernel_inc[i_spp, j]
              }
            }
          }
        }
      }
      # Add dispersed seeds to seed bank
      seedBank <- medfate::regeneration_seedrefill(seedBank, spp, seeds_perc)
      
      # Add seed rain from control
      if(!is.null(local_control$seedRain)) {
        seedBank <- regeneration_seedrefill(seedBank, local_control$seedRain)
      }
      
      # Filter species not reaching minimum percent
      seedBank <- seedBank[seedBank$Percent >= min_percent, , drop = FALSE]
      
      # Sort by alphabetical species order
      if(nrow(seedBank) > 0) seedBank <- seedBank[order(seedBank$Species),]
      row.names(seedBank) <- NULL
      
      # Store updated seed bank
      seedbank_list[[i]] <- seedBank
    }
  }
  if(progress) cli::cli_progress_done()
  return(seedbank_list)
}