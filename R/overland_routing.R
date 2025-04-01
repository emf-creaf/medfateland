.raster_sf_matching<-function(r, sf) {
  if(!inherits(r, "SpatRaster")) cli::cli_abort("'r' has to be of class 'SpatRaster'.")
  if(!inherits(sf, "sf")) cli::cli_abort("'sf' has to be of class 'sf'.")
  if(sf::st_crs(sf)!=sf::st_crs(r)) cli::cli_abort("'sf' and 'r' need to have the same CRS.")
  sf_coords <- sf::st_coordinates(sf)
  sf2cell <- terra::cellFromXY(r, sf_coords)
  if(any(is.na(sf2cell))) cli::cli_abort("Some coordinates are outside the raster definition.")
  if(length(sf2cell)!=length(unique(sf2cell))) cli::cli_abort("Only one element in 'sf' is allowed per cell in 'r'.")
  nrastercells <- prod(dim(r)[1:2])
  cell2sf <- rep(NA, nrastercells)
  for(i in 1:length(sf2cell)) cell2sf[sf2cell[i]] <- i
  return(list(sf_coords = sf_coords, sf2cell = sf2cell, cell2sf = cell2sf))
}
.neighFun<-function(r, sf2cell, cell2sf) {
  ncol <- dim(r)[2]
  nrow <- dim(r)[1]
  queenNeigh <- vector("list", length(sf2cell))
  rowcol <- terra::rowColFromCell(r, sf2cell)
  m2 <- matrix(c(-1,-1,
                 0,-1,
                 1,-1,
                 -1,0,
                 1,0,
                 -1,1,
                 0,1,
                 1,1), nrow=8, ncol=2, byrow = TRUE)
  for(i in 1:length(sf2cell)) {
    m1 <-matrix(rep(rowcol[i,],8),
                nrow = 8, ncol=2, byrow = TRUE) 
    m <- m1 + m2
    m <- m[m[,1]>0 & m[,1]<=nrow & m[,2]>0 & m[,2]<=ncol, ]
    v <- cell2sf[terra::cellFromRowCol(r, m[,1], m[,2])]
    queenNeigh[[i]] <- sort(v[!is.na(v)])
  }
  return(queenNeigh)
}
.waterQFun <-function(waterRank, queenNeigh, coords, elevation, channel = NULL) {
  Q = vector("list", length(queenNeigh))
  qfun<-function(ri, xi, yi, zi, X, Y, Z, R) {
    n = length(X)
    Li = sqrt((X-xi)^2+(Y-yi)^2+(Z-zi)^2)
    dZ = zi-Z #dif. in elevation
    dZLi = dZ/Li 
    dZLi[dZ<=0] = 0 #Set to zero for neighbour cells at higher or equal elevation
    if(sum(dZLi)>0) return(dZLi/sum(dZLi))
    # If on a flat area, divide equally among neighbours that are processed later
    flat_later <- (dZ==0 & R > ri)
    # print(ri)
    # print(zi)
    # print(dZ)
    # print(R)
    # print(which(flat_later))
    if(sum(flat_later) > 0) {
      q <- rep(0, n)
      q[flat_later] <- 1/sum(flat_later)
      # print(q)
      return(q)
    }
    return(rep(0, n)) #If in a hole or no neighbors return as outlet
  }
  for(i in 1:length(queenNeigh)) {
    wne = queenNeigh[[i]]
    Q[[i]] = qfun(ri = waterRank[i], xi = coords[i,1], yi=coords[i,2],zi = elevation[i],
                  X = coords[wne,1], Y = coords[wne,2], Z = elevation[wne], R = waterRank[wne])
  }  
  if(!is.null(channel)) { # Channel cells to not drain to other cells via runoff
    for(i in which(as.logical(channel))) {
      wne <- queenNeigh[[i]]
      Q[[i]] <- rep(0, length(wne))
    }
  }
  return(Q)
}

.directDraining <- function(queenNeigh, waterQ, target) {
  ne <- queenNeigh[[target]]
  direct <- numeric(0)
  for(i in ne) {
    p <- which(queenNeigh[[i]]==target)
    if(waterQ[[i]][p] > 0.0) {
      direct <- c(direct, i)
    }
  }
  return(direct)
}
.findDrainageBasin<-function(queenNeigh, waterQ, target) {

  drainage_new <- .directDraining(queenNeigh, waterQ, target)
  basin <- c(target, drainage_new)
  to_check <- drainage_new
  while(length(to_check)>0) {
    target <- to_check[1]
    drainage <- .directDraining(queenNeigh, waterQ, target)
    drainage_new  <- drainage[!(drainage %in% basin)]
    basin <- c(basin, drainage_new)
    to_check <- c(to_check[-1], drainage_new)
  }
  return(sort(basin))
}

.findLowOverlapDrainageBasins <- function(channel, outlet, queenNeigh, waterQ, max_overlap = 0.25) {
  out_ch <- which(channel | outlet)
  n <- length(channel)
  db <- vector("list", length(out_ch))
  names(db) <- out_ch
  for(i in 1:length(out_ch)) {
    db[[i]] <- .findDrainageBasin(queenNeigh, waterQ, out_ch[i])
  }
  checked <- rep(FALSE, length(db))
  while(any(!checked)) {
    # Determine next drainage basin to be checked
    db_size <- unlist(lapply(db, length))
    smallest_size <- min(db_size[!checked])
    to_be_checked <- which((db_size == smallest_size) & (!checked))[1]
    # cat(paste0("Checking ", to_be_checked, " out of ", length(db),"\n"))
    # Determine if it has to be merged
    merge_with <- NA
    maxoverfound <- 0
    for(j in ((1:length(db))[-to_be_checked])) {
      overlap <- sum((db[[to_be_checked]] %in% db[[j]]))/length(db[[to_be_checked]])
      if(overlap > maxoverfound) {
        if(overlap > max_overlap) {
          maxoverfound <- overlap
          merge_with <- j
        }
      }
    }  
    # If it has to be merged, perform merging and reset "checked" and "db"
    if(!is.na(merge_with)) {
      db[[merge_with]] <- sort(c(db[[to_be_checked]], db[[merge_with]]))
      new_name <- paste(names(db)[to_be_checked], names(db)[merge_with], sep = "_")
      names(db)[merge_with] <- new_name
      db <- db[-to_be_checked]
      checked <- rep(FALSE, length(db))
    } else {
      checked[to_be_checked] <- TRUE
    }
  }
  # Identify overlapping cells and keep largest drainage value
  # cat(paste0("Identifying cell membership to basin \n"))
  cells_to_basins <- vector("list", n)
  for(i in 1:n) {
    v <- numeric(0)
    for(j in 1:length(db)) {
      if(i %in% db[[j]]) {
        v <- c(v, j)
      }
    }
    cells_to_basins[[i]] <- v
  }
  # cat(paste0("Checking cells \n"))
  to_be_checked <- 1:n
  num_border_cells <- 0
  while(length(to_be_checked)>0) {
    target <- to_be_checked[1]
    target_basins <- cells_to_basins[[target]]
    if(length(target_basins)==1) {
      # If only one basin, remove from vector
      to_be_checked <- to_be_checked[-1]
    } else {
      ne <- queenNeigh[[target]]
      q <- waterQ[[target]]
      Q_basin <- rep(0, length(target_basins))
      for(i in 1:length(ne)) {
        if(q[i]>0.0) {
          ne_basin  <- cells_to_basins[[ne[i]]]
          for(j in 1:length(target_basins)) {
            if(target_basins[j] %in% ne_basin) Q_basin[j] <- Q_basin[j] + q[i]
          }
        }
      }
      # print(Q_basin)
      Q_max <- max(Q_basin)
      maxQ_basins <- which(Q_basin == Q_max)
      if(length(maxQ_basins)==1) {
        num_border_cells <- num_border_cells + 1
        true_basin <- target_basins[maxQ_basins]
        false_basins <- target_basins[-maxQ_basins]
        # cat(paste0("Moving cell: ", target, " to basin ", true_basin,"\n"))
        # update list of membership
        cells_to_basins[[target]] <- true_basin
        # remove cell from false drainage basins
        for(j in false_basins) {
          db_j <- db[[j]]
          db_j <- db_j[db_j != target]
          db[[j]] <- db_j
        }
        # remove from vector to be checked
        to_be_checked <- to_be_checked[-1]
      } else {
        # put in the back of the list
        to_be_checked <- c(to_be_checked[-1], to_be_checked[1])
      }
    }
  }
  # cat(paste0("Number of cells re-arranged: ", num_border_cells,"\n"))

  # Build drainage basin vector
  drainage_basins <- rep(NA, length(channel))
  for(i in 1:length(db)) {
    drainage_basins[db[[i]]] <- i
  }
  return(drainage_basins)
}
.overland_routing_inner<-function(r, sf, raster_matching, channel_flow_speed, patchsize,
                                  subwatersheds = FALSE, max_overlap = 0.2) {
  if(!all(c("elevation") %in% names(sf))) stop("Column 'elevation' must be defined in 'sf'.")
  waterOrder <- order(sf$elevation, decreasing = TRUE)
  waterRank <- order(waterOrder)
  nCells <- nrow(sf)
  
  isInputBacklog <- ("outlet_backlog" %in% names(sf))
  
  out <- sf::st_sf(geometry=sf::st_geometry(sf))
  out$elevation <- sf$elevation
  out$waterRank <- waterRank
  out$waterOrder <- waterOrder
  out$queenNeigh <- .neighFun(r, raster_matching$sf2cell, raster_matching$cell2sf)
  if("channel" %in% names(sf)) {
    out$waterQ <- .waterQFun(out$waterRank, out$queenNeigh,  raster_matching$sf_coords, sf$elevation, sf$channel)
    out$channel <- sf$channel
    out$outlet <- rep(FALSE, nCells)
    # Define outlets as channel cells in the domain limits and not having a neighbor channel at lower elevation
    for(i in 1:nCells) {
      if(sf$channel[i]) {
        wne <- out$queenNeigh[[i]]
        elev_i <- sf$elevation[i]
        channel_ne <- sf$channel[wne]
        elev_ne <- sf$elevation[wne][channel_ne]
        if((length(wne) < 8)) {
          if(length(elev_ne)==0) { # No channel neighbors
            out$outlet[i] <- TRUE
          } else {
            if(all(elev_ne >= elev_i)) {
              out$outlet[i] <- TRUE
            }
          }
        }
      } else if(sum(out$waterQ[[i]])==0){ # Outlet cells may be outside the channel (e.g. holes in topography)
        out$outlet[i] <- TRUE
      }
    }
    out$target_outlet <- rep(NA, nCells)
    out$target_outlet[out$outlet] <- which(out$outlet)
    out$distance_to_outlet <- rep(NA, nCells)
    out$distance_to_outlet[out$outlet] <- 0
    out$distance_to_outlet[sf$channel] <- 0
    to_be_processed <- which(out$outlet)
    processed <- numeric(0)
    while(length(to_be_processed)>0) {
      origin <- to_be_processed[1] # get first
      wne <- out$queenNeigh[[origin]]
      wne <- wne[wne %in% which(sf$channel)] # restrict to channel cells
      wne <- wne[!(wne %in% processed)] # avoid cells already processed
      wne <- wne[!(wne %in% which(out$outlet))] # avoid outlets
      if(length(wne)>0) {
        out$target_outlet[wne] <- out$target_outlet[origin]
        out$distance_to_outlet[wne] <- out$distance_to_outlet[origin]+1
        to_be_processed <- c(to_be_processed, wne)
      }
      processed <- c(processed, origin)
      to_be_processed <- to_be_processed[to_be_processed!=origin] # Remove processed to avoid infinite loop
    }
    out$outlet_backlog <- vector("list", nCells)
    for(i in 1:nCells) {
      if(out$outlet[i]) {
        cell_dist <- out$distance_to_outlet[out$target_outlet == i]
        max_dist <- max(cell_dist[!is.na(cell_dist)])
        max_ndays <- max(1, ceiling((max_dist*sqrt(patchsize)) / (3600*24*channel_flow_speed)))
        out$outlet_backlog[[i]] <- rep(0, max_ndays)
        if(isInputBacklog) {
          sf_bl <- sf$outlet_backlog[[i]]
          if(length(sf_bl)==max_ndays) {
            out$outlet_backlog[[i]] <- sf_bl
          }
        }
      } 
    }
  } else {
    out$waterQ <- .waterQFun(out$waterRank, out$queenNeigh,  raster_matching$sf_coords, sf$elevation)
    out$channel <- rep(FALSE, nCells)
    out$outlet <- (unlist(lapply(out$waterQ, sum))==0)
    out$target_outlet <- rep(NA, nCells)
    out$distance_to_outlet <- rep(NA, nCells)
    out$outlet_backlog <- vector("list", nCells)
  }
  if(subwatersheds) {
    out$subwatershed <- .findLowOverlapDrainageBasins(out$channel, out$outlet, out$queenNeigh, out$waterQ,
                                                      max_overlap = max_overlap)
    # Force that overland flow goes to the same watershed
    for(i in 1:nCells) { 
      ni <- out$queenNeigh[[i]]
      qi <- out$waterQ[[i]]
      ni_sub <- out$subwatershed[ni]
      i_sub <- out$subwatershed[i]
      qi[ni_sub!=i_sub] <- 0
      qi <- qi/sum(qi, na.rm = TRUE)
      out$waterQ[[i]] <- qi
    }
  }
  # Check
  for(i in 1:nCells) { 
    ni <- out$queenNeigh[[i]]
    qi <- out$waterQ[[i]]
    if(max(ni)>nCells || min(ni) < 1) {
      cli::cli_abort(paste0("Cell ", i, " pointed to non-existing neighbors"))
    }
    if(length(qi) != length(ni)) {
      cli::cli_abort(paste0("Cell ", i, " has different number of neighbors in 'waterQ' than 'queenNeigh'"))
    }
    if((!out$outlet[i]) && (!out$channel[i])) {
      if(abs(sum(qi) - 1) > 0.0001) {
        cli::cli_abort(paste0("'waterQ' values for cell ", i, " do not add up to 1"))
      }
    }
  }
  return(sf::st_as_sf(tibble::as_tibble(out)))
}

.get_backlog_sum <- function(sf_routing) {
  nCells  <- nrow(sf_routing)
  backlog_sum <- 0
  for(i in 1:nCells) {
    backlog_i <- sf_routing$outlet_backlog[[i]]
    if(!is.null(backlog_i)) {
      backlog_sum <- backlog_sum + sum(backlog_i, na.rm = TRUE) 
    }
  }
  return(backlog_sum)
}

#' Overland routing for TETIS sub-model
#' 
#' Determines overland routing given a raster definition and a set of target locations for watershed simulations. If channel is supplied,
#' it also determines channel routing.
#'
#' @param r An object of class \code{\link[terra]{SpatRaster}}, defining the raster topology.
#' @param sf An object of class \code{\link[sf]{sf}} with the following columns:
#'   \itemize{
#'     \item{\code{geometry}: Spatial point geometry corresponding to cell centers.}
#'     \item{\code{elevation}: Elevation above sea level (in m).}
#'     \item{\code{channel}: An optional logical (or binary) vector indicating cells corresponding to river channel.}
#'    }
#' @param channel_flow_speed Average flow speed in the channel (in m/s).
#' @param subwatersheds A boolean flag to define watershed subunits.
#' @param max_overlap Maximum proportion of overlapping cells for watershed subunits to be considered independent. Lower values will normally produce larger subunits.
#' 
#' @returns  An object of class \code{\link[sf]{sf}} describing overland routing parameters and outlet cells:
#'     \itemize{
#'       \item{\code{geometry}: Spatial point geometry corresponding to cell centers.}
#'       \item{\code{elevation}: Elevation above sea level (in m).}
#'       \item{\code{waterRank}: Ranked elevation in decreasing order.}
#'       \item{\code{waterOrder}: A vector with the cell's processing order for overland routing (based on elevation). First value corresponds to the row index of the first processed cell, second value corresponds to the row index of the second processed cell and so forth.}
#'       \item{\code{queenNeigh}: A list where, for each cell, a vector gives the identity of neighbours (up to eight).}
#'       \item{\code{waterQ}: A list where, for each cell, a vector gives the proportion of overland flow to each neighbour.}
#'       \item{\code{channel}: A logical vector indicating channel cells.}
#'       \item{\code{outlet}: A logical vector indicating outlet cells.}
#'       \item{\code{target_outlet}: Index of the outlet cell to which the channel leads  (\code{NA} for non-channel cells).}
#'       \item{\code{distance_to_outlet}: Distance to the target outlet in number of cells (\code{NA} for non-channel cells).}
#'       \item{\code{outlet_backlog}: For each outlet, a backlog vector of watershed export (\code{NA} for non-outlet cells).}
#'       \item{\code{subwatershed}: Integer vector indicating watershed subunits (if \code{subwatersheds = TRUE}).}
#'     } 
#'     
#' @details
#' If \code{channel} is not supplied, then cells where all neighbors are at higher elevation are considered outlet cells.
#' If \code{channel} is supplied, then outlets are channel cells in the domain limits and not having a neighbor channel at lower elevation. In this case,
#' model simulations will include channel routing towards outlet cells.
#' 
#' If defining watershed subunits is requested (i.e. if \code{subwatersheds = TRUE}), subunits are defined first by determining the area draining to each channel or outlet cell. Then, those areas are progressively merged if one is nested into the other or when the proportion of overlapping cells is lower than 
#' a pre-specified threshold (i.e. larger than \code{max_overlap}). A given cell cannot belong to more than one subunit. Therefore, the overlap between the final subwatersheds is eliminated by deciding the main subwatershed for each cell, the proportion of overland flow to neighbors is modified for cells in located in subunit boundaries.
#' 
#' @export
#'
#' @examples
#' # Load example watershed data
#' data("example_watershed")
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
#' # Generate overland routing
#' or <- overland_routing(r, example_watershed)
#' 
#' # Plot elevation
#' plot(or["elevation"])
#' 
#' # Rank (decreasing elevation) for processing
#' plot(or["waterRank"])
#' 
#' # Plot outlet cells
#' plot(or["outlet"])
#' 
#' # Define 4-cell channel
#' example_watershed$channel <- FALSE
#' example_watershed$channel[c(6, 11, 12, 20)] <- TRUE
#' 
#' # Generate overland and channel routing
#' or_channel <- overland_routing(r, example_watershed)
#' 
#' # Plot outlet and distance to outlet
#' plot(or_channel["outlet"])
#' plot(or_channel["distance_to_outlet"])
#' 
#' @name overland_routing
overland_routing<-function(r, sf, 
                           channel_flow_speed = 1.0,
                           subwatersheds = FALSE, 
                           max_overlap = 0.2) {
  represented_area_m2 <- as.vector(terra::values(terra::cellSize(r)))
  patchsize <- mean(represented_area_m2, na.rm=TRUE)
  raster_matching <- .raster_sf_matching(r, sf)
  return(.overland_routing_inner(r, sf, raster_matching, 
                                 channel_flow_speed = channel_flow_speed, 
                                 patchsize = patchsize, 
                                 subwatersheds = subwatersheds,
                                 max_overlap = max_overlap))
}
#' @rdname overland_routing
#' @export
cell_neighbors<-function(r, sf) {
  if(!inherits(sf, "sf"))  cli::cli_abort("Object 'sf' has to be of class 'sf'")
  if(!inherits(r, "SpatRaster")) cli::cli_abort("'r' has to be of class 'SpatRaster'.")
  if(sf::st_crs(sf)!=sf::st_crs(r)) cli::cli_abort("'sf' and 'r' need to have the same CRS.")
  sf_coords <- sf::st_coordinates(sf)
  sf2cell <- terra::cellFromXY(r, sf_coords)
  if(any(is.na(sf2cell))) cli::cli_abort("Some coordinates are outside the raster definition.")
  if(length(sf2cell)!=length(unique(sf2cell))) cli::cli_abort("Only one element in 'sf' is allowed per cell in 'r'.")
  nrastercells <- prod(dim(r)[1:2])
  cell2sf <- rep(NA, nrastercells)
  for(i in 1:length(sf2cell)) cell2sf[sf2cell[i]] <- i
  return(.neighFun(r, sf2cell, cell2sf))
}
