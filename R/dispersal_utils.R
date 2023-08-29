.extended_neighbours<-function(sf, order = 4) {
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
.neighbour_distances<- function(sf, neighbours, patchsize) {
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
.kernel<- function(r, alpha = 50, c = 2) 100*exp(-(r/alpha)^c)

