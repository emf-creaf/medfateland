buildWatershedTopography<-function(boundaries, grid, topo, proj4string, merge = FALSE) {
  b = as(boundaries, "SpatialPolygons")
  cat(paste("Number of watersheds: ", length(b),"\n"))
  cat(paste("1. Building grid coordinates.\n"))
  sp_fullgrid=SpatialPoints(coordinates(grid), proj4string)
  cat(paste("2. Intersecting with boundaries.\n"))
  overboundaries=over(spTransform(sp_fullgrid, b@proj4string), b)
  cat(paste("3. Building SpatialPixelsTopography object(s).\n"))
  if(!merge) {
    l = vector("list", length(b))
    for(i in 1:length(b)) {
      sel = (overboundaries==i)
      sel[is.na(sel)] = FALSE
      sp_cells = sp_fullgrid[sel]
      sp2 = spTransform(sp_cells, topo@proj4string)
      gi = getGridIndex(sp2@coords, topo@grid)
      topo_df = topo@data[gi,]
      spdf_topo=SpatialPixelsTopography(sp_cells, topo_df$elevation, topo_df$slope, topo_df$aspect)
      l[[i]] = spdf_topo
    }
    if(length(l)==1) l = l[[1]]
  } else {
    sel = !is.na(overboundaries)
    sp_cells = sp_fullgrid[sel]
    sp2 = spTransform(sp_cells, topo@proj4string)
    gi = getGridIndex(sp2@coords, topo@grid)
    topo_df = topo@data[gi,]
    l=SpatialPixelsTopography(sp_cells, topo_df$elevation, topo_df$slope, topo_df$aspect)
  }
  return(l)
}