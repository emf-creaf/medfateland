#Example watershed
examplewatershed = readRDS(paste0("D:/Datasets/Hydrology/Products/DistributedWatersheds/CanVila_watershed_100m.rds")) 
usethis::use_data(examplewatershed, overwrite = T)

#Example SpatialPointsLandscape
ifn3 = readRDS("D:/Datasets/IFN/Products/IFN3/Rdata/forestlist_roots_IFN3_Catalunya.rds")
ifn3_soils = readRDS("D:/Datasets/IFN/Products/Soils/soillist_cat_ifn23_unique_mod.rds")
ifn3_topo = readRDS("D:/Datasets/IFN/Products/Topography/IFN23_spt_cat_unique_ETRS89H31.rds")
spt = ifn3_topo[1001:1030,] #30 IFN stands
codes = rownames(spt@coords)
spt@coords = 1000*round(spt@coords/1000) #Decrease resolution to 1km
lct = rep("wildland", length(codes))
examplepointslandscape = SpatialPointsLandscape(spt, lct, ifn3[codes], ifn3_soils[codes])
usethis::use_data(examplepointslandscape, overwrite = T)
