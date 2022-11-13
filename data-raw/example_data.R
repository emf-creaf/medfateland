#Example watershed
examplewatershed = readRDS(paste0("/home/miquel/OneDrive/Datasets/Hydrology/Products/DistributedWatersheds/CanVila_watershed_100m.rds")) 
crs <- CRS(SRS_string = "EPSG:32631")
comment(crs)<-gsub("°", "º", comment(crs)) # Replace non-ASCII character
examplewatershed@proj4string = crs
usethis::use_data(examplewatershed, overwrite = T)

#Example SpatialPointsLandscape
ifn3 = readRDS("/home/miquel/OneDrive/EMFProducts/MEDFATE_Initialisation/IFN/Products/IFN3/Rdata/forestlist_roots_IFN3_Catalunya.rds")
ifn3_topo = readRDS("/home/miquel/OneDrive/EMFProducts/MEDFATE_Initialisation/IFN/Products/Topography/IFN23_spt_cat_unique_ETRS89H31.rds")
ifn3_soils = readRDS("/home/miquel/OneDrive/EMFProducts/MEDFATE_Initialisation/IFN/Products/Soils/soillist_cat_ifn23_unique_mod.rds")
spt = ifn3_topo[1001:1100,] #40 IFN stands
codes = rownames(spt@coords)
spt@coords = 1000*round(spt@coords/1000) #Decrease resolution to 1km
lct = rep("wildland", length(codes))
names(lct) = codes
ifn3 = ifn3[codes]
for(i in 1:length(ifn3)) {
  ifn3[[i]]$ID = NULL
  ifn3[[i]]$patchsize = NULL
  ifn3[[i]]$treeData = ifn3[[i]]$treeData[,1:6]
  ifn3[[i]] = forest_mergeTrees(ifn3[[i]])
}
epl = sf::st_as_sf(spt)
epl$id = row.names(epl)
row.names(epl)<-NULL
epl = epl[,c(4,5,1:3)]
epl$landcovertype = lct
epl$managementunit = NA
epl$managementarguments = NA
epl$representedarea = NA
exampleSFLandscape = SFLandscape(epl, ifn3, ifn3_soils[codes])
usethis::use_data(exampleSFLandscape, overwrite = T)

examplepointslandscape = SpatialPointsLandscape(spt= spt, lct = lct, forestlist = ifn3[codes], soillist = ifn3_soils[codes])
crs <- CRS(SRS_string = "EPSG:25831")
comment(crs)<-gsub("°", "º", comment(crs)) # Replace non-ASCII character
examplepointslandscape@proj4string <- crs
usethis::use_data(examplepointslandscape, overwrite = T)
