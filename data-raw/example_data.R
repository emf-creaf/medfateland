#Example watershed
examplewatershed = readRDS(paste0("/home/miquel/OneDrive/EMFProducts/MEDFATE_Initialisation/Watersheds/Products/DistributedWatersheds/CanVila_watershed_100m.rds")) 
for(i in 1:length(examplewatershed@forestlist)) {
  examplewatershed@forestlist[[i]]$ID = NULL
  examplewatershed@forestlist[[i]]$patchsize = NULL
}
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
forest_list = ifn3[codes]
soil_list = ifn3_soils[codes]
for(i in 1:length(forest_list)) {
  forest_list[[i]]$ID = NULL
  forest_list[[i]]$patchsize = NULL
  forest_list[[i]]$treeData = forest_list[[i]]$treeData[,1:6]
  forest_list[[i]] = medfate::forest_mergeTrees(forest_list[[i]])
  soil_list[[i]] = soil(soil_list[[i]])
}
examplepointslandscape = SpatialPointsLandscape(spt= spt, lct = lct, 
                                                forestlist = forest_list, 
                                                soillist = forest_list)
crs <- CRS(SRS_string = "EPSG:25831")
comment(crs)<-gsub("°", "º", comment(crs)) # Replace non-ASCII character
examplepointslandscape@proj4string <- crs
usethis::use_data(examplepointslandscape, overwrite = T)


defaultPrescriptionsBySpecies<-openxlsx::read.xlsx("data-raw/DefaultPrescriptionsBySpecies.xlsx")
usethis::use_data(defaultPrescriptionsBySpecies, overwrite = TRUE)
