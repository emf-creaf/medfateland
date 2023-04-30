#Example watershed
# examplewatershed = readRDS(paste0("/home/miquel/OneDrive/EMFProducts/MEDFATE_Initialisation/Watersheds/Products/DistributedWatersheds/CanVila_watershed_100m.rds")) 
# for(i in 1:length(examplewatershed@forestlist)) {
#   examplewatershed@forestlist[[i]]$ID = NULL
#   examplewatershed@forestlist[[i]]$patchsize = NULL
# }
# crs <- CRS(SRS_string = "EPSG:32631")
# comment(crs)<-gsub("°", "º", comment(crs)) # Replace non-ASCII character
# examplewatershed@proj4string = crs
# usethis::use_data(examplewatershed, overwrite = T)

#Example sf from IFN3 (original coordinates)
ifn3 <- readRDS("/home/miquel/OneDrive/EMFProducts/MEDFATE_Initialisation/IFN/Products/IFN3/Catalunya/IFN3_cat_final_ETRS89H31.rds")
original_coords <- sf::read_sf("/home/miquel/OneDrive/EMFDatasets/ForestInventories/IFN/Products/Coordinates/IFN3_cat_original_ETRS89H31.gpkg")
ifn3$land_cover_type <- "wildland"
example_ifn <- ifn3[1001:1100, c("geom","id", "elevation", "slope", "aspect", "land_cover_type", "soil", "forest")]
example_ifn$forest <- lapply(example_ifn$forest, function(x){
  x$treeData$OrdenIf2 <- NULL
  x$treeData$OrdenIf3 <- NULL
  return(x)
  })
sf::st_geometry(example_ifn) <- sf::st_geometry(original_coords[1001:1100,])
usethis::use_data(example_ifn, overwrite = T)

defaultPrescriptionsBySpecies<-openxlsx::read.xlsx("data-raw/DefaultPrescriptionsBySpecies.xlsx")
usethis::use_data(defaultPrescriptionsBySpecies, overwrite = TRUE)
