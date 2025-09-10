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

# for(i in 1:nrow(example_watershed)) {
#   s = example_watershed$soil[[i]]
#   if(!is.null(s)) {
#     df = data.frame(width = s$widths, clay = s$clay, sand = s$sand, om = s$om, bd = s$bd, rfc = s$rfc)
#     example_watershed$soil[[i]] = df
#   }
# }

# for(i in 1:nrow(example_watershed)) {
#   df = example_watershed$soil[[i]]
#   if(!is.null(df)) {
#     names(df)[1] = "widths"
#     example_watershed$soil[[i]] = df
#   }
# }

# Generate burnin dataset
data("example_watershed")
data("examplemeteo")
data("SpParamsMED")
example_watershed$crop_factor <- NA
example_watershed$crop_factor[example_watershed$land_cover_type=="agriculture"] <- 0.75
b <- sf::st_bbox(example_watershed)
r <-terra::rast(xmin = 401380, ymin = 4671820, xmax = 402880, ymax = 4672620, 
                nrow = 8, ncol = 15, crs = "epsg:32631")
dates <- seq(as.Date("2001-01-01"), as.Date("2001-12-31"), by="day")
ws_control <- default_watershed_control("tetis")
example_init <- initialize_landscape(example_watershed, SpParams = SpParamsMED,
                                     local_control = defaultControl(soilDomains = "single"),
                                     reduce_to_dominant = TRUE)
# first year
res1 <- spwb_land(r, example_init, SpParamsMED, examplemeteo, 
                 dates = dates, summary_frequency = "month",
                 watershed_control = ws_control)
plot(res1, "Export")
plot(res1, "Aquifer")
summary(res1$sf$aquifer)
example_watershed_burnin <- update_landscape(example_init, res1)
# second year
res2 <- spwb_land(r, example_watershed_burnin, SpParamsMED, examplemeteo,
                 dates = dates, summary_frequency = "month",
                 watershed_control = ws_control)
plot(res2, "Export")
plot(res2, "Aquifer")
sum(res2$sf$aquifer)
example_watershed_burnin <- update_landscape(example_watershed_burnin, res2)
# third year
res3 <- spwb_land(r, example_watershed_burnin, SpParamsMED, examplemeteo,
                  dates = dates, summary_frequency = "month",
                  watershed_control = ws_control)
plot(res3, "Export")
plot(res3, "Aquifer")
sum(res3$sf$aquifer)
example_watershed_burnin <- update_landscape(example_watershed_burnin, res3)


#Example sf from IFN3 (original coordinates)
ifn3 <- readRDS("/home/miquel/OneDrive/mcaceres_work/model_initialisation/medfate_initialisation/IFN2medfate/data/SpParamsMED/IFN3/Catalunya/IFN3_cat_final_ETRS89H31.rds")
original_coords <- sf::read_sf("/home/miquel/OneDrive/EMF_datasets/ForestInventories/IFN/Products/Coordinates/IFN3_cat_original_ETRS89H31.gpkg")
ifn3$land_cover_type <- "wildland"
example_ifn <- ifn3[1001:1100, c("geom","id", "elevation", "slope", "aspect", "land_cover_type", "soil", "forest")]
example_ifn$forest <- lapply(example_ifn$forest, function(x){
  x$treeData$OrdenIf2 <- NULL
  x$treeData$OrdenIf3 <- NULL
  return(x)
  })
sf::st_geometry(example_ifn) <- sf::st_geometry(original_coords[1001:1100,])
example_ifn <- sf::st_transform(example_ifn, 4326) # TO geographic coordinates


## Save as INTERNAL
usethis::use_data(example_ifn, example_watershed, example_watershed_burnin, overwrite = T, internal = TRUE)
## REBUILD

defaultPrescriptionsBySpecies<-openxlsx::read.xlsx("data-raw/DefaultPrescriptionsBySpecies.xlsx")
usethis::use_data(defaultPrescriptionsBySpecies, overwrite = TRUE)
