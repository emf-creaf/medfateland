library(medfate)
library(medfateland)

version <- as.character(packageVersion("medfate"))

data("example_watershed")
data("example_ifn")

data("SpParamsMED")

example_watershed$crop_factor <- NA
example_watershed$crop_factor[example_watershed$land_cover_type=="agriculture"] <- 0.75
b <- sf::st_bbox(example_watershed)
r <-terra::rast(xmin = 401380, ymin = 4671820, xmax = 402880, ymax = 4672620, 
                nrow = 8, ncol = 15, crs = "epsg:32631")
dates <- seq(as.Date("2001-01-01"), as.Date("2001-12-31"), by="day")
ws_control <- default_watershed_control("tetis")
ws_init <- initialize_landscape(example_watershed, SpParams = SpParamsMED,
                                     local_control = defaultControl(soilDomains = "single"),
                                     reduce_to_dominant = TRUE)

saveRDS(ws_init, file = paste0("initialized_objects/ws_init_", version,".rds"))

local_control <- defaultControl()
ifn_init <- initialize_landscape(example_ifn[1:10,], SpParamsMED, 
                                 local_control = local_control, 
                                 model = "spwb")
saveRDS(ifn_init, file = paste0("initialized_objects/ifn_init_", version,".rds"))
