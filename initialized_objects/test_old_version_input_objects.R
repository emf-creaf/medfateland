library(testthat)
library(medfateland)

data(examplemeteo)

versions <- c("4.8.3","4.8.4")

r <-terra::rast(xmin = 401380, ymin = 4671820, xmax = 402880, ymax = 4672620, 
                nrow = 8, ncol = 15, crs = "epsg:32631")
date <- "2001-01-03"

test_that("spwb_spatial can be run from stored inputs coming from older versions",{
  for(ver in versions) {
    ifn_init <- readRDS(paste0("../initialized_objects/ifn_init_", ver,".rds"))  
    ifn_S1 <- spwb_spatial(ifn_init, 
                       SpParams = SpParamsMED, 
                       meteo = examplemeteo[1:10,], progress = FALSE, local_verbose = FALSE)
    expect_s3_class(ifn_S1, "sf")
    expect_equal(ifn_S1$state[[1]]$version, as.character(packageVersion("medfate")))
  }
})

test_that("spwb_spatial_day can be run from stored inputs coming from older versions",{
  for(ver in versions) {
    ifn_init <- readRDS(paste0("../initialized_objects/ifn_init_", ver,".rds"))  
    ifn_S1 <- spwb_spatial_day(ifn_init, 
                           SpParams = SpParamsMED, 
                           meteo = examplemeteo[1:10,], 
                           date = date, progress = FALSE)
    expect_s3_class(ifn_S1, "sf")
    expect_equal(ifn_S1$state[[1]]$version, as.character(packageVersion("medfate")))
  }
})

test_that("spwb_land can be run from stored inputs coming from older versions",{
  for(ver in versions) {
    ws_init <- readRDS(paste0("../initialized_objects/ws_init_", ver,".rds"))  
    ws_S1 <- spwb_land(r, ws_init,
                       SpParams = SpParamsMED, 
                       meteo = examplemeteo[1:10,],
                       progress = FALSE)
    expect_s3_class(ws_S1, "spwb_land")
    expect_equal(ws_S1$sf$state[[1]]$version, as.character(packageVersion("medfate")))
  }
})
