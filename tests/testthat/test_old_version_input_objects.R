library(medfateland)

data("examplemeteo")
data("example_watershed")
data("example_ifn")
data("SpParamsMED")

current_version <- as.character(packageVersion("medfate"))
old_versions <- c("4.8.3","4.8.4")

dates <- seq(as.Date("2001-01-01"), as.Date("2001-12-31"), by="day")
r <-terra::rast(xmin = 401380, ymin = 4671820, xmax = 402880, ymax = 4672620, 
                nrow = 8, ncol = 15, crs = "epsg:32631")
date <- "2001-01-03"

test_that("spwb_spatial can be run from stored inputs coming from older versions",{
  testthat::skip_on_cran()
  testthat::skip_on_ci()

  # Generate current version objects   
  ws_init <- initialize_landscape(example_watershed, SpParams = SpParamsMED,
                                  local_control = defaultControl(soilDomains = "single"),
                                  reduce_to_dominant = TRUE, progress = FALSE)
  saveRDS(ws_init, file = paste0("initialized_objects/ws_init_", current_version,".rds"))
  ifn_init <- initialize_landscape(example_ifn[1:10,], SpParamsMED, 
                                   local_control =  defaultControl(), 
                                   model = "spwb", progress = FALSE)
  saveRDS(ifn_init, file = paste0("initialized_objects/ifn_init_", current_version,".rds"))
  
  # Test older version objects 
  ws_control <- default_watershed_control("tetis")
  for(ver in old_versions) {
    ifn_init <- readRDS(paste0("initialized_objects/ifn_init_", ver,".rds"))  
    ifn_S1 <- spwb_spatial(ifn_init, 
                       SpParams = SpParamsMED, 
                       meteo = examplemeteo[1:10,], progress = FALSE, local_verbose = FALSE)
    expect_s3_class(ifn_S1, "sf")
    expect_equal(ifn_S1$state[[1]]$version, as.character(packageVersion("medfate")))
  }
})

test_that("spwb_spatial_day can be run from stored inputs coming from older versions",{
  testthat::skip_on_cran()
  testthat::skip_on_ci()
  for(ver in old_versions) {
    ifn_init <- readRDS(paste0("initialized_objects/ifn_init_", ver,".rds"))  
    ifn_S1 <- spwb_spatial_day(ifn_init, 
                           SpParams = SpParamsMED, 
                           meteo = examplemeteo[1:10,], 
                           date = date, progress = FALSE)
    expect_s3_class(ifn_S1, "sf")
    expect_equal(ifn_S1$state[[1]]$version, as.character(packageVersion("medfate")))
  }
})

test_that("spwb_land can be run from stored inputs coming from older versions",{
  testthat::skip_on_cran()
  testthat::skip_on_ci()
  for(ver in old_versions) {
    ws_init <- readRDS(paste0("initialized_objects/ws_init_", ver,".rds"))  
    ws_S1 <- spwb_land(r, ws_init,
                       SpParams = SpParamsMED, 
                       meteo = examplemeteo[1:10,],
                       progress = FALSE)
    expect_s3_class(ws_S1, "spwb_land")
    expect_equal(ws_S1$sf$state[[1]]$version, as.character(packageVersion("medfate")))
  }
})
