library(medfateland)
library(meteoland)

data("example_watershed")

# Example raster topology
r <-terra::rast(xmin = 401380, ymin = 4671820, xmax = 402880, ymax = 4672620, 
                nrow = 8, ncol = 15, crs = "epsg:32631")

# SAME raster topology
# dem <- terra::rast("serghei_test_example_watershed/input/dem.INPUT")

example_watershed$crop_factor = NA
example_watershed$crop_factor[example_watershed$land_cover_type=="agriculture"] = 0.75
example_watershed$result_cell <- FALSE
example_watershed$result_cell[c(3,6,9)] <- TRUE

yws_swpb <- initialize_landscape(example_watershed, SpParams = SpParamsMED, local_control = defaultControl(),
                                 model = "spwb", progress = FALSE)

data("examplemeteo")
examplemeteo2 <- examplemeteo
row.names(examplemeteo2) <- as.character(examplemeteo2$dates)
examplemeteo2$dates <- NULL

data("SpParamsMED")
dates = seq(as.Date("2001-03-01"), as.Date("2001-03-01"), by="day")

ws_control <- default_watershed_control("serghei")
ws_control$serghei_parameters$input_dir <- "/home/miquel/Rpackages/medfateland/tests/testthat/serghei_test_example_watershed/input/"
ws_control$serghei_parameters$output_dir <- "/home/miquel/Rpackages/medfateland/tests/testthat/serghei_test_example_watershed/output/"

print(dir.exists(ws_control$serghei_parameters$input_dir))
# dir.exists(ws_control$serghei_parameters$output_dir)
test_that("Can simulate three days over landscape and watershed-level plots can be obtained",{
  s1 <- spwb_land(r, yws_swpb, meteo = examplemeteo, dates = dates, summary_frequency = "month", 
                  watershed_control = ws_control, SpParams = SpParamsMED, progress = FALSE)
  expect_s3_class(s1, "spwb_land")
})
