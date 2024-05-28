library(medfateland)
library(meteoland)

data("example_watershed")

r <-terra::rast(xmin = 401380, ymin = 4671820, xmax = 402880, ymax = 4672620, 
                nrow = 8, ncol = 15, crs = "epsg:32631")
 
example_watershed$crop_factor = NA
example_watershed$crop_factor[example_watershed$land_cover_type=="agriculture"] = 0.75
yws_swpb <- initialize_landscape(example_watershed, SpParams = SpParamsMED, local_control = defaultControl(),
                                 model = "spwb", progress = FALSE)
yws_growth <- initialize_landscape(example_watershed, SpParams = SpParamsMED, local_control = defaultControl(),
                                 model = "growth", progress = FALSE)
data("examplemeteo")
examplemeteo2 <- examplemeteo
row.names(examplemeteo2) <- as.character(examplemeteo2$dates)
examplemeteo2$dates <- NULL

interpolator <- meteoland::with_meteo(meteoland_meteo_example, verbose = FALSE) |>
  meteoland::create_meteo_interpolator(params = defaultInterpolationParams(), verbose = FALSE)


data("SpParamsMED")
dates = seq(as.Date("2001-03-01"), as.Date("2001-03-01"), by="day")


test_that("Can simulate three days over landscape",{
  expect_s3_class(spwb_land(r, yws_swpb, meteo = examplemeteo, dates = dates, summary_frequency = "month", 
                           SpParams = SpParamsMED, progress = FALSE), "spwb_land")
  expect_s3_class(growth_land(r, yws_growth, meteo = examplemeteo, dates = dates, summary_frequency = "month", 
                             SpParams = SpParamsMED, progress = FALSE), "growth_land")
})

test_that("Can simulate three days over landscape with dates in column",{
  expect_s3_class(spwb_land(r, yws_swpb, meteo = examplemeteo2, dates = dates, summary_frequency = "month", 
                            SpParams = SpParamsMED, progress = FALSE), "spwb_land")
  expect_s3_class(growth_land(r, yws_growth, meteo = examplemeteo2, dates = dates, summary_frequency = "month", 
                              SpParams = SpParamsMED, progress = FALSE), "growth_land")
})


# The following test is too long for CRAN
# test_that("Can simulate three days over landscape using new interpolator",{
#   expect_s3_class(spwb_land(r, yws_swpb, meteo = interpolator, summary_frequency = "month",
#                             SpParams = SpParamsMED, progress = FALSE), "spwb_land")
#   expect_s3_class(growth_land(r, yws_growth, meteo = interpolator, summary_frequency = "month",
#                               SpParams = SpParamsMED, progress = FALSE), "growth_land")
# })
