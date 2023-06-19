library(medfateland)


data("example_watershed")
yws = example_watershed
yws$crop_factor = NA
yws$crop_factor[yws$land_cover_type=="agriculture"] = 0.75

data("examplemeteo")
examplemeteo2 <- examplemeteo
examplemeteo2$dates <- as.Date(row.names(examplemeteo2))
row.names(examplemeteo2) <- NULL

data("SpParamsMED")
dates = seq(as.Date("2001-03-01"), as.Date("2001-03-03"), by="day")

test_that("Can simulate three days over landscape",{
  expect_s3_class(spwb_land(yws, meteo = examplemeteo, dates = dates, summary_frequency = "month", 
                           SpParams = SpParamsMED, progress = FALSE), "spwb_land")
  expect_s3_class(growth_land(yws, meteo = examplemeteo, dates = dates, summary_frequency = "month", 
                             SpParams = SpParamsMED, progress = FALSE), "growth_land")
})

test_that("Can simulate three days over landscape with dates in column",{
  expect_s3_class(spwb_land(yws, meteo = examplemeteo2, dates = dates, summary_frequency = "month", 
                            SpParams = SpParamsMED, progress = FALSE), "spwb_land")
  expect_s3_class(growth_land(yws, meteo = examplemeteo2, dates = dates, summary_frequency = "month", 
                              SpParams = SpParamsMED, progress = FALSE), "growth_land")
})

test_that("Can simulate three days over landscape using new interpolator",{
  interpolator = meteoland::with_meteo(meteoland_meteo_example, verbose = FALSE) |>
    meteoland::create_meteo_interpolator(params = defaultInterpolationParams(), verbose = FALSE)
  expect_s3_class(spwb_land(yws, meteo = interpolator, summary_frequency = "month",
                            SpParams = SpParamsMED, progress = FALSE), "spwb_land")
  expect_s3_class(growth_land(yws, meteo = interpolator, summary_frequency = "month",
                              SpParams = SpParamsMED, progress = FALSE), "growth_land")
})
