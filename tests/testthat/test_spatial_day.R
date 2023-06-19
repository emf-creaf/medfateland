library(medfateland)

data("example_ifn")
data("example_watershed")
ypts = example_ifn
yws = example_watershed
yws$crop_factor = NA
yws$crop_factor[yws$land_cover_type=="agriculture"] = 0.75

data("examplemeteo")
examplemeteo2 <- examplemeteo
examplemeteo2$dates <- as.Date(row.names(examplemeteo2))
row.names(examplemeteo2) <- NULL

data("SpParamsMED")
date = "2001-03-01"

test_that("Can simulate one day over landscape",{
  expect_s3_class(spwb_spatial_day(ypts, meteo = examplemeteo, date = date, 
                                 SpParams = SpParamsMED, progress = FALSE), "sf")
  expect_s3_class(spwb_spatial_day(yws, meteo = examplemeteo, date = date, 
                                 SpParams = SpParamsMED, progress = FALSE), "sf")
  expect_s3_class(growth_spatial_day(ypts, meteo = examplemeteo, date = date, 
                                 SpParams = SpParamsMED, progress = FALSE), "sf")
  expect_s3_class(growth_spatial_day(yws, meteo = examplemeteo, date = date, 
                                 SpParams = SpParamsMED, progress = FALSE), "sf")
})

test_that("Can simulate one day over landscape with dates in column",{
  expect_s3_class(spwb_spatial_day(ypts, meteo = examplemeteo2, date = date, 
                                   SpParams = SpParamsMED, progress = FALSE), "sf")
  expect_s3_class(spwb_spatial_day(yws, meteo = examplemeteo2, date = date, 
                                   SpParams = SpParamsMED, progress = FALSE), "sf")
  expect_s3_class(growth_spatial_day(ypts, meteo = examplemeteo2, date = date, 
                                     SpParams = SpParamsMED, progress = FALSE), "sf")
  expect_s3_class(growth_spatial_day(yws, meteo = examplemeteo2, date = date, 
                                     SpParams = SpParamsMED, progress = FALSE), "sf")
})

test_that("Can simulate one day over landscape using new interpolator",{
  interpolator = with_meteo(meteoland_meteo_example, verbose = FALSE) |>
    create_meteo_interpolator(params = defaultInterpolationParams(), verbose=FALSE)
  expect_s3_class(spwb_spatial_day(ypts[1,], meteo = interpolator, date = "2022-04-01",
                                     SpParams = SpParamsMED, progress = FALSE), "sf")
  expect_s3_class(growth_spatial_day(ypts[1,], meteo = interpolator, date = "2022-04-01",
                                   SpParams = SpParamsMED, progress = FALSE), "sf")
})