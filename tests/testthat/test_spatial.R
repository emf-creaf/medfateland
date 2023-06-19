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
dates = seq(as.Date("2001-03-01"), as.Date("2001-03-03"), by="day")

test_that("Can simulate three days over landscape",{
  expect_s3_class(spwb_spatial(ypts, meteo = examplemeteo, dates = dates, 
                             SpParams = SpParamsMED, progress = FALSE), "sf")
  expect_s3_class(spwb_spatial(yws, meteo = examplemeteo, dates = dates, 
                             SpParams = SpParamsMED, progress = FALSE), "sf")
  expect_s3_class(growth_spatial(ypts, meteo = examplemeteo, dates = dates, 
                             SpParams = SpParamsMED, progress = FALSE), "sf")
  expect_s3_class(growth_spatial(yws, meteo = examplemeteo, dates = dates, 
                             SpParams = SpParamsMED, progress = FALSE), "sf")
})

test_that("Can simulate three days over landscape with dates as column",{
  expect_s3_class(spwb_spatial(ypts, meteo = examplemeteo2, dates = dates, 
                               SpParams = SpParamsMED, progress = FALSE), "sf")
  expect_s3_class(spwb_spatial(yws, meteo = examplemeteo2, dates = dates, 
                               SpParams = SpParamsMED, progress = FALSE), "sf")
  expect_s3_class(growth_spatial(ypts, meteo = examplemeteo2, dates = dates, 
                                 SpParams = SpParamsMED, progress = FALSE), "sf")
  expect_s3_class(growth_spatial(yws, meteo = examplemeteo2, dates = dates, 
                                 SpParams = SpParamsMED, progress = FALSE), "sf")
})


test_that("Can simulate one year forest dynamics with management",{
  expect_s3_class(fordyn_spatial(ypts[1,], meteo = examplemeteo, 
                               SpParams = SpParamsMED, progress = FALSE), "sf")
})

test_that("Can simulate one year forest dynamics with management with dates in column",{
  expect_s3_class(fordyn_spatial(ypts[1,], meteo = examplemeteo2, 
                                 SpParams = SpParamsMED, progress = FALSE), "sf")
})

test_that("Can simulate 3 days over landscape using new meteoland interpolator",{
  interpolator = meteoland::with_meteo(meteoland_meteo_example, verbose = FALSE) |>
    meteoland::create_meteo_interpolator(params = defaultInterpolationParams(), verbose = FALSE)
  datesMeteo <- as.Date(stars::st_get_dimension_values(interpolator, "date"))
  expect_s3_class(spwb_spatial(ypts[1,], meteo = interpolator, dates = datesMeteo[1:3],
                               SpParams = SpParamsMED, progress = FALSE), "sf")
  expect_s3_class(spwb_spatial(yws, meteo = interpolator, dates = datesMeteo[1:3],
                               SpParams = SpParamsMED, progress = FALSE), "sf")
  expect_s3_class(growth_spatial(ypts[1,], meteo = interpolator, dates = datesMeteo[1:3],
                               SpParams = SpParamsMED, progress = FALSE), "sf")
  expect_s3_class(spwb_spatial(yws, meteo = interpolator, dates = datesMeteo[1:3],
                               SpParams = SpParamsMED, progress = FALSE), "sf")
})
