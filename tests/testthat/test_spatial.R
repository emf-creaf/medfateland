library(medfateland)

data("examplepointslandscape")
data("examplewatershed")
ypts = sp_to_sf(examplepointslandscape)
yws = sp_to_sf(examplewatershed)
yws$crop_factor = NA
yws$crop_factor[yws$land_cover_type=="agriculture"] = 0.75

data("examplemeteo")
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


test_that("Can simulate one year with management",{
  expect_s3_class(fordyn_spatial(ypts[1,], meteo = examplemeteo, 
                               SpParams = SpParamsMED, progress = FALSE), "sf")
})


# test_that("Can simulate three days over landscape using old interpolator",{
#   data("exampleinterpolationdata")
#   expect_s3_class(spwb_spatial(ypts[1,], meteo = exampleinterpolationdata, 
#                                SpParams = SpParamsMED, progress = FALSE), "sf")
# })
# 
# test_that("Can simulate three days over landscape using new interpolator",{
#   interpolator = meteoland::with_meteo(meteoland_meteo_example) |>
#     meteoland::create_meteo_interpolator(params = defaultInterpolationParams(), verbose = FALSE)
#   expect_s3_class(spwb_spatial(ypts[1,], meteo = interpolator,
#                                SpParams = SpParamsMED, progress = FALSE), "sf")
# })
