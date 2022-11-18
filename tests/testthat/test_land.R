library(medfateland)

data("examplewatershed")
yws = sp_to_sf(examplewatershed)

data("SpParamsMED")
dates = seq(as.Date("2001-03-01"), as.Date("2001-03-03"), by="day")

test_that("Can simulate three days over landscape",{
  expect_s3_class(spwb_land(yws, meteo = examplemeteo, dates = dates, summaryFreq = "month", 
                           SpParams = SpParamsMED, progress = FALSE), "spwb_land")
  expect_s3_class(growth_land(yws, meteo = examplemeteo, dates = dates, summaryFreq = "month", 
                             SpParams = SpParamsMED, progress = FALSE), "growth_land")
})


# test_that("Can simulate three days over landscape using old interpolator",{
#   expect_s3_class(spwb_land(yws, meteo = exampleinterpolationdata, dates = dates, summaryFreq = "month", 
#                             SpParams = SpParamsMED, progress = FALSE), "spwb_land")
# })


# test_that("Can simulate three days over landscape using new interpolator",{
#   interpolator = meteoland::with_meteo(meteoland_meteo_example) |>
#     meteoland::create_meteo_interpolator(params = defaultInterpolationParams(), verbose = FALSE)
#   expect_s3_class(spwb_land(yws, meteo = interpolator, summaryFreq = "month",
#                             SpParams = SpParamsMED, progress = FALSE), "spwb_land")
# })
