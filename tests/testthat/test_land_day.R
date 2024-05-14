library(medfateland)
library(meteoland)

data("example_watershed")

r <-terra::rast(xmin = 401380, ymin = 4671820, xmax = 402880, ymax = 4672620, 
                nrow = 8, ncol = 15, crs = "epsg:32631")

yws = example_watershed
yws$crop_factor = NA
yws$crop_factor[yws$land_cover_type=="agriculture"] = 0.75

data("examplemeteo")

interpolator <- meteoland::with_meteo(meteoland_meteo_example, verbose = FALSE) |>
  meteoland::create_meteo_interpolator(params = defaultInterpolationParams(), verbose = FALSE)


data("SpParamsMED")
dates = seq(as.Date("2001-03-01"), as.Date("2001-03-03"), by="day")

test_that("Can simulate single day over landscape",{
  expect_s3_class(spwb_land_day(r, yws, meteo = examplemeteo, date = "2001-01-01",  
                                SpParams = SpParamsMED, progress = FALSE), "sf")
  expect_s3_class(growth_land_day(r, yws, meteo = examplemeteo, date = "2001-01-01",  
                                  SpParams = SpParamsMED, progress = FALSE), "sf")
})

test_that("Can simulate single day over landscape using new interpolator",{
  expect_s3_class(spwb_land_day(r, yws, meteo = interpolator, date = "2022-04-01",  
                                SpParams = SpParamsMED, progress = FALSE), "sf")
  expect_s3_class(growth_land_day(r, yws, meteo = interpolator, date = "2022-04-01",  
                                  SpParams = SpParamsMED, progress = FALSE), "sf")
})
