library(medfateland)
library(meteoland)

data("example_watershed")

r <-terra::rast(xmin = 401380, ymin = 4671820, xmax = 402880, ymax = 4672620, 
                nrow = 8, ncol = 15, crs = "epsg:32631")
 
example_watershed$crop_factor = NA
example_watershed$crop_factor[example_watershed$land_cover_type=="agriculture"] = 0.75
example_watershed$result_cell <- FALSE
example_watershed$result_cell[c(3,6,9)] <- TRUE

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


yws_agric <- example_watershed
yws_agric$forest[yws_agric$land_cover_type=="wildland"] <- list(NULL)
yws_agric$land_cover_type[yws_agric$land_cover_type=="wildland"] <- "agriculture"
yws_agric$crop_factor[yws_agric$land_cover_type=="agriculture"] <- 0.75

yws_rock <- example_watershed
yws_rock$forest[yws_rock$land_cover_type=="wildland"] <- list(NULL)
yws_rock$forest[yws_rock$land_cover_type=="agriculture"] <- list(NULL)
yws_rock$soil[yws_rock$land_cover_type=="wildland"] <- list(NULL)
yws_rock$soil[yws_rock$land_cover_type=="agriculture"] <- list(NULL)
yws_rock$land_cover_type[yws_rock$land_cover_type=="wildland"] <- "rock"
yws_rock$land_cover_type[yws_rock$land_cover_type=="agriculture"] <- "rock"


data("SpParamsMED")
dates = seq(as.Date("2001-03-01"), as.Date("2001-03-01"), by="day")

test_that("Overland routing can be estimated",{
  expect_s3_class(overland_routing(r, yws_swpb[1:10,]), "sf")
})
test_that("Can simulate three days over landscape and watershed-level plots can be obtained",{
  s1 <- spwb_land(r, yws_swpb[1:10,], meteo = examplemeteo, dates = dates, summary_frequency = "month", 
                  SpParams = SpParamsMED, progress = FALSE)
  g1 <- growth_land(r, yws_growth[1:10,], meteo = examplemeteo, dates = dates, summary_frequency = "month", 
                    SpParams = SpParamsMED, progress = FALSE)
  expect_s3_class(s1, "spwb_land")
  expect_s3_class(g1, "growth_land")
  types <- medfateland:::.getWatershedWaterBalancePlotTypes()
  for(type in types) {
    expect_s3_class(plot(s1, type), "ggplot")
    expect_s3_class(plot(g1, type), "ggplot")
  }
})

test_that("Can simulate three days over agricultural landscape and watershed-level plots can be obtained",{
  s1 <- spwb_land(r, yws_agric[1:10,], meteo = examplemeteo, dates = dates, summary_frequency = "month", 
                  SpParams = SpParamsMED, progress = FALSE)
  g1 <- growth_land(r, yws_agric[1:10,], meteo = examplemeteo, dates = dates, summary_frequency = "month", 
                    SpParams = SpParamsMED, progress = FALSE)
  expect_s3_class(s1, "spwb_land")
  expect_s3_class(g1, "growth_land")
  types <- medfateland:::.getWatershedWaterBalancePlotTypes()
  for(type in types) {
    expect_s3_class(plot(s1, type), "ggplot")
    expect_s3_class(plot(g1, type), "ggplot")
  }
})

test_that("Can simulate three days over rocky landscape and watershed-level plots can be obtained",{
  s1 <- spwb_land(r, yws_rock[1:10,], meteo = examplemeteo, dates = dates, summary_frequency = "month", 
                  SpParams = SpParamsMED, progress = FALSE)
  g1 <- growth_land(r, yws_rock[1:10,], meteo = examplemeteo, dates = dates, summary_frequency = "month", 
                    SpParams = SpParamsMED, progress = FALSE)
  expect_s3_class(s1, "spwb_land")
  expect_s3_class(g1, "growth_land")
  types <- medfateland:::.getWatershedWaterBalancePlotTypes()
  for(type in types) {
    expect_s3_class(plot(s1, type), "ggplot")
    expect_s3_class(plot(g1, type), "ggplot")
  }
})
test_that("Can simulate three days over landscape with dates in column",{
  expect_s3_class(spwb_land(r, yws_swpb[1:10,], meteo = examplemeteo2, dates = dates, summary_frequency = "month", 
                            SpParams = SpParamsMED, progress = FALSE), "spwb_land")
  expect_s3_class(growth_land(r, yws_growth[1:10,], meteo = examplemeteo2, dates = dates, summary_frequency = "month", 
                              SpParams = SpParamsMED, progress = FALSE), "growth_land")
})


yws = example_watershed
yws$crop_factor = NA
yws$crop_factor[yws$land_cover_type=="agriculture"] = 0.75

# Translate to numeric codes
f <- yws$forest[[4]]
for(i in 1:nrow(f$treeData)) {
  f$treeData$Species[i] <- SpParamsMED$SpIndex[SpParamsMED$Name==f$treeData$Species[i]]
}
f$treeData$Species <- as.numeric(f$treeData$Species)
for(i in 1:nrow(f$shrubData)) {
  f$shrubData$Species[i] <- SpParamsMED$SpIndex[SpParamsMED$Name==f$shrubData$Species[i]]
}
f$shrubData$Species <- as.numeric(f$shrubData$Species)
yws$forest[[4]] <- f

test_that("Can simulate one year of fordyn over landscape using new interpolator",{
  skip_on_cran()
  expect_s3_class(fordyn_land(r, yws[1:10,], meteo = interpolator,
                              SpParams = SpParamsMED, progress = FALSE), "fordyn_land")
})

# The following test is too long for CRAN
# test_that("Can simulate three days over landscape using new interpolator",{
#   expect_s3_class(spwb_land(r, yws_swpb, meteo = interpolator, summary_frequency = "month",
#                             SpParams = SpParamsMED, progress = FALSE), "spwb_land")
#   expect_s3_class(growth_land(r, yws_growth, meteo = interpolator, summary_frequency = "month",
#                               SpParams = SpParamsMED, progress = FALSE), "growth_land")
# })
