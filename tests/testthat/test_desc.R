library(medfateland)

data("examplepointslandscape")
data("examplewatershed")
spdf = "SpatialPointsDataFrame"
vars <- c("elevation", "slope", "aspect", "lct", "basalArea", "SWE", "WTD","texture1", "texture2", "texture3",
          "SoilVol")

test_that("Can extract variables from examplepointslandscape",{
  for(v in vars) expect_s4_class(getLandscapeLayer(examplepointslandscape, v), spdf)
})

test_that("Can extract variables from examplewatershed",{
  for(v in vars) expect_s4_class(getLandscapeLayer(examplewatershed, v), spdf)
})