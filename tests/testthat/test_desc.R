library(medfateland)

data("examplepointslandscape")
data("examplewatershed")
data("SpParamsMED")
spdf = "SpatialPointsDataFrame"
vars <- c("elevation", "slope", "aspect", "lct",
          "texture1", "texture2", "texture3", "SoilVolExtract", "SoilVolSAT", "SoilVolFC", "SoilVolWP", "SoilVolCurr",
          "basalArea", "LAI", "foliarBiomass", "fuel", "phytovolume")
vars_ws <- c(vars,
             "numNeigh", "waterOrder", "outlets", "channel","DepthToBedrock","RockPorosity", "RockConductivity",
             "AquiferElevation", "DepthToAquifer","AquiferVolume", "Snowpack")
test_that("Can extract variables from examplepointslandscape",{
  for(v in vars) {
    expect_s4_class(getLandscapeLayer(examplepointslandscape, variable = v, SpParams = SpParamsMED), spdf)
  }
})

test_that("Can plot variables from examplepointslandscape",{
  for(v in vars) expect_s3_class(plot(examplepointslandscape, y = v, SpParams = SpParamsMED), "ggplot")
})

test_that("Can extract variables from examplewatershed",{
  for(v in vars_ws) expect_s4_class(getLandscapeLayer(examplewatershed, variable = v, SpParams = SpParamsMED), spdf)
})

test_that("Can plot variables from examplewatershed",{
  for(v in vars_ws) expect_s3_class(plot(examplewatershed, y=v, SpParams = SpParamsMED), "ggplot")
})
