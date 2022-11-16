library(medfateland)

data("examplepointslandscape")
data("examplewatershed")
ypts = sp_to_sf(examplepointslandscape)
yws = sp_to_sf(examplewatershed)

data("SpParamsMED")
vars <- c("elevation", "slope", "aspect", "landcovertype",
          "texture1", "texture2", "texture3", "soilvolextract", "soilvolsat", "soilvolfc", "soilvolwp", "soilvolcurr",
          "basalarea", "leafareaindex", "foliarbiomass", "fuel", "phytovolume")
vars_ws <- c(vars,
             "numneigh", "waterorder", "outlets", "channel","depthtobedrock","bedrockporosity", "bedrockconductivity",
             "aquiferelevation", "depthtoaquifer","aquifervolume", "snowpack")
test_that("Can extract variables from 'sf' object",{
  expect_s3_class(extract_variables(ypts, vars = vars, SpParams = SpParamsMED), "sf")
})

test_that("Can plot variables from point 'sf' object",{
  for(v in vars) expect_s3_class(plot_variable(ypts, variable = v, SpParams = SpParamsMED), "ggplot")
})

test_that("Can extract variables from watershed 'sf' object",{
  expect_s3_class(extract_variables(yws, vars = vars_ws, SpParams = SpParamsMED), "sf")
})

test_that("Can plot variables from watershed 'sf' object",{
  for(v in vars_ws) {
    expect_s3_class(plot_variable(yws, variable=v, SpParams = SpParamsMED), "ggplot")
  }
})
