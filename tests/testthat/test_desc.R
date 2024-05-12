library(medfateland)

data("example_ifn")
data("example_watershed")
ypts = example_ifn
yws = example_watershed

data("SpParamsMED")
vars <- c("elevation", "slope", "aspect", "land_cover_type",
          "soil_vol_extract", "soil_vol_sat", "soil_vol_fc", "soil_vol_wp", "soil_vol_curr",
          "soil_theta_curr", "soil_psi_curr", "soil_rwc_curr", "soil_rew_curr",
          "basal_area", "leaf_area_index", "foliar_biomass", "fuel", "shrub_volume")
vars_ws <- c(vars,
             "depth_to_bedrock","bedrock_porosity", "bedrock_conductivity",
             "aquifer_elevation", "depth_to_aquifer","aquifer", "snowpack")
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
