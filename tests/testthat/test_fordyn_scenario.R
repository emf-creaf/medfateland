library(medfateland)
library(meteoland)

# Load example landscape data
data("example_ifn")

# Load example meteo data frame from package meteoland
data("examplemeteo")

#Prepare a three-year meteorological data 
meteo_01_02 <- rbind(examplemeteo, examplemeteo)
row.names(meteo_01_02) <- seq(as.Date("2001-01-01"), 
                              as.Date("2002-12-31"), by="day")

# Load default medfate parameters
data("SpParamsMED")

# Creates scenario with one management unit and annual demand for P. nigra 
scen <- create_management_scenario(1, c("Pinus nigra" = 2300))

# Assign management unit to all stands
example_ifn$management_unit <- 1 

# Assume that each stand represents 1km2 = 100 ha
example_ifn$represented_area <- 100

test_that("Can simulate two years of scenario for two plots",{
  sf <- example_ifn[1:2,]
  expect_s3_class(fds <- fordyn_scenario(sf, SpParamsMED, meteo = meteo_01_02, 
                                  volume_function = NULL, management_scenario = scen,
                                  parallelize = FALSE, progress = FALSE), "fordyn_scenario")
  sf <- update_landscape(sf, fds)
})

