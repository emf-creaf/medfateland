library(medfateland)
library(meteoland)

# Load example landscape data
data("example_ifn")

# Load example meteo data frame from package meteoland
data("examplemeteo")

#Prepare a three-year meteorological data 
meteo_01_02 <- rbind(examplemeteo, examplemeteo)
meteo_01_02$dates <- seq(as.Date("2001-01-01"), 
                         as.Date("2002-12-31"), by="day")

# Load default medfate parameters
data("SpParamsMED")
data("defaultPrescriptionsBySpecies")

# Assume that each stand represents 1km2 = 100 ha
example_ifn$represented_area_ha <- 100


test_that("Can build different scenario definitions",{
  expect_s3_class(create_management_scenario(3,  c("Quercus ilex" = 1000, "Pinus nigra" = 2000)),
                  "management_scenario")
  
  expect_s3_class(create_management_scenario(3,  
                                       c("Quercus ilex" = 1000, "Pinus nigra" = 2000),
                                       c("2002" = 30, "2003" = 50)),
                  "management_scenario")
  
  expect_s3_class(create_management_scenario(defaultPrescriptionsBySpecies),
                  "management_scenario")
  
  expect_s3_class(create_management_scenario(3,  c("Quercus ilex/Quercus pubescens" = 1000, 
                                             "Pinus nigra" = 2000)),
                  "management_scenario")
                                             
})

test_that("Can simulate two years of scenario for two plots",{
  # Creates demand-based scenario with one management unit and annual demand for P. nigra 
  scen1 <- create_management_scenario(1, c("Pinus nigra" = 2300))
  example_ifn$management_unit <- 1 
  sf <- example_ifn[1:2,, drop = FALSE]
  expect_s3_class(fds <- fordyn_scenario(sf, SpParamsMED, meteo = meteo_01_02, 
                                  volume_function = NULL, management_scenario = scen1,
                                  parallelize = FALSE, progress = FALSE), "fordyn_scenario")
  sf <- update_landscape(sf, fds)
  # # Creates bottom-up scenario with one management unit 
  # scen2 <- create_management_scenario(defaultPrescriptionsBySpecies)
  # example_ifn$management_unit <- 1 
  # expect_s3_class(fds <- fordyn_scenario(sf, SpParamsMED, meteo = meteo_01_02, 
  #                                        volume_function = NULL, management_scenario = scen2,
  #                                        parallelize = FALSE, progress = FALSE), "fordyn_scenario")
  
  # # Creates bottom-up scenario with no management units 
  # scen3 <- create_management_scenario(1)
  # example_ifn$management_unit <- NA 
  # expect_s3_class(fds <- fordyn_scenario(sf, SpParamsMED, meteo = meteo_01_02, 
  #                                        volume_function = NULL, management_scenario = scen3,
  #                                        parallelize = FALSE, progress = FALSE), "fordyn_scenario")
  
})

