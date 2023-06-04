----------------------------------
 NEWS for R Package "medfateland"
----------------------------------

# Version 2.0.0
- IMPORTANT: In this version we removed sp-type S4 classes that were deprecated in 1.0.1. Use release version 1.2.1 to continue using deprecated classes.
- New function 'fordyn_land'
- Compliance with medfate 3.1.1

# Version 1.2.1
- Compliance with medfate 3.0.0
- New function 'initialize_state'

# Version 1.1.2
- Package 'cli' adopted for client interface
- Soil water balance in agricultural soils

# Version 1.1.1
- Compliance with medfate 2.9.0
- Allowing the use of an input state in fordyn_spatial simulations
- Simulation errors, when occurring, are now stored in 'result'

# Version 1.1.0
- New function 'fordyn_scenario'
- Allowing for a column called 'meteo' in input 'sf' objects

# Version 1.0.1
- Adapting to new meteoland interpolation procedure

# Version 1.0.0
- Major restructuring to work with 'sf' instead of 'sp'

# Version 0.5.0
- Reduction of package functions to 'spatial' functions
- Moving documentation to Roxygen

# Version 0.4.3
- Compliance with medfate '2.7.6'

# Version 0.4.2
- Compliance with medfate '2.6.2'

# Version 0.4.1
- Bug correction in modelspatial during coordinate checking
- Welcome sentence

# Version 0.4.0
- New functions 'spwbpoints_day', 'growthpoints_day',...
- New function 'updateState'

# Version 0.3.0
- Function 'wswb' renamed 'spwbland'
- New function 'growthland' and 'plot.growthland'
- New summary functions 'summary.spwbpoints'....
- New plotting functions 'plot.summarypoints', 'plot.summarypixels' and 'plot.summarygrid'
- New function 'shinyplot_land'

# Version 0.2.5
- Update medfate 2.2

# Version 0.2.4
- Update medfate 2.0

# Version 0.2.3
- Parallelization in spwb and growth simulations with Spatial*Landscape objects

# Version 0.2.2
- New spatial object definitions
- New function buildWatershedTopography

# Version 0.2.1
- Update for medfate version 0.2.1
- Revision of runoff
- Grid simulations with MeteorologyInterpolationData as input
- Cell state output
- Outlet cell daily runoff

# Version 0.2.0
- SFI extract functions moved from 'medfate'

# Version 0.1.0
- Corrections to energy balance for z
- Package creation from files previously in package 'medfate'.
- Linking to package 'medfate'
