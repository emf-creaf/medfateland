# medfateland 2.7.0
- Runoff-runon dynamics coupled to local water balance in TETIS
- PET is now stored in watershed-level results
- New functions for plotting watershed-level simulation results
- New channel routing algorithm (variable velocity) suggested by Maria Gonzalez-Sanchis

# medfateland 2.6.1
- Bug correction: subwatershed mergin in spwb_land
- Correction of tree density using maps of aboveground tree biomass
- Option `simplify` in function initialize_landscape() replaced by `reduce_to_dominant`, `merge_trees` and `merge_shrubs`

# medfateland 2.6.0
- *_land() functions can process subwatersheds
- parallelization in *_land() for subwatersheds

# medfateland 2.5.3
- new function optimization_rock()
- channel cells are now forced to be outlet in *_land simulations
- channel routing via outlet backlog implemented

# medfateland 2.5.2
- information on soil domain and transpiration mode of initialized landscapes
- overland routing available in new function overland_routing()
- overland flow allowed in completely flat areas (according to processing order)
- bug correction: management in fordyn_land()

# medfateland 2.5.1
- id mapping existence checked in add_forests
- overland routing exported in spwb_land

# medfateland 2.5.0
- Compliance with medfate 4.8.0: internal communication structures
- Bug correction: input column regen is no longer kept in parse_forestable()

# medfateland 2.4.6
- Numerical species codes allowed as input for fordyn_land()
- Added filterMissingSpecies in parse_forestable()
- Added keepUnfilteredCopy in parse_forestable()
- Revision of input checks

# medfateland 2.4.5
- New helper function parse_forestable()
- Soil domains set to "single" by default

# medfateland 2.4.4
- Improvements in modify_forest_structure()

# medfateland 2.4.3
- Revision fordyn_scenario vs dispersal
- Version submitted to CRAN

# medfateland 2.4.2
- New control parameters for dispersal

# medfateland 2.4.1
- Compliance with medfate v. 4.3.1
- New helper function add_forests() to parse forest plot data tables
- New option `simplify` in function initialize_landscape()
- Submission to CRAN

# medfateland 2.4.0
- Compliance with medfate v. 4.3.0
- New watershed parameters n_interflow, n_baseflow
- Daily substeps allowed in interflow calculations

# medfateland 2.3.0
- Infiltration and runoff in non-soil cells
- Helper functions to create landscape sf objects
- Loss to a deeper aquifer implemented

# medfateland 2.2.1
- Weather aggregation for interpolation
- New functions spwb_land_day and spwb_growth_day

# medfateland 2.2.0
- Compliance with medfate ver. 4.1.0
- spwb_land revisited (tetis)
- rasterized maps using tidyterra

# medfateland 2.1.1
- Removing the need to define 'channel'
- Input sf can contain column 'local_control' to override control for specific cells/locations.

# medfateland 2.1.0
- SERGHEI coupling 
- Raster topology from terra package required in watershed simulations
- Parallel computation enabled for spwb_land (only with SERGHEI)

# medfateland 2.0.5
- Seed production and seed bank dynamics (from medfate v.3.2.0)
- Dispersal process implemented for fordyn_land and fordyn_scenario
- Fire regime in growth_spatial

# medfateland 2.0.3
- Fire regimes can be simulated in functions fordyn_spatial and fordyn_scenario

# medfateland 2.0.2
- Improvements in fordyn_scenario
- Allowing management scenarios to specify demand for groups of species
- Ignoring convex hull checks when calling meteoland interpolation
- New vignette 'ManagementScenario'

# medfateland 2.0.0
- IMPORTANT: In this version we removed sp-type S4 classes that were deprecated in 1.0.1. Use release version 1.2.1 to continue using deprecated classes.
- New function 'fordyn_land'
- Compliance with medfate 3.1.1
- Can accept a list of interpolator objects as weather input

# medfateland 1.2.1
- Compliance with medfate 3.0.0
- New function 'initialize_state'

# medfateland 1.1.2
- Package 'cli' adopted for client interface
- Soil water balance in agricultural soils

# medfateland 1.1.1
- Compliance with medfate 2.9.0
- Allowing the use of an input state in fordyn_spatial simulations
- Simulation errors, when occurring, are now stored in 'result'

# medfateland 1.1.0
- New function 'fordyn_scenario'
- Allowing for a column called 'meteo' in input 'sf' objects

# medfateland 1.0.1
- Adapting to new meteoland interpolation procedure

# medfateland 1.0.0
- Major restructuring to work with 'sf' instead of 'sp'

# medfateland 0.5.0
- Reduction of package functions to 'spatial' functions
- Moving documentation to Roxygen

# medfateland 0.4.3
- Compliance with medfate '2.7.6'

# medfateland 0.4.2
- Compliance with medfate '2.6.2'

# medfateland 0.4.1
- Bug correction in modelspatial during coordinate checking
- Welcome sentence

# medfateland 0.4.0
- New functions 'spwbpoints_day', 'growthpoints_day',...
- New function 'updateState'

# medfateland 0.3.0
- Function 'wswb' renamed 'spwbland'
- New function 'growthland' and 'plot.growthland'
- New summary functions 'summary.spwbpoints'....
- New plotting functions 'plot.summarypoints', 'plot.summarypixels' and 'plot.summarygrid'
- New function 'shinyplot_land'

# medfateland 0.2.5
- Update medfate 2.2

# medfateland 0.2.4
- Update medfate 2.0

# medfateland 0.2.3
- Parallelization in spwb and growth simulations with Spatial*Landscape objects

# medfateland 0.2.2
- New spatial object definitions
- New function buildWatershedTopography

# medfateland 0.2.1
- Update for medfate version 0.2.1
- Revision of runoff
- Grid simulations with MeteorologyInterpolationData as input
- Cell state output
- Outlet cell daily runoff

# medfateland 0.2.0
- SFI extract functions moved from 'medfate'

# medfateland 0.1.0
- Corrections to energy balance for z
- Package creation from files previously in package 'medfate'.
- Linking to package 'medfate'
