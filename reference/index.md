# Package index

## Input preparation

Functions to help preparing spatial inputs

- [`add_topography()`](https://emf-creaf.github.io/medfateland/reference/add_topography.md)
  [`add_land_cover()`](https://emf-creaf.github.io/medfateland/reference/add_topography.md)
  : Add topography and land cover
- [`check_topography()`](https://emf-creaf.github.io/medfateland/reference/check_inputs.md)
  [`check_land_cover()`](https://emf-creaf.github.io/medfateland/reference/check_inputs.md)
  [`check_forests()`](https://emf-creaf.github.io/medfateland/reference/check_inputs.md)
  [`check_soils()`](https://emf-creaf.github.io/medfateland/reference/check_inputs.md)
  : Check spatial inputs
- [`add_forests()`](https://emf-creaf.github.io/medfateland/reference/add_forests.md)
  : Add forests
- [`impute_forests()`](https://emf-creaf.github.io/medfateland/reference/forest_parametrization.md)
  [`modify_forest_structure()`](https://emf-creaf.github.io/medfateland/reference/forest_parametrization.md)
  : Landscape forest parametrization
- [`parse_forestable()`](https://emf-creaf.github.io/medfateland/reference/parse_forestable.md)
  : Parse forestable
- [`add_soilgrids()`](https://emf-creaf.github.io/medfateland/reference/soil_parametrization.md)
  [`modify_soils()`](https://emf-creaf.github.io/medfateland/reference/soil_parametrization.md)
  : Landscape soil parametrization
- [`optimization_rock()`](https://emf-creaf.github.io/medfateland/reference/optimization_rock.md)
  : Rock optimization

## Example data

Example data sets for distributed stands and watershed

- [`example_ifn`](https://emf-creaf.github.io/medfateland/reference/example_ifn.md)
  : Example of distributed forest inventory stands
- [`example_watershed`](https://emf-creaf.github.io/medfateland/reference/example_watershed.md)
  [`example_watershed_burnin`](https://emf-creaf.github.io/medfateland/reference/example_watershed.md)
  : Example of watershed

## Summary/extract functions for spatial classes

Forest and soil summary functions for spatial classes

- [`landscape_summary()`](https://emf-creaf.github.io/medfateland/reference/landscape_summary.md)
  : Forest and soil summaries over space
- [`extract_variables()`](https://emf-creaf.github.io/medfateland/reference/extract_variables.md)
  [`plot_variable()`](https://emf-creaf.github.io/medfateland/reference/extract_variables.md)
  : Landscape variables

## Default parameters

Defaults for model inputs

- [`default_dispersal_control()`](https://emf-creaf.github.io/medfateland/reference/default_dispersal_control.md)
  : Default control parameters for dispersal
- [`default_watershed_control()`](https://emf-creaf.github.io/medfateland/reference/default_watershed_control.md)
  : Default control parameters for watershed processes
- [`default_volume_function()`](https://emf-creaf.github.io/medfateland/reference/default_volume_function.md)
  : Default volume function
- [`create_management_scenario()`](https://emf-creaf.github.io/medfateland/reference/create_management_scenario.md)
  : Create management scenario
- [`create_fire_regime()`](https://emf-creaf.github.io/medfateland/reference/create_fire_regime.md)
  : Create fire regime
- [`fire_regime_instance()`](https://emf-creaf.github.io/medfateland/reference/fire_regime_instance.md)
  : Fire regime instance
- [`defaultPrescriptionsBySpecies`](https://emf-creaf.github.io/medfateland/reference/defaultPrescriptionsBySpecies.md)
  : Default prescriptions by species

## Simulation

Simulation model functions

- [`spwb_land()`](https://emf-creaf.github.io/medfateland/reference/spwb_land.md)
  [`growth_land()`](https://emf-creaf.github.io/medfateland/reference/spwb_land.md)
  [`fordyn_land()`](https://emf-creaf.github.io/medfateland/reference/spwb_land.md)
  [`summary(`*`<spwb_land>`*`)`](https://emf-creaf.github.io/medfateland/reference/spwb_land.md)
  [`summary(`*`<growth_land>`*`)`](https://emf-creaf.github.io/medfateland/reference/spwb_land.md)
  : Watershed simulations
- [`spwb_land_day()`](https://emf-creaf.github.io/medfateland/reference/spwb_land_day.md)
  [`growth_land_day()`](https://emf-creaf.github.io/medfateland/reference/spwb_land_day.md)
  : One-day watershed simulations
- [`spwb_spatial()`](https://emf-creaf.github.io/medfateland/reference/spwb_spatial.md)
  [`growth_spatial()`](https://emf-creaf.github.io/medfateland/reference/spwb_spatial.md)
  [`fordyn_spatial()`](https://emf-creaf.github.io/medfateland/reference/spwb_spatial.md)
  : Simulations for spatially-distributed forest stands
- [`spwb_spatial_day()`](https://emf-creaf.github.io/medfateland/reference/spwb_spatial_day.md)
  [`growth_spatial_day()`](https://emf-creaf.github.io/medfateland/reference/spwb_spatial_day.md)
  : One-day simulation for spatially-distributed forest stands
- [`fordyn_scenario()`](https://emf-creaf.github.io/medfateland/reference/fordyn_scenario.md)
  : Scenario of forest dynamics
- [`update_landscape()`](https://emf-creaf.github.io/medfateland/reference/update_landscape.md)
  : Updates the state of a landscape object
- [`initialize_landscape()`](https://emf-creaf.github.io/medfateland/reference/initialize_landscape.md)
  : Initialization of model inputs for spatially-distributed forest
  stands
- [`overland_routing()`](https://emf-creaf.github.io/medfateland/reference/overland_routing.md)
  [`cell_neighbors()`](https://emf-creaf.github.io/medfateland/reference/overland_routing.md)
  : Overland routing for TETIS sub-model
- [`dispersal()`](https://emf-creaf.github.io/medfateland/reference/dispersal.md)
  : Seed production, dispersal and seed bank dynamics

## Post-processing

Plot and summary functions for simulation results

- [`plot(`*`<spwb_land>`*`)`](https://emf-creaf.github.io/medfateland/reference/plot.spwb_land.md)
  [`plot(`*`<growth_land>`*`)`](https://emf-creaf.github.io/medfateland/reference/plot.spwb_land.md)
  [`plot(`*`<fordyn_land>`*`)`](https://emf-creaf.github.io/medfateland/reference/plot.spwb_land.md)
  : Displays watershed-level simulation results
- [`plot_summary()`](https://emf-creaf.github.io/medfateland/reference/plot_summary.md)
  : Displays spatial simulation summaries
- [`unnest_summary()`](https://emf-creaf.github.io/medfateland/reference/unnest_summary.md)
  : Extracts cell simulation summaries
- [`simulation_summary()`](https://emf-creaf.github.io/medfateland/reference/simulation_summary.md)
  : Summarizes spatial simulation results
- [`shinyplot(`*`<sf>`*`)`](https://emf-creaf.github.io/medfateland/reference/shinyplot.sf.md)
  : Shiny app with interactive plots and maps
- [`shinyplot(`*`<spwb_land>`*`)`](https://emf-creaf.github.io/medfateland/reference/shinyplot.spwb_land.md)
  [`shinyplot(`*`<growth_land>`*`)`](https://emf-creaf.github.io/medfateland/reference/shinyplot.spwb_land.md)
  [`shinyplot(`*`<fordyn_land>`*`)`](https://emf-creaf.github.io/medfateland/reference/shinyplot.spwb_land.md)
  : Shiny app with interactive plots and maps of watershed simulation
  results
