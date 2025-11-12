# Scenario of forest dynamics

Evaluates forest dynamics over a landscape including climate and
management scenarios

## Usage

``` r
fordyn_scenario(
  sf,
  SpParams,
  meteo = NULL,
  management_scenario,
  volume_function = NULL,
  volume_arguments = NULL,
  local_control = defaultControl(),
  dispersal_control = default_dispersal_control(),
  dates = NULL,
  CO2ByYear = numeric(0),
  fire_regime = NULL,
  summary_function = NULL,
  summary_arguments = NULL,
  parallelize = FALSE,
  num_cores = detectCores() - 1,
  chunk_size = NULL,
  progress = TRUE,
  verbose = FALSE
)
```

## Arguments

- sf:

  An object of class
  [`sf`](https://r-spatial.github.io/sf/reference/sf.html) with the
  following columns:

  - `geometry`: Spatial geometry.

  - `id`: Stand identifiers.

  - `elevation`: Elevation above sea level (in m).

  - `slope`: Slope (in degrees).

  - `aspect`: Aspect (in degrees).

  - `forest`: Objects of class
    [`forest`](https://emf-creaf.github.io/medfate/reference/forest.html).

  - `soil`: Objects of class
    [`soil`](https://emf-creaf.github.io/medfate/reference/soil.html).

  - `state`: Objects of class
    [`spwbInput`](https://emf-creaf.github.io/medfate/reference/modelInput.html)
    or
    [`growthInput`](https://emf-creaf.github.io/medfate/reference/modelInput.html)
    (optional).

  - `meteo`: Data frames with weather data (required if parameter
    `meteo = NULL`).

  - `management_unit`: Management unit corresponding to each stand.

  - `represented_area_ha`: Area represented by each stand in hectares.

  - `ignition_weights`: Relative weights to determine stands to be
    burned. Optional, relevant when `fire_regime` is supplied only).

  - `local_control`: A list of control parameters (optional). Used to
    override function parameter `local_control` for specific stands
    (values can be `NULL` for the remaining ones).

  Alternatively, the user may supply the result of a previous call to
  `fordyn_scenario`, where to continue simulations.

- SpParams:

  A data frame with species parameters (see
  [`SpParamsMED`](https://emf-creaf.github.io/medfate/reference/SpParams.html)).

- meteo:

  Meteorology data (see
  [`fordyn_spatial`](https://emf-creaf.github.io/medfateland/reference/spwb_spatial.md)).

- management_scenario:

  A list defining the management scenario (see
  [`create_management_scenario`](https://emf-creaf.github.io/medfateland/reference/create_management_scenario.md))

- volume_function:

  A function accepting a forest object or a tree data table, and a
  species parameter table, as input and returning the wood volume
  (m3/ha) corresponding to each tree cohort. The function may accept
  additional arguments. If NULL, the default volume function is used
  (not recommended!).

- volume_arguments:

  List with additional arguments for the volume function.

- local_control:

  A list of local model control parameters (see
  [`defaultControl`](https://emf-creaf.github.io/medfate/reference/defaultControl.html)).

- dispersal_control:

  A list of dispersal control parameters (see
  [`default_dispersal_control`](https://emf-creaf.github.io/medfateland/reference/default_dispersal_control.md)).
  If NULL, then dispersal is not simulated.

- dates:

  A [`Date`](https://rdrr.io/r/base/Dates.html) object with the days of
  the period to be simulated. If `NULL`, then the whole period of
  `meteo` is used.

- CO2ByYear:

  A named numeric vector with years as names and atmospheric CO2
  concentration (in ppm) as values. Used to specify annual changes in
  CO2 concentration along the simulation (as an alternative to
  specifying daily values in `meteo`).

- fire_regime:

  A list of parameters defining the fire regime (see
  [`create_fire_regime`](https://emf-creaf.github.io/medfateland/reference/create_fire_regime.md))
  or a matrix representing a fire regime instance (see
  [`fire_regime_instance`](https://emf-creaf.github.io/medfateland/reference/fire_regime_instance.md)).
  If NULL, wildfires are not simulated. Details are given in
  [`fordyn_spatial`](https://emf-creaf.github.io/medfateland/reference/spwb_spatial.md).

- summary_function:

  An appropriate function to calculate summaries from an object of class
  'fordyn' (e.g.,
  [`summary.fordyn`](https://emf-creaf.github.io/medfate/reference/summary.spwb.html)).

- summary_arguments:

  List with additional arguments for the summary function.

- parallelize:

  Boolean flag to try parallelization (will use all clusters minus one).

- num_cores:

  Integer with the number of cores to be used for parallel computation.

- chunk_size:

  Integer indicating the size of chunks to be sent to different
  processes (by default, the number of spatial elements divided by the
  number of cores).

- progress:

  Boolean flag to display progress information for simulations.

- verbose:

  Boolean flag to display additional console output.

## Value

An list of class 'fordyn_scenario' with the following elements:

- `result_sf`: An object of class 'sf' using a UTM projection and
  containing four elements:

  - `geometry`: Spatial geometry.

  - `id`: Stand id, taken from the input.

  - `tree_table`: A list of data frames for each simulated stand,
    containing the living trees at each time step.

  - `shrub_table`: A list of data frames for each simulated stand,
    containing the living shrub at each time step.

  - `dead_tree_table`: A list of data frames for each simulated stand,
    containing the dead trees at each time step.

  - `dead_shrub_table`: A list of data frames for each simulated stand,
    containing the dead shrub at each time step.

  - `cut_tree_table`: A list of data frames for each simulated stand,
    containing the cut trees at each time step.

  - `cut_shrub_table`: A list of data frames for each simulated stand,
    containing the cut shrub at each time step.

  - `summary`: A list of model output summaries for each simulated stand
    (if `summary_function` was not `NULL`).

- `result_volumes`: A data frame with initial, growth, extracted and
  final volumes (m3) by year. In demand-based scenarios volumes
  corresponding to species with demand are also included.

- `result_volumes_spp`: A data frame with growth and extracted volumes
  (m3) by species and year.

- `result_volumes_demand`: In demand-based scenarios target volumes are
  also included, a data frame with growth, target and extracted volumes
  (m3) by demand entity and year. .

- `next_sf`: An object of class 'sf' to continue simulations in
  subsequent calls to `fordyn_scenario`.

- `next_demand`: In demand-based scenarios, a list with information
  (i.e. demand offset by species and last volume growth) to modify
  demand in subsequent calls to `fordyn_scenario`.

## Details

This function allows coordinating the dynamics of simulated forest
stands via a management scenario defined at the landscape/regional level
(see different kinds of scenarios and how to specify them in
[`create_management_scenario`](https://emf-creaf.github.io/medfateland/reference/create_management_scenario.md)).

The input 'sf' object has to be in a Universal Transverse Mercator (UTM)
coordinate system (or any other projection using meters as length unit)
for appropriate behavior of dispersal sub-model.

For each year to be simulated, the function determines which forest
stands will be managed, possibly depending on the demand, and then calls
function
[`fordyn_spatial`](https://emf-creaf.github.io/medfateland/reference/spwb_spatial.md)
for one year (normally including parallelization). If the simulation of
some stands results in an error, the function will try to restore the
previous state of the forest stand for the next year steps. Finally, the
function evaluates how much of the specified demand has been fulfilled
and stores the results, including demand offsets to be applied the year
after.

Management is implemented using the
[`defaultManagementFunction`](https://emf-creaf.github.io/medfate/reference/defaultManagementFunction.html)
in medfate, meaning that management parameters need to follow the
structure of
[`defaultManagementArguments`](https://emf-creaf.github.io/medfate/reference/defaultManagementFunction.html)

Details about the inclusion of fire regimes in simulations are explained
in
[`fordyn_spatial`](https://emf-creaf.github.io/medfateland/reference/spwb_spatial.md).

## See also

[`fordyn_spatial`](https://emf-creaf.github.io/medfateland/reference/spwb_spatial.md),
[`create_management_scenario`](https://emf-creaf.github.io/medfateland/reference/create_management_scenario.md),
[`dispersal`](https://emf-creaf.github.io/medfateland/reference/dispersal.md)

## Author

Miquel De Cáceres Ainsa, CREAF

Aitor Améztegui, UdL

## Examples

``` r
# \donttest{
# Load example landscape data
data("example_ifn")
  
# Load example meteo data frame from package meteoland
data("examplemeteo")
  
# Load default medfate parameters
data("SpParamsMED")

# Creates scenario with one management unit and annual demand for P. nigra 
scen <- create_management_scenario(1, c("Pinus nigra/Pinus sylvestris" = 2300))

# Assign management unit to all stands
example_ifn$management_unit <- 1 

# Assume that each stand represents 1km2 = 100 ha
example_ifn$represented_area_ha <- 100

# Transform to UTM31 (necessary for dispersal)
example_ifn_utm31 <- sf::st_transform(example_ifn, crs = 32631)

# Subset three plots to speed-up calculations
example_subset <- example_ifn_utm31[31:33, ]

# Launch simulation scenario
fs_12 <- fordyn_scenario(example_subset, SpParamsMED, meteo = examplemeteo, 
                         volume_function = NULL, management_scenario = scen,
                         parallelize = FALSE)
#> 
#> ── Simulation of a management/fire scenario with fordyn ────────────────────────
#> ℹ Checking sf input
#> ✔ Checking sf input [7ms]
#> 
#> ℹ Checking meteo object input
#> ✔ Checking meteo object input [15ms]
#> 
#> 
#> ── Scenario parameters ──
#> 
#> • Number of stands: 3
#> • Represented area: 300 ha
#> • Number of years: 1
#> • Management scenario type: input_demand
#> • Adding column 'management_arguments'
#> • Default volume function
#> • Initial volume: 12072 m3
#> • Seed dispersal process included.
#> 
#> ── Simulation ──
#> 
#> ──  [ Year 2001 (1/1) ] 
#> • Demand (incl. offset): 2300 m3
#> • Determining available volumes and final cuts
#> • Demand (after final cuts): 2300 m3
#> • Determining thinning operations
#> • Seed bank dynamics and seed dispersal...
#> • Calling fordyn_spatial...
#> ℹ Checking sf input
#> ✔ Checking sf input [8ms]
#> 
#> ℹ Checking meteo object input
#> ✔ Checking meteo object input [14ms]
#> 
#> ℹ Performing 'fordyn' simulations on 3 locations
#> ✔ Performing 'fordyn' simulations on 3 locations [28ms]
#> 
#> Stands ■■■■■■■■■■■                       33% | ETA:  7s
#> Stands ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  4s
#> Stands ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100% | ETA:  0s
#> ✔ No simulation errors detected
#> • Final volume: 12230 m3
#> 
#> ── Arranging output ──
#> 
#> • Tree/shrub tables
#> • Wood volume table
# }
```
