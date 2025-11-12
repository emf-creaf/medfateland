# Simulations for spatially-distributed forest stands

Functions that allow calling local models
[`spwb`](https://emf-creaf.github.io/medfate/reference/spwb.html),
[`growth`](https://emf-creaf.github.io/medfate/reference/growth.html) or
[`fordyn`](https://emf-creaf.github.io/medfate/reference/fordyn.html),
for a set of forest stands distributed in specific locations. No spatial
processes are simulated.

## Usage

``` r
spwb_spatial(
  sf,
  SpParams,
  meteo = NULL,
  local_control = defaultControl(),
  dates = NULL,
  CO2ByYear = numeric(0),
  keep_results = TRUE,
  summary_function = NULL,
  summary_arguments = NULL,
  parallelize = FALSE,
  num_cores = detectCores() - 1,
  chunk_size = NULL,
  progress = TRUE,
  local_verbose = FALSE
)

growth_spatial(
  sf,
  SpParams,
  meteo = NULL,
  local_control = defaultControl(),
  dates = NULL,
  CO2ByYear = numeric(0),
  fire_regime = NULL,
  keep_results = TRUE,
  summary_function = NULL,
  summary_arguments = NULL,
  parallelize = FALSE,
  num_cores = detectCores() - 1,
  chunk_size = NULL,
  progress = TRUE,
  local_verbose = FALSE
)

fordyn_spatial(
  sf,
  SpParams,
  meteo = NULL,
  local_control = defaultControl(),
  dates = NULL,
  CO2ByYear = numeric(0),
  fire_regime = NULL,
  keep_results = TRUE,
  management_function = NULL,
  summary_function = NULL,
  summary_arguments = NULL,
  parallelize = FALSE,
  num_cores = detectCores() - 1,
  chunk_size = NULL,
  progress = TRUE,
  local_verbose = FALSE
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

  - `land_cover_type`: Land cover type of each grid cell (values should
    be 'wildland' or 'agriculture').

  - `forest`: Objects of class
    [`forest`](https://emf-creaf.github.io/medfate/reference/forest.html).

  - `soil`: Objects of class
    [`soil`](https://emf-creaf.github.io/medfate/reference/soil.html) or
    data frames of physical properties.

  - `state`: Objects of class
    [`spwbInput`](https://emf-creaf.github.io/medfate/reference/modelInput.html)
    or
    [`growthInput`](https://emf-creaf.github.io/medfate/reference/modelInput.html)
    (optional).

  - `meteo`: Data frames with weather data (required if parameter
    `meteo = NULL`).

  - `crop_factor`: Crop evapo-transpiration factor. Only required for
    'agriculture' land cover type.

  - `local_control`: A list of control parameters (optional). Used to
    override function parameter `local_control` for specific locations
    (values can be `NULL` for the remaining ones).

  - `management_arguments`: Lists with management arguments. Optional,
    relevant for `fordyn_spatial` only.

  - `represented_area_ha`: Area represented by each stand in hectares.
    Optional, relevant for `fordyn_spatial` when `fire_regime` is
    supplied only).

  - `ignition_weights`: Relative weights to determine stands to be
    burned. Optional, relevant for `fordyn_spatial` when `fire_regime`
    is supplied only).

- SpParams:

  A data frame with species parameters (see
  [`SpParamsMED`](https://emf-creaf.github.io/medfate/reference/SpParams.html)).

- meteo:

  Input meteorological data (see section details). If NULL, the function
  will expect a column 'meteo' in parameter `y`.

- local_control:

  A list of control parameters (see
  [`defaultControl`](https://emf-creaf.github.io/medfate/reference/defaultControl.html))
  for function
  [`spwb_day`](https://emf-creaf.github.io/medfate/reference/spwb_day.html)
  or
  [`growth_day`](https://emf-creaf.github.io/medfate/reference/spwb_day.html).

- dates:

  A [`Date`](https://rdrr.io/r/base/Dates.html) object describing the
  days of the period to be modeled.

- CO2ByYear:

  A named numeric vector with years as names and atmospheric CO2
  concentration (in ppm) as values. Used to specify annual changes in
  CO2 concentration along the simulation (as an alternative to
  specifying daily values in `meteo`).

- keep_results:

  Boolean flag to indicate that point/cell simulation results are to be
  returned (set to `FALSE` and use summary functions for large data
  sets).

- summary_function:

  An appropriate function to calculate summaries (e.g.,
  [`summary.spwb`](https://emf-creaf.github.io/medfate/reference/summary.spwb.html)).

- summary_arguments:

  List with additional arguments for the summary function.

- parallelize:

  Boolean flag to try parallelization (will use all clusters minus one).

- num_cores:

  Integer with the number of cores to be used for parallel computation.

- chunk_size:

  Integer indicating the size of chuncks to be sent to different
  processes (by default, the number of spatial elements divided by the
  number of cores).

- progress:

  Boolean flag to display progress information of simulations.

- local_verbose:

  Boolean flag to display detailed progress information in local
  simulations.

- fire_regime:

  A list of parameters defining the fire regime (see
  [`create_fire_regime`](https://emf-creaf.github.io/medfateland/reference/create_fire_regime.md))
  or a matrix representing a fire regime instance (see
  [`fire_regime_instance`](https://emf-creaf.github.io/medfateland/reference/fire_regime_instance.md)),
  to be used in simulations with `fordyn_spatial`. If NULL, wildfires
  are not simulated.

- management_function:

  A function that implements forest management actions (see
  [`fordyn`](https://emf-creaf.github.io/medfate/reference/fordyn.html)).
  of such lists, one per spatial unit.

## Value

An object of class 'sf' containing four elements:

- `geometry`: Spatial geometry.

- `id`: Stand id, taken from the input.

- `state`: A list of
  [`spwbInput`](https://emf-creaf.github.io/medfate/reference/modelInput.html)
  or
  [`growthInput`](https://emf-creaf.github.io/medfate/reference/modelInput.html)
  objects for each simulated stand, to be used in subsequent simulations
  (see
  [`update_landscape`](https://emf-creaf.github.io/medfateland/reference/update_landscape.md))
  or with NULL values whenever simulation errors occurred.

- `forest`: A list of
  [`forest`](https://emf-creaf.github.io/medfate/reference/forest.html)
  objects for each simulated stand (only in function `fordyn_spatial`),
  to be used in subsequent simulations (see
  [`update_landscape`](https://emf-creaf.github.io/medfateland/reference/update_landscape.md))
  or with NULL values whenever simulation errors occurred.

- `management_arguments`: A list of management arguments for each
  simulated stand (only in function `fordyn_spatial` if management
  function was supplied), to be used in subsequent simulations (see
  [`update_landscape`](https://emf-creaf.github.io/medfateland/reference/update_landscape.md)).

- `result`: A list of model output for each simulated stand. Some
  elements can contain an error condition if the simulation resulted in
  an error. Values will be NULL (or errors) if `keep_results = FALSE`.

- `summary`: A list of model output summaries for each simulated stand
  (if `summary_function` was not `NULL`), with NULL values whenever
  simulation errors occurred.

## Details

Simulation functions accept different formats for meteorological input
(parameter `meteo`). The user may supply two kinds of daily weather
sources:

1.  A data frame with meteorological data common for all spatial
    location (spatial variation of weather not considered).

2.  An object or (a list of objects) of class `stars` with reference
    interpolation data created by package
    [`meteoland`](https://emf-creaf.github.io/meteoland/reference/meteoland-package.html).
    If a list of such *interpolator* objects is supplied, the simulation
    functions will interpolate on the target locations for the periods
    covered by each interpolator, but the user will be responsible for
    supplying interpolators in the correct temporal order.

Alternatively, the user may leave parameter `meteo = NULL` and specify a
weather data frame for each element of `y` in a column named 'meteo'.

Fire regimes are only allowed for function `fordyn_spatial`. If an
object of class `fire_regime` is supplied, the function will call
[`fire_regime_instance`](https://emf-creaf.github.io/medfateland/reference/fire_regime_instance.md)
to generate a realization of the fire regime before conducting
simulations. Alternatively, users can directly supply a fire regime
instance matrix, derived from another source (e.g. a fire landscape
model). Note that operating with fire regimes assumes all forest stands
share the same period of simulation, but enforcing this is left to the
user.

## See also

[`spwb`](https://emf-creaf.github.io/medfate/reference/spwb.html),
[`growth`](https://emf-creaf.github.io/medfate/reference/growth.html),
[`fordyn`](https://emf-creaf.github.io/medfate/reference/fordyn.html),
[`spwb_spatial_day`](https://emf-creaf.github.io/medfateland/reference/spwb_spatial_day.md),
[`simulation_summary`](https://emf-creaf.github.io/medfateland/reference/simulation_summary.md)
,
[`plot_summary`](https://emf-creaf.github.io/medfateland/reference/plot_summary.md),
[`initialize_landscape`](https://emf-creaf.github.io/medfateland/reference/initialize_landscape.md),
[`update_landscape`](https://emf-creaf.github.io/medfateland/reference/update_landscape.md)

## Author

Miquel De Cáceres Ainsa, CREAF

## Examples

``` r
# \donttest{
# Load example landscape data
data("example_ifn")
  
# Load example meteo data frame from package meteoland
data("examplemeteo")
  
# Load default medfate parameters
data("SpParamsMED")
  
# Subset two plots to speed-up calculations
example_subset <- example_ifn[31:32, ]

# Perform simulation
dates <- seq(as.Date("2001-03-01"), as.Date("2001-03-15"), by="day")
res <- spwb_spatial(example_subset, SpParamsMED, examplemeteo, dates = dates)
#> 
#> ── Simulation of model 'spwb' ──────────────────────────────────────────────────
#> ℹ Checking sf input
#> ✔ Checking sf input [18ms]
#> 
#> ℹ Checking meteo object input
#> ✔ Checking meteo object input [34ms]
#> 
#> ℹ Creating 2 input objects for model 'spwb'
#> ✔ Creating 2 input objects for model 'spwb' [259ms]
#> 
#> ℹ Performing 'spwb' simulations on 2 locations
#> ✔ Performing 'spwb' simulations on 2 locations [12ms]
#> 
#> ✔ No simulation errors detected
  
# Perform fordyn simulation for one year (one stand) without management
res_noman <- fordyn_spatial(example_subset, SpParamsMED, examplemeteo)
#> 
#> ── Simulation of model 'fordyn' ────────────────────────────────────────────────
#> ℹ Checking sf input
#> ✔ Checking sf input [16ms]
#> 
#> ℹ Checking meteo object input
#> ✔ Checking meteo object input [32ms]
#> 
#> ℹ Performing 'fordyn' simulations on 2 locations
#> ✔ Performing 'fordyn' simulations on 2 locations [30ms]
#> 
#> Stands ■■■■■■■■■■■■■■■■                  50% | ETA:  6s
#> Stands ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100% | ETA:  0s
#> ✔ No simulation errors detected

# }
```
