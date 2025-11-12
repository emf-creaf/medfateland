# Watershed simulations

Functions to perform simulations on a watershed described by a set of
connected grid cells.

- Function `spwb_land` implements a distributed hydrological model that
  simulates daily local water balance, from
  [`spwb_day`](https://emf-creaf.github.io/medfate/reference/spwb_day.html),
  on grid cells of a watershed while accounting for overland runoff,
  subsurface flow and groundwater flow between cells.

- Function `growth_land` is similar to `spwb_land`, but includes daily
  local carbon balance, growth and mortality processes in grid cells,
  provided by
  [`growth_day`](https://emf-creaf.github.io/medfate/reference/spwb_day.html).

- Function `fordyn_land` extends the previous two functions with the
  simulation of management, seed dispersal, recruitment and resprouting.

## Usage

``` r
spwb_land(
  r,
  sf,
  SpParams,
  meteo = NULL,
  dates = NULL,
  CO2ByYear = numeric(0),
  summary_blocks = NULL,
  summary_frequency = "years",
  local_control = defaultControl(soilDomains = "single"),
  watershed_control = default_watershed_control(),
  parallelize = FALSE,
  num_cores = parallel::detectCores() - 1,
  progress = TRUE
)

growth_land(
  r,
  sf,
  SpParams,
  meteo = NULL,
  dates = NULL,
  CO2ByYear = numeric(0),
  summary_blocks = NULL,
  summary_frequency = "years",
  local_control = medfate::defaultControl(soilDomains = "single"),
  watershed_control = default_watershed_control(),
  parallelize = FALSE,
  num_cores = parallel::detectCores() - 1,
  progress = TRUE
)

fordyn_land(
  r,
  sf,
  SpParams,
  meteo = NULL,
  dates = NULL,
  CO2ByYear = numeric(0),
  summary_blocks = NULL,
  summary_frequency = "years",
  local_control = medfate::defaultControl(soilDomains = "single"),
  watershed_control = default_watershed_control(),
  dispersal_control = default_dispersal_control(),
  management_function = NULL,
  parallelize = FALSE,
  num_cores = parallel::detectCores() - 1,
  progress = TRUE
)

# S3 method for class 'spwb_land'
summary(object, ...)

# S3 method for class 'growth_land'
summary(object, ...)
```

## Arguments

- r:

  An object of class
  [`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html),
  defining the raster topology.

- sf:

  An object of class
  [`sf`](https://r-spatial.github.io/sf/reference/sf.html) with the
  following columns:

  - `geometry`: Spatial point geometry corresponding to cell centers.

  - `elevation`: Elevation above sea level (in m).

  - `slope`: Slope (in degrees).

  - `aspect`: Aspect (in degrees).

  - `land_cover_type`: Land cover type of each grid cell (values should
    be 'wildland', 'agriculture', 'rock', 'artificial' or 'water').

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
    override function parameter `local_control` for specific cells
    (values can be `NULL` for the remaining ones).

  - `snowpack`: An optional numeric vector with the snow water
    equivalent content of the snowpack in each cell (in mm). If missing
    it will be initialized to zero.

  - `management_arguments`: Lists with management arguments (optional,
    relevant for `fordyn_land` only).

  - `result_cell`: A logical indicating that local model results are
    desired (optional, relevant for `spwb_land` and `growth_land` only).
    Model results are only produced for wildland and agriculture cells.

  When using TETIS watershed model, the following columns are also
  REQUIRED:

  - `depth_to_bedrock`: Depth to bedrock (mm).

  - `bedrock_conductivity`: Bedrock (saturated) conductivity (in
    m·day-1).

  - `bedrock_porosity`: Bedrock porosity (the proportion of pore space
    in the rock).

  When using TETIS watershed model, the following columns are OPTIONAL:

  - `aquifer`: A numeric vector with the water content of the aquifer in
    each cell (in mm). If missing, it will be initialized to zero.

  - `deep_aquifer_loss`: A numeric vector with the maximum daily loss to
    a deeper aquifer (in mm·day-1). If missing all cells take their
    value from `deep_aquifer_loss` in
    [`default_watershed_control`](https://emf-creaf.github.io/medfateland/reference/default_watershed_control.md)

  - `channel`: A logical (or binary) vector indicating overland channel
    routing.

  - `outlet_backlog`: A vector indicating, for outlet cells, backlog
    volume of water (m3) of the corresponding channel network from a
    previous simulation.

- SpParams:

  A data frame with species parameters (see
  [`SpParamsMED`](https://emf-creaf.github.io/medfate/reference/SpParams.html)).
  IMPORTANT: If `sf` has been already initialized, this parameter has no
  effect.

- meteo:

  Input meteorological data (see
  [`spwb_spatial`](https://emf-creaf.github.io/medfateland/reference/spwb_spatial.md)
  and details).

- dates:

  A [`Date`](https://rdrr.io/r/base/Dates.html) object describing the
  days of the period to be modeled.

- CO2ByYear:

  A named numeric vector with years as names and atmospheric CO2
  concentration (in ppm) as values. Used to specify annual changes in
  CO2 concentration along the simulation (as an alternative to
  specifying daily values in `meteo`).

- summary_blocks:

  A character vector with variable blocks for cell summaries (or `NULL`
  to retain only basic summaries). Accepted summary blocks for
  `spwb_land` are "WaterBalance", "Stand" and "FireHazard". For
  `growth_land` and `fordyn_land`, "CarbonBalance" and "BiomassBalance"
  are also accepted.

- summary_frequency:

  Frequency in which cell summary will be produced (e.g. "years",
  "months", "weeks", ...) (see
  [`cut.Date`](https://rdrr.io/r/base/cut.POSIXt.html)). In
  `fordyn_land` summary frequency can only be "months" or "years".

- local_control:

  A list of control parameters (see
  [`defaultControl`](https://emf-creaf.github.io/medfate/reference/defaultControl.html))
  for function
  [`spwb_day`](https://emf-creaf.github.io/medfate/reference/spwb_day.html)
  or
  [`growth_day`](https://emf-creaf.github.io/medfate/reference/spwb_day.html).
  By default, parameter `soilDomains` is set to `"single"`, meaning a
  single-domain Richards model. IMPORTANT: If `sf` has been already
  initialized, this parameter has no effect.

- watershed_control:

  A list of watershed control parameters (see
  [`default_watershed_control`](https://emf-creaf.github.io/medfateland/reference/default_watershed_control.md)).
  Importantly, the sub-model used for lateral water flows - either
  Francés et al. (2007) or Caviedes-Voullième et al. (2023) - is
  specified there.

- parallelize:

  Boolean flag to try parallelization (only works with subwatersheds,
  see details).

- num_cores:

  Integer with the number of cores to be used for parallel computation
  (by default it will use all clusters minus one).

- progress:

  Boolean flag to display progress information for simulations.

- dispersal_control:

  A list of dispersal control parameters (see
  [`default_dispersal_control`](https://emf-creaf.github.io/medfateland/reference/default_dispersal_control.md)).
  If NULL, then dispersal is not simulated.

- management_function:

  A function that implements forest management actions (see
  [`fordyn`](https://emf-creaf.github.io/medfate/reference/fordyn.html)).
  of such lists, one per spatial unit.

- object:

  An object of class `spwb_land` or `groth_land`

- ...:

  Additional parameters for summary functions

## Value

Functions `spwb_land`, `growth_land` and `fordyn_land` return a list of
class of the same name as the function with the following elements:

- `watershed_control`: A list with input control parameters.

- `sf`: An object of class
  [`sf`](https://r-spatial.github.io/sf/reference/sf.html), similar to
  the output of
  [`spwb_spatial`](https://emf-creaf.github.io/medfateland/reference/spwb_spatial.md),
  with the following columns:

  - `geometry`: Spatial geometry.

  - `state`: A list of model input objects for each simulated stand.

  - `aquifer`: A numeric vector with the water volume in the aquifer of
    each cell.

  - `snowpack`: A numeric vector with the snowpack water equivalent
    volume of each cell.

  - `summary`: A list of cell summaries containing at least the
    following variables (additional variables are summarized using
    `summary_blocks`):

    - `MinTemperature`: Minimum temperature (degrees Celsius).

    - `MaxTemperature`: Maximum temperature (degrees Celsius).

    - `PET`: Potential evapotranspiration (in mm).

    - `Rain`: Rainfall (in mm).

    - `Snow`: Snowfall (in mm).

    - `SWE`: Snow water equivalent (in mm) of the snowpack.

    - `RWC`: Soil relative water content with respect to field capacity
      (in percent).

    - `SoilVol`: Soil water volume integrated across vertical layers (in
      mm).

    - `WTD`: Saturated soil water table depth (in mm from surface).

    - `DTA`: Depth to aquifer (in m from surface).

  - `result`: A list of cell detailed results (only for those indicated
    in the input), with contents depending on the local model.

  - `outlet`: A logical vector indicating outlet cells.

  - `channel`: A logical vector indicating channel cells.

  - `target_outlet`: Index of the outlet cell to which the channel leads
    (`NA` for non-channel cells).

  - `outlet_backlog`: A vector indicating channel water volume (m3)
    backlog of outlet cells (for subsequent simulations).

  - `subwatershed`: Integer vector indicating watershed subunits (`NA`
    if `subwatersheds = FALSE` in watershed control parameters).

  In function `fordyn_land` the
  [`sf`](https://r-spatial.github.io/sf/reference/sf.html) object
  contains additional columns:

  - `forest`: A list of
    [`forest`](https://emf-creaf.github.io/medfate/reference/forest.html)
    objects for each simulated stand, to be used in subsequent
    simulations (see
    [`update_landscape`](https://emf-creaf.github.io/medfateland/reference/update_landscape.md)).

  - `management_arguments`: A list of management arguments for each
    simulated stand, to be used in subsequent simulations (see
    [`update_landscape`](https://emf-creaf.github.io/medfateland/reference/update_landscape.md)).

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

- `watershed_balance`: A data frame with as many rows as days and where
  columns are (spatially-averaged) components of the water balance at
  the watershed level (i.e., rain, snow, interception, infiltration,
  soil evaporation, plant transpiration, ...).

- `watershed_soil_balance`: A data frame with as many rows as days and
  where columns are (spatially-averaged) components of the water balance
  at the watershed level restricted to those cells with a soil
  definition.

- `channel_export_m3s`: A matrix with daily values of runoff (in m3/s)
  reaching each of the channel cells of the landscape (useful for
  channel processing with an external model).

- `outlet_export_m3s`: A matrix with daily values of runoff (in m3/s)
  reaching each of the outlet cells of the landscape. Each outlet drains
  its own subset of cells (sometimes including channel routing), so the
  daily overall watershed export corresponds to the sum of row values.

## Details

IMPORTANT: Simulation function will normally call the initialization of
state variables via an internal call to
[`initialize_landscape`](https://emf-creaf.github.io/medfateland/reference/initialize_landscape.md),
using parameters `local_control` and `SpParams` in this call. The
default `soilDomains = "single"` means that vertical bulk soil flows are
simulated using a single permeability domain with Richards equation.
However, if object `sf` has been previously initialized, then the
control parameters of this previous initialization will remain the same.
In other words, parameters `local_control` and `SpParams` will have no
effect in the call to the simulation routines if the `sf` has been
previously initialized.

Two sub-models are available for lateral water transfer processes
(overland flow, sub-surface flow, etc.), either "TETIS" (similar to
Francés et al. 2007) or "SERGHEI" (Caviedes-Voullième et al. 2023).

IMPORTANT: medfateland needs to be compiled along with SERGHEI model in
order to launch simulations with using this distributed hydrological
model.

When running `fordyn_land`, the input 'sf' object has to be in a
Universal Transverse Mercator (UTM) coordinate system (or any other
projection using meters as length unit) for appropriate behavior of
dispersal sub-model.

Due to the large communication overload, parallel computation is only
allowed for TETIS in combination with definition of subwatersheds (see
flag of TETIS parameters in
[`default_watershed_control`](https://emf-creaf.github.io/medfateland/reference/default_watershed_control.md)).

When dealing with large data sets, weather data included in the 'sf'
object will likely be very data hungry. In those cases, it is
recommended to resort on weather interpolation (see
[`spwb_spatial`](https://emf-creaf.github.io/medfateland/reference/spwb_spatial.md)).
Weather interpolation can be done using a coarser resolution than that
of raster 'r', by changing the watershed control parameter called
'weather_aggregation_factor' (see
[`default_watershed_control`](https://emf-creaf.github.io/medfateland/reference/default_watershed_control.md)).

## References

Francés, F., Vélez, J.I. & Vélez, J.J. (2007). Split-parameter structure
for the automatic calibration of distributed hydrological models.
Journal of Hydrology, 332, 226–240.

Caviedes-Voullième, D., Morales-Hernández, M., Norman, M.R. &
Ogzen-Xian, I. (2023). SERGHEI (SERGHEI-SWE) v1.0: a
performance-portable high-performance parallel-computing shallow-water
solver for hydrology and environmental hydraulics. Geoscientific Model
Development, 16, 977-1008.

## See also

[`default_watershed_control`](https://emf-creaf.github.io/medfateland/reference/default_watershed_control.md),
[`initialize_landscape`](https://emf-creaf.github.io/medfateland/reference/initialize_landscape.md),
[`overland_routing`](https://emf-creaf.github.io/medfateland/reference/overland_routing.md),
[`spwb_land_day`](https://emf-creaf.github.io/medfateland/reference/spwb_land_day.md),
[`spwb_day`](https://emf-creaf.github.io/medfate/reference/spwb_day.html),
[`growth_day`](https://emf-creaf.github.io/medfate/reference/spwb_day.html),
[`spwb_spatial`](https://emf-creaf.github.io/medfateland/reference/spwb_spatial.md),
[`fordyn_spatial`](https://emf-creaf.github.io/medfateland/reference/spwb_spatial.md),
[`dispersal`](https://emf-creaf.github.io/medfateland/reference/dispersal.md),
[`unnest_summary`](https://emf-creaf.github.io/medfateland/reference/unnest_summary.md)

## Author

Miquel De Cáceres Ainsa, CREAF.

Maria González-Sanchís, Universitat Politecnica de Valencia.

Daniel Caviedes-Voullième, Forschungszentrum Julich.

Mario Morales-Hernández, Universidad de Zaragoza.

## Examples

``` r
# \donttest{
# Load example watershed data
data("example_watershed")

# Set crop factor 
example_watershed$crop_factor <- NA
example_watershed$crop_factor[example_watershed$land_cover_type=="agriculture"] <- 0.75

# Set request for daily model results in cells number 3, 6 (outlet) and 9
example_watershed$result_cell <- FALSE
example_watershed$result_cell[c(3,6,9)] <- TRUE

# Get bounding box to determine limits
b <- sf::st_bbox(example_watershed)
b
#>    xmin    ymin    xmax    ymax 
#>  401430 4671870  402830 4672570 

# Define a raster topology, using terra package, 
# with the same CRS as the watershed. In this example cells have 100 m side.
# Coordinates in the 'sf' object are assumed to be cell centers
r <-terra::rast(xmin = 401380, ymin = 4671820, xmax = 402880, ymax = 4672620, 
                nrow = 8, ncol = 15, crs = "epsg:32631")

# Load example meteo data frame from package meteoland
data("examplemeteo")
  
# Load default medfate parameters
data("SpParamsMED")
  
# Set simulation period
dates <- seq(as.Date("2001-01-01"), as.Date("2001-03-31"), by="day")

# Watershed control parameters (TETIS model; Frances et al. 2007)
ws_control <- default_watershed_control("tetis")

# Launch simulations 
res <- spwb_land(r, example_watershed, SpParamsMED, examplemeteo, 
                 dates = dates, summary_frequency = "month",
                 watershed_control = ws_control)
#> 
#> ── Simulation of model 'spwb' over a watershed ─────────────────────────────────
#> 
#> ── INPUT CHECKING ──
#> 
#> ℹ Checking raster topology
#> ✔ Checking raster topology [9ms]
#> 
#> ℹ Checking 'sf' data columns
#> ✔ Checking 'sf' data columns [32ms]
#> 
#> ℹ Determining neighbors and overland routing for TETIS
#> ✔ Determining neighbors and overland routing for TETIS [18ms]
#> 
#> • Hydrological model: TETIS
#> • Number of grid cells: 120 Number of target cells: 66
#> • Average cell area: 10006 m2, Total area: 120 ha, Target area: 66 ha
#> • Cell land use [wildland: 48 agriculture: 17 artificial: 0 rock: 1 water: 0]
#> • Cells with soil: 65
#> • Number of days to simulate: 90
#> • Number of temporal cell summaries: 3
#> • Number of cells with daily model results requested: 3
#> • Number of channel cells: 0
#> • Number of outlet cells: 1
#> 
#> ── INITIALISATION ──
#> 
#> ℹ Creating 65 state objects for model 'spwb'.
#> ✔ Creating 65 state objects for model 'spwb'. [6ms]
#> 
#> • Transpiration mode [Granier: 65, Sperry: 0, Sureau: 0]
#> • Soil domains [buckets: 0, single: 65, dual: 0]
#> 
#> ── WHOLE-WATERSHED SIMULATION ──
#> 
#> Daily simulations ■■■■                              10% | ETA:  9s
#> Daily simulations ■■■■                              11% | ETA: 10s
#> Daily simulations ■■■■■                             12% | ETA: 10s
#> Daily simulations ■■■■■                             13% | ETA: 10s
#> Daily simulations ■■■■■                             14% | ETA: 11s
#> Daily simulations ■■■■■■                            16% | ETA: 11s
#> Daily simulations ■■■■■■                            17% | ETA: 11s
#> Daily simulations ■■■■■■■                           19% | ETA: 11s
#> Daily simulations ■■■■■■■                           20% | ETA: 11s
#> Daily simulations ■■■■■■■                           21% | ETA: 11s
#> Daily simulations ■■■■■■■■                          22% | ETA: 11s
#> Daily simulations ■■■■■■■■                          23% | ETA: 11s
#> Daily simulations ■■■■■■■■                          24% | ETA: 11s
#> Daily simulations ■■■■■■■■■                         26% | ETA: 11s
#> Daily simulations ■■■■■■■■■                         27% | ETA: 11s
#> Daily simulations ■■■■■■■■■                         28% | ETA: 11s
#> Daily simulations ■■■■■■■■■■                        29% | ETA: 11s
#> Daily simulations ■■■■■■■■■■                        30% | ETA: 11s
#> Daily simulations ■■■■■■■■■■                        31% | ETA: 11s
#> Daily simulations ■■■■■■■■■■■                       32% | ETA: 11s
#> Daily simulations ■■■■■■■■■■■                       33% | ETA: 10s
#> Daily simulations ■■■■■■■■■■■                       34% | ETA: 10s
#> Daily simulations ■■■■■■■■■■■■                      36% | ETA: 10s
#> Daily simulations ■■■■■■■■■■■■                      37% | ETA: 10s
#> Daily simulations ■■■■■■■■■■■■                      38% | ETA: 10s
#> Daily simulations ■■■■■■■■■■■■■                     39% | ETA: 10s
#> Daily simulations ■■■■■■■■■■■■■                     40% | ETA: 10s
#> Daily simulations ■■■■■■■■■■■■■                     41% | ETA: 10s
#> Daily simulations ■■■■■■■■■■■■■■                    42% | ETA:  9s
#> Daily simulations ■■■■■■■■■■■■■■                    43% | ETA:  9s
#> Daily simulations ■■■■■■■■■■■■■■                    44% | ETA:  9s
#> Daily simulations ■■■■■■■■■■■■■■■                   46% | ETA:  9s
#> Daily simulations ■■■■■■■■■■■■■■■                   47% | ETA:  9s
#> Daily simulations ■■■■■■■■■■■■■■■                   48% | ETA:  9s
#> Daily simulations ■■■■■■■■■■■■■■■■                  49% | ETA:  9s
#> Daily simulations ■■■■■■■■■■■■■■■■                  50% | ETA:  8s
#> Daily simulations ■■■■■■■■■■■■■■■■                  51% | ETA:  8s
#> Daily simulations ■■■■■■■■■■■■■■■■■                 52% | ETA:  8s
#> Daily simulations ■■■■■■■■■■■■■■■■■                 53% | ETA:  8s
#> Daily simulations ■■■■■■■■■■■■■■■■■                 54% | ETA:  8s
#> Daily simulations ■■■■■■■■■■■■■■■■■■                56% | ETA:  8s
#> Daily simulations ■■■■■■■■■■■■■■■■■■                57% | ETA:  8s
#> Daily simulations ■■■■■■■■■■■■■■■■■■                58% | ETA:  7s
#> Daily simulations ■■■■■■■■■■■■■■■■■■■               59% | ETA:  7s
#> Daily simulations ■■■■■■■■■■■■■■■■■■■               60% | ETA:  7s
#> Daily simulations ■■■■■■■■■■■■■■■■■■■               61% | ETA:  7s
#> Daily simulations ■■■■■■■■■■■■■■■■■■■■              62% | ETA:  7s
#> Daily simulations ■■■■■■■■■■■■■■■■■■■■              63% | ETA:  7s
#> Daily simulations ■■■■■■■■■■■■■■■■■■■■              64% | ETA:  7s
#> Daily simulations ■■■■■■■■■■■■■■■■■■■■■             66% | ETA:  6s
#> Daily simulations ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  6s
#> Daily simulations ■■■■■■■■■■■■■■■■■■■■■             68% | ETA:  6s
#> Daily simulations ■■■■■■■■■■■■■■■■■■■■■■            69% | ETA:  7s
#> Daily simulations ■■■■■■■■■■■■■■■■■■■■■■            70% | ETA:  6s
#> Daily simulations ■■■■■■■■■■■■■■■■■■■■■■            71% | ETA:  6s
#> Daily simulations ■■■■■■■■■■■■■■■■■■■■■■■           72% | ETA:  6s
#> Daily simulations ■■■■■■■■■■■■■■■■■■■■■■■           73% | ETA:  6s
#> Daily simulations ■■■■■■■■■■■■■■■■■■■■■■■           74% | ETA:  5s
#> Daily simulations ■■■■■■■■■■■■■■■■■■■■■■■■          76% | ETA:  5s
#> Daily simulations ■■■■■■■■■■■■■■■■■■■■■■■■          77% | ETA:  5s
#> Daily simulations ■■■■■■■■■■■■■■■■■■■■■■■■          78% | ETA:  5s
#> Daily simulations ■■■■■■■■■■■■■■■■■■■■■■■■■         79% | ETA:  4s
#> Daily simulations ■■■■■■■■■■■■■■■■■■■■■■■■■         80% | ETA:  4s
#> Daily simulations ■■■■■■■■■■■■■■■■■■■■■■■■■         81% | ETA:  4s
#> Daily simulations ■■■■■■■■■■■■■■■■■■■■■■■■■■        82% | ETA:  4s
#> Daily simulations ■■■■■■■■■■■■■■■■■■■■■■■■■■        83% | ETA:  4s
#> Daily simulations ■■■■■■■■■■■■■■■■■■■■■■■■■■        84% | ETA:  3s
#> Daily simulations ■■■■■■■■■■■■■■■■■■■■■■■■■■■       86% | ETA:  3s
#> Daily simulations ■■■■■■■■■■■■■■■■■■■■■■■■■■■       87% | ETA:  3s
#> Daily simulations ■■■■■■■■■■■■■■■■■■■■■■■■■■■       88% | ETA:  3s
#> Daily simulations ■■■■■■■■■■■■■■■■■■■■■■■■■■■■      89% | ETA:  2s
#> Daily simulations ■■■■■■■■■■■■■■■■■■■■■■■■■■■■      90% | ETA:  2s
#> Daily simulations ■■■■■■■■■■■■■■■■■■■■■■■■■■■■      91% | ETA:  2s
#> Daily simulations ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■     92% | ETA:  2s
#> Daily simulations ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■     93% | ETA:  2s
#> Daily simulations ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■     94% | ETA:  1s
#> Daily simulations ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■    96% | ETA:  1s
#> Daily simulations ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■    97% | ETA:  1s
#> Daily simulations ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■    98% | ETA:  1s
#> Daily simulations ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■   99% | ETA:  0s
#> Daily simulations ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100% | ETA:  0s
#> • Snowpack balance content (mm): 0 fluxes (mm): 0
#> • Soil balance content (mm): -45.68 fluxes (mm): -45.68
#> • Aquifer balance content (mm): 23.69 fluxes (mm): 23.69
#> • Aquifer fluxes (mm) Drainage input: 88.31 Exfiltration: 64.62 Capillary rise:
#> 0 Negative aquifer correction: 0 Deep loss: 0
#> • Watershed balance content (mm): -21.3 fluxes (mm): -21.3
#> • Watershed fluxes (mm) Precipitation: 154.74 Surface export: 64.62
#> Evapotransp.: 111.42 Negative aquifer correction: 0 Deep loss: 0
#> 
#> ── FINAL BALANCE CHECK ──
#> 
#> • Final channel sum (m3): 0
#> • Final outlet sum (m3): 42672
#> • Final watershed export sum (m3): 42672
                 
# Print a summary of water balance components
summary(res)
#>   Snowpack water balance components:
#>     Snow fall (mm) 19.19  Snow melt (mm) 19.19
#>   Soil water balance components:
#>     Infiltration (mm) 122.87  Saturation excess (mm) 0
#>     Deep drainage (mm) 87.8  Capillarity rise (mm) 0
#>     Soil evaporation (mm) 2.59  Plant transpiration (mm) 78.16
#>     Interflow balance (mm) 0
#>   Aquifer water balance components:
#>     Deep drainage (mm) 88.31  Capillarity rise (mm) 0
#>     Exfiltration (mm) 64.62  Deep aquifer loss (mm) 0
#>     Negative aquifer correction (mm) 0
#>   Watershed water balance components:
#>     Precipitation (mm) 154.74
#>     Interception (mm) 31.9  Soil evaporation (mm) 2.55
#>     Plant transpiration (mm) 76.98
#>     Subsurface flow balance (mm) 0
#>     Groundwater flow balance (mm) 0
#>     Export runoff (mm) 64.62

# Option 'reduce_to_dominant = TRUE' in initialization, may be useful to speed up calculations
example_simplified <- initialize_landscape(example_watershed, SpParams = SpParamsMED,
                                           local_control = defaultControl(soilDomains = "single"), 
                                           reduce_to_dominant = TRUE)
#> ℹ Creating 65 state objects for model 'spwb'.
#> ✔ Creating 65 state objects for model 'spwb'. [8ms]
#> 
#> • Transpiration mode [Granier: 65, Sperry: 0, Sureau: 0]
#> • Soil domains [buckets: 0, single: 65, dual: 0]

# Launch simulations over simplified landscape (should be considerably faster)
res_simplified <- spwb_land(r, example_simplified, SpParamsMED, examplemeteo, 
                            dates = dates, summary_frequency = "month",
                            watershed_control = ws_control)
#> 
#> ── Simulation of model 'spwb' over a watershed ─────────────────────────────────
#> 
#> ── INPUT CHECKING ──
#> 
#> ℹ Checking raster topology
#> ✔ Checking raster topology [21ms]
#> 
#> ℹ Checking 'sf' data columns
#> ✔ Checking 'sf' data columns [110ms]
#> 
#> ℹ Determining neighbors and overland routing for TETIS
#> ✔ Determining neighbors and overland routing for TETIS [35ms]
#> 
#> • Hydrological model: TETIS
#> • Number of grid cells: 120 Number of target cells: 66
#> • Average cell area: 10006 m2, Total area: 120 ha, Target area: 66 ha
#> • Cell land use [wildland: 48 agriculture: 17 artificial: 0 rock: 1 water: 0]
#> • Cells with soil: 65
#> • Number of days to simulate: 90
#> • Number of temporal cell summaries: 3
#> • Number of cells with daily model results requested: 3
#> • Number of channel cells: 0
#> • Number of outlet cells: 1
#> 
#> ── INITIALISATION ──
#> 
#> ℹ All state objects are already available for 'spwb'.
#> • Transpiration mode [Granier: 65, Sperry: 0, Sureau: 0]
#> • Soil domains [buckets: 0, single: 65, dual: 0]
#> 
#> ── WHOLE-WATERSHED SIMULATION ──
#> 
#> Daily simulations ■■■                                8% | ETA: 14s
#> Daily simulations ■■■■                               9% | ETA: 17s
#> Daily simulations ■■■■                              10% | ETA: 18s
#> Daily simulations ■■■■                              11% | ETA: 19s
#> Daily simulations ■■■■■                             12% | ETA: 20s
#> Daily simulations ■■■■■                             13% | ETA: 22s
#> Daily simulations ■■■■■                             14% | ETA: 23s
#> Daily simulations ■■■■■■                            16% | ETA: 24s
#> Daily simulations ■■■■■■                            17% | ETA: 25s
#> Daily simulations ■■■■■■                            18% | ETA: 26s
#> Daily simulations ■■■■■■■                           19% | ETA: 26s
#> Daily simulations ■■■■■■■                           20% | ETA: 26s
#> Daily simulations ■■■■■■■                           21% | ETA: 27s
#> Daily simulations ■■■■■■■■                          22% | ETA: 27s
#> Daily simulations ■■■■■■■■                          23% | ETA: 27s
#> Daily simulations ■■■■■■■■                          24% | ETA: 27s
#> Daily simulations ■■■■■■■■■                         26% | ETA: 27s
#> Daily simulations ■■■■■■■■■                         27% | ETA: 26s
#> Daily simulations ■■■■■■■■■                         28% | ETA: 26s
#> Daily simulations ■■■■■■■■■■                        29% | ETA: 25s
#> Daily simulations ■■■■■■■■■■                        30% | ETA: 25s
#> Daily simulations ■■■■■■■■■■                        31% | ETA: 25s
#> Daily simulations ■■■■■■■■■■■                       32% | ETA: 25s
#> Daily simulations ■■■■■■■■■■■                       33% | ETA: 24s
#> Daily simulations ■■■■■■■■■■■                       34% | ETA: 24s
#> Daily simulations ■■■■■■■■■■■■                      36% | ETA: 24s
#> Daily simulations ■■■■■■■■■■■■                      37% | ETA: 23s
#> Daily simulations ■■■■■■■■■■■■                      38% | ETA: 23s
#> Daily simulations ■■■■■■■■■■■■■                     39% | ETA: 23s
#> Daily simulations ■■■■■■■■■■■■■                     40% | ETA: 22s
#> Daily simulations ■■■■■■■■■■■■■                     41% | ETA: 22s
#> Daily simulations ■■■■■■■■■■■■■■                    42% | ETA: 21s
#> Daily simulations ■■■■■■■■■■■■■■                    43% | ETA: 21s
#> Daily simulations ■■■■■■■■■■■■■■                    44% | ETA: 20s
#> Daily simulations ■■■■■■■■■■■■■■■                   46% | ETA: 20s
#> Daily simulations ■■■■■■■■■■■■■■■                   47% | ETA: 20s
#> Daily simulations ■■■■■■■■■■■■■■■                   48% | ETA: 19s
#> Daily simulations ■■■■■■■■■■■■■■■■                  49% | ETA: 19s
#> Daily simulations ■■■■■■■■■■■■■■■■                  50% | ETA: 18s
#> Daily simulations ■■■■■■■■■■■■■■■■                  51% | ETA: 18s
#> Daily simulations ■■■■■■■■■■■■■■■■■                 52% | ETA: 17s
#> Daily simulations ■■■■■■■■■■■■■■■■■                 53% | ETA: 17s
#> Daily simulations ■■■■■■■■■■■■■■■■■                 54% | ETA: 16s
#> Daily simulations ■■■■■■■■■■■■■■■■■■                56% | ETA: 16s
#> Daily simulations ■■■■■■■■■■■■■■■■■■                57% | ETA: 15s
#> Daily simulations ■■■■■■■■■■■■■■■■■■                58% | ETA: 15s
#> Daily simulations ■■■■■■■■■■■■■■■■■■■               59% | ETA: 14s
#> Daily simulations ■■■■■■■■■■■■■■■■■■■               60% | ETA: 14s
#> Daily simulations ■■■■■■■■■■■■■■■■■■■               61% | ETA: 14s
#> Daily simulations ■■■■■■■■■■■■■■■■■■■■              62% | ETA: 13s
#> Daily simulations ■■■■■■■■■■■■■■■■■■■■              63% | ETA: 13s
#> Daily simulations ■■■■■■■■■■■■■■■■■■■■              64% | ETA: 12s
#> Daily simulations ■■■■■■■■■■■■■■■■■■■■■             66% | ETA: 12s
#> Daily simulations ■■■■■■■■■■■■■■■■■■■■■             67% | ETA: 12s
#> Daily simulations ■■■■■■■■■■■■■■■■■■■■■             68% | ETA: 11s
#> Daily simulations ■■■■■■■■■■■■■■■■■■■■■■            69% | ETA: 11s
#> Daily simulations ■■■■■■■■■■■■■■■■■■■■■■            70% | ETA: 10s
#> Daily simulations ■■■■■■■■■■■■■■■■■■■■■■            71% | ETA: 10s
#> Daily simulations ■■■■■■■■■■■■■■■■■■■■■■■           72% | ETA: 10s
#> Daily simulations ■■■■■■■■■■■■■■■■■■■■■■■           73% | ETA:  9s
#> Daily simulations ■■■■■■■■■■■■■■■■■■■■■■■           74% | ETA:  9s
#> Daily simulations ■■■■■■■■■■■■■■■■■■■■■■■■          76% | ETA:  8s
#> Daily simulations ■■■■■■■■■■■■■■■■■■■■■■■■          77% | ETA:  8s
#> Daily simulations ■■■■■■■■■■■■■■■■■■■■■■■■          78% | ETA:  8s
#> Daily simulations ■■■■■■■■■■■■■■■■■■■■■■■■■         79% | ETA:  7s
#> Daily simulations ■■■■■■■■■■■■■■■■■■■■■■■■■         80% | ETA:  7s
#> Daily simulations ■■■■■■■■■■■■■■■■■■■■■■■■■         81% | ETA:  7s
#> Daily simulations ■■■■■■■■■■■■■■■■■■■■■■■■■■        82% | ETA:  6s
#> Daily simulations ■■■■■■■■■■■■■■■■■■■■■■■■■■        83% | ETA:  6s
#> Daily simulations ■■■■■■■■■■■■■■■■■■■■■■■■■■        84% | ETA:  5s
#> Daily simulations ■■■■■■■■■■■■■■■■■■■■■■■■■■■       86% | ETA:  5s
#> Daily simulations ■■■■■■■■■■■■■■■■■■■■■■■■■■■       87% | ETA:  5s
#> Daily simulations ■■■■■■■■■■■■■■■■■■■■■■■■■■■       88% | ETA:  4s
#> Daily simulations ■■■■■■■■■■■■■■■■■■■■■■■■■■■■      89% | ETA:  4s
#> Daily simulations ■■■■■■■■■■■■■■■■■■■■■■■■■■■■      90% | ETA:  3s
#> Daily simulations ■■■■■■■■■■■■■■■■■■■■■■■■■■■■      91% | ETA:  3s
#> Daily simulations ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■     92% | ETA:  3s
#> Daily simulations ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■     93% | ETA:  2s
#> Daily simulations ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■     94% | ETA:  2s
#> Daily simulations ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■    96% | ETA:  2s
#> Daily simulations ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■    97% | ETA:  1s
#> Daily simulations ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■    98% | ETA:  1s
#> Daily simulations ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■   99% | ETA:  0s
#> Daily simulations ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100% | ETA:  0s
#> • Snowpack balance content (mm): 0 fluxes (mm): 0
#> • Soil balance content (mm): -46.37 fluxes (mm): -46.37
#> • Aquifer balance content (mm): 23.7 fluxes (mm): 23.7
#> • Aquifer fluxes (mm) Drainage input: 88.62 Exfiltration: 64.92 Capillary rise:
#> 0 Negative aquifer correction: 0 Deep loss: 0
#> • Watershed balance content (mm): -21.97 fluxes (mm): -21.97
#> • Watershed fluxes (mm) Precipitation: 154.74 Surface export: 64.92
#> Evapotransp.: 111.79 Negative aquifer correction: 0 Deep loss: 0
#> 
#> ── FINAL BALANCE CHECK ──
#> 
#> • Final channel sum (m3): 0
#> • Final outlet sum (m3): 42871
#> • Final watershed export sum (m3): 42871
# }
```
