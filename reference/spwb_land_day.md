# One-day watershed simulations

Functions to perform one-day simulations on a watershed described by a
set of connected grid cells.

- Function `spwb_land_day` implements a distributed hydrological model
  that simulates daily local water balance, from
  [`spwb_day`](https://emf-creaf.github.io/medfate/reference/spwb_day.html),
  on grid cells of a watershed while accounting for overland runoff,
  subsurface flow and groundwater flow between cells.

- Function `growth_land_day` is similar to `spwb_land_day`, but includes
  daily local carbon balance, growth and mortality processes in grid
  cells, provided by
  [`growth_day`](https://emf-creaf.github.io/medfate/reference/spwb_day.html).

## Usage

``` r
spwb_land_day(
  r,
  sf,
  SpParams,
  meteo = NULL,
  date = NULL,
  local_control = medfate::defaultControl(soilDomains = "single"),
  watershed_control = default_watershed_control(),
  progress = TRUE
)

growth_land_day(
  r,
  sf,
  SpParams,
  meteo = NULL,
  date = NULL,
  local_control = medfate::defaultControl(soilDomains = "single"),
  watershed_control = default_watershed_control(),
  progress = TRUE
)
```

## Arguments

- r:

  An object of class
  [`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html),
  defining the raster topology.

- sf:

  An object of class
  [`sf`](https://r-spatial.github.io/sf/reference/sf.html) as described
  in
  [`spwb_land`](https://emf-creaf.github.io/medfateland/reference/spwb_land.md).

- SpParams:

  A data frame with species parameters (see
  [`SpParamsMED`](https://emf-creaf.github.io/medfate/reference/SpParams.html)).

- meteo:

  Input meteorological data (see
  [`spwb_spatial`](https://emf-creaf.github.io/medfateland/reference/spwb_spatial.md)
  and details).

- date:

  A string with the date to be simulated.

- local_control:

  A list of control parameters (see
  [`defaultControl`](https://emf-creaf.github.io/medfate/reference/defaultControl.html))
  for function
  [`spwb_day`](https://emf-creaf.github.io/medfate/reference/spwb_day.html)
  or
  [`growth_day`](https://emf-creaf.github.io/medfate/reference/spwb_day.html).

- watershed_control:

  A list of watershed control parameters (see
  [`default_watershed_control`](https://emf-creaf.github.io/medfateland/reference/default_watershed_control.md)).
  Importantly, the sub-model used for lateral water flows - either
  Francés et al. (2007) or Caviedes-Voullième et al. (2023) - is
  specified there.

- progress:

  Boolean flag to display progress information for simulations.

## Value

Functions `spwb_land_day` and `spwb_land_day` return a sf object:

- `geometry`: Spatial geometry.

- `state`: A list of model input objects for each simulated stand.

- `aquifer`: A numeric vector with the water volume in the aquifer of
  each cell.

- `snowpack`: A numeric vector with the snowpack water equivalent volume
  of each cell.

- `result`: A list of cell detailed results (only for those indicated in
  the input), with contents depending on the local model.

- `outlet`: A logical vector indicating outlet cells (for subsequent
  simulations).

- `outlet_backlog`: A vector indicating channel water volume (m3)
  backlog of outlet cells.

- `MinTemperature`: Minimum temperature (degrees Celsius).

- `MaxTemperature`: Maximum temperature (degrees Celsius).

- `PET`: Potential evapotranspiration (in mm).

- `Rain`: Rainfall (in mm).

- `Snow`: Snowfall (in mm).

- `Snowmelt`: Snow melt (in mm).

- `Interception`: Rainfall interception (in mm).

- `NetRain`: Net rainfall, i.e. throughfall, (in mm).

- `Infiltration`: The amount of water infiltrating into the soil (in
  mm).

- `InfiltrationExcess`: The amount of water exceeding the soil
  infiltration capacity (in mm).

- `SaturationExcess`: The amount of water that reaches the soil surface
  because of soil saturation (in mm).

- `Runoff`: The amount of water exported via surface runoff (in mm).

- `DeepDrainage`: The amount of water draining from soil to the aquifer
  via deep drainage (in mm).

- `CapillarityRise`: Water entering the soil via capillarity rise (mm)
  from the water table.

- `SoilEvaporation`: Bare soil evaporation (in mm).

- `Transpiration`: Woody plant transpiration (in mm).

- `HerbTranspiration`: Herbaceous transpiration (in mm).

- `InterflowInput`: The amount of water that reaches the soil of the
  cell from adjacent cells via subsurface flow (in mm).

- `InterflowOutput`: The amount of water that leaves the soil of the
  cell towards adjacent cells via subsurface flow (in mm).

- `InterflowBalance`: The balance of water circulating via subsurface
  flow (in mm).

- `BaseflowInput`: The amount of water that reaches the aquifer of the
  cell from adjacent cells via groundwater flow (in mm).

- `BaseflowOutput`: The amount of water that leaves the aquifer of the
  cell towards adjacent cells via groundwater flow (in mm).

- `BaseflowBalance`: The balance of water circulating via groundwater
  flow (in mm).

- `AquiferExfiltration`: The amount of water of the cell that generates
  surface runoff due to the aquifer reaching the soil surface (in mm).

## Details

See details in
[`spwb_land`](https://emf-creaf.github.io/medfateland/reference/spwb_land.md).
Subwatershed units and parallelization are not possible, at present, for
single-day watershed simulations.

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
[`spwb_day`](https://emf-creaf.github.io/medfate/reference/spwb_day.html),
[`growth_day`](https://emf-creaf.github.io/medfate/reference/spwb_day.html),
[`spwb_land`](https://emf-creaf.github.io/medfateland/reference/spwb_land.md),

## Author

Miquel De Cáceres Ainsa, CREAF.

Maria González-Sanchís, Universitat Politecnica de Valencia.

Daniel Caviedes-Voullième, Forschungszentrum Julich.

Mario Morales-Hernández, Universidad de Zaragoza.

## Examples

``` r
# Load example watershed data after burnin period
data("example_watershed_burnin")

# Set request for daily model results in cells number 3, 6 (outlet) and 9
example_watershed_burnin$result_cell <- FALSE
example_watershed_burnin$result_cell[c(3,6,9)] <- TRUE

# Get bounding box to determine limits
b <- sf::st_bbox(example_watershed_burnin)
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
  
# Watershed control parameters (TETIS model; Frances et al. 2007)
ws_control <- default_watershed_control("tetis")

# Launch simulation 
date <- "2001-03-01"
sf_out <- spwb_land_day(r, example_watershed_burnin, SpParamsMED, examplemeteo, 
                        date = date, 
                        watershed_control = ws_control)
#> 
#> ── Simulation of model 'spwb' over a watershed for day '2001-03-01' ────────────
#> ℹ Checking topology
#> ✔ Checking topology [27ms]
#> 
#> ℹ Checking 'sf' data
#> ✔ Checking 'sf' data [61ms]
#> 
#> ℹ Determining neighbors and discharge for TETIS
#> ✔ Determining neighbors and discharge for TETIS [55ms]
#> 
#> • Hydrological model: TETIS
#> • Number of grid cells: 120 Number of target cells: 66
#> • Average cell area: 10006 m2, Total area: 120 ha, Target area: 66 ha
#> • Cell land use wildland: 48 agriculture: 17 artificial: 0 rock: 1 water: 0
#> • Cells with soil: 65
#> • Number of cells with daily model results requested: 3
#> • Number of channel cells: 0
#> • Number of outlet cells: 1
#> ℹ Building spwb input
#> ✔ Building spwb input [16ms]
#> 
#> ℹ 0 cells needed initialization
#> ✔ 0 cells needed initialization [493ms]
#> 
```
