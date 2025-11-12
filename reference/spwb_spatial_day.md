# One-day simulation for spatially-distributed forest stands

Functions that allow calling local models
[`spwb_day`](https://emf-creaf.github.io/medfate/reference/spwb_day.html)
or
[`growth_day`](https://emf-creaf.github.io/medfate/reference/spwb_day.html),
for a set of forest stands distributed in specific locations and a given
date. No spatial processes are simulated.

## Usage

``` r
spwb_spatial_day(
  sf,
  meteo = NULL,
  date,
  SpParams,
  local_control = defaultControl(),
  parallelize = FALSE,
  num_cores = detectCores() - 1,
  chunk_size = NULL,
  progress = TRUE
)

growth_spatial_day(
  sf,
  meteo = NULL,
  date,
  SpParams,
  local_control = defaultControl(),
  parallelize = FALSE,
  num_cores = detectCores() - 1,
  chunk_size = NULL,
  progress = TRUE
)
```

## Arguments

- sf:

  An object of class
  [`sf`](https://r-spatial.github.io/sf/reference/sf.html) with
  landscape information (see
  [`spwb_spatial`](https://emf-creaf.github.io/medfateland/reference/spwb_spatial.md)).

- meteo:

  Meteorology data (see
  [`spwb_spatial`](https://emf-creaf.github.io/medfateland/reference/spwb_spatial.md)).

- date:

  A string with the date to be simulated.

- SpParams:

  A data frame with species parameters (see
  [`SpParamsMED`](https://emf-creaf.github.io/medfate/reference/SpParams.html)).

- local_control:

  A list of local model control parameters (see
  [`defaultControl`](https://emf-creaf.github.io/medfate/reference/defaultControl.html)).

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

## Value

An object of class
[`sf`](https://r-spatial.github.io/sf/reference/sf.html) the same name
as the function called containing three elements:

- `geometry`: Spatial geometry.

- `id`: Stand id, taken from the input.

- `state`: A list of model input objects for each simulated stand, to be
  used in subsequent simulations.

- `result`: A list of model output for each simulated stand.

## Details

Simulation functions accept different formats for meteorological input
(described in
[`spwb_spatial`](https://emf-creaf.github.io/medfateland/reference/spwb_spatial.md)).

## See also

[`spwb_day`](https://emf-creaf.github.io/medfate/reference/spwb_day.html),
[`growth_day`](https://emf-creaf.github.io/medfate/reference/spwb_day.html),
[`spwb_spatial`](https://emf-creaf.github.io/medfateland/reference/spwb_spatial.md)

## Author

Miquel De Cáceres Ainsa, CREAF

## Examples

``` r
# \donttest{
#Load example landscape data
data("example_ifn")

#Load example meteo data frame from package meteoland
data("examplemeteo")
  
#Load default medfate parameters
data("SpParamsMED")
  
#Perform simulation
date <- "2001-03-01"
res <- spwb_spatial_day(example_ifn, examplemeteo, date, SpParamsMED)
#> 
#> ── Simulation of model 'spwb' for date '2001-03-01' ────────────────────────────
#> ℹ Checking sf input
#> ✔ Checking sf input [13ms]
#> 
#> ℹ Checking meteo object input
#> ✔ Checking meteo object input [22ms]
#> 
#> ℹ Creating '100' input objects.
#> ✔ Creating '100' input objects. [4.6s]
#> 
#> • Performing 'spwb' simulations.
#> Stands ■■■■■■■■■■■■■■                    44% | ETA:  2s
#> Stands ■■■■■■■■■■■■■■■■■■■■■■■■■         80% | ETA:  1s
#> Stands ■■■■■■■■■■■■■■■■■■■■■■■■■■        84% | ETA:  0s
#> Stands ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■     93% | ETA:  0s
#> Stands ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100% | ETA:  0s
#> ✔ No simulation errors detected
# }
```
