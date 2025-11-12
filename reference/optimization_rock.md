# Rock optimization

Optimization of rock fragment content

## Usage

``` r
optimization_rock(
  sf,
  SpParams,
  meteo = NULL,
  local_control = defaultControl(),
  dates = NULL,
  parallelize = FALSE,
  num_cores = detectCores() - 1,
  chunk_size = NULL,
  PLCquantile = 0.9,
  qPLC_target = 12,
  qPLC_tol = 0.5,
  sew_min = 30,
  max_rocks = 99,
  progress = TRUE
)
```

## Arguments

- sf:

  An object of class
  [`sf`](https://r-spatial.github.io/sf/reference/sf.html) (see
  [`spwb_spatial`](https://emf-creaf.github.io/medfateland/reference/spwb_spatial.md)).

- SpParams:

  A data frame with species parameters (see
  [`SpParamsMED`](https://emf-creaf.github.io/medfate/reference/SpParams.html)).

- meteo:

  Input meteorological data (see section details in
  [`spwb_spatial`](https://emf-creaf.github.io/medfateland/reference/spwb_spatial.md)).

- local_control:

  A list of control parameters (see
  [`defaultControl`](https://emf-creaf.github.io/medfate/reference/defaultControl.html)).

- dates:

  A [`Date`](https://rdrr.io/r/base/Dates.html) object describing the
  days of the period to be modeled.

- parallelize:

  Boolean flag to try parallelization (will use all clusters minus one).

- num_cores:

  Integer with the number of cores to be used for parallel computation.

- chunk_size:

  Integer indicating the size of chuncks to be sent to different
  processes (by default, the number of spatial elements divided by the
  number of cores).

- PLCquantile:

  Maximum PLC quantile to be calculated across years.

- qPLC_target:

  Target PLC to be achieved (by default 12%).

- qPLC_tol:

  Tolerance of PLC difference to target accepted when finding solution.

- sew_min:

  Minimum soil extractable water (mm) for rock exploration.

- max_rocks:

  Maximum content in coarse fragments allowed for any soil layer.

- progress:

  Boolean flag to display progress information of simulations.

## Value

An object of class
[`sf`](https://r-spatial.github.io/sf/reference/sf.html) with a modified
`soil` column and an additional column `optimization_message` with text
information about the optimization.

## See also

[`utils_rockOptimization`](https://emf-creaf.github.io/medfate/reference/utils_rockOptimization.html)

## Author

Miquel De Cáceres Ainsa, CREAF

## Examples

``` r
# \donttest{
data("example_ifn")
data("examplemeteo")
data("SpParamsMED")
example_subset <- example_ifn[31:32, ]
optimization_rock(example_subset, SpParamsMED, examplemeteo)
#> ℹ Checking meteo object input
#> ✔ Checking meteo object input [7ms]
#> 
#> ℹ Rock optimization on 2 locations
#> ✔ Rock optimization on 2 locations [16ms]
#> 
#> Stands ■■■■■■■■■■■■■■■■                  50% | ETA:  8s
#> Stands ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100% | ETA:  0s
#> Simple feature collection with 2 features and 8 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 1.901727 ymin: 41.96974 xmax: 1.925861 ymax: 41.96997
#> Geodetic CRS:  WGS 84
#> # A tibble: 2 × 9
#>                  geom id        elevation slope aspect land_cover_type soil  
#>           <POINT [°]> <chr>         <dbl> <dbl>  <dbl> <chr>           <list>
#> 1 (1.901727 41.96974) 081047_A1       478  12.0   259. wildland        <df>  
#> 2 (1.925861 41.96997) 081048_A1       540  16.4   109. wildland        <df>  
#> # ℹ 2 more variables: forest <list>, optimization_message <chr>
# }
```
