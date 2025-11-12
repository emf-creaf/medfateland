# Initialization of model inputs for spatially-distributed forest stands

Initializes state for local models
[`spwb`](https://emf-creaf.github.io/medfate/reference/spwb.html) or
[`growth`](https://emf-creaf.github.io/medfate/reference/growth.html).

## Usage

``` r
initialize_landscape(
  x,
  SpParams,
  local_control,
  model = "spwb",
  merge_trees = FALSE,
  merge_shrubs = FALSE,
  reduce_to_dominant = FALSE,
  replace = FALSE,
  progress = TRUE
)
```

## Arguments

- x:

  An object of class
  [`sf`](https://r-spatial.github.io/sf/reference/sf.html) with the
  following columns:

  - `geometry`: Spatial geometry.

  - `forest`: Objects of class
    [`forest`](https://emf-creaf.github.io/medfate/reference/forest.html).

  - `soil`: Objects of class
    [`soil`](https://emf-creaf.github.io/medfate/reference/soil.html) or
    data frames of physical properties.

  - `land_cover_type`: Land cover type of each grid cell (values should
    be 'wildland' or 'agriculture').

  - `crop_factor`: Crop evapo-transpiration factor. Only required for
    'agriculture' land cover type.

  - `local_control`: A list of control parameters (optional). Used to
    override function parameter `local_control` for specific cells
    (values can be `NULL` for the remaining ones).

- SpParams:

  A data frame with species parameters (see
  [`SpParamsMED`](https://emf-creaf.github.io/medfate/reference/SpParams.html)).

- local_control:

  A list of control parameters (see
  [`defaultControl`](https://emf-creaf.github.io/medfate/reference/defaultControl.html)).

- model:

  A string to indicate the model, either `"spwb"` or `"growth"`.

- merge_trees:

  A logical flag to simplify tree cohorts by merging tree records in DBH
  classes (see
  [`forest_mergeTrees`](https://emf-creaf.github.io/medfate/reference/forest_simplification.html)).

- merge_shrubs:

  A logical flag to simplify shrub cohorts by merging shrub records in
  height classes (see
  [`forest_mergeShrubs`](https://emf-creaf.github.io/medfate/reference/forest_simplification.html)).

- reduce_to_dominant:

  Boolean flag to simplify forest to the tree and shrub cohorts with
  largest leaf area index. The leaf area index of the whole tree
  (respectively, shrub) layer will be attributed to the selected cohort.
  See function
  [`forest_reduceToDominant`](https://emf-creaf.github.io/medfate/reference/forest_simplification.html).

- replace:

  Boolean flag to replace existing initialized states

- progress:

  Boolean flag to display progress information.

## Value

Replaces or adds column 'state' whose elements are
[`spwbInput`](https://emf-creaf.github.io/medfate/reference/modelInput.html)
or
[`growthInput`](https://emf-creaf.github.io/medfate/reference/modelInput.html)
objects and returns the modified object of class 'sf'.

## Details

Initialization is normally dealt automatically when calling simulation
functions
[`spwb_spatial`](https://emf-creaf.github.io/medfateland/reference/spwb_spatial.md),
[`growth_spatial`](https://emf-creaf.github.io/medfateland/reference/spwb_spatial.md),
[`spwb_spatial_day`](https://emf-creaf.github.io/medfateland/reference/spwb_spatial_day.md)
or
[`growth_spatial_day`](https://emf-creaf.github.io/medfateland/reference/spwb_spatial_day.md).
However, function `initialize_landscape` allows separating
initialization from model simulations.

Options `merge_shrubs`, `merge_trees` and `reduce_to_dominant` have been
implemented to allow simplification of forests during watershed
simulations where focus is on runoff (e.g. calibration of watershed
parameters or burnin periods). Elements identified as `result_cell` will
not be simplified.

## See also

[`spwb_spatial`](https://emf-creaf.github.io/medfateland/reference/spwb_spatial.md),
[`spwb_spatial_day`](https://emf-creaf.github.io/medfateland/reference/spwb_spatial_day.md),
[`update_landscape`](https://emf-creaf.github.io/medfateland/reference/update_landscape.md)

## Author

Miquel De Cáceres Ainsa, CREAF

## Examples

``` r
# Load example landscape data
data("example_ifn")
  
# Load example meteo data frame from package meteoland
data("examplemeteo")
  
# Load default medfate parameters
data("SpParamsMED")

# Define local control parameters using function in medfate
local_control <- defaultControl()

# If necessary, change defaults

# Initialize state for 'spwb' simulations
example_ifn_init <- initialize_landscape(example_ifn, SpParamsMED, 
                                         local_control = local_control, 
                                         model = "spwb")
#> ℹ Creating 100 state objects for model 'spwb'.
#> ✔ Creating 100 state objects for model 'spwb'. [8ms]
#> 
#> Stands ■■■■■■■■■■                        31% | ETA:  2s
#> Stands ■■■■■■■■■■■■                      38% | ETA:  2s
#> Stands ■■■■■■■■■■■■■■                    42% | ETA:  2s
#> Stands ■■■■■■■■■■■■■■■■                  49% | ETA:  2s
#> Stands ■■■■■■■■■■■■■■■■■■■■              62% | ETA:  1s
#> Stands ■■■■■■■■■■■■■■■■■■■■■             67% | ETA:  1s
#> Stands ■■■■■■■■■■■■■■■■■■■■■■■           75% | ETA:  1s
#> Stands ■■■■■■■■■■■■■■■■■■■■■■■■■■        82% | ETA:  1s
#> Stands ■■■■■■■■■■■■■■■■■■■■■■■■■■■■      89% | ETA:  0s
#> Stands ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■     95% | ETA:  0s
#> Stands ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100% | ETA:  0s
#> • Transpiration mode [Granier: 100, Sperry: 0, Sureau: 0]
#> • Soil domains [buckets: 100, single: 0, dual: 0]
```
