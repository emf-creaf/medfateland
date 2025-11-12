# Landscape forest parametrization

Utility functions to define forest inputs in a landscape:

- `impute_forests()` performs imputation of forest objects from a forest
  inventory using a forest map to match forest types and topography as
  covariates.

- `modify_forest_structure()` uses forest structure rasters supplied by
  the user to correct forest structure metrics.

- [`check_forests()`](https://emf-creaf.github.io/medfateland/reference/check_inputs.md)
  checks that forests are defined and do not contain missing values in
  key tree/shrub attributes.

## Usage

``` r
impute_forests(
  x,
  sf_fi,
  dem,
  forest_map,
  var_class = NA,
  max_distance_km = 100,
  replace_existing = FALSE,
  missing_class_imputation = FALSE,
  missing_class_forest = NULL,
  merge_trees = TRUE,
  merge_shrubs = TRUE,
  progress = TRUE
)

modify_forest_structure(
  x,
  structure_map,
  variable,
  map_var = NA,
  ratio_limits = NULL,
  minDBH = 7.5,
  biomass_function = NULL,
  biomass_arguments = NULL,
  SpParams = NULL,
  progress = TRUE
)
```

## Arguments

- x:

  An object of class
  [`sf`](https://r-spatial.github.io/sf/reference/sf.html). If it
  contains a column named 'land_cover_type', imputation will be
  performed for locations whose land cover is "wildland". Otherwise,
  forest imputation is done for all locations. For structural
  corrections or when checking, `x` should already contain a column
  named 'forest' containing
  [`forest`](https://emf-creaf.github.io/medfate/reference/forest.html)
  objects.

- sf_fi:

  An object of class
  [`sf`](https://r-spatial.github.io/sf/reference/sf.html) with forest
  inventory data column 'forest'.

- dem:

  A digital elevation model (class
  [`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html))
  with meters as units

- forest_map:

  An object of class
  [`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  or
  [`SpatVector`](https://rspatial.github.io/terra/reference/SpatVector-class.html)
  with the forest class map

- var_class:

  Variable name or index containing forest classes in `forest_map`. If
  missing the first column is taken.

- max_distance_km:

  Maximum distance, in km, for forest inventory plot imputation.

- replace_existing:

  A logical flag to force the replacement of existing
  [`forest`](https://emf-creaf.github.io/medfate/reference/forest.html)
  objects, when present.

- missing_class_imputation:

  A logical flag to force imputation in locations where forest class is
  not defined. If `missing_class_imputation = TRUE`, imputation in those
  locations will be based on geographic and topographic criteria only.

- missing_class_forest:

  A
  [`forest`](https://emf-creaf.github.io/medfate/reference/forest.html)
  object to be used for locations with missing class.

- merge_trees:

  A logical flag to simplify tree cohorts by merging tree records in DBH
  classes (see
  [`forest_mergeTrees`](https://emf-creaf.github.io/medfate/reference/forest_simplification.html)).

- merge_shrubs:

  A logical flag to simplify shrub cohorts by merging shrub records in
  height classes (see
  [`forest_mergeShrubs`](https://emf-creaf.github.io/medfate/reference/forest_simplification.html)).

- progress:

  A logical flag to print console output.

- structure_map:

  An object of class
  [`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  or
  [`SpatVector`](https://rspatial.github.io/terra/reference/SpatVector-class.html)
  with a forest structural variable map

- variable:

  Structural variable to correct. See options in details.

- map_var:

  Variable name or index containing structural variable in
  'structure_map'. If missing the first column is taken.

- ratio_limits:

  Limits for ratio of variable in corrections, used to avoid outliers.

- minDBH:

  Minimum diameter for stand metric calculation. If `minDBH > 0` then
  those stands with smaller trees will not be corrected because of the
  missing stand metric. A special case occurs for correction following
  basal area (see details).

- biomass_function:

  A function accepting a forest object or a tree data table (as
  parameter name `x`) and returning the aboveground tree biomass (Mg/ha)
  of the forest stand. The function may accept additional arguments.
  Needed if `variable = "aboveground_tree_biomass"`.

- biomass_arguments:

  List with additional arguments to be supplied to the biomass function.

- SpParams:

  A data frame with species parameters (see
  [`SpParamsMED`](https://emf-creaf.github.io/medfate/reference/SpParams.html)).
  This will be passed onto `biomass_function`, but can be left as `NULL`
  if not actually needed.

## Value

Functions `impute_forests()` and `modify_forest_structure()` return a
modified object of class
[`sf`](https://r-spatial.github.io/sf/reference/sf.html). Function
[`check_forests()`](https://emf-creaf.github.io/medfateland/reference/check_inputs.md)
returns an invisible data frame with columns indicating missing forest
data and missing values in tree or shrub parameters.

## Details

Function `impute_forests()` performs imputation of forest inventory
plots on target locations provided that they correspond to the same
forest class, defined in the input forest map, and are geographically
closer than a distance threshold (`max_distance_km`). Among the multiple
stands that can have fulfill these two requirements, the function
chooses the one that has the most similar elevation and position in the
N-to-S slopes (i.e. the product of the cosine of aspect and slope). Both
topographic features are standardized to zero mean and unit standard
deviation (using the supplied digital elevation model to calculate those
metrics), to make their weight on the imputation equal. This imputation
method will be more or less successful depending on the resolution of
forest classes and the number of forest inventory plots available for
each of them. Additionally, tree and shrub cohorts can be simplified
after imputation (`merge_trees` and `merge_shrubs`), to reduce the
number of records (and hence, speed-up simulations).

Function `modify_forest_structure()` can be used to modify specific
structure variables of the imputed forests building on rasters supplied
by the user (typically from aerial or satellite LiDAR products). For any
given metric, the function will calculate the ratio of the structure
metric between the target
[`forest`](https://emf-creaf.github.io/medfate/reference/forest.html)
object (see
[`stand_basalArea`](https://emf-creaf.github.io/medfate/reference/stand_values.html))
and the input map in the target location. Options for structural
variables are the following:

- `mean_tree_height`: Should contain values in cm. Corrects tree heights
  and diameters (assuming a constant diameter-height relationship).

- `dominant_tree_height`: Should contain values in cm. Corrects tree
  heights and diameters (assuming a constant diameter-height
  relationship).

- `tree_density`: Should contain values in individuals per hectare.
  Corrects tree density.

- `basal_area`: Should contain values in squared meters per hectare
  (m2/ha). Corrects tree density.

- `aboveground_tree_biomass`: Should contain values in tons per hectare
  (Mg/ha) of aboveground tree dry weight. Corrects tree density.

Locations where the metric value in the map is missing are left
unmodified. The same happens if metric value is zero, to avoid division
by zero. A special case occurs for correction of basal area or
aboveground tree biomass. In those cases, if there are no trees larger
than `minDBH` but structural map indicates positive values of basal area
or aboveground tree biomass, DBH values will be set to minDBH, and
correction will be performed.

## See also

[`add_topography()`](https://emf-creaf.github.io/medfateland/reference/add_topography.md),
[`add_forests()`](https://emf-creaf.github.io/medfateland/reference/add_forests.md),
[`add_soilgrids()`](https://emf-creaf.github.io/medfateland/reference/soil_parametrization.md),
[`forest_mergeTrees`](https://emf-creaf.github.io/medfate/reference/forest_simplification.html)

## Author

Miquel De Cáceres Ainsa, CREAF

Rodrigo Balaguer-Romano, CREAF

## Examples

``` r
# See package vignettes 'Preparing inputs'
```
