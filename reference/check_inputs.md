# Check spatial inputs

Functions to check and correct spatial inputs for simulations

## Usage

``` r
check_topography(
  x,
  missing_action = "no_action",
  default_values = c(elevation = 0, slope = NA, aspect = NA),
  verbose = TRUE
)

check_land_cover(
  x,
  missing_action = "no_action",
  default_values = "wildland",
  verbose = TRUE
)

check_forests(
  x,
  SpParams = NULL,
  missing_action = "no_action",
  default_forest = NULL,
  progress = FALSE,
  verbose = TRUE
)

check_soils(
  x,
  check_equal_layers = FALSE,
  missing_action = "no_action",
  default_values = c(clay = 25, sand = 25, bd = 1.5, rfc = 25),
  progress = FALSE,
  verbose = TRUE
)
```

## Arguments

- x:

  An object of class
  [`sf`](https://r-spatial.github.io/sf/reference/sf.html) to be
  checked.

- missing_action:

  Action to perform for missing values, either "no_action" (for checks),
  "filter" (filter missing data), "default" (impute default values)

- default_values:

  Vector of default values for locations with missing data.

- verbose:

  Logical flag to indicate extra console output.

- SpParams:

  A data frame with species parameters (see
  [`SpParamsMED`](https://emf-creaf.github.io/medfate/reference/SpParams.html)).

- default_forest:

  Default
  [`forest`](https://emf-creaf.github.io/medfate/reference/forest.html)
  object to fill locations where missing (e.g.
  `default_forest = medfate::emptyforest()`).

- progress:

  A logical flag to print information about progress.

- check_equal_layers:

  Logical flag to test whether soils have the same number of layers.

## Value

All functions return a modified
[`sf`](https://r-spatial.github.io/sf/reference/sf.html) object if
`missing_action` is either `"filter"` or `"default"`. Otherwise, they
return an invisible tibble with logical columns indicating where missing
information is. In `check_forests()` the function will return a modified
object if parameter `default_forest` is provided.

## Details

Function `check_topography()` checks that columns `"elevation"`,
`"slope"` and `"aspect"` do not contain missing values.

Function `check_land_cover()` checks that column `"land_cover_type"`
does not contain missing values.

Function `check_forests()` checks first that
[`forest`](https://emf-creaf.github.io/medfate/reference/forest.html)
objects are defined in "wildland" locations. Then, it looks for missing
data in tree or shrub attributes required for simulations. If `SpParams`
is provided, the function also checks whether species names are within
the taxa represented in `SpParams`. If `default_forest` is provided, the
function will use it to fill locations with missing forests objects.

Function `check_soils()` checks first that "wildland" and "agriculture"
locations have a defined soil object. Then it looks for missing data in
required soil physical parameters.

## Examples

``` r
data(example_ifn)

check_topography(example_ifn)
#> ✔ No missing values in topography.
check_land_cover(example_ifn)
#> ✔ No missing values in land cover.
check_forests(example_ifn)
#> ✔ No wildland locations with NULL values in column 'forest'.
#> ✔ All objects in column 'forest' have the right class.
#> ✔ No missing/wrong values detected in key tree/shrub attributes of 'forest' objects.
check_soils(example_ifn)
#> ✔ No wildland/agriculture locations with NULL values in column 'soil'.
#> ✔ No missing values detected in key soil attributes.
```
