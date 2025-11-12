# Add topography and land cover

Initializes topography and land cover type for a set of target locations

## Usage

``` r
add_topography(x, dem, progress = TRUE)

add_land_cover(
  x,
  land_cover_map,
  wildland = NULL,
  agriculture = NULL,
  rock = NULL,
  artificial = NULL,
  water = NULL,
  progress = TRUE
)
```

## Arguments

- x:

  An object of class
  [`sf`](https://r-spatial.github.io/sf/reference/sf.html)

- dem:

  A digital elevation model (class
  [`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html))
  with meters as units

- progress:

  A logical flag to print console output

- land_cover_map:

  An object of class
  [`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  of land cover type. If missing, all locations are considered
  'wildland'.

- wildland, agriculture, rock, artificial, water:

  Strings indicating the mapping from the legend of land_cover_map.

## Value

Function `add_topography()` returns a modified object of class
[`sf`](https://r-spatial.github.io/sf/reference/sf.html) with columns:

- `id`: Numeric location identifiers (if not existing).

- `elevation`: Elevation above sea level (in m).

- `slope`: Slope (in degrees).

- `aspect`: Aspect (in degrees).

- `land_cover_type`: Land cover type.

Function `add_land_cover()` returns a modified object of class
[`sf`](https://r-spatial.github.io/sf/reference/sf.html) with new
column:

- `id`: Numeric location identifiers (if not existing).

- `land_cover_type`: Land cover type.

## Details

The user should manually define the mapping of land cover classes in
`land_cover_map` to the land cover types used in medfateland.

## See also

[`check_topography()`](https://emf-creaf.github.io/medfateland/reference/check_inputs.md),
[`check_land_cover()`](https://emf-creaf.github.io/medfateland/reference/check_inputs.md)

## Examples

``` r
# See package vignettes 'Preparing inputs'
```
