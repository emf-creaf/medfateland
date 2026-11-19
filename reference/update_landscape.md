# Updates the state of a landscape object

Updates the state of a spatial object 'x' according to the final state
in simulation outcome 'y'

## Usage

``` r
update_landscape(
  x,
  y,
  columns = c("state", "soil", "forest", "management_arguments", "aquifer", "snowpack",
    "outlet_backlog")
)
```

## Arguments

- x:

  An object of class
  [`sf`](https://r-spatial.github.io/sf/reference/sf.html) with the
  corresponding landscape columns.

- y:

  The object resulting of a simulation previously carried on `x` using
  [`spwb_spatial`](https://emf-creaf.github.io/medfateland/reference/spwb_spatial.md),
  [`growth_spatial`](https://emf-creaf.github.io/medfateland/reference/spwb_spatial.md),
  [`spwb_land`](https://emf-creaf.github.io/medfateland/reference/spwb_land.md),
  etc.

- columns:

  String vector with the names of columns to be modified

## Value

An object of class
[`sf`](https://r-spatial.github.io/sf/reference/sf.html) with modified
state variables.

## See also

[`spwb_spatial`](https://emf-creaf.github.io/medfateland/reference/spwb_spatial.md),
[`spwb_spatial_day`](https://emf-creaf.github.io/medfateland/reference/spwb_spatial_day.md),
[`spwb_land`](https://emf-creaf.github.io/medfateland/reference/spwb_land.md)

## Author

Miquel De Cáceres Ainsa, CREAF.
