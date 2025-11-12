# Shiny app with interactive plots and maps of watershed simulation results

Creates a shiny app with interactive plots for simulation results

## Usage

``` r
# S3 method for class 'spwb_land'
shinyplot(x, r = NULL, ...)

# S3 method for class 'growth_land'
shinyplot(x, r = NULL, ...)

# S3 method for class 'fordyn_land'
shinyplot(x, r = NULL, ...)
```

## Arguments

- x:

  An object of class
  [`spwb_land`](https://emf-creaf.github.io/medfateland/reference/spwb_land.md),
  [`growth_land`](https://emf-creaf.github.io/medfateland/reference/spwb_land.md)
  or
  [`fordyn_land`](https://emf-creaf.github.io/medfateland/reference/spwb_land.md).

- r:

  An object of class
  [`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html),
  defining the raster topology.

- ...:

  Additional parameters for function shinyplot (not used).

## Value

An object that represents the shiny app

## See also

[`spwb_land`](https://emf-creaf.github.io/medfateland/reference/spwb_land.md),
[`plot.spwb_land`](https://emf-creaf.github.io/medfateland/reference/plot.spwb_land.md),
[`plot_variable`](https://emf-creaf.github.io/medfateland/reference/extract_variables.md)

## Author

Miquel De Cáceres Ainsa, CREAF
