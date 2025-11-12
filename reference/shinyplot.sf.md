# Shiny app with interactive plots and maps

Creates a shiny app with interactive plots for spatial inputs and
simulation results

## Usage

``` r
# S3 method for class 'sf'
shinyplot(x, SpParams = NULL, r = NULL, ...)
```

## Arguments

- x:

  The object of class 'sf' containing information to be drawn (see
  details).

- SpParams:

  A data frame with species parameters (see
  [`SpParamsMED`](https://emf-creaf.github.io/medfate/reference/SpParams.html)),
  required for most forest stand variables.

- r:

  An object of class
  [`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html),
  defining the raster topology.

- ...:

  Additional parameters for function shinyplot (not used).

## Value

An object that represents the shiny app

## Details

Only run this function in interactive mode. The shiny app can be used to
display spatial inputs or simulation results.

*Spatial inputs*: This is the case if the user supplies an object of
class [`sf`](https://r-spatial.github.io/sf/reference/sf.html) with
simulation inputs.

*Simulation result summaries*: This is the case if the user supplies an
object of class [`sf`](https://r-spatial.github.io/sf/reference/sf.html)
with simulation summaries. Available plots depend on the summary
function used to create the result summaries.

## See also

[`plot_summary`](https://emf-creaf.github.io/medfateland/reference/plot_summary.md),
[`extract_variables`](https://emf-creaf.github.io/medfateland/reference/extract_variables.md)

## Author

Miquel De Cáceres Ainsa, CREAF
