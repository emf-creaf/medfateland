# Displays spatial simulation summaries

Produces graphical output of the summaries of a simulation models

## Usage

``` r
plot_summary(x, variable, date, r = NULL, ...)
```

## Arguments

- x:

  An object of class
  [`sf`](https://r-spatial.github.io/sf/reference/sf.html), with
  simulation summaries. Alternatively an object of class
  [`spwb_land`](https://emf-creaf.github.io/medfateland/reference/spwb_land.md),
  [`growth_land`](https://emf-creaf.github.io/medfateland/reference/spwb_land.md)
  or
  [`fordyn_land`](https://emf-creaf.github.io/medfateland/reference/spwb_land.md).

- variable:

  The variable to be drawn.

- date:

  The date of the summary to be plotted.

- r:

  An object of class
  [`SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html),
  defining the raster topology.

- ...:

  Additional parameters (passed to scale definition, such as `limits`).

## Value

An object of class
[`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html).

## Details

Appropriate input objects for `x` can be originated from calls to
[`simulation_summary`](https://emf-creaf.github.io/medfateland/reference/simulation_summary.md).
Alternatively, if summary functions were specified at the time of
performing simulations, the result of the spatial simulation function
(e.g.
[`spwb_spatial`](https://emf-creaf.github.io/medfateland/reference/spwb_spatial.md))
will already contain the summaries. A special case is made for
[`spwb_land`](https://emf-creaf.github.io/medfateland/reference/spwb_land.md),
[`growth_land`](https://emf-creaf.github.io/medfateland/reference/spwb_land.md),
and
[`fordyn_land`](https://emf-creaf.github.io/medfateland/reference/spwb_land.md),
that are accepted inputs as `x`, because their child element 'sf' is
used.

## See also

[`unnest_summary`](https://emf-creaf.github.io/medfateland/reference/unnest_summary.md),
[`spwb_spatial`](https://emf-creaf.github.io/medfateland/reference/spwb_spatial.md),
[`spwb_land`](https://emf-creaf.github.io/medfateland/reference/spwb_land.md),
[`simulation_summary`](https://emf-creaf.github.io/medfateland/reference/simulation_summary.md)

## Author

Miquel De Cáceres Ainsa, CREAF.
