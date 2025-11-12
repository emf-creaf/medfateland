# Extracts cell simulation summaries

Extracts cell simulation summaries

## Usage

``` r
unnest_summary(x)
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

## Value

An sf object with all cell summaries.

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

[`plot_summary`](https://emf-creaf.github.io/medfateland/reference/plot_summary.md),
[`spwb_spatial`](https://emf-creaf.github.io/medfateland/reference/spwb_spatial.md),
[`spwb_land`](https://emf-creaf.github.io/medfateland/reference/spwb_land.md),
[`simulation_summary`](https://emf-creaf.github.io/medfateland/reference/simulation_summary.md)
