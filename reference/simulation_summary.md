# Summarizes spatial simulation results

Creates spatial objects containing summaries of simulations

## Usage

``` r
simulation_summary(object, summary_function, ...)
```

## Arguments

- object:

  An object of class 'sf' simulation results (e.g. the result of calling
  [`spwb_spatial`](https://emf-creaf.github.io/medfateland/reference/spwb_spatial.md)).

- summary_function:

  The summary function to be executed on simulation results (see
  details).

- ...:

  Additional parameters to the summary function.

## Value

An object of class
[`sf`](https://r-spatial.github.io/sf/reference/sf.html), with the
following two elements:

- `geometry`: Spatial geometry.

- `id`: Stand id, taken from the input.

- `summary`: A list of model output summaries for each simulated
  location.

## Details

The function supplied should take as input an object of local simulation
function, i.e.
[`spwb`](https://emf-creaf.github.io/medfate/reference/spwb.html),
[`growth`](https://emf-creaf.github.io/medfate/reference/growth.html),
or
[`fordyn`](https://emf-creaf.github.io/medfate/reference/fordyn.html).
The output should be a matrix with dates as rows and variables in
columns. An example of suitable function is
[`summary.spwb`](https://emf-creaf.github.io/medfate/reference/summary.spwb.html).

Functions
[`plot_summary`](https://emf-creaf.github.io/medfateland/reference/plot_summary.md)
and
[`unnest_summary`](https://emf-creaf.github.io/medfateland/reference/unnest_summary.md)
can be later used to plot and reformat summaries, respectively.

## See also

[`spwb_spatial`](https://emf-creaf.github.io/medfateland/reference/spwb_spatial.md),
[`plot_summary`](https://emf-creaf.github.io/medfateland/reference/plot_summary.md),
[`unnest_summary`](https://emf-creaf.github.io/medfateland/reference/unnest_summary.md)

## Author

Miquel De Cáceres Ainsa, CREAF.
