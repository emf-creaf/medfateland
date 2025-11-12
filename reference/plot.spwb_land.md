# Displays watershed-level simulation results

Plots time series of the watershed-level balance results of simulations
with `spwb_land`, `growth_land` or `fordyn_land`.

## Usage

``` r
# S3 method for class 'spwb_land'
plot(x, type = "Hydrograph_Hietograph", dates = NULL, summary.freq = NULL, ...)

# S3 method for class 'growth_land'
plot(x, type = "Hydrograph_Hietograph", dates = NULL, summary.freq = NULL, ...)

# S3 method for class 'fordyn_land'
plot(x, type = "Hydrograph_Hietograph", dates = NULL, summary.freq = NULL, ...)
```

## Arguments

- x:

  An object of class `spwb_land`, `growth_land` or `fordyn_land`.

- type:

  The information to be plotted (see details).

- dates:

  A Date vector with a subset of dates to be plotted.

- summary.freq:

  Frequency of summary statistics (see
  [`cut.Date`](https://rdrr.io/r/base/cut.POSIXt.html)).

- ...:

  Additional parameters for function `plot` (not used).

## Value

A ggplot object

## Details

The following plots are currently available:

- `"Hydrograph_Hietograph"`: A combination of hydrograph and hietograph
  (in a secondary, reversed, axis).

- `"PET_Precipitation"`: Potential evapotranspiration, rainfall and
  snow.

- `"Channel"`: Partitioning of overall discharge into discharge from
  channel and direct outlet discharge. Channel loading is also shown to
  evidence the routing effect.

- `"Export"`: Water exported through different fluxes: (total watershed
  export, channel routing, deep drainage).

- `"Overland_Runoff"`: Origin of overland runoff flows (i.e. saturation
  excess or infiltration excess).

- `"Aquifer_Balance"`: Water exchanged between soil and aquifer (i.e.
  soil deep drainage and capillarity rise).

- `"Evapotranspiration"`: Interception, woody transpiration, herb
  transpiration and soil evaporation.

## See also

[`spwb_land`](https://emf-creaf.github.io/medfateland/reference/spwb_land.md),
[`growth_land`](https://emf-creaf.github.io/medfateland/reference/spwb_land.md),
[`fordyn_land`](https://emf-creaf.github.io/medfateland/reference/spwb_land.md),
[`plot_summary`](https://emf-creaf.github.io/medfateland/reference/plot_summary.md),
[`shinyplot.spwb_land`](https://emf-creaf.github.io/medfateland/reference/shinyplot.spwb_land.md)

## Author

Miquel De Cáceres Ainsa, CREAF
