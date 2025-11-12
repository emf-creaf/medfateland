# Fire regime instance

Applies a fire regime object over a set of landscape units to determine
a fire realization

## Usage

``` r
fire_regime_instance(sf, fire_regime)
```

## Arguments

- sf:

  An object of class
  [`sf`](https://r-spatial.github.io/sf/reference/sf.html) with the
  following columns:

  - `geometry`: Spatial geometry.

  - `id`: Stand identifiers.

  - `represented_area_ha`: Area represented by each stand (in hectares).

  - `ignition_weights`: Relative weights to determine stands to be
    burned (optional).

- fire_regime:

  A list of parameters defining the fire regime (see
  [`create_fire_regime`](https://emf-creaf.github.io/medfateland/reference/create_fire_regime.md)).

## Value

An integer matrix specifying the day of the year of burning of each
landscape unit for every year in the fire regime definition. Values are
interpreted as follows:

- NA - No wildfire this year

- 0 - Wildfire will occur the driest day (i.e. the one with largest
  vapor pressure deficit).

- 1...366 - Day of the year when wildfire will occur

## Details

The function randomly determines the landscape units that will burn
every year, depending on the specifications of the fire regime object.
Users can define their own fire regime instances from other models (e.g.
a fire landscape model) and then use those directly in functions
[`fordyn_spatial`](https://emf-creaf.github.io/medfateland/reference/spwb_spatial.md)
or
[`fordyn_scenario`](https://emf-creaf.github.io/medfateland/reference/fordyn_scenario.md).

## See also

[`create_fire_regime`](https://emf-creaf.github.io/medfateland/reference/create_fire_regime.md),
[`fordyn_spatial`](https://emf-creaf.github.io/medfateland/reference/spwb_spatial.md),
[`fordyn_scenario`](https://emf-creaf.github.io/medfateland/reference/fordyn_scenario.md)

## Author

Miquel De Cáceres Ainsa, CREAF

## Examples

``` r
# Load example data
data("example_ifn")

# Assume that each stand represents 1km2 = 100 ha
example_ifn$represented_area_ha <- 100

# Define fire regime characteristics
reg1 <- create_fire_regime(c("2002" = 200, "2003" = 500)) 

# Create a fire regime instance
m1 <- fire_regime_instance(example_ifn, reg1)

# Check number of plots burned
colSums(!is.na(m1))
#> 2002 2003 
#>    2    5 

# Define fire regime characteristics with stochastic area burned
reg2 <- create_fire_regime(annual_burned_area = c("2002" = 200, "2003" = 500),
                           sd_burned_area = c("2002" = 0.4, "2003" = 0.5)) 

# Create a fire regime instance
m2 <- fire_regime_instance(example_ifn, reg2)

# Check number of plots burned
colSums(!is.na(m2))
#> 2002 2003 
#>    2    3 
```
