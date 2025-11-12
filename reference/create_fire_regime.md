# Create fire regime

Defines an object containing fire regime parameters for simulations of
forest dynamics.

## Usage

``` r
create_fire_regime(annual_burned_area, sd_burned_area = NULL, doy = NULL)
```

## Arguments

- annual_burned_area:

  A named vector of burned area in hectares for simulation years.

- sd_burned_area:

  A named vector of standard deviation (in log scale) of burned area. If
  specified, annual target to burn will be determined using a log-normal
  distribution with mean values given by `annual_burned_area`.

- doy:

  A named integer vector with the day of the year (i.e. between 1
  and 366) when fires will be simulated for each simulation year in
  `annual_burned_area`. If NULL fires will be simulated on the driest
  day (i.e. when vapor pressure deficit is largest).

## Value

A list with the supplied parameters

## Details

Names of `annual_burned_area` should be simulation years. If provided,
`sd_burned_area` should be a vector of the same size as
`annual_burned_area` and have the same names.

## See also

[`fire_regime_instance`](https://emf-creaf.github.io/medfateland/reference/fire_regime_instance.md),
[`fordyn_scenario`](https://emf-creaf.github.io/medfateland/reference/fordyn_scenario.md),
[`fordyn_spatial`](https://emf-creaf.github.io/medfateland/reference/spwb_spatial.md)

## Author

Miquel De Cáceres Ainsa, CREAF

## Examples

``` r
# Fire regime with pre-defined burned area values
reg1 <- create_fire_regime(annual_burned_area = c("2002" = 1000, "2003" = 5000)) 

# Fire regime with log-normal distribution for burned area
reg2 <- create_fire_regime(annual_burned_area = c("2002" = 1000, "2003" = 5000),
                           sd_burned_area = c("2002" = 0.9, "2003" = 0.8)) 
```
